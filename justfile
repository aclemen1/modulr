#!/usr/bin/env -S just --justfile
#
# modulr — dev environment & test runner
#
# Quickstart:
#   just bootstrap        # one-shot: colima k3s profile + namespace + first build
#   just dev              # enter R 4.4.1 dev shell (default)
#   just dev r363         # enter R 3.6.3 dev shell
#   just test-all         # run the full suite on both R 3.6.3 and R 4.4.1
#   just teardown         # stop the K3S profile, keep images
#   just nuke             # full cleanup, including the K3S profile and PVCs

set shell := ["bash", "-cu"]
set positional-arguments

# Colima profile that hosts K3S for modulr dev.
PROFILE     := "modulr-k3s"
NS          := "modulr-dev"
KUBE_CTX    := "colima-modulr-k3s"
# colima --kubernetes uses a containerd shared with the docker daemon, so
# `docker build` against this context populates K3S' image store directly.
DOCKER_CTX  := "colima-modulr-k3s"

# All cluster-touching recipes inherit this env so devspace + docker target
# the right daemon regardless of the user's current shell context.
export DOCKER_CONTEXT := DOCKER_CTX

# ──────────────────────────────────────────────────────────────────────────
#  Help & status
# ──────────────────────────────────────────────────────────────────────────

# List recipes (default). `just` invoked with no recipe runs this one.
default:
    @just --list --unsorted

# Safety guard: refuse to operate on any cluster other than
# colima-modulr-k3s. Belt and braces against accidentally deploying into
# a work/prod cluster you happen to be currently kubectl-pointing at.
_assert-context:
    @actual=$(kubectl config current-context 2>/dev/null || echo "(none)"); \
    if [ "$actual" != "{{KUBE_CTX}}" ]; then \
      echo "✘ Refusing to act: kube context is '$actual', expected '{{KUBE_CTX}}'." >&2; \
      echo "  Run 'just colima-up' first, or 'kubectl config use-context {{KUBE_CTX}}'." >&2; \
      exit 1; \
    fi

# Show the state of every moving piece.
status:
    @echo "=== colima ==="
    @colima list 2>/dev/null || true
    @echo
    @echo "=== kubectl current context ==="
    @kubectl config current-context 2>/dev/null || echo "(none)"
    @echo
    @echo "=== modulr-dev workloads (in {{KUBE_CTX}}) ==="
    @kubectl --context {{KUBE_CTX}} -n {{NS}} get deploy,pvc,pod 2>/dev/null \
      || echo "(context or namespace absent — run 'just bootstrap')"

# ──────────────────────────────────────────────────────────────────────────
#  Provisioning
# ──────────────────────────────────────────────────────────────────────────

# One-shot bootstrap. Idempotent.
bootstrap: colima-up ns-up
    @echo
    @echo "✔ modulr dev environment ready."
    @echo "  Next: 'just dev' to enter the R 4.4.1 shell, or 'just dev r363'."

# Start the dedicated colima profile with K3S enabled.
# Runtime is docker (not containerd) so that `docker build` populates the
# same image store K3S reads from — no manual `ctr import` is needed.
colima-up:
    @if ! colima list 2>/dev/null | grep -q '^{{PROFILE}}'; then \
      echo "→ creating colima profile {{PROFILE}} with K3S…"; \
      colima start {{PROFILE}} --kubernetes --runtime docker \
        --cpu 4 --memory 8 --disk 30 --mount "$HOME:w"; \
    else \
      echo "→ {{PROFILE}} already exists; ensuring it is running…"; \
      colima start {{PROFILE}}; \
    fi
    @kubectl config use-context colima-{{PROFILE}}

# Create the dev namespace if absent.
ns-up: _assert-context
    @kubectl get ns {{NS}} >/dev/null 2>&1 \
      || kubectl create ns {{NS}}
    @kubectl config set-context --current --namespace={{NS}} >/dev/null

# ──────────────────────────────────────────────────────────────────────────
#  Build
# ──────────────────────────────────────────────────────────────────────────

# Build the dev image for one R version (default: 4.4.1).
build version="r441": _assert-context
    devspace build --profile {{version}} --namespace {{NS}}

build-all: (build "r363") (build "r441")

# ──────────────────────────────────────────────────────────────────────────
#  Dev shell
# ──────────────────────────────────────────────────────────────────────────

# Enter the dev shell. Syncs the working tree, opens bash in the pod.
# Usage: `just dev` (r441) | `just dev r363`
dev version="r441": _assert-context
    devspace dev --profile {{version}} --namespace {{NS}}

# Open an extra shell against the running pod (without re-syncing).
shell version="r441": _assert-context
    kubectl -n {{NS}} exec -it \
      deploy/modulr-dev-$(echo {{version}} | sed 's/r//') -- bash

# ──────────────────────────────────────────────────────────────────────────
#  Tests
# ──────────────────────────────────────────────────────────────────────────

# Run the full testthat suite in one R version.
#   `just test r441`   →  R 4.4.1
#   `just test r363`   →  R 3.6.3
test version="r441": _assert-context
    @just _ensure-running {{version}}
    @TAG=$(echo {{version}} | sed 's/r//'); \
    kubectl -n {{NS}} exec -i deploy/modulr-dev-${TAG} -- bash -lc '\
      cd /pkg && R CMD INSTALL --no-test-load . >/tmp/i.log 2>&1 \
        && Rscript -e "suppressMessages({library(testthat); library(modulr)}); \
                       attach(asNamespace(\"modulr\"), name=\"m\", warn.conflicts=FALSE); \
                       suppressMessages(set_verbosity(0L)); \
                       res <- test_dir(\"/pkg/tests/testthat\", \
                                       reporter=SilentReporter\$new(), \
                                       stop_on_failure=FALSE); \
                       df <- as.data.frame(res); \
                       for (c in c(\"nb\",\"failed\",\"skipped\",\"warning\",\"error\")) df[[c]] <- as.numeric(df[[c]]); \
                       agg <- aggregate(cbind(nb,failed,skipped,warning,error) ~ file, df, sum); \
                       agg\$status <- ifelse(agg\$failed+agg\$error>0,\"FAIL\",ifelse(agg\$skipped>0,\"SKIP\",\"OK\")); \
                       print(agg, row.names=FALSE); \
                       cat(\"\nTotal: \", sum(df\$nb), \" tests, \", sum(df\$failed), \" failed, \", sum(df\$error), \" errors, \", sum(df\$skipped), \" skipped\n\", sep=\"\")"'

# Run the suite on both R versions, sequentially.
test-all: (test "r363") (test "r441")

# Run `R CMD check` (slower, catches more things than test_dir alone).
check version="r441": _assert-context
    @just _ensure-running {{version}}
    @TAG=$(echo {{version}} | sed 's/r//'); \
    kubectl -n {{NS}} exec -i deploy/modulr-dev-${TAG} -- bash -lc '\
      cd /pkg && R CMD build --no-build-vignettes . && \
      R CMD check --no-manual --no-vignettes modulr_*.tar.gz'

# Build the source tarball locally (without docker).
tarball:
    R CMD build --no-build-vignettes .
    @ls -lh modulr_*.tar.gz

# ──────────────────────────────────────────────────────────────────────────
#  Teardown
# ──────────────────────────────────────────────────────────────────────────

# Stop the K3S profile (preserves images & PVCs).
teardown:
    colima stop {{PROFILE}} || true

# Delete the user library PVCs (force a full reinstall on next run).
nuke-cache: _assert-context
    kubectl -n {{NS}} delete pvc -l app=modulr-dev --ignore-not-found

# Nuclear: delete everything including the colima profile.
# (No context guard: nuke must work even when the cluster is in a bad state.)
nuke:
    -kubectl --context {{KUBE_CTX}} delete ns {{NS}}
    -colima delete {{PROFILE}} --force

# ──────────────────────────────────────────────────────────────────────────
#  Internals
# ──────────────────────────────────────────────────────────────────────────

# Ensure the deployment is up & rolled out, otherwise deploy it.
_ensure-running version: _assert-context
    @TAG=$(echo {{version}} | sed 's/r//'); \
    if ! kubectl -n {{NS}} get deploy/modulr-dev-${TAG} >/dev/null 2>&1; then \
      devspace deploy --profile {{version}} --namespace {{NS}}; \
    fi; \
    kubectl -n {{NS}} rollout status deploy/modulr-dev-${TAG} --timeout=120s
