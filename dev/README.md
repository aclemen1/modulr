# modulr — dev environment

Reproducible cross-version dev environment for `modulr`, targeting R 3.6.3
(legacy floor) and R 4.4.1 (current focus). The whole stack runs on a
dedicated [colima](https://github.com/abiosoft/colima) profile with K3S, and
is driven by [DevSpace](https://devspace.sh) + a top-level `justfile`.

## Why this stack?

| Component | Role |
|---|---|
| **colima** profile `modulr-k3s` | Lightweight Linux VM with containerd + K3S. Isolated from your default Docker context. |
| **K3S** | Hosts the dev pods. Provides `PersistentVolumeClaim`s for the per-version R user libraries (no rebuild on every restart). |
| **DevSpace** | Builds the images, deploys to K3S, syncs your working tree into the pod, opens a terminal. |
| **`justfile`** | Single user-facing surface (`just dev`, `just test-all`, …). Wraps the above. |

The two pods (`modulr-dev-r363`, `modulr-dev-r441`) each carry a pinned CRAN
snapshot (Posit Package Manager) contemporary with their R release, so
package versions are deterministic.

## Prerequisites

```bash
brew install colima docker docker-compose kubectl helm just devspace
```

(You probably already have most of these.)

## One-shot bootstrap

```bash
just bootstrap
```

This creates the `modulr-k3s` colima profile (4 CPU / 8 GiB / 30 GiB disk,
mounting `$HOME` writable), points `kubectl` at it, and creates the
`modulr-dev` namespace.

## Daily workflow

```bash
# Enter the R 4.4.1 dev shell (default). Syncs the working tree on entry
# and keeps it in sync until you exit.
just dev

# Same for R 3.6.3.
just dev r363

# Quick test runs (do not need an active `just dev` session — they spawn a
# one-off exec into the deployment).
just test r441
just test r363
just test-all          # both, sequentially

# Full R CMD check (slower).
just check r441

# Inspect everything.
just status
```

Inside the pod you have a vanilla R toolchain at `/pkg`:

```bash
R                          # interactive REPL
Rscript -e 'devtools::test()'
R CMD build .
R CMD check --as-cran modulr_*.tar.gz
```

The user library lives at `$R_LIBS_USER` (= `/root/R/library`) and is
backed by a per-version PVC, so `install.packages()` runs persist across
pod restarts.

## File sync details

DevSpace runs a two-way rsync-like sync between the host working tree and
`/pkg` in the pod (configured in `devspace.yaml`). Excluded:

- `.jj/`, `.git/`, `.Rproj.user/`, `.Rcheck/`
- generated artefacts: `tests-detail-*.rds`, `modulr_*.tar.gz`
- `dev/k8s/` itself (avoid sync loops when DevSpace re-applies manifests)

Edits made *inside* the pod (e.g. `vim /pkg/R/foo.R`) round-trip back to the
host. Watch this with `just status` if you suspect drift.

## Cache invalidation

```bash
just nuke-cache        # wipe the R user-library PVCs, keep the cluster
just teardown          # stop K3S (preserves images & PVCs)
just nuke              # nuclear: delete namespace + colima profile
```

The Docker images themselves can be rebuilt via:

```bash
just build r441
just build-all
```

## Trade-offs we accepted

- **Two images**, not a multi-stage hybrid. Debian buster (R 3.6.3) is
  archived, jammy (R 4.4.1) is current — sharing a base is more pain than
  it's worth.
- **PVC, not host bind-mount** for the R user library. We saw virtiofs
  flakiness on `/Users/...` from this sandbox. PVCs sit on the colima
  ext4 volume.
- **No RStudio Server** by design. Edit on the host, exec into the pod for
  R commands. If you want a browser IDE later, add `rstudio` to the
  Dockerfiles and a port-forward in `devspace.yaml`.
- **No GitHub Actions CI yet** — the same images are usable from CI, but
  not wired in. The old `.travis.yml` is preserved for reference and
  should be replaced separately.

## Troubleshooting

| Symptom | Fix |
|---|---|
| `colima-modulr-k3s` context missing | `just colima-up` |
| `Unable to find image …` on `devspace dev` | `just build <version>` then retry |
| Pod stuck in `ContainerCreating` | `kubectl -n modulr-dev describe pod …` — usually a PVC binding issue, fix with `just nuke-cache` |
| Sync silently does nothing | Check `devspace logs --provider sync` and exclude paths in `devspace.yaml` |
| `just test` fails on R 4.4.1 with `with_mock()` defunct errors | Make sure testthat ≤ 3.1.x is in the image, or that `skip_if_with_mock_defunct()` is wired in the tests (already shipped). |
