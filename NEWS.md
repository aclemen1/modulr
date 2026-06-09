# `modulr` 0.1.7.9216

## Bug fixes

* `modulr::browser()`: stop trapping agentic tooling at a nested
  `Browse[n+1]>` *while* keeping the user's locals reachable at the
  prompt across all three modes (plain interactive, magrittr pipe,
  module body).

  The previous implementation invoked base::browser via
  `do.call(base::browser, args, envir = parent.frame(1L))` so that the
  prompt would scope to the caller's frame (locals visible). That
  same `envir` also armed R's internal RSTEP / RDEBUG flag on the
  caller's frame, with the consequence that any `{ ... }` block typed
  at the next `Browse[]>` prompt — including the `ℝ(~{ … })` wrappers
  issued by `rstudio-cli r send` and similar agentic tools — was
  interpreted by R as "the next group of statements to step through",
  landing the user at `Browse[n+1]>` with a `debug at #1: ...` line
  and no clean way out short of typing `Q`.

  The fix introduces a *sandwich environment*: a freshly-created
  `new.env(parent = parent.frame(1L))` is passed as `envir =` to the
  do.call instead of the caller's frame itself. Two consequences:

    - Variable resolution at the prompt chains
      `wrap -> caller -> ...`, so bare `x`, `m`, `.` etc. resolve
      against the caller's frame exactly like before.
    - The debug-stepper bit base::browser arms on `wrap` (`skipCalls`
      is left at 0, no longer bumped) does not escape: `wrap` is not
      on R's function-context stack and is garbage-collected the
      moment modulr::browser returns. The caller's frame is therefore
      untouched.

  Side note: the `skipCalls = 2` / `8` historically bumped in this
  function would defeat the sandwich (any non-zero skipCalls climbs
  onto a caller-stack frame and would re-introduce the leak). It is
  intentionally removed. The visible cost: `n` and `s` from the
  initial pause now act like `c` rather than stepping through the
  caller's next statement — that step-through behaviour was a
  side-effect of the bug, not a feature. Users who want true
  step-through can still call `debug()` on the function of interest.

  Structural regression test at `tests/testthat/test-browser-trap.R`
  pins the contract (no `envir = parent.frame(...)` in any do.call to
  base::browser). Interactive validation matrix recorded in the
  commit message.

* `modulr::browser()`: also restore pipe-aware behaviour under
  magrittr >= 2.0 *and* extend the same contract to the native pipe
  `|>` (R >= 4.1). The previous detection relied on a `function_list`
  variable that magrittr 1.x stashed in `parent.frame(2L)`. magrittr 2.x
  rewrote the pipe machinery (`pipe_eager_lexical`) and dropped that
  variable, so `lhs %>% modulr::browser() %>% rhs` silently returned
  `invisible(NULL)` in non-interactive contexts and broke any pipe
  chain. The native pipe `|>` was never supported at all (it is
  resolved at parse time, so `x |> modulr::browser()` is
  indistinguishable from `modulr::browser(x)` at runtime).

  Two complementary changes restore the contract for every pipe variant:

  1. Non-interactive: forward the first positional `...` argument
     whenever there is one. The LHS of any pipe — magrittr or native —
     always lands there, so the chain keeps flowing without needing
     to detect the pipe kind. Direct-call usage like
     `modulr::browser("text")` is unaffected: base::browser is itself
     a no-op non-interactively, so any return value is moot.

  2. Interactive: a fresh `.` binding is set on the sandwich env so
     users can inspect the LHS at the prompt regardless of which
     pipe brought them there. The rich "surrounding stages" message
     is restored for both magrittr 1.x and 2.x via a new
     `.magrittr_pipe_context()` helper that walks `sys.calls()` for
     the `%>%` expression (magrittr 2.x) and falls back to a
     parent-frame scan for the legacy `function_list / k` pair
     (magrittr 1.x). The "current stage" highlighting works on 2.x
     (`i` derivable from the frame distance to the `%>%` call) and
     degrades gracefully to a full chain listing on 1.x (the
     `freduce` loop variable is volatile and not introspectable
     from the outside; the legacy `i` lookup in original modulr
     code was already broken silently on magrittr >= 1.5). The
     native `|>` does not show the rich display at all (it leaves
     no runtime trace), but `.` and the LHS value remain
     accessible at the prompt via the explicit binding.

  Regression coverage added in `tests/testthat/test-browser-trap.R`
  for both pipe flavours (the `|>` test skips on R < 4.1).

## Dev environment

* Replace the R 4.4.1 dev image with R 4.5.3 (now the latest minor R
  release we routinely test against). The cross-version floor stays at
  R 3.6.3. `dev/Dockerfile.r441` is removed; `dev/Dockerfile.r453` and
  the matching `r453` DevSpace profile are added. `justfile`'s default
  `version` argument is now `r453`, and `just test-all` boucle sur
  r363 + r453.

# `modulr` 0.1.7.9215

## Bug fix

* Fix a fatal error raised on R >= 4.3 when resolving an on-disk module from a
  file that begins directly with several definitions sharing the same
  namespace (e.g. `foo`, `foo#1.0.0`, `foo#2.0.0`). The fast-path of
  `.extract_name()` would compare a length-> 1 vector with `==` inside `&&`,
  which became an error in R 4.3 (`'length = N' in coercion to 'logical(1)'`).
  The guard now bails out of the fast-path when more than one name is
  returned and lets the regular full-file parse run. Stack:
  `make() -> load_module() -> find_module() -> .resolve_name() -> .extract_name()`.

## Test suite cross-version stability (R 3.6.3 <-> R 4.4.x)

* New helper `tests/testthat/helper-modulr.R` provides:
  - `expect_provider_equal()` to compare two providers ignoring the `srcref`,
    `srcfile` and `wholeSrcref` attributes (and the closure environment),
    whose presence varies between R 3.6 and R 4.x.
  - `skip_if_with_mock_defunct()` to gracefully skip tests relying on
    `testthat::with_mock()` (defunct since testthat 3.2.0) for mocking
    foreign-package functions such as `httr::GET`. Once those tests migrate
    to `mockery::stub()` (or modulr exposes an HTTP transport hook), the
    skip can be lifted.
* `tests/testthat/test_define.R`: the three `.digest` assertions on hardcoded
  xxHash64 hex strings are replaced by property-based checks (hex shape,
  determinism, sensitivity to formals/body/comments). The hashes themselves
  legitimately differ across R versions because they depend on
  `deparse(., control = "useSource")` formatting and on `digest` version.
* `tests/testthat/test_define.R`, `tests/testthat/test_make.R`: six
  `expect_equal(<provider>, function(){...})` switched to
  `expect_provider_equal()`.
* `tests/testthat/test_import.R`, `tests/testthat/test_gears.R`: every
  `test_that` block built on `with_mock(httr::...)` now opens with
  `skip_if_with_mock_defunct()`.

Result: full pass on **R 3.6.3** (966 tests, 0 fail, 6 skipped) and on
**R 4.4.1** (952 tests, 0 fail, 18 skipped: 12 cross-version mock skips +
6 legitimate skips) under
`_R_CHECK_LENGTH_1_LOGIC2_=true _R_CHECK_LENGTH_1_CONDITION_=true`.

## Dev environment (developer-only, excluded from the R build)

* New `dev/Dockerfile.r363` / `dev/Dockerfile.r441` images for the two-rung
  R test matrix, pinned to Posit Package Manager CRAN snapshots
  contemporary with each R release.
* New `devspace.yaml` driving a K3S-based DevSpace dev loop: build → deploy
  → file sync → terminal. Profiles `r363` and `r441` switch the R version.
  K8s manifests are inlined so DevSpace var substitution reaches the body.
* New top-level `justfile` orchestrating the whole stack — `just bootstrap`,
  `just dev [r363|r441]`, `just test [...]`, `just test-all`, `just check`,
  `just nuke`. All cluster-touching recipes refuse to act on any context
  other than `colima-modulr-k3s` (belt-and-braces against cross-cluster
  mishaps).
* New `dev/README.md` documenting the workflow and trade-offs.
* `.Rbuildignore` extended (`^dev$`, `^devspace\.yaml$`, `^justfile$`) so
  these dev artefacts are excluded from the published source tarball.

# `modulr` 0.2.0

## First release on CRAN

`modulr` is a Dependency Injection Framework for R. Until now, `modulr` has been
exclusively used by the University of Lausanne, Information Systems and 
Statistics, Switzerland. This first release on CRAN brings `modulr` to the 
vibrant community of R Users.
