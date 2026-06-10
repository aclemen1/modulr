# tests/testthat/test-browser-trap.R
#
# Regression test for the R debug-stepper trap that affected agentic
# tooling (rstudio-cli, etc.) when typing a `{ ... }` block at a
# Browse[n]> prompt opened by modulr::browser().
#
# === Background ===
#
# A prior implementation invoked base::browser via:
#
#     do.call(base::browser, args = args, envir = parent.frame(1L))
#
# The explicit `envir = parent.frame(1L)` makes base::browser arm R's
# internal RSTEP single-step flag on the caller's frame instead of on
# its own return frame. As a result, any `{ ... }` block typed at the
# next Browse[]> prompt is interpreted by R's interpreter as "the next
# group of statements to step through" — landing the user at Browse[n+1]>
# with "debug at #1: ..." output, with no way back except `Q`.
#
# Effects observed in the wild:
#   - rstudio-cli's `r send 'expr'` (which types `ℝ(~{ expr })` at the
#     prompt) caused a nested Browse to open on every send.
#   - The CLI's poll loop then deadlocked, holding the per-session lock.
#   - User had to manually type `Q` in the RStudio console to recover.
#
# === Fix ===
#
# Remove the `envir = parent.frame(...)` argument from the do.call.
# base::browser's `skipCalls` parameter (already wired via
# increment_skipCalls_) is the right mechanism to direct base::browser
# to the correct srcref — without contaminating the parent frame's
# debug-stepper state.
#
# Either of these forms is acceptable:
#
#     do.call(base::browser, args = args)
#     base::browser(<args>)             # direct call, no do.call magic
#
# Both leave RSTEP armed on base::browser's own return frame, which is
# unwound as soon as it returns, so the user's frame stays clean.
#
# Trade-off the maintainers acknowledged: without `envir = parent.frame()`,
# the prompt at the initial Browse[]> scopes to modulr::browser's own
# frame (not the user's). That means `ls()` and bare variable references
# at the prompt show modulr::browser's locals, not the user's. To inspect
# the user's locals one has to type `where` then go through
# `parent.frame()$<var>`, or step once with `n` (which lands cleanly in
# the user's frame). This is judged a smaller cost than the agentic
# tooling trap.
#
# === Manual repro (for the maintainer to verify once after the fix) ===
#
# In an interactive R session with the FIXED modulr loaded:
#
#     library(modulr)
#     f <- function() { x <- 1; browser(); x + 1 }
#     f()
#     Browse[1]> { y <- 2; y * 10 }
#
# Expected after fix:
#     [1] 20
#     Browse[1]>      # back to same depth, no "debug at #1:" message
#
# Without the fix:
#     debug at #1: y <- 2
#     Browse[2]>      # trapped one level deeper
#
# === Structural tests below ===
#
# We test the SHAPE of modulr::browser's body rather than runtime
# behaviour because the actual trap only manifests in an interactive
# session at a live prompt (R's RSTEP flag is not introspectable from
# R-level code, and an interactive subprocess test would be brittle on
# CI). The structural guard pins the contract: future refactors cannot
# re-introduce the buggy `envir = parent.frame(...)` pattern in any
# do.call to base::browser.

context("browser trap")

test_that("modulr::browser never delegates to base::browser with envir = parent.frame(...)", {
  # Concatenate the deparsed body into a single searchable string. We
  # match across newlines so multiline `do.call(...)` calls are caught.
  body_src <- paste(deparse(body(modulr::browser)), collapse = " ")

  # Regex matches `do.call(...base::browser..., envir = parent.frame(...))`
  # tolerating arbitrary intermediate arguments and whitespace.
  buggy_pattern <- "do\\.call\\([^)]*base::browser[^)]*envir\\s*=\\s*parent\\.frame"

  expect_false(
    grepl(buggy_pattern, body_src),
    info = paste(
      "modulr::browser delegates to base::browser via",
      "`do.call(base::browser, ..., envir = parent.frame(...))`.",
      "This desyncs R's RSTEP flag from base::browser's natural return",
      "frame and arms it on the caller's frame instead — trapping any",
      "{ ... } block typed at the next Browse[]> prompt into a nested",
      "Browse[n+1]> with `debug at #1: ...` output.",
      "Drop the `envir = parent.frame(...)` argument; `skipCalls`",
      "(already passed via increment_skipCalls_) is the right mechanism",
      "to direct base::browser to the correct srcref without contaminating",
      "the caller's stepper state."
    )
  )
})

test_that("modulr::browser's pipe branch also avoids envir = parent.frame(...)", {
  # The pipe branch (active when `function_list` exists in parent.frame(2L),
  # i.e. inside a magrittr %>% chain) historically duplicated the same
  # buggy pattern with skipCalls = 8L instead of 2L. The structural guard
  # above already covers both occurrences via the same regex, but we add
  # an explicit assertion on the count for extra clarity in failure messages.
  body_src <- paste(deparse(body(modulr::browser)), collapse = " ")
  occurrences <- length(
    regmatches(body_src, gregexpr("envir\\s*=\\s*parent\\.frame", body_src))[[1]]
  )
  # Acceptable uses of `parent.frame(...)` elsewhere in the body (e.g. the
  # `where = parent.frame(2L)` for function_list lookup, or the
  # `pos = parent.frame(1L)` for module_name) are NOT named `envir = `,
  # so this assertion is tight: any `envir = parent.frame(...)` is the
  # buggy pattern.
  expect_equal(
    occurrences, 0L,
    info = paste(
      "Found", occurrences, "occurrence(s) of `envir = parent.frame(...)`",
      "in modulr::browser. Each is a candidate trigger for the debug-stepper",
      "trap. See the comment at the top of test-browser-trap.R for the",
      "diagnosis and fix."
    )
  )
})

test_that("modulr::browser still forwards arguments to base::browser via do.call (contract preserved)", {
  # Positive guard: the fix must KEEP delegating to base::browser, not
  # silently drop the call. We just want the buggy `envir` arg removed.
  body_src <- paste(deparse(body(modulr::browser)), collapse = " ")
  expect_true(
    grepl("do\\.call\\([^)]*base::browser", body_src) ||
      grepl("base::browser\\s*\\(", body_src),
    info = "modulr::browser must still delegate to base::browser (via do.call or direct call)."
  )
})

# Behavioural smoke tests for the orthogonal magrittr 2.x portability
# fix (the original `function_list` detection broke when magrittr 2.0
# rewrote the pipe machinery). These complement the structural tests
# above and exercise the non-interactive code paths.

test_that("browser() is a silent no-op outside a pipe in non-interactive mode", {
  expect_silent(out <- modulr::browser())
  expect_null(out)
})

test_that("browser() forwards the LHS inside a magrittr pipe (non-interactive)", {
  skip_if_not_installed("magrittr")
  `%>%` <- magrittr::`%>%`
  # `42L %>% modulr::browser()` is rewritten by magrittr to
  # `modulr::browser(42L)`. The pipe-aware path must return 42L.
  out <- 42L %>% modulr::browser()
  expect_equal(out, 42L)
})

test_that(".magrittr_pipe_context() detects pipes across magrittr 1.x and 2.x", {
  skip_if_not_installed("magrittr")
  `%>%` <- magrittr::`%>%`
  outside <- (function() modulr:::.magrittr_pipe_context())()
  expect_null(outside)
  probe <- function(.) modulr:::.magrittr_pipe_context()
  inside <- NULL %>% probe()
  expect_false(is.null(inside))
  # `i` is NA on magrittr 1.x (legacy freduce loop variable is not
  # introspectable). Both vintages expose `k` and `display`.
  expect_true(is.na(inside$i) || (inside$i >= 1L && inside$i <= inside$k))
  expect_type(inside$display, "character")
  expect_true(inside$vintage %in% c("1.x", "2.x"))
})

test_that(".magrittr_pipe_context() returns correct stage indices on 2.x", {
  skip_if_not_installed("magrittr")
  # i is only computed on magrittr 2.x; on 1.x we expect NA and just
  # verify k is right.
  is_2x <- utils::packageVersion("magrittr") >= "2.0"
  `%>%` <- magrittr::`%>%`
  probe <- function(x) {
    ctx <- modulr:::.magrittr_pipe_context()
    list(i = ctx$i, k = ctx$k)
  }
  # Single stage
  r1 <- "A" %>% probe()
  expect_equal(r1$k, 1L)
  if (is_2x) expect_equal(r1$i, 1L) else expect_true(is.na(r1$i))
  # Probe in the middle of a longer chain
  r2 <- "B" %>% identity() %>% probe() %>% identity()
  expect_equal(r2$k, 3L)
  if (is_2x) expect_equal(r2$i, 2L) else expect_true(is.na(r2$i))
  # Probe at the end
  r3 <- "C" %>% identity() %>% identity() %>% probe()
  expect_equal(r3$k, 3L)
  if (is_2x) expect_equal(r3$i, 3L) else expect_true(is.na(r3$i))
})

test_that("modulr::browser() does not leak entries onto R's sink stack", {
  # Regression for the agentic-tooling cascade where the step-flag trap
  # caused nested browsers to accumulate until `Error: sink stack is full`
  # made the session unrecoverable (see commit history). Even with the
  # current sandwich-env design, this guarantee must hold: repeated
  # entry/exit of modulr::browser must not leave any residual sink.
  #
  # We do not actually pause R (base::browser is a no-op in non-interactive
  # mode), but the non-interactive code path still executes most of
  # modulr::browser's bookkeeping (on.exit hooks, breadcrumb messages,
  # etc.) and any sink it would open would leak just as well as in
  # interactive mode.
  before <- sink.number()
  for (i in seq_len(50L)) {
    modulr::browser()
  }
  expect_equal(sink.number(), before,
               info = "modulr::browser() opened a sink that is not closed.")
})

test_that("browser() forwards the LHS inside a native `|>` pipe (R >= 4.1)", {
  # The native pipe `|>` is a parse-time transformation: `x |> f()` is
  # parsed straight to `f(x)`, with no runtime trace of `|>`. So we
  # cannot detect "we are in a pipe" the way we do for magrittr's `%>%`.
  # The contract is enforced by always forwarding the first positional
  # `...` arg in non-interactive mode.
  if (getRversion() < "4.1") {
    skip("native |> requires R >= 4.1")
  }
  # Parsing `|>` requires R 4.1+, so we guard the literal.
  expr <- parse(text = "42L |> modulr::browser() |> identity()")
  out <- eval(expr)
  expect_equal(out, 42L)
})
