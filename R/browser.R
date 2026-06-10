increment_skipCalls_ <- function(args, increment = 1L) {
  if (!is.null(args[["skipCalls"]]) && is.numeric(args[["skipCalls"]])) {
    args[["skipCalls"]] <- args[["skipCalls"]] + increment
  } else {
    args[["skipCalls"]] <- increment
  }
  args
}

prompt_ <- function(fun) {
  paste(paste(">", deparse(body(fun), width.cutoff = 78L)), collapse = "\n")
}

# Detect a magrittr pipe context and, when possible, reconstruct the
# surrounding stages so modulr::browser can show the user-facing
# breadcrumbs (`>  ... %>% probe(.) %>% identity() %>% ...`).
#
# Returns NULL when no pipe is active. Otherwise a list with:
#
#   display  : character vector — one deparsed entry per stage of the
#              pipeline (already prefixed with "> " and ready to be
#              joined with " %>% \n" by the caller).
#   i        : 1-based index of the current stage (== modulr::browser).
#   k        : total number of stages in the pipeline.
#   vintage  : "1.x" or "2.x" — diagnostic only.
#
# Two paths because magrittr 2.0.0 explicitly broke call-stack
# introspection (NEWS, section "Breaking changes / Incorrect call stack
# introspection") and removed the `function_list / i / k` variables 1.x
# used to expose. Per that same NEWS section, sys.frames / sys.parent
# are off-limits for "actual functionality" but still tolerated for
# debugging tools — which is exactly our case.
#
# Path A (magrittr 1.x): walk parent frames looking for the
#   `function_list / i / k` triplet stashed by the legacy pipe code,
#   and use it directly (most faithful to original behaviour).
#
# Path B (magrittr 2.x): find the topmost `%>%` call on sys.calls()
#   (its expression is the whole user-visible pipe), flatten its
#   left-associative `lhs %>% rhs` tree into a list of stages, and
#   derive the current stage index from the frame distance between
#   the `%>%` call and modulr::browser. The 2.x C-level rewrite makes
#   this distance equal to (k - i), so i = k - frames_between.
.magrittr_pipe_context <- function() {
  # Path A — magrittr 2.x (and any vintage that keeps `%>%` on the
  # call stack with the full pipe expression). We introspect via
  # sys.calls() and derive `i` from the frame distance to the `%>%`
  # frame, exploiting the fact that magrittr 2.x's eager-lexical
  # evaluator stacks unresolved future stages between `%>%` and the
  # current stage. So frames_between == k - i.
  calls <- sys.calls()
  pipe_idx <- NA_integer_
  pipe_call <- NULL
  for (idx in seq_along(calls)) {
    cl <- calls[[idx]]
    if (length(cl) == 0L) next
    h <- cl[[1L]]
    is_pipe <- identical(h, quote(`%>%`)) ||
      (is.call(h) && length(h) == 3L &&
        identical(h[[1L]], quote(`::`)) &&
        identical(h[[3L]], quote(`%>%`)))
    if (is_pipe) {
      pipe_idx <- idx
      pipe_call <- cl
      break
    }
  }

  if (!is.na(pipe_idx)) {
    # Flatten `((A %>% B) %>% C) %>% D` into [B, C, D] (RHSs only).
    stages <- list()
    walker <- pipe_call
    while (is.call(walker) && identical(walker[[1L]], quote(`%>%`))) {
      stages <- c(list(walker[[3L]]), stages)
      walker <- walker[[2L]]
    }
    k <- length(stages)
    if (k > 0L) {
      caller_depth <- sys.parent()
      i <- k - (caller_depth - pipe_idx - 1L)
      if (i >= 1L && i <= k) {
        display <- vapply(
          stages,
          function(e) {
            paste(">", paste(deparse(e, width.cutoff = 78L),
                             collapse = "\n  "))
          },
          character(1L))
        return(list(display = display, i = i, k = k, vintage = "2.x"))
      }
      # i out of range — fall through to magrittr 1.x's function_list
      # path which is more reliable on that vintage (the frame layout
      # differs because of freduce + withVisible insertion).
    }
  }

  # Path B — magrittr 1.x: walk parent frames for the `function_list`
  # + `k` pair stashed by freduce/`_fseq`. magrittr 1.5 dropped the
  # explicit `i` variable (it now reuses `k` as the for-loop counter,
  # which is volatile and unreliable from outside). We therefore
  # surface all stages WITHOUT highlighting the current one — better
  # than nothing, and faithful to the call stack actually present.
  for (depth in seq_len(min(20L, sys.nframe()))) {
    fr <- tryCatch(parent.frame(depth), error = function(e) NULL)
    if (is.null(fr)) next
    if (exists("function_list", where = fr, inherits = FALSE) &&
        exists("k", where = fr, inherits = FALSE)) {
      fl <- get("function_list", envir = fr, inherits = FALSE)
      kk <- get("k", envir = fr, inherits = FALSE)
      if (!is.list(fl) || length(fl) == 0L) next
      display <- vapply(
        fl,
        function(f) {
          if (is.function(f)) {
            paste(paste(">", deparse(body(f), width.cutoff = 78L)),
                  collapse = "\n  ")
          } else {
            paste(">", paste(deparse(f, width.cutoff = 78L),
                             collapse = "\n  "))
          }
        },
        character(1L))
      return(list(display = display, i = NA_integer_, k = kk,
                  vintage = "1.x"))
    }
  }

  NULL
}

#' Environment, Module, and Pipe Browser.
#'
#' Interrupt the execution of an expression or a pipe and allow the inspection
#' of the environment where browser was called from.
#'
#' @param ... Further arguments to be passed to \code{\link[base]{browser}}.
#'
#' @details
#'
#' See \code{\link[base]{browser}}.
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' (function() {
#'   "foobar" %>%
#'     browser() %>%
#'     print
#' })()}
#'
#' reset()
#' define("foo", NULL, function() {foo <- "bar"; browser()})
#' \dontrun{make()}
#'
#' reset()
#' define("foobar", NULL, function() {
#'   library(magrittr)
#'   "foobar" %>%
#'     browser %>%
#'     print
#' })
#' \dontrun{make()}
#' @export
browser <- function(...) {
  pipe_ctx <- .magrittr_pipe_context()
  in_pipe  <- !is.null(pipe_ctx)

  if (!interactive()) {
    # Pipe-forwarding contract. Both pipes (magrittr `%>%` and the native
    # `|>` introduced in R 4.1) rewrite `lhs <pipe> rhs(args)` so the LHS
    # becomes the first positional argument of the right-hand call. The
    # native pipe leaves NO runtime trace — it is resolved at parse time
    # to `rhs(lhs, args)`, indistinguishable from a direct call. So instead
    # of trying to detect the pipe context, we simply forward the first
    # positional `...` argument whenever there is one. This naturally
    # handles `x %>% browser()`, `x |> browser()`, and the direct-call
    # `browser("text")` case (base::browser is a non-interactive no-op
    # anyway, so any return value is irrelevant there).
    args <- list(...)
    if (length(args) >= 1L &&
        (is.null(names(args)) || identical(names(args)[[1L]], ""))) {
      return(args[[1L]])
    }
    return(invisible())
  }

  # Module context — display breadcrumbs once before delegating.
  if (exists(".__name__", where = parent.frame(1L))) {
    module_name <- get(".__name__", pos = parent.frame(1L))
    if (module_name != "__main__") get_breadcrumbs(NULL, verbose = TRUE)
  }

  # Sandwich env and on.exit hooks.
  #
  #   wrap : a fresh env whose lexical parent is the caller's frame.
  #          We pass it as `envir =` to do.call(base::browser, ...) so
  #          that the prompt's variable resolution chains through to
  #          the caller's locals (reads work) WITHOUT base::browser
  #          arming RDEBUG/RSTEP on the caller (the wrap is off-stack
  #          and is GC'd when we return; no debug-bit can survive).
  #
  #   on.exit propagation: a bare `x <- 1` at the prompt would by
  #          default land in `wrap`, shadowing rather than mutating
  #          the caller's binding. We copy `wrap`'s contents onto the
  #          caller right before returning, so mutations persist —
  #          which is what `base::browser` does natively. Side-effect
  #          is that NEW names introduced at the prompt also propagate
  #          (same as `base::browser`).
  caller <- parent.frame(1L)
  wrap <- new.env(parent = caller)
  on.exit({
    for (.nm in ls(envir = wrap, all.names = TRUE)) {
      assign(.nm, get(.nm, envir = wrap), envir = caller)
    }
  }, add = TRUE)

  if (in_pipe) {
    message(
      "Use ", sQuote("."), " to get the left-hand side value of the pipe.")
    # Rich surrounding-stages display via `.magrittr_pipe_context()`.
    # On magrittr 2.x we have `i` and can show a narrow window around the
    # current stage. On 1.x the helper returns `i = NA` (the legacy
    # `freduce` loop variable is volatile and not introspectable from the
    # outside); we degrade gracefully to listing all stages.
    i <- pipe_ctx$i
    k <- pipe_ctx$k
    disp <- pipe_ctx$display
    if (is.na(i)) {
      message(paste(unlist(c(list("> ."), disp)), collapse = " %>% \n"))
    } else {
      message(paste(unlist(c(
        if (i %in% 1L:2L) list("> .") else if (i > 2L) list("> ..."),
        disp[max(1L, i - 1L):min(i + 1L, k)],
        if (i < k - 1L) list("> ...")
      )),
      collapse = " %>% \n"))
    }
    args <- list(...)
    deparse.max.lines.bak <-
      options(deparse.max.lines =
                max(getOption("modulr.deparse.max.lines.in.pipes"),
                    getOption("deparse.max.lines")))
    # `add = TRUE` so this stacks atop the propagation on.exit above
    # rather than replacing it. The pipe contract also demands we
    # return the LHS so the chain keeps flowing post-`c`.
    on.exit({
      options(deparse.max.lines.bak)
      return(args[[1L]])
    }, add = TRUE)
    do.call(base::browser, args = utils::tail(args, -1L), envir = wrap)
  } else {
    # Non-pipe non-module. Hand off through the shared `wrap` env
    # already set up above. Expose the first positional argument as
    # `.` inside `wrap` so users get a uniform `.` to inspect the LHS
    # regardless of whether they got here via magrittr (`%>%` exposes
    # `.` via the lexical chain) or the native `|>` (which leaves no
    # runtime trace and would otherwise have no `.`). Harmless for
    # direct-call usage where the user would not normally reference
    # `.`.
    args_ <- list(...)
    if (length(args_) >= 1L &&
        (is.null(names(args_)) || identical(names(args_)[[1L]], ""))) {
      assign(".", args_[[1L]], envir = wrap)
    }
    do.call(base::browser, args = list(...), envir = wrap)
  }
}
