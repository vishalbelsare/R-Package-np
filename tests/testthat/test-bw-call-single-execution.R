np_eval_bw_once <- getFromNamespace(".np_eval_bw_call", "np")

test_that("automatic npindex does not replay a real evaluator failure", {
  old <- options(np.messages = FALSE)
  on.exit(options(old), add = TRUE)
  state <- new.env(parent = emptyenv())
  state$visits <- 0L
  original <- structure(list(message = "single-index evaluator failure",
                             call = quote(index_objective(h)), stage = "raw"),
                        class = c("np_test_evaluator_error", "error", "condition"))
  leaf <- getFromNamespace(".npindexbw_eval_ichimura_lp_via_npreg", "np")
  x <- data.frame(x1 = seq(.1, .9, length.out = 36L),
                  x2 = cos(seq_len(36L) * 1.7))
  y <- sin(6 * x$x1) + .1 * x$x2
  testthat::with_mocked_bindings({
    set.seed(73L)
    got <- tryCatch(npindex(txdat = x, tydat = y), error = identity)
    expect_identical(got, original)
    expect_identical(state$visits, 2L)
  }, .npindexbw_eval_ichimura_lp_via_npreg = function(...) {
    state$visits <- state$visits + 1L
    if (state$visits == 2L) stop(original)
    leaf(...)
  }, .package = "np")
  set.seed(73L)
  expect_s3_class(npindex(txdat = x, tydat = y), "singleindex")
})

test_that("selector computation errors propagate once without reconstruction", {
  env <- new.env(parent = baseenv())
  env$calls <- 0L
  env$failure <- structure(list(message = "evaluator failed", call = quote(objective(h)),
                                detail = list(stage = "raw", id = 7L)),
                           class = c("np_test_evaluator_error", "error", "condition"))
  env$selector <- eval(quote(function() {
    calls <- NULL
    e <- parent.env(environment())
    e$calls <- e$calls + 1L
    if (e$calls == 1L) stop(e$failure)
    "a retry would conceal the error"
  }), env)
  out <- tryCatch(np_eval_bw_once(quote(selector()), env), error = identity)
  expect_identical(out, env$failure)
  expect_identical(env$calls, 1L)
})

test_that("selector-local error handlers retain ordinary R semantics", {
  env <- new.env(parent = baseenv())
  env$selector <- function() tryCatch(stop("handled"), error = function(e) 42L)
  expect_identical(np_eval_bw_once(quote(selector()), env), 42L)
})

test_that("selector arguments and active bindings remain in caller context", {
  env <- new.env(parent = baseenv())
  env$touches <- 0L
  state <- new.env(parent = emptyenv())
  state$reads <- 0L
  makeActiveBinding("selector", local({
    s <- state
    function(value) { s$reads <- s$reads + 1L; identity }
  }), env)
  out <- np_eval_bw_once(quote(selector({touches <- touches + 1L; 23L})), env)
  expect_identical(out, 23L)
  expect_identical(state$reads, 1L)
  expect_identical(env$touches, 1L)
})

test_that("selector arguments are not recovered by replaying in unrelated frames", {
  env <- new.env(parent = baseenv())
  env$selector <- identity
  missing_elsewhere <- 17L
  out <- tryCatch(np_eval_bw_once(quote(selector(missing_elsewhere)), env),
                  error = identity)
  expect_s3_class(out, "error")
  expect_match(conditionMessage(out), "missing_elsewhere")
})

test_that("namespace-only selector calls preserve data and canonical provenance", {
  env <- new.env(parent = baseenv())
  env$x <- data.frame(x = seq(.1, .9, length.out = 12L))
  env$y <- sin(env$x$x)
  out <- np_eval_bw_once(quote(npregbw(xdat = x, ydat = y, bws = .3,
                                      bandwidth.compute = FALSE)), env)
  expect_s3_class(out, "rbandwidth")
  expect_identical(as.numeric(out$bw), .3)
  reference <- eval(quote(np::npregbw(xdat = x, ydat = y, bws = .3,
                                       bandwidth.compute = FALSE)), env)
  expect_identical(environment(out$call), environment(reference$call))
  expect_identical(out$call, reference$call)
  expect_identical(out$bw, reference$bw)
})
