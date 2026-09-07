t2_terminal_condition <- function(class = "t2_leaf_error") {
  structure(list(message = "terminal callback contract", call = quote(t2_leaf()),
                 token = list(value = 733L)),
            class = c(class, "error", "condition"))
}

t2_terminal_driver <- function(eval_fun, build_payload, ...) {
  getFromNamespace(".np_nomad_search", "np")(
    engine = "nomad", baseline_record = NULL, start_degree = 1L,
    x0 = 0, bbin = 1L, lb = 0, ub = 4,
    eval_fun = eval_fun, build_payload = build_payload,
    native.r.bridge = TRUE, preserve.eval.error = TRUE,
    random.seed = 42L, nomad.opts = list(MAX_BB_EVAL = 20L), ...)
}

test_that("terminal callbacks cannot evaluate or publish after an unexpected failure", {
  skip_if_not_installed("crs", minimum_version = "0.15.46")
  old <- options(np.messages = FALSE)
  on.exit(options(old), add = TRUE)
  for (stage in c("first", "interior", "last", "input", "result", "admissible")) {
    visits <- payloads <- requests <- 0L
    original <- t2_terminal_condition()
    failure.at <- switch(stage, first = 1L, interior = 2L, last = 4L, 2L)
    got <- testthat::with_mocked_bindings({
      tryCatch(t2_terminal_driver(
        function(point) {
          visits <<- visits + 1L
          if (visits == failure.at) {
            if (stage %in% c("first", "interior", "last")) stop(original)
            if (stage == "result")
              return(list(objective = list(new.env()), degree = 1L))
            if (stage == "admissible")
              return(list(objective = 1, degree = 1L, admissible = NA))
          }
          list(objective = point[1L]^2 + 1, degree = 1L)
        },
        function(...) { payloads <<- payloads + 1L; list(payload = TRUE) },
        nmulti = 2L, remin = TRUE), error = identity)
    }, .np_nomad_native_r_callback_search = function(eval.f, ...) {
      # Deliberately request more callbacks even after native-caught errors.
      for (i in 1:6) {
        requests <<- requests + 1L
        point <- if (stage == "input" && i == 2L) list(new.env()) else (i - 1) %% 5
        tryCatch(eval.f(point), error = function(e) NULL)
      }
      list(value = list(status = "error", native_status = 2L,
                        message = "generic native status"), output = character())
    }, .package = "np")
    expect_s3_class(got, "error")
    if (stage %in% c("first", "interior", "last")) expect_identical(got, original)
    expect_identical(visits, if (stage == "input") 1L else failure.at, info = stage)
    expect_identical(payloads, 0L, info = stage)
    expect_identical(requests, 6L, info = stage)
    expect_false(identical(conditionMessage(got), "generic native status"))
  }
})

test_that("rejection stays exploratory and required payload failures propagate", {
  skip_if_not_installed("crs", minimum_version = "0.15.46")
  old <- options(np.messages = FALSE)
  on.exit(options(old), add = TRUE)
  visits <- payloads <- 0L
  rejection <- t2_terminal_condition("np_nn_candidate_invalid")
  got <- t2_terminal_driver(function(point) {
    visits <<- visits + 1L
    if (visits == 2L) stop(rejection)
    list(objective = (point[1L] - 3)^2 + 1, degree = 1L)
  }, function(point, ...) { payloads <<- payloads + 1L; list(payload = point) },
  nmulti = 2L, remin = TRUE)
  expect_gt(visits, 2L)
  expect_identical(payloads, 1L)
  expect_equal(as.numeric(got$best_point), 3)
  # The same typed rejection is terminal in required payload/certification.
  got <- tryCatch(t2_terminal_driver(function(point)
    list(objective = point[1L]^2 + 1, degree = 1L),
    function(...) stop(rejection)), error = identity)
  expect_identical(got, rejection)
})

test_that("interrupt cannot publish an incumbent or enter another restart", {
  skip_if_not_installed("crs", minimum_version = "0.15.46")
  old <- options(np.messages = FALSE)
  on.exit(options(old), add = TRUE)
  for (interrupt.solve in 1:2) {
    visits <- payloads <- solves <- 0L
    original <- structure(list(message = "test interrupt", call = NULL),
                          class = c("interrupt", "condition"))
    got <- testthat::with_mocked_bindings({
      tryCatch(t2_terminal_driver(function(point) {
        visits <<- visits + 1L
        if (solves == interrupt.solve && point == 1) stop(original)
        list(objective = (point - 3)^2 + 1, degree = 1L)
      }, function(...) { payloads <<- payloads + 1L; list(payload = TRUE) },
      nmulti = 1L, remin = TRUE), interrupt = identity)
    }, .np_nomad_native_r_callback_search = function(eval.f, ...) {
      solves <<- solves + 1L
      for (i in 0:3) tryCatch(eval.f(i), interrupt = function(e) NULL)
      list(value = list(status = "ok", native_status = 0L, solution = 3,
                        objective = 1, message = "ok"), output = character())
    }, .package = "np")
    expect_identical(got, original)
    expect_identical(visits, (interrupt.solve - 1L) * 4L + 2L)
    expect_identical(payloads, 0L)
    expect_identical(solves, interrupt.solve)
  }
})

test_that("abort rendering cannot replace the original terminal condition", {
  skip_if_not_installed("crs", minimum_version = "0.15.46")
  old <- options(np.messages = FALSE)
  on.exit(options(old), add = TRUE)
  for (stage in c("callback", "payload")) {
    visits <- aborts <- 0L
    original <- t2_terminal_condition()
    got <- testthat::with_mocked_bindings({
      tryCatch(t2_terminal_driver(function(point) {
        visits <<- visits + 1L
        if (stage == "callback" && visits == 2L) stop(original)
        list(objective = point[1L]^2 + 1, degree = 1L)
      }, function(...) stop(original)), error = identity)
    }, .np_progress_abort = function(...) {
      aborts <<- aborts + 1L
      stop("diagnostic rendering failed")
    }, .package = "np")
    expect_identical(got, original)
    expect_identical(aborts, 1L)
  }
})

test_that("public npindex and nplsqreg stop at the actual failing producer", {
  skip_on_cran()
  skip_if_not_installed("crs", minimum_version = "0.15.46")
  old <- options(np.messages = FALSE)
  on.exit(options(old), add = TRUE)
  set.seed(941L)
  x <- data.frame(x1 = seq(-1, 1, length.out = 24L), x2 = runif(24L, -1, 1))
  y <- sin(3 * x$x1) + .2 * x$x2 + seq_len(24L) / 1000
  configs <- list(
    index = list(fun = npindexbw, leaf = ".npindexbw_eval_ichimura_lp_via_npreg",
                 args = list(xdat = x, ydat = y, bws = c(1, .5, .4),
                             method = "ichimura", regtype = "lp")),
    lsq = list(fun = nplsqregbw, leaf = ".nplsqreg_call_fixed_degree_core",
               args = list(xdat = x[1], ydat = y, scale = rep(1, 24), bws = .4)))
  for (config in configs) {
    args <- c(config$args, list(nomad = TRUE, search.engine = "nomad",
      degree.min = 1L, degree.max = 2L, degree.start = 1L, nmulti = 1L,
      nomad.opts = list(MAX_BB_EVAL = 12L), random.seed = 941L))
    healthy <- do.call(config$fun, args)
    expect_true(is.finite(if (inherits(healthy, "sibandwidth")) healthy$fval else healthy$objective))
    leaf <- getFromNamespace(config$leaf, "np")
    visits <- 0L
    original <- t2_terminal_condition()
    replacement <- function(...) {
      intended <- any(vapply(sys.calls(), function(cl)
        is.call(cl) && identical(cl[[1L]], as.name("eval_fun")), logical(1)))
      if (intended) {
        visits <<- visits + 1L
        if (visits == 2L) stop(original)
      }
      leaf(...)
    }
    got <- do.call(testthat::with_mocked_bindings, c(
      list(code = substitute(tryCatch(do.call(FUN, ARGS), error = identity),
                             list(FUN = config$fun, ARGS = args))),
      setNames(list(replacement), config$leaf), list(.package = "np")))
    expect_identical(got, original)
    expect_identical(visits, 2L)
  }
})
