test_that("opted-in NOMAD preserves the original evaluator condition", {
  skip_on_cran()
  skip_if_not_installed("crs", minimum_version = "0.15.46")
  old <- options(np.messages = FALSE)
  on.exit(options(old), add = TRUE)

  original <- structure(list(
    message = paste(rep("original evaluator failure", 70L), collapse = ":"),
    call = quote(scoef_leaf(x = 1L)), token = "original",
    values = c(1.25, -7), detail = list(index = 2L)
  ), class = c("scoef_test_error", "error", "condition"))
  visits <- payloads <- 0L
  driver <- function(active = TRUE, condition = NULL) {
    visits <<- 0L
    payloads <<- 0L
    .np_nomad_search(
      engine = "nomad", baseline_record = NULL, start_degree = 1L,
      x0 = 0, bbin = 1L, lb = 0, ub = 4,
      eval_fun = function(point) {
        visits <<- visits + 1L
        if (visits == 2L && !is.null(condition)) stop(condition)
        list(objective = (point[1L] - 3)^2 + 1, degree = 1L, num.feval = 1L)
      },
      build_payload = function(point, best_record, solution, interrupted) {
        payloads <<- payloads + 1L
        list(payload = list(point = point, objective = best_record$objective))
      },
      native.r.bridge = TRUE, preserve.eval.error = active,
      nmulti = 2L, remin = TRUE, random.seed = 42L,
      nomad.opts = list(MAX_BB_EVAL = 20L)
    )
  }
  got <- tryCatch(driver(condition = original), error = identity)
  expect_identical(got, original)
  expect_identical(visits, 2L)
  expect_identical(payloads, 0L)
  # A fresh invocation has its own condition state.
  good <- driver()
  expect_identical(as.numeric(good$best_point), 3)
  expect_identical(payloads, 1L)
  typed <- original
  class(typed) <- c("np_nn_candidate_invalid", "error", "condition")
  expect_false(inherits(driver(condition = typed), "condition"))
  expect_gt(visits, 2L)
  expect_identical(payloads, 1L)
  expect_false(inherits(driver(active = FALSE, condition = original), "condition"))
  expect_gt(visits, 2L)
})

test_that("serial npscoefbw stops before Powell after an evaluator failure", {
  skip_on_cran()
  skip_if_not_installed("crs", minimum_version = "0.15.46")
  old <- options(np.messages = FALSE)
  on.exit(options(old), add = TRUE)
  ns <- asNamespace("np")
  leaf <- get(".npscoefbw_nomad_moment_state", ns)
  refine <- get(".npscoefbw_run_fixed_degree", ns)
  visits <- refinements <- 0L
  original <- structure(list(message = "scoef actual leaf", call = quote(moment()),
                             token = list(index = 2L)),
                        class = c("scoef_test_error", "error", "condition"))
  n <- 24L
  t <- seq(.05, .95, length.out = n)
  x <- data.frame(x = .2 + sin(seq_len(n) * 1.7))
  z <- data.frame(z = t)
  y <- (1 + t^2) * x$x + sin(seq_len(n) * 2.1) * .01
  testthat::with_mocked_bindings({
    for (type in c("fixed", "generalized_nn", "adaptive_nn")) {
      visits <- refinements <- 0L
      got <- tryCatch(npscoefbw(
        xdat = x, zdat = z, ydat = y, bwmethod = "cv.ls", bwtype = type,
        regtype = "lp", bernstein.basis = TRUE, degree.select = "coordinate",
        search.engine = "nomad+powell", degree.min = 0L, degree.max = 1L,
        degree.verify = FALSE, nmulti = 2L, nomad.remin = TRUE,
        nomad.opts = list(MAX_BB_EVAL = 12L), random.seed = 42L
      ), error = identity)
      expect_identical(got, original)
      expect_identical(visits, 2L)
      expect_identical(refinements, 0L)
    }
  }, .npscoefbw_nomad_moment_state = function(...) {
    visits <<- visits + 1L
    if (visits == 2L) stop(original)
    leaf(...)
  }, .npscoefbw_run_fixed_degree = function(...) {
    refinements <<- refinements + 1L
    refine(...)
  }, .package = "np")
})
