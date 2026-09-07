test_that("NOMAD index counts all scientific visits across restarts", {
  old <- options(np.messages = FALSE)
  on.exit(options(old), add = TRUE)
  skip_if_not_installed("crs")
  original <- .np_nomad_search
  original.certificate <- .npindexbw_certify_selected_candidate
  visits <- 0L
  certificates <- 0L
  local_mocked_bindings(.np_nomad_search = function(...) {
    args <- list(...)
    evaluate <- args$eval_fun
    args$eval_fun <- function(point) {
      visits <<- visits + 1L
      evaluate(point)
    }
    do.call(original, args)
  }, .npindexbw_certify_selected_candidate = function(...) {
    certificates <<- certificates + 1L
    original.certificate(...)
  }, .package = "np")
  set.seed(5)
  x <- data.frame(x1 = rnorm(30), x2 = rnorm(30))
  y <- x$x1 + .5 * x$x2 + rnorm(30, sd = .3)
  b <- npindexbw(xdat = x, ydat = y, method = "ichimura", regtype = "lp",
    degree.select = "coordinate", search.engine = "nomad", degree.max = 1L,
    nmulti = 2L, random.seed = 7L, nomad.opts = list(MAX_BB_EVAL = 8L))
  expect_gt(visits, 0L)
  expect_identical(certificates, 1L)
  expect_identical(b$num.feval, as.numeric(visits + certificates))
})
