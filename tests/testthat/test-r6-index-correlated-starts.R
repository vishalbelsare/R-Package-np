test_that("correlated index restarts enter both optim and NOMAD feasibly", {

  withr::local_options(np.messages = FALSE)
  withr::local_preserve_seed()
  for (s in c(9L,11L)) {
    set.seed(s)
    x1 <- rnorm(60); x2 <- -2*x1 + .1*rnorm(60)
    y <- x1+x2+.2*rnorm(60)
    X <- data.frame(x1,x2)
    b <- npindexbw(xdat=X,ydat=y,method="ichimura",random.seed=s)
    expect_true(is.finite(b$fval))
    b <- npindexbw(xdat=X,ydat=y,method="ichimura",random.seed=s,
      regtype="lp",degree.select="coordinate",search.engine="nomad",
      degree.max=2,nmulti=3)
    expect_true(is.finite(b$fval))
  }
})
