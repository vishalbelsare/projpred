# tests for glm_elnet, currently untested combinations:
# - gaussian with non-id-link
# - binomial with non-logit-link
# - binomial with weights
# - poisson with non-log-link
# - ridge penalty
# - gamma with any link

set.seed(1235)
n <- 40
nv <- 10
nv_fit <- nv - 5
x <- matrix(rnorm(n*nv, 0, 1), n, nv)
b <- runif(nv)-0.5
dis <- runif(1, 1, 2)
x_tr <- x[,1:nv_fit]
weights <- sample(1:4, n, replace = T)
weights_norm <- weights / sum(weights) * n
# change this to something else once offsets work
offset <- rep(0, n)

tol <- 1e-04
extra_thresh <- 1e-10

context("lasso")
test_that("glm_elnet: gaussian, id-link, intercept, lambda = 0", {
  fam <- gaussian(link = 'identity')
  y <- rnorm(n, x%*%b, dis)
  lambda <- 0

  glmfit <- glm(y ~ x_tr, family = fam, weights = weights,
                offset = offset)
  lassofit <- glm_elnet(x_tr, y, family = fam, lambda = lambda, intercept = TRUE,
                        normalize = FALSE, weights = weights, offset = offset,
                        thresh = extra_thresh)

  expect_equal(unname(coef(glmfit)), c(lassofit$beta0, lassofit$beta),
               tolerance = tol)
})

test_that("glm_elnet: gaussian, id-link, no intercept, lambda = 0", {
  fam <- gaussian(link = 'identity')
  y <- rnorm(n, x%*%b, dis)
  lambda <- 0

  glmfit <- glm(y ~ x_tr - 1, family = fam, weights = weights, offset = offset)
  lassofit <- glm_elnet(x_tr, y, family = fam, lambda = lambda, intercept = FALSE,
                        normalize = FALSE, weights = weights, offset = offset,
                        thresh = extra_thresh)

  expect_equal(unname(coef(glmfit)), c(lassofit$beta), tolerance = tol)
})

test_that("glm_elnet: gaussian, id-link, intercept, lambda = 0.5", {
  fam <- gaussian(link = 'identity')
  y <- rnorm(n, x%*%b, dis)
  lambda <- 0.5

  lassofit <- glm_elnet(x_tr, y, family = fam, lambda = lambda, alpha = 1,
                        weights = weights_norm, offset = offset,
                        intercept = TRUE, normalize = FALSE)
  # Calculated with:
  # glmnetfit <- glmnet::glmnet(x_tr, y, family = fam$family, alpha = 1,
  #                             weights = weights_norm, offset = offset,
  #                             lambda = lambda/n,
  #                             intercept = TRUE, standardize = FALSE)
  # exp_beta <- unname(c(glmnetfit$a0, as.matrix(glmnetfit$beta)))
  exp_beta <- c(0.02547437, -0.005815953, -0.02727875, 0.1743722, -0.1582208, -0.2507765)

  expect_equal(exp_beta, c(lassofit$beta0, lassofit$beta), tolerance = tol)
})

test_that("glm_elnet: gaussian, id-link, no intercept, lambda = 0.5", {
  fam <- gaussian(link = 'identity')
  y <- rnorm(n, x%*%b, dis)
  lambda <- 0.5

  lassofit <- glm_elnet(x_tr, y, family = fam, lambda = lambda, alpha = 1,
                        weights = weights_norm, offset = offset,
                        intercept = FALSE, normalize = FALSE)
  # Calculated with:
  # glmnetfit <- glmnet::glmnet(x_tr, y, family = fam$family, alpha = 1,
  #                             weights = weights_norm, offset = offset,
  #                             lambda = lambda/n,
  #                             intercept = FALSE, standardize = FALSE)
  # exp_beta <- c(as.matrix(glmnetfit$beta))
  exp_beta <- c(0.01715529, -0.03150905, 0.5954335, -0.472687, -0.6820674)

  expect_equal(exp_beta, c(lassofit$beta), tolerance = tol)
})


test_that("glm_elnet: binomial, logit-link, intercept, lambda = 0", {
  fam <- binomial(link = 'logit')
  y <- rbinom(n, weights, fam$linkinv(x%*%b))
  lambda <- 0

  lassofit <- glm_elnet(x_tr, y/weights, family = fam, lambda = lambda, alpha = 1,
                        offset = offset, weights = weights,
                        intercept = TRUE, normalize = FALSE)
  glmfit <- glm(cbind(y, weights-y) ~ x_tr, family = fam, offset = offset)

  expect_equal(unname(coef(glmfit)), c(lassofit$beta0, lassofit$beta),
               tolerance = tol)
})

test_that("glm_elnet: binomial, logit-link, no intercept, lambda = 0", {
  fam <- binomial(link = 'logit')
  y <- rbinom(n, weights, fam$linkinv(x%*%b))
  lambda <- 0

  lassofit <- glm_elnet(x_tr, y/weights, family = fam, lambda = lambda, alpha = 1,
                        offset = offset, weights = weights,
                        intercept = FALSE, normalize = FALSE)
  glmfit <- glm(cbind(y, weights-y) ~ x_tr - 1, family = fam, offset = offset)

  expect_equal(unname(coef(glmfit)), c(lassofit$beta), tolerance = tol)
})


test_that("glm_elnet: binomial, logit-link, intercept, lambda = 0.5", {
  fam <- binomial(link = 'logit')
  y <- rbinom(n, 1, fam$linkinv(x%*%b))
  lambda <- 0.5

  lassofit <- glm_elnet(x_tr, y, family = fam, lambda = lambda, alpha = 1,
                        offset = offset, thresh = extra_thresh,
                        intercept = TRUE, normalize = FALSE)
  # Calculated with:
  # glmnetfit <- glmnet::glmnet(x_tr, y, family = fam$family, alpha = 1,
  #                             offset = offset, lambda = lambda/n,
  #                             intercept = TRUE, standardize = FALSE)
  # exp_beta <- unname(c(glmnetfit$a0, as.matrix(glmnetfit$beta)))
  exp_beta <- c(-0.3189198, 0.396411, -0.1368026, 0.377641, 0, -0.2283437)

  expect_equal(exp_beta, c(lassofit$beta0, lassofit$beta), tolerance = tol)
})

test_that("glm_elnet: binomial, logit-link, no intercept, lambda = 0.5", {
  fam <- binomial(link = 'logit')
  y <- rbinom(n, 1, fam$linkinv(x%*%b))
  lambda <- 0.5

  lassofit <- glm_elnet(x_tr, y, family = fam, lambda = lambda, alpha = 1,
                        offset = offset,
                        intercept = FALSE, normalize = FALSE)
  # Calculated with:
  # glmnetfit <- glmnet::glmnet(x_tr, y, family = fam$family, alpha = 1,
  #                             offset = offset, lambda = lambda/n,
  #                             intercept = FALSE, standardize = FALSE)
  # exp_beta <- c(as.matrix(glmnetfit$beta))
  exp_beta <- c(0.1065949, 0, 0, -0.1024071, -0.301079)

  expect_equal(exp_beta, c(lassofit$beta), tolerance = tol)
})


test_that("glm_elnet: poisson, log-link, intercept, lambda = 0", {
  fam <- poisson(link = 'log')
  y <- rpois(n, fam$linkinv(x%*%b))
  lambda <- 0

  lassofit <- glm_elnet(x_tr, y, family = fam, lambda = lambda, alpha = 1,
                        offset = offset, weights = weights,
                        intercept = TRUE, normalize = FALSE)
  glmfit <- glm(y ~ x_tr, family = fam, weights = weights, offset = offset)

  expect_equal(unname(coef(glmfit)), c(lassofit$beta0, lassofit$beta),
               tolerance = tol)
})

test_that("glm_elnet: poisson, log-link, no intercept, lambda = 0", {
  fam <- poisson(link = 'log')
  y <- rpois(n, fam$linkinv(x%*%b))
  lambda <- 0

  lassofit <- glm_elnet(x_tr, y, family = fam, lambda = lambda, alpha = 1,
                        offset = offset, weights = weights,
                        intercept = FALSE, normalize = FALSE)
  glmfit <- glm(y ~ x_tr - 1, family = fam, weights = weights, offset = offset)

  expect_equal(unname(coef(glmfit)), c(lassofit$beta), tolerance = tol)
})

test_that("glm_elnet: poisson, log-link, intercept, lambda = 0.5", {
  fam <- poisson(link = 'log')
  y <- rpois(n, fam$linkinv(x%*%b))
  lambda <- 0.5

  lassofit <- glm_elnet(x_tr, y, family = fam, lambda = lambda, alpha = 1,
                        offset = offset, weights = weights_norm,
                        intercept = TRUE, normalize = FALSE, thresh = extra_thresh)
  # Calculated with:
  # glmnetfit <- glmnet::glmnet(x_tr, y, family = fam$family, alpha = 1,
  #                             offset = offset,  weights = weights_norm, lambda = lambda/n,
  #                             intercept = TRUE, standardize = FALSE, thresh = extra_thresh)
  # exp_beta <- unname(c(glmnetfit$a0, as.matrix(glmnetfit$beta)))
  exp_beta <- c(0.007600988, 0.3194901, 0, 0.2412289, -0.3393914, -0.3656913)

  expect_equal(exp_beta, c(lassofit$beta0, lassofit$beta), tolerance = tol)
})

test_that("glm_elnet: poisson, log-link, no intercept, lambda = 0.5", {
  fam <- poisson(link = 'log')
  y <- rpois(n, fam$linkinv(x%*%b))
  lambda <- 0.5

  lassofit <- glm_elnet(x_tr, y, family = fam, lambda = lambda, alpha = 1,
                        offset = offset, weights = weights_norm,
                        intercept = FALSE, normalize = FALSE)
  # Calculated with:
  # glmnetfit <- glmnet::glmnet(x_tr, y, family = fam$family, alpha = 1,
  #                             offset = offset,  weights = weights_norm, lambda = lambda/n,
  #                             intercept = FALSE, standardize = FALSE)
  # exp_beta <- c(as.matrix(glmnetfit$beta))
  exp_beta <- c(0.2681732, 0.08606586, 0.05406078, -0.2778106, -0.4429331)

  expect_equal(exp_beta, c(lassofit$beta), tolerance = tol)
})