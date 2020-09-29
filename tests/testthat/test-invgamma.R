if (requireNamespace("invgamma", quietly = TRUE)) {
# tests
set.seed(7)
x = runif(50, 0, 10)
shape = runif(50, 0, 10)
scale = runif(50, 0, 10)

## test dinvgamma
# test rate argument works
rate_da = invgamma::dinvgamma(x, shape, rate = 1/scale)
rate_db = dinvgamma(x, shape, rate = 1/scale)

# test scale argument works
scale_da = invgamma::dinvgamma(x, shape, scale = scale)
scale_db = dinvgamma(x, shape, scale = scale)

# test log argument works for rate
lograte_da = invgamma::dinvgamma(x, shape, rate = 1/scale, log = TRUE)
lograte_db = dinvgamma(x, shape, rate = 1/scale, log = TRUE)

# test log argument works for scale
logscale_da = invgamma::dinvgamma(x, shape, scale = scale, log = TRUE)
logscale_db = dinvgamma(x, shape, scale = scale, log = TRUE)

test_that("dinvgamma is correct", {
  expect_true(all.equal(rate_da, rate_db))
  expect_true(all.equal(scale_da, scale_db))
  expect_true(all.equal(lograte_da, lograte_db))
  expect_true(all.equal(logscale_da, logscale_db))
})

## test qinvgamma
p = runif(50)
rate_qa = invgamma::qinvgamma(p, shape, rate = 1/scale)
rate_qb = qinvgamma(p, shape, rate = 1/scale)

scale_qa = invgamma::qinvgamma(p, shape, scale = scale)
scale_qb = qinvgamma(p, shape, scale = scale)

logp = exp(p)
logp_rate_qa = invgamma::qinvgamma(logp, shape, rate = 1/scale, log.p = TRUE)
logp_rate_qb = qinvgamma(logp, shape, rate = 1/scale, log.p = TRUE)

logp_scale_qa = invgamma::qinvgamma(logp, shape, scale = scale, log.p = TRUE)
logp_scale_qb = qinvgamma(logp, shape, scale = scale, log.p = TRUE)

ut_rate_qa = invgamma::qinvgamma(p, shape, rate = 1/scale, lower.tail = FALSE)
ut_rate_qb = qinvgamma(p, shape, rate = 1/scale, lower.tail = FALSE)

ut_scale_qa = invgamma::qinvgamma(p, shape, scale = scale, lower.tail = FALSE)
ut_scale_qb = qinvgamma(p, shape, scale = scale, lower.tail = FALSE)

ut_logp_rate_qa = invgamma::qinvgamma(logp, shape, rate = 1/scale, lower.tail = FALSE, log.p = TRUE)
ut_logp_rate_qb = qinvgamma(logp, shape, rate = 1/scale, lower.tail = FALSE, log.p = TRUE)

ut_logp_scale_qa = invgamma::qinvgamma(logp, shape, scale = scale, lower.tail = FALSE, log.p = TRUE)
ut_logp_scale_qb = qinvgamma(logp, shape, scale = scale, lower.tail = FALSE, log.p = TRUE)

test_that("qinvgamma is correct", {
  expect_equal(rate_qa, rate_qb)
  expect_equal(scale_qa, scale_qb)
  expect_equal(logp_rate_qa, logp_rate_qb)
  expect_equal(logp_scale_qa, logp_scale_qb)
  expect_equal(ut_rate_qa, ut_rate_qb)
  expect_equal(ut_scale_qa, ut_scale_qb)
  expect_equal(ut_logp_rate_qa, ut_logp_rate_qb)
  expect_equal(ut_logp_scale_qa, ut_logp_scale_qb)
})

## test pinvgamma
rate_pa = invgamma::pinvgamma(x, shape, rate = 1/scale)
rate_pb = pinvgamma(x, shape, rate = 1/scale)

scale_pa = invgamma::pinvgamma(x, shape, scale = scale)
scale_pb = pinvgamma(x, shape, scale = scale)

logp_rate_pa = invgamma::pinvgamma(x, shape, rate = 1/scale, log.p = TRUE)
logp_rate_pb = pinvgamma(x, shape, rate = 1/scale, log.p = TRUE)

logp_scale_pa = invgamma::pinvgamma(x, shape, scale = scale, log.p = TRUE)
logp_scale_pb = pinvgamma(x, shape, scale = scale, log.p = TRUE)

ut_rate_pa = invgamma::pinvgamma(x, shape, rate = 1/scale, lower.tail = FALSE)
ut_rate_pb = pinvgamma(x, shape, rate = 1/scale, lower.tail = FALSE)

ut_scale_pa = invgamma::pinvgamma(x, shape, scale = scale, lower.tail = FALSE)
ut_scale_pb = pinvgamma(x, shape, scale = scale, lower.tail = FALSE)

ut_logp_rate_pa = invgamma::pinvgamma(x, shape, rate = 1/scale, lower.tail = FALSE, log.p = TRUE)
ut_logp_rate_pb = pinvgamma(x, shape, rate = 1/scale, lower.tail = FALSE, log.p = TRUE)

ut_logp_scale_pa = invgamma::pinvgamma(x, shape, scale = scale, lower.tail = FALSE, log.p = TRUE)
ut_logp_scale_pb = pinvgamma(x, shape, scale = scale, lower.tail = FALSE, log.p = TRUE)

test_that("pinvgamma is correct", {
  expect_equal(rate_pa, rate_pb)
  expect_equal(scale_pa, scale_pb)
  expect_equal(logp_rate_pa, logp_rate_pb)
  expect_equal(logp_scale_pa, logp_scale_pb)
  expect_equal(ut_rate_pa, ut_rate_pb)
  expect_equal(ut_scale_pa, ut_scale_pb)
  expect_equal(ut_logp_rate_pa, ut_logp_rate_pb)
  expect_equal(ut_logp_scale_pa, ut_logp_scale_pb)
})

## test rinvgamma
set.seed(1)
r1 = invgamma::rinvgamma(3, 1.5, rate = 1.5)
set.seed(1)
r2 = rinvgamma(3, 1.5, rate = 1.5)
set.seed(3)
r3 = invgamma::rinvgamma(3, 1.5, scale = 1.5)
set.seed(3)
r4 = rinvgamma(3, 1.5, scale = 1.5)

test_that("rinvgamma is correct", {
  expect_equal(r1, r2)
  expect_equal(r3, r4)
})

} # end testing if invgamma present

