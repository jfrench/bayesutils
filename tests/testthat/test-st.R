set.seed(8)
x = runif(50, 0, 10)
nu = runif(50, 0, 10)
m = rnorm(50)
s = runif(50, 0, 10)
p = runif(50)

# if (requireNamespace("brms", quietly = TRUE)) {

## test dst
da = brms::dstudent_t(x, nu, m, s)
db = dst(x, nu, m, s)

log_da = brms::dstudent_t(x, nu, m, s, log = TRUE)
log_db = dst(x, nu, m, s, log = TRUE)

test_that("dst is correct", {
  expect_true(all.equal(da, db))
  expect_true(all.equal(log_da, log_db))
})

## test qst
qa = brms::qstudent_t(p, nu, m, s)
qb = qst(p, nu, m, s)

test_that("qst is correct", {
  expect_equal(qa, qb)
})

## test pst
pa = brms::pstudent_t(x, nu, m, s)
pb = pst(x, nu, m, s)

logp_pa = brms::pstudent_t(x, nu, m, s, log.p = TRUE)
logp_pb = pst(x, nu, m, s, log.p = TRUE)

ut_pa = brms::pstudent_t(x, nu, m, s, lower.tail = TRUE)
ut_pb = pst(x, nu, m, s, lower.tail = TRUE)

ut_logp_pa = brms::pstudent_t(x, nu, m, s, lower.tail = TRUE, log.p = TRUE)
ut_logp_pb = pst(x, nu, m, s, lower.tail = TRUE, log.p = TRUE)

test_that("pst is correct", {
  expect_equal(pa, pb)
  expect_equal(logp_pa, logp_pb)
  expect_equal(ut_pa, ut_pb)
  expect_equal(ut_logp_pa, ut_logp_pb)
})

# ## test rst
# set.seed(1)
# r1 = brms::rstudent_t(50, nu, m, s)
# set.seed(1)
# r2 = rst(50, nu, m, s)
#
# test_that("rst is correct", {
#   expect_equal(r1, r2)
# })
#
