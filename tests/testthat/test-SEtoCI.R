test_that("SEtoCI returns correct dimensions", {
  res <- SEtoCI(
    estimates = c(1, 2),
    std_errors = c(0.1, 0.2)
  )

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 2)
  expect_equal(colnames(res), c("Lower", "Upper"))
})

test_that("SEtoCI handles correlations", {
  res <- SEtoCI(
    estimates = c(0.3),
    std_errors = c(0.05),
    is_correlation = TRUE
  )

  expect_true(res$Lower < res$Upper)
  expect_true(abs(res$Upper) <= 1)
})
