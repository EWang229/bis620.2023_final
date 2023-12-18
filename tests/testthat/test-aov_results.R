test_that("aov_results works", {
  data(diabetes)
  expect_snapshot(aov_results(diabetes))
})
