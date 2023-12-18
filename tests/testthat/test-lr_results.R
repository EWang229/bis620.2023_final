test_that("lr_results works", {
  data(diabetes)
  expect_snapshot(lr_results(diabetes))
})
