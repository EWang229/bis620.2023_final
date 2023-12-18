test_that("clean_data works", {
  data(diabetes)
  expect_snapshot(clean_data(diabetes))
})
