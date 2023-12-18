test_that("explore_hist works", {
  data(diabetes)
  vdiffr::expect_doppelganger("explore_hist1", explore_hist(diabetes))
})
