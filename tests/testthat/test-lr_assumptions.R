test_that("lr_assumptions works", {
  data(diabetes)
  par(mfrow = c(2, 2))
  vdiffr::expect_doppelganger("lr_plots1", lr_assumptions(diabetes))
})
