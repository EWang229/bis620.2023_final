test_that("lr_assumptions works", {
  data(diabetes)
  vdiffr::expect_doppelganger("lr_plots1", lr_assumptions(diabetes))
})
