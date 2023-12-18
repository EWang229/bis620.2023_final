test_that("anova_assumptions works", {
  data(diabetes)
  par(mfrow = c(2, 2))
  vdiffr::expect_doppelganger("anova_plots1", anova_assumptions(diabetes))
})
