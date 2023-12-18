test_that("anova_assumptions works", {
  data(diabetes)
  vdiffr::expect_doppelganger("anova_plots1", anova_assumptions(diabetes))
})
