test_that("create_intervention_histogram() works", {
  data(studies_subset)
  data(designs)
  vdiffr::expect_doppelganger("interventional-histogram-1",
                              create_intervention_histogram(studies_subset,
                                                            designs))
})
