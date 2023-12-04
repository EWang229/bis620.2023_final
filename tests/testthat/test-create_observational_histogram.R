test_that("create_observational_histogram() works", {
  data(studies_subset)
  data(designs)
  vdiffr::expect_doppelganger("observational-histogram-1",
                              create_observational_histogram(studies_subset,
                                                            designs))
})
