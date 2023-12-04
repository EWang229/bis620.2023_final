test_that("create_conditions_histogram() works", {
  data(studies_subset)
  data(conditions)
  vdiffr::expect_doppelganger("conditions-histogram-1",
                              create_conditions_histogram(studies_subset,
                                                        conditions))
})
