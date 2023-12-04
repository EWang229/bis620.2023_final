test_that("create_phase_histogram() works", {
  data(studies_subset)
  vdiffr::expect_doppelganger("phase-histogram-1",
                              create_phase_histogram(studies_subset,
                                                        studies_subset))
})
