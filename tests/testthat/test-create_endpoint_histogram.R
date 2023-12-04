test_that("create_endpoint_histogram() works", {
  data(studies_subset)
  data(endpoints)
  vdiffr::expect_doppelganger("endpoint-histogram-1",
                              create_endpoint_histogram(studies_subset,
                                                            endpoints))
})
