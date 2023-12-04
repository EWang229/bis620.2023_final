## code to prepare `DATASET` dataset goes here

studies_subset = readRDS("studies_subset.rds")
designs = readRDS("designs.rds")
accel = readRDS("accel.rds")
usethis::use_data(accel, overwrite = TRUE)
usethis::use_data(designs, overwrite = TRUE)
usethis::use_data(studies_subset, overwrite = TRUE)
