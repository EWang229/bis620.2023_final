## code to prepare `DATASET` dataset goes here

studies_subset = readRDS("studies_subset.rds")
designs = readRDS("designs.rds")
accel = readRDS("accel.rds")
endpoints = readRDS("endpoints.rds")
conditions = readRDS("conditions.rds")
diabetes = readRDS("diabetes.rds")
usethis::use_data(accel, overwrite = TRUE)
usethis::use_data(designs, overwrite = TRUE)
usethis::use_data(studies_subset, overwrite = TRUE)
usethis::use_data(endpoints, overwrite = TRUE)
usethis::use_data(conditions, overwrite = TRUE)
usethis::use_data(diabetes, overwrite = TRUE)
