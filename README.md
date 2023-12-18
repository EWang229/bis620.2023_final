
<!-- README.md is generated from README.Rmd. Please edit that file here-->

# bis620.2023

<!-- badges: start -->

[![R-CMD-check](https://github.com/EWang229/bis620.2023/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EWang229/bis620.2023/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/EWang229/bis620.2023/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/EWang229/bis620.2023/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

[Test Coverage
Link](https://github.com/EWang229/bis620.2023/actions/workflows/test-coverage.yaml)

# BIS 620 Final

The bis620.2023 package contains functions that work on accelerometry
data, as well as data from ctrialsgov. Make sure to look at the function
descriptions in order to decide which functions to use for their
respective data.

Additionally, it allows for statistical analysis on diabetes data. There
are various functions such as explore_hist() and anova_assumptions() to
perform EDA, as well as aov_results and lr_results to perform the
statistical analysis. The functions are specific to the diabetes data
and a few choice predictors, but the functions work quite well and will
save a lot of time.

For the accelerometry data, there are simple functions to use, such as
plot_accel() and spec_sig().

For the ctrialsgov data, use the run_shiny_app() function in order to
start the shiny app. As you will see from the shiny app UI, there are
several ways to query the data, such as typing in keyword(s) or
selecting a sponsor type from the dropdown. Then, you may look at
histograms of certain attributes, such as conditions, endpoints, adverse
events, and interventional/observational study types. Enjoy!

## Installation

You can install the development version of bis620.2023 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EWang229/bis620.2023")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(bis620.2023)
accel |> 
  head(100) |> 
  plot_accel()
```

<img src="man/figures/README-example-1.png" width="100%" />

Here is one example of a histogram pair you could generate within the
shiny app:

``` r
create_intervention_histogram(studies_subset, designs)
create_observational_histogram(studies_subset, designs)
```

<img src="man/figures/README-figures-side-1.png" width="50%" /><img src="man/figures/README-figures-side-2.png" width="50%" />

And one example of using a function from the package to perform
Exploratory Data Analysis:

``` r
par(mfrow = c(2, 2))
anova_assumptions(diabetes)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
