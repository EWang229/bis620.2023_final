#' ANOVA Results
#'
#' @param x a dataframe to get the ANOVA results of (for location and frame)
#' @returns A dataframe that contains the summary statistics of the ANOVA
#' @importFrom pixiedust dust sprinkle pvalString sprinkle_colnames
#' @importFrom kableExtra kable kable_styling
#' @export
aov_results = function(x){
  x <- aov(glyhb ~ location*frame, data = x)
  dust(x) |>  # formats the anova object
    sprinkle(col = 2:5, round = 3) |>  # rounds to the 3rd decimal place for
    # columns 2-5
    sprinkle(col = 6, fn = quote(pvalString(value))) |>  # make the p-value
    # not in scientific notation
    sprinkle_colnames(df = "Degrees of Freedom",
                      sumsq = "Sum of Squares",
                      meansq = "Mean Squared Error",
                      statistic = "F-value",
                      p.value = "p-value") |>  # re-naming the columns
    kable() |>  # this makes the table
    kable_styling()  # changes the style of the table
}
