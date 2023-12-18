#' Linear Regression Results
#'
#' @param x a dataframe to get the linear regression results of (for location
#' and frame)
#' @returns A dataframe that contains the summary statistics of the linear
#' regression
#' @importFrom pixiedust dust sprinkle pvalString sprinkle_colnames
#' @importFrom kableExtra kable kable_styling
#' @export
lr_results = function(x){
  log_glyhb <- log(x$glyhb)
  x <- lm(log_glyhb ~ ratio, data = x)
  dust(x) |>   # formats the lm object
    sprinkle(col = 2:4, round = 3) |>  # rounds columns 2-4 to 3 decimals
    sprinkle(col = 5, fn = quote(pvalString(value))) |>  # takes away
    # scientific notation for p-value
    sprinkle_colnames(term = "Term",
                      estimate = "Estimate",
                      std.error = "SE",
                      statistic = "T-statistic",
                      p.value = "p-value") |>  # renaming the columns
    kable() |>  # makes the table
    kable_styling()  # changes the style of the table
}
