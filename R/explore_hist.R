#' Explore Diabetes Data with Histogram
#'
#' @param x a dataframe to perform exploratory data analysis on
#' @returns two histograms of the quantitative variables
#' @export
explore_hist = function(x){
  hist(x$glyhb, xlab = "glyhb levels", ylab = "Count",
       main = "Histogram of glyhb Levels", col = "lightgreen", xlim = c(2, 10))
  hist(x$ratio, xlab = "Cholesterol/HDL ratio", ylab = "Count",
       main = "Histogram of Cholesterol/HDL ratio", col = "lightblue",
       xlim = c(0, 10))

}
