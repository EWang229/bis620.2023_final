#' Diagnose Linear Regression Assumptions
#'
#' @param x a dataframe to check linear regression assumptions on (for the
#' ratio of cholesterol to high-density lipoproteins)
#' @param log a logical that asks whether to log the response variable
#' @returns 4 plots that allow us to diagnose linear regression assumptions
#' @export
lr_assumptions = function(x, log = FALSE){
  if (log == FALSE){
    pred <- x$glyhb
  }
  else {
    pred <- log(x$glyhb)
  }
  diabetes_fit <- lm(pred ~ ratio, data = x)  # creating a fitted line with the
  # numerical predictor, ratio
  plot(diabetes_fit, which = 1)
  plot(diabetes_fit, which = 2)
  plot(diabetes_fit, which = 3)
  plot(diabetes_fit, which = 4)
}
