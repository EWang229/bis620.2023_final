#' Diagnose ANOVA Assumptions
#'
#' @param x a dataframe to check anova assumptions on (for location and frame)
#' @returns 4 plots that allow us to diagnose ANOVA assumptions
#' @export
anova_assumptions = function(x){
  diabetes_aov <- aov(glyhb ~ location*frame, data = x)
  plot(diabetes_aov)
}
