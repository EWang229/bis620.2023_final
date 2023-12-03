#' @title Plots histogram of endpoints from the trials
#' @description This function creates a histogram of the endpoints in the trial
#' @param d a dataset given to the function for merging
#' @param endpoints a dataset with the endpoints
#' @return ggplot object that shows a histogram of the distribution of
#' endpoints
#' @importFrom dplyr select left_join group_by summarize
#' @importFrom ggplot2 aes ggplot geom_col theme_bw labs scale_y_log10
#' @export
#'
create_endpoint_histogram = function(d, endpoints) {
  em = d |>
    select(nct_id) |>
    left_join(endpoints, by = "nct_id") |>
    group_by(endpoint_met) |>
    summarize(n = n())

  # multiply counts by 10 and divide them by 10 when scaling to make counts of 1 visible
  ggplot(em, aes(x = endpoint_met, y = n * 10)) +
    geom_col() +
    scale_y_log10(labels = function(x) x/10) +
    labs(x = "Endpoint Met", y = "Count") +
    theme_bw()
}
