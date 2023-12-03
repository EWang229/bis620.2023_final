#' @title Plots histogram of intervention study types from the trials
#' @description Displays a histogram to show the distribution of interventional
#' study types from the trials in a query
#' @param d the dataframe of the query result
#' @param designs the handle referencing the "designs" database table
#' @return ggplot object that shows a histogram of the distribution of
#' intervention study types
#' @importFrom dplyr select left_join group_by summarize collect filter
#' @importFrom ggplot2 aes ggplot geom_col theme_bw labs coord_flip xlab ylab
#' @export
#'
create_intervention_histogram = function(d, designs) {
  merged <- left_join(d, collect(designs), by = "nct_id")
  merged_interv <- merged |>
    select(intervention_model) |>
    filter(!is.na(intervention_model)) |>
    group_by(intervention_model) |>
    summarize(n = n())
  sum_interv <- sum(merged_interv$n)

  ggplot(merged_interv, aes(x = intervention_model, y = n)) +
    geom_col() +
    theme_bw() +
    labs(title = paste("Total Amount: ", sum_interv)) +
    coord_flip() +
    xlab("Intervention Type") +
    ylab("Count")
}
