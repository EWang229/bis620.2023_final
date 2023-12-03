#' @title Plots histogram of observational study types from the trials
#' @description Displays a histogram to show the distribution of observational
#' study types from the trials in a query
#' @param d the dataframe of the query result
#' @param designs the handle referencing the "designs" database table
#' @return ggplot object that shows a histogram of the distribution of
#' observational study types
#' @importFrom dplyr select left_join group_by summarize collect filter
#' @importFrom ggplot2 aes ggplot geom_col theme_bw labs coord_flip xlab ylab
#' @export
#'
create_observational_histogram = function(d, designs) {
  merged <- left_join(d, collect(designs), by = "nct_id")
  merged_observ <- merged |>
    select(observational_model) |>
    filter(!is.na(observational_model)) |>
    group_by(observational_model) |>
    summarize(n = n())
  sum_observ <- sum(merged_observ$n)  # this is for the title to give a total count

  ggplot(merged_observ, aes(x = observational_model, y = n)) +
    geom_col() +
    theme_bw() +
    labs(title = paste("Total Amount: ", sum_observ)) +
    coord_flip() +
    xlab("Observational Type") +
    ylab("Count")
}
