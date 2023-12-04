#' @title Plots histogram of conditions from the trials
#' @description This function creates a histogram of the conditions in the trial
#' @param d a dataset given to the function for merging
#' @param conditions a dataset with the conditions
#' @return ggplot object that shows a histogram of the distribution of
#' conditions
#' @importFrom dplyr select left_join group_by summarize mutate arrange desc
#' @importFrom ggplot2 aes ggplot geom_col theme_bw labs scale_y_log10 theme
#' element_text
#' @importFrom forcats fct_lump_n
#' @export
#'
create_conditions_histogram = function(d, conditions) {
  em = d |>
    select(nct_id) |>
    left_join(conditions |> collect(), by = "nct_id")

  # lump together conditions that aren't considered the top 15 most frequent
  em <- em |>
    mutate(name = fct_lump_n(name, 15)) |>
    group_by(name) |>
    summarize(n = n()) |>
    arrange(desc(n)) |>
    mutate(conditionsordered = factor(name, levels = name))
  # multiply counts by 10 and divide them by 10 when scaling to make counts of 1 visible
  ggplot(em, aes(x = conditionsordered, y = n * 10)) +
    geom_col() +
    scale_y_log10(labels = function(x) x/10) +
    labs(x = "Condition Name", y = "Count") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}
