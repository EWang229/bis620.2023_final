#' @title Plots histogram of phases from the trials
#' @description This function creates a histogram of the phases in the trial
#' @param d a dataset given to the function
#' @param studies a dataset with the phase information
#' @return ggplot object that shows a histogram of the distribution of
#' phases
#' @importFrom dplyr compute collect select group_by summarize
#' @importFrom ggplot2 aes ggplot geom_col theme_bw xlab ylab
#' @export
#'
create_phase_histogram = function(d, studies) {
  d$phase[is.na(d$phase)] = "NA"

  # save all possible phases in a vector
  sorted_phases <- (studies |> collect())$phase |>
    unique() |>
    append("NA") |>
    sort()

  d$newphase <- factor(d$phase, levels=sorted_phases)

  d <- d |>
    select(newphase) |>
    group_by(newphase) |>
    summarize(n = n()) |>
    complete(newphase)

  ggplot(d, aes(x = newphase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
}
