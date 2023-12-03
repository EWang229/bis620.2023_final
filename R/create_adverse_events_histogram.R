#' @title Plots histogram of adverse events from the trials
#' @description This function creates a histogram of the adverse events in the
#' trial
#' @param d a dataset given to the function for merging
#' @param con a duckdkb connection that will help with computational efficiency
#' Otherwise, it will take a long time to run this function. SQL helps a lot.
#' @return ggplot object that shows a histogram of the distribution of
#' adverse events
#' @importFrom DBI dbExecute dbGetQuery
#' @importFrom dplyr select left_join group_by summarize copy_to mutate filter
#' @importFrom ggplot2 aes ggplot geom_col theme_bw coord_flip xlab ylab
#' @export
#'
create_adverse_events_histogram = function(d, con) {
  # have to use SQL to help us merge tables as dplyr's left_join is computationally
  # expensive for large datasets
  dbExecute(con, paste("DROP TABLE IF EXISTS", "studies_merge"))  # drop table if it exists
  copy_to(con, d, name = "studies_merge", temporary = TRUE)  # copies to connection
  # so we can sql query
  query <- "SELECT adverse_event_term
            FROM studies_merge
            LEFT JOIN reported_events
            ON studies_merge.nct_id = reported_events.nct_id"
  merged_ae <- dbGetQuery(con, query)
  #d$merged_ae[is.na(d$merged_ae)] = "NA"
  merged_ae <- merged_ae |>
    mutate(adverse_event_term = fct_lump_n(adverse_event_term, 10)) |>  # keeps
    # the top 10 most frequent adverse events
    group_by(adverse_event_term) |>
    summarize(n = n()) |>
    filter(adverse_event_term != "Other")  # gets rid of the "Other" column, contains
  # too many adverse events

  ggplot(merged_ae, aes(x = adverse_event_term, y = n)) +
    geom_col() +
    theme_bw() +
    coord_flip() +
    xlab("Adverse Events") +
    ylab("Count")
}
