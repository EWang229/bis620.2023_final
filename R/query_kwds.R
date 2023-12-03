#' @title Query the Keywords from the Given Table
#' @description This function queries a given data set using keywords given to the function.
#' @param tbl a dataset to query on
#' @param kwds word(s) that will be used to filter the dataset
#' @param column name of the column/variable within the dataset to query the
#' words from
#' @param ignore_case should the cases be ignored. default is 'TRUE'
#' @param match_all should the words be a complete match. default is 'FALSE'
#' @return a data frame with the specified query
#' @importFrom dplyr filter sql
#' @export
#'
query_kwds <- function(tbl, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds <- paste0("%", kwds, "%")
  kwds <- gsub("'", "''", x = kwds)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query <- paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )

  dplyr::filter(tbl, dplyr::sql(query))
}
