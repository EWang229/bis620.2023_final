#' Clean Diabetes Data
#'
#' @param x a dataframe to clean data on.
#' @returns A dataframe that cleans out na values, drops empty columns, and
#' selects the variables of interest.
#' @importFrom dplyr filter_at vars all_vars select
#' @export
clean_data = function(x){
  x <- x |> filter_at(vars(glyhb, ratio, frame, location), all_vars(!is.na(.)))
  # filtering out if there are any na's in our desired columns
  x <- x[x$frame != "",]  # filtering out the empty columns
  x <- x |> select(glyhb, ratio, frame, location)
  x
}
