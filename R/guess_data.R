#' Guess the type of an input dataset (internal, experimental)
#'
#' Helper used to estimate which kind of object the user supplied so that, in
#' the future, the package can seamlessly support several input types
#' (transactions, tidy data, binary incidence matrix, ...). Not exported.
#'
#' @param data An input object (transactions object, tidy data.frame, or matrix).
#' @return A character string: one of "transactions", "tidy" or
#'   "binary incidence matrix".
#' @keywords internal
#' @noRd

guess_data <- function(data) {

  if (inherits(data, "transactions")) {
    data_guess <- "transactions"
  } else if (ncol(data) == 2) {
    data_guess <- "tidy"
  } else {
    data_guess <- "binary incidence matrix"
  }

  return(data_guess)
}
