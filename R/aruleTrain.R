#' Mine association rules from a tidy data.frame
#'
#' \code{aruleTrain()} mines association rules from a tidy customer-item
#' data.frame and returns them so they can later be reused for prediction with
#' \code{\link{arulePredict}}.
#'
#' @param data A data.frame in tidy (long) format with one customer-item pair
#'   per row. It must contain a customer ID column and a product ID column.
#' @param productkey The (unquoted) column containing the item/product IDs.
#' @param customerkey The (unquoted) column containing the customer IDs.
#' @param support Minimum support of the mined association rules.
#' @param confidence Minimum confidence of the mined association rules.
#' @param minlen Minimum length of the mined association rules.
#' @param maxlen Maximum length of the mined association rules. Defaults to the
#'   number of distinct products plus one.
#'
#' @return A data.frame of mined association rules with their quality metrics.
#'   When apriori finds no rules an empty data.frame with the same columns is
#'   returned (and an informative message is emitted).
#'
#' @export
#' @examples
#' \dontrun{
#' rules <- aruleTrain(data = dat, productkey = ProductKey,
#'                     customerkey = CustomerKey, support = 0.01,
#'                     confidence = 0.7, minlen = 2, maxlen = 20)
#' }

aruleTrain <- function(data,
                       productkey,
                       customerkey,
                       support    = 0.01,
                       confidence = 0.7,
                       minlen     = 2,
                       maxlen     = NULL) {

  # Resolve the default maxlen from the *actual* product column the user passed.
  if (is.null(maxlen)) {
    maxlen <- as.integer(dplyr::n_distinct(dplyr::pull(data, {{ productkey }})) + 1)
  }

  rules <- rule_flow(data,
                     {{ customerkey }},
                     {{ productkey }},
                     support,
                     confidence,
                     minlen,
                     maxlen)

  aruleTrain <- rules$rules %>%
    dplyr::mutate(lhs = as.character(lhs))

  export_function(aruleTrain)

  return(aruleTrain)
}
