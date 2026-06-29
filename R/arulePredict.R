#' Generate recommendations from previously mined association rules
#'
#' \code{arulePredict()} takes a set of association rules (typically produced by
#' \code{\link{aruleTrain}}) and applies them to a new tidy customer-item
#' data.frame to produce per-customer cross-selling recommendations.
#'
#' @param rules A data.frame of association rules, e.g. the output of
#'   \code{\link{aruleTrain}}.
#' @param newdata A new data.frame in tidy (long) format with one customer-item
#'   pair per row.
#' @param customerkey The (unquoted) column containing the customer IDs.
#' @param productkey The (unquoted) column containing the item/product IDs.
#' @param keep_all Logical. If \code{FALSE} (default) only customers for whom a
#'   recommendation could be generated are returned. If \code{TRUE} every
#'   customer is returned, with \code{NA} in the rule columns where no
#'   recommendation exists.
#' @param nrec Optional integer. If supplied, only the top \code{nrec}
#'   recommendations (ranked by lift, descending) are returned per customer.
#'   Defaults to \code{NULL}, which returns all recommendations.
#'
#' @return A data.frame with one row per customer-recommendation pair. When the
#'   supplied rule set contains no usable rules, an empty data.frame with the
#'   same columns is returned (and an informative message is emitted).
#'
#' @export
#' @examples
#' \dontrun{
#' rules   <- aruleTrain(dat, ProductKey, CustomerKey)
#' preds   <- arulePredict(rules, newdata, CustomerKey, ProductKey,
#'                         keep_all = TRUE, nrec = 3)
#' }

arulePredict <- function(rules, newdata, customerkey, productkey,
                         keep_all = FALSE, nrec = NULL) {

  ruless <- rules %>%
    dplyr::mutate(lhs = as.character(lhs)) %>%
    dplyr::filter(lift >= 1)

  if (nrow(ruless) == 0) {
    message("arulePredict(): the supplied rule set contains no usable rules; returning an empty result.")
  }

  lhs_dat <- newdata %>%
    dplyr::select({{ customerkey }}, {{ productkey }}) %>%
    dplyr::group_by({{ customerkey }}, {{ productkey }}) %>%
    dplyr::mutate(rownumber = dplyr::row_number({{ customerkey }})) %>%
    dplyr::filter(rownumber == 1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ customerkey }}) %>%
    dplyr::arrange({{ productkey }}) %>%
    dplyr::mutate(lhs = as.character(paste0({{ productkey }}, collapse = ","))) %>%
    dplyr::arrange({{ customerkey }}) %>%
    dplyr::top_n(1, {{ productkey }}) %>%
    dplyr::select({{ customerkey }}, lhs) %>%
    as.data.frame()

  if (is.na(keep_all) || keep_all == FALSE) {
    predictions <- complete_fun(merge(x = lhs_dat, y = ruless, by = "lhs"), "rhs") %>%
      dplyr::arrange(dplyr::desc(lift)) %>%
      as.data.frame()
  } else if (keep_all == TRUE) {
    predictions <- merge(x = lhs_dat, y = ruless, by = "lhs", all.x = TRUE) %>%
      dplyr::arrange(dplyr::desc(lift)) %>%
      as.data.frame()
  }

  predictions <- predictions %>%
    dplyr::rename(item_history   = lhs,
                  recommendation = rhs)

  # Optionally keep only the top-N recommendations per customer (by lift).
  if (!is.null(nrec)) {
    predictions <- predictions %>%
      dplyr::group_by({{ customerkey }}) %>%
      dplyr::filter(dplyr::row_number() <= nrec) %>%
      dplyr::ungroup() %>%
      as.data.frame()
  }

  return(predictions)
}
