
#' Easy and fast recommendations with association rule learning in R
#'
#' \code{aruleRec()} mines association rules from a tidy customer-item
#' data.frame and turns them into per-customer cross-selling recommendations.
#'
#' @param data A data.frame in tidy (long) format with one customer-item pair
#'   per row. It must contain (at least) a customer ID column and a product ID
#'   column.
#' @param productkey The (unquoted) column containing the item/product IDs.
#' @param customerkey The (unquoted) column containing the customer IDs.
#' @param keep_all Logical. If \code{FALSE} (default) only customers for whom a
#'   recommendation could be generated are returned. If \code{TRUE} every
#'   customer is returned, with \code{NA} in the rule columns where no
#'   recommendation exists.
#' @param support Minimum support of the mined association rules.
#' @param confidence Minimum confidence of the mined association rules.
#' @param minlen Minimum length of the mined association rules.
#' @param maxlen Maximum length of the mined association rules. Defaults to the
#'   number of distinct products plus one.
#' @param nrec Optional integer. If supplied, only the top \code{nrec}
#'   recommendations (ranked by lift, descending) are returned per customer.
#'   Defaults to \code{NULL}, which returns all recommendations.
#'
#' @return A data.frame with one row per customer-recommendation pair,
#'   containing the customer ID, the item history, the recommended item and the
#'   rule quality metrics (support, confidence, coverage, lift, count). When the
#'   apriori algorithm finds no rules, an empty data.frame with the same columns
#'   is returned (and an informative message is emitted).
#'
#' @export
#' @examples
#' \dontrun{
#' aruleRec(data = dat, productkey = ProductKey, customerkey = CustomerKey,
#'          keep_all = FALSE, minlen = 2, maxlen = 20,
#'          support = 0.01, confidence = 0.7, nrec = 3)
#' }

aruleRec <- function(data,
                     productkey,
                     customerkey,
                     keep_all   = FALSE,
                     support    = 0.01,
                     confidence = 0.7,
                     minlen     = 2,
                     maxlen     = NULL,
                     nrec       = NULL) {

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

  all_rules <- rules$rules %>%
    dplyr::mutate(lhs = as.character(lhs)) %>%
    dplyr::filter(lift >= 1)

  lhs_dat <- data %>%
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
    recommendations <- complete_fun(merge(x = lhs_dat, y = all_rules, by = "lhs"), "rhs") %>%
      dplyr::arrange(dplyr::desc(lift)) %>%
      as.data.frame()
  } else if (keep_all == TRUE) {
    recommendations <- merge(x = lhs_dat, y = all_rules, by = "lhs", all.x = TRUE) %>%
      dplyr::arrange(dplyr::desc(lift)) %>%
      as.data.frame()
  }

  recommendations <- recommendations %>%
    dplyr::rename(item_history   = lhs,
                  recommendation = rhs)

  # Optionally keep only the top-N recommendations per customer (by lift).
  if (!is.null(nrec)) {
    recommendations <- recommendations %>%
      dplyr::group_by({{ customerkey }}) %>%
      dplyr::filter(dplyr::row_number() <= nrec) %>%
      dplyr::ungroup() %>%
      as.data.frame()
  }

  if (nrow(recommendations) == 0) {
    message("aruleRec(): no recommendations could be generated for the given parameters.")
  }

  export_function(all_rules)

  return(recommendations)
}
