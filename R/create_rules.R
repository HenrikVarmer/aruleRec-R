#' Mine the association rules for the subsequent recommendations
#'
#' This function runs the apriori algorithm on a transactions object and returns
#' the mined rules as a tidy data.frame. When apriori finds no rules an empty
#' data.frame with the canonical rule columns is returned, so that downstream
#' code can handle the "no rules" case without erroring.
#' @param transactions A transactions object.
#' @param support Minimum support of the mined association rules.
#' @param confidence Minimum confidence of the mined association rules.
#' @param minlen Minimum length of the mined association rules.
#' @param maxlen Maximum length of the mined association rules.
#' @return A data.frame of rules with columns \code{lhs}, \code{rhs},
#'   \code{support}, \code{confidence}, \code{coverage}, \code{lift} and
#'   \code{count}.
#' @export
#' @examples
#' \dontrun{
#' create_rules(transactions, support = 0.01, confidence = 0.7,
#'              minlen = 2, maxlen = 20)
#' }

create_rules <- function(transactions, support, confidence, minlen, maxlen){

  # Canonical, correctly-typed empty rule set.
  empty_rules <- function() {
    data.frame(lhs        = character(0),
               rhs        = character(0),
               support    = numeric(0),
               confidence = numeric(0),
               coverage   = numeric(0),
               lift       = numeric(0),
               count      = integer(0),
               stringsAsFactors = FALSE)
  }

  rules <- arules::apriori(data      = transactions,
                           parameter = list(supp   = support,
                                            conf   = confidence,
                                            minlen = minlen,
                                            maxlen = maxlen))

  # Gracefully handle the case where apriori finds no rules.
  if (length(rules) == 0) {
    message("apriori() found no rules for the given support/confidence/length parameters. Returning an empty rule set.")
    return(empty_rules())
  }

  topicrules           <- arules::DATAFRAME(rules)
  colnames(topicrules) <- tolower(colnames(topicrules))

  # Output rules arranged by lift descending.
  # Regex removes curly brackets from rules prior to later joins.
  topicrules <- topicrules %>%
    dplyr::arrange(dplyr::desc(lift)) %>%
    tidyr::extract(rhs,
                   into   = c("rhs"),
                   regex  = "\\{(.*)\\}",
                   remove = TRUE) %>%
    tidyr::extract(lhs,
                   into   = c("lhs"),
                   regex  = "\\{(.*)\\}",
                   remove = TRUE)

  return(topicrules)
}
