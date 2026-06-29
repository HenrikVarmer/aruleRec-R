#' A data matrix function
#'
#' This function creates a data matrix
#' @param items_by_contact A dataframe containing items by contact
#' @param productkey The column containing item ID's
#' @param value The column containing the values used to fill the matrix
#' @param customerkey The column containing customer ID's
#' @export
#' @examples
#' \dontrun{
#' create_data_matrix(items_by_contact, productkey, value, customerkey)
#' }


create_data_matrix <- function(items_by_contact, productkey, value, customerkey){
  return(
    items_by_contact %>%
      spread(!! productkey,!! value, fill = 0)  %>%
      select(!! -customerkey)
  )
}