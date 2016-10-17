#' get_gene_products
#'
#' Retrieve all gene products in Staphopia.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_gene_products()
get_gene_products <- function(term=FALSE) {
    if (term == FALSE) {
        return(submit_get_request('/gene/product/'))
    } else {
        return(submit_get_request(paste0('/gene/product/?term=', term)))
    }
}
