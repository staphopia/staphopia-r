#' get_genes
#'
#' Retrieve all predicted genes for a given sample.
#'
#' @param sample_id An integer sample ID or A vector of sample IDs
#' @param product_id Optional integer product ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_genes(500)
#' get_genes(c(500, 501, 502)
#' get_genes(c(500, 501), product_id=179)
get_genes <- function(sample_id, product_id=FALSE) {
    q <- ''
    if (product_id) {
        q <- paste0('?product_id=', product_id)
    }

    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/genes/', q)
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/annotation/gene/bulk_by_sample/', q)
        return(submit_post_request(request, sample_id, chunk_size=20))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}
