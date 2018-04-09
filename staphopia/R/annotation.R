#' get_genes
#'
#' Retrieve all predicted genes for a given sample.
#'
#' @param sample_id An integer sample ID or A vector of sample IDs
#' @param product_id Optional integer product ID
#' @param exclude_sequence Boolean, Do not return DNA and Amino Acid sequence
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_genes(500)
#' get_genes(c(500, 501, 502)
#' get_genes(c(500, 501), product_id=179)
get_genes <- function(sample_id, product_id=FALSE, exclude_sequence = FALSE) {
    q = paste0('?', ifelse(product_id, paste0('product_id=', product_id), ''),
               '&', ifelse(exclude_sequence, 'exclude_sequence', ''))
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


#' get_gene_products
#'
#' Retrieve a single gene product, gene products associated with a term, or
#' all gene products in Staphopia
#'
#' @param query An interger, query string or empty value.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_gene_products()
#' get_gene_products('meca')
#' get_gene_products(5:50)
get_gene_products <- function(query=NULL) {
    if (is.null(query)) {
        request <- paste0('/annotation/inference/bulk/')
        return(submit_get_request(request))
    } else if (is.character(query)) {
        request <- paste0('/annotation/inference/?term=', query)
        return(submit_get_request(request))
    } else {
        if (is_single_id(query)) {
            request <- paste0('/annotation/inference/', format_id(query), '/')
            return(submit_get_request(request))
        } else if (is_multiple_ids(query)) {
            request <- paste0('/annotation/inference/bulk_by_product/')
            return(submit_post_request(request, query, chunk_size=500))
        }
    }
}
