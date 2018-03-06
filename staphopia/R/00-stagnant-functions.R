# These are functions to be reveiwed for removal
# Review the following:
#   gwas.R
#   kmer.R
#   heatmap.R
#   plots.R

#' get_contig
#'
#' Retrieve assembled contigs by a given contig_id.  REWORK THIS QUERY
#'
#' @param contig_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_contig(500)
get_contig <- function(contig_id) {
    request <- paste0('/assembly/contig/', format_id(contig_id), '/')
    return(submit_get_request(request))
}


#' get_gene_products
#'
#' Retrieve all gene products in Staphopia.  BRING THIS BACK
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


#' get_gene_product
#'
#' Retrieve gene product infor for a given ID. BRING THIS BACK AS SINGLE QUERY
#'
#' @param product_id An integer product ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_gene_product(500)
get_gene_product <- function(product_id) {
    return(submit_get_request(paste0('/gene/product/', product_id, '/')))
}

#' get_variant_counts
#'
#' Given a list of Sample IDs return SNP/InDel counts.  REWORK
#'
#' @param sample_ids A vector of sample IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_variant_counts(c(5000,5001,5002))
get_variant_counts <- function(sample_ids) {
    request <- '/variant/count/bulk_by_sample/'
    return(submit_post_request(request, sample_ids, chunk_size=500))
}
