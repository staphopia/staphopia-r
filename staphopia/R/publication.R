#' get_publications
#'
#' Retrieve all publications associated with a given set of Sample ID(s).
#'
#' @param pmid An integer sample ID, or vector of sample IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_publications(500)
#' get_publications(c(500,501))
get_publications <- function(sample_id) {
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), "/pmid/")
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/publication/bulk_by_sample/')
        return(submit_post_request(request, sample_id, chunk_size=500))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}

#' get_citations
#'
#' Retrieve all publications in a given PubMed ID(s).
#'
#' @param pmid An integer PubMed ID, or vector of PubMed IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_citations()
#' get_citations(22674929)
#' get_citations(c(22674929,26672018))
get_citations <- function(pmid=FALSE) {
    if (is_single_id(pmid)) {
        request <- paste0('/publication/', format_id(pmid), "/")
        return(submit_get_request(request))
    } else if (is_multiple_ids(pmid)) {
        request <- paste0('/publication/bulk/', q)
        return(submit_post_request(request, pmid, chunk_size=100))
    } else {
        return(submit_get_request('/publication/'))
    }
}
