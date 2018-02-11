#' get_assembly_contigs
#'
#' Retrieve all assembled contigs for a given sample(s).
#'
#' @param sample_id An integer sample ID
#' @param plasmids Return plasmid stats. BOOL (Default FALSE)
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_assembly_contigs(500)
#' get_assembly_contigs(500, plasmids=TRUE)
#' get_assembly_contigs(c(500, 1000, 1500))
get_assembly_contigs<- function(sample_id, plasmids=FALSE) {
    q <- ''
    if (plasmids) {
        q <- '?plasmids'
    }
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/contigs/', q)
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/assembly/contig/bulk_by_sample/', q)
        return(submit_post_request(request, sample_id, chunk_size=20))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}


#' get_assembly_stats
#'
#' Retrieve all stats associated with an assembly for a given sample.
#'
#' @param sample_id An integer sample ID, or vector of sample IDs
#' @param plasmids Return plasmid stats. BOOL (Default FALSE)
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_assembly_stats(500)
#' get_assembly_stats(500, plasmids=TRUE)
#' get_assembly_stats(c(500, 1000, 1500))
get_assembly_stats <- function(sample_id, plasmids=FALSE) {
    q <- ''
    if (plasmids) {
        q <- '?plasmids'
    }
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/assembly/', q)
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/assembly/stat/bulk_by_sample/', q)
        return(submit_post_request(request, sample_id, chunk_size=500))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}
