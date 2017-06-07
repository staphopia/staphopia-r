#' get_contig
#'
#' Retrieve assembled contigs by a given contig_id.
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


#' get_assembly
#'
#' Retrieve all assembled contigs for a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_assembly(500)
get_assembly <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/contigs/')
    return(submit_get_request(request))
}


#' get_assembly_stats
#'
#' Retrieve all stats associated with an assembly for a given sample.
#'
#' @param sample_id An integer sample ID, or vector of sample IDs
#' @param scaffolds Return scaffolds based stats. BOOL (Default FALSE)
#' @param plasmids Return plasmid stats. BOOL (Default FALSE)
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_assembly_stats(500)
#' get_assembly_stats(500, scaffolds=TRUE)
#' get_assembly_stats(500, scaffolds=TRUE, plasmids=TRUE)
#' get_assembly_stats(500, plasmids=TRUE)
#' get_assembly_stats(c(500, 1000, 1500))
get_assembly_stats <- function(sample_id, scaffolds=FALSE, plasmids=FALSE) {
    q = paste0('?', ifelse(scaffolds, 'scaffolds', ''),
               '&', ifelse(plasmids, 'plasmids', ''))
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
