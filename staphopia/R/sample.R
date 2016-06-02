#' get_assembly_stats
#'
#' Retrieve all stats associated with an assembly for a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_assembly_stats(500)
get_assembly_stats <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/assembly/')
    return(submit_get_request(request))
}

#' get_contigs
#'
#' Retrieve all assembled contigs for a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_contigs(500)
get_contigs <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/contigs/')
    return(submit_get_request(request))
}

#' get_genes
#'
#' Retrieve all predicted genes for a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_genes(500)
get_genes <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/genes/')
    return(submit_get_request(request))
}

#' get_indels
#'
#' Retrieve all InDels present in a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_indels(500)
get_indels <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/indels/')
    return(submit_get_request(request))
}

#' get_sequence_quality
#'
#' Retrieve sequence quality metrics for a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sequence_quality(500)
get_sequence_quality <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/qc/')
    return(submit_get_request(request))
}

#' get_snps
#'
#' Retrieve all SNPs present in a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_snps(500)
get_snps <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/snps/')
    return(submit_get_request(request))
}

#' get_st
#'
#' Retrieve MLST results based on SRST2 for a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_st(500)
get_st <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/st_srst2/')
    return(submit_get_request(request))
}

#' get_st_blast
#'
#' Retrieve MLST results based on BLAST hits for a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_st_blast(500)
get_st_blast <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/st_blast/')
    return(submit_get_request(request))
}

#' get_tags
#'
#' Retrieve all Tags associated with a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_tags(500)
get_tags <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/tags/')
    return(submit_get_request(request))
}
