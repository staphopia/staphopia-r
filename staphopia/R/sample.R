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


#' get_public_samples
#'
#' Retrieve publicly available ENA samples.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_public_samples()
get_public_samples <- function() {
    request <- paste0('/sample/public/')
    return(submit_get_request(request))
}

#' get_published_samples
#'
#' Retrieve published ENA samples.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_published_samples()
get_published_samples <- function() {
    request <- paste0('/sample/published/')
    return(submit_get_request(request))
}

#' get_unique_st_samples()
#'
#' Retrieve published ENA samples with a unique ST. Disclaimer: Same set of
#' samples is not gauranteed. Use 'tag' if you want a set that does not change.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_unique_st_samples()
get_unique_st_samples <- function() {
    request <- paste0('/sample/unique_st/')
    return(submit_get_request(request))
}

#' get_metadata
#'
#' Retrieve metadata associated with a sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_metadata(500)
get_metadata <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/metadata/')
    return(submit_get_request(request))
}

