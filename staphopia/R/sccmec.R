#' get_sccmec_primer_hits
#'
#' Retrieve SCCmec Primer hits for a given sample(s).
#'
#' @param sample_id An integer sample ID, or vector of sample IDs
#' @param exact_hits Return only those hits that are an exact match. BOOL (Default FALSE)
#' @param predict Return a predicted SCCmec type based on primer hits. BOOL (Default FALSE)
#' @param hamming Return a hamming distance per SCCmec type based on primer hits. BOOL (Default FALSE)
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sccmec_primer_hits(500)
#' get_sccmec_primer_hits(500, exact_hits=TRUE)
#' get_sccmec_primer_hits(500, predict=TRUE)
#' get_sccmec_primer_hits(c(500, 1000, 1500), predict=TRUE)
get_sccmec_primer_hits <- function(sample_id, exact_hits=FALSE, predict=FALSE, hamming=FALSE) {
    q = paste0('?', ifelse(exact_hits, 'exact_hits', ''),
               '&', ifelse(predict, 'predict', ''),
               '&', ifelse(hamming, 'hamming_distance', ''))
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/sccmec_primers/', q)
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/sccmec/primer/bulk_by_sample/', q)
        return(submit_post_request(request, format_id(sample_id), chunk_size=500))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}


#' get_sccmec_subtype_hits
#'
#' Retrieve SCCmec SubType Primer hits for a given sample(s).
#'
#' @param sample_id An integer sample ID, or vector of sample IDs
#' @param exact_hits Return only those hits that are an exact match. BOOL (Default FALSE)
#' @param predict Return a predicted SCCmec sub-type based on primer hits. BOOL (Default FALSE)
#' @param hamming Return a hamming distance per SCCmec type based on primer hits. BOOL (Default FALSE)
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sccmec_subtype_hits(500)
#' get_sccmec_subtype_hits(500, exact_hits=TRUE)
#' get_sccmec_subtype_hits(500, predict=TRUE)
#' get_sccmec_subtype_hits(c(500, 1000, 1500), predict=TRUE)
get_sccmec_subtype_hits <- function(sample_id, exact_hits=FALSE, predict=FALSE, hamming=FALSE) {
    q = paste0('?', ifelse(exact_hits, 'exact_hits', ''),
               '&', ifelse(predict, 'predict', ''),
               '&', ifelse(hamming, 'hamming_distance', ''))
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/sccmec_subtypes/', q)
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/sccmec/subtype/bulk_by_sample/', q)
        return(submit_post_request(request, format_id(sample_id), chunk_size=500))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}


#' get_sccmec_type
#'
#' Retrieve sccmec type based on primer hits for a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sccmec_type(500)
#' get_sccmec_type(c(500,502))
get_sccmec_type <- function(sample_id, hamming=FALSE) {
    return(get_sccmec_primer_hits(sample_id, predict=TRUE, hamming=hamming))
}

#' get_sccmec_subtype
#'
#' Retrieve sccmec subtype based on primer hits for a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sccmec_subtype(500)
#' get_sccmec_subtype(c(500,502))
get_sccmec_subtype <- function(sample_id, hamming=FALSE) {
    return(get_sccmec_subtype_hits(sample_id, predict=TRUE, hamming=hamming))
}

#' get_sccmec_protein_hits
#'
#' Retrieve SCCmec Protein hits for a given sample(s).
#'
#' @param sample_id An integer sample ID, or vector of sample IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sccmec_protein_hits(500)
#' get_sccmec_protein_hits(c(500, 1000, 1500))
get_sccmec_protein_hits <- function(sample_id, exact_hits=FALSE, predict=FALSE) {
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/sccmec_proteins/')
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/sccmec/protein/bulk_by_sample/')
        return(submit_post_request(request, format_id(sample_id), chunk_size=500))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}

#' get_sccmec_cassette_coverages
#'
#' Retrieve SCCmec Protein hits for a given sample(s).
#'
#' @param sample_id An integer sample ID, or vector of sample IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sccmec_cassette_coverages(500)
#' get_sccmec_cassette_coverages(c(500, 1000, 1500))
get_sccmec_cassette_coverages <- function(sample_id) {
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/sccmec_coverages/')
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/sccmec/coverage/bulk_by_sample/')
        return(submit_post_request(request, format_id(sample_id), chunk_size=500))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}
