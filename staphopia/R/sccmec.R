#' get_sccmec_primer_hits
#'
#' Retrieve SCCmec Primer hits for a given sample(s).
#'
#' @param sample_id An integer sample ID, or vector of sample IDs
#' @param exact_hits Return only those hits that are an exact match. BOOL (Default FALSE)
#' @param predict Return a predicted SCCmec type based on primer hits. BOOL (Default FALSE)
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sccmec_primer_hits(500)
#' get_sccmec_primer_hits(500, exact_hits=TRUE)
#' get_sccmec_primer_hits(500, predict=TRUE)
#' get_sccmec_primer_hits(c(500, 1000, 1500), predict=TRUE)
get_sccmec_primer_hits <- function(sample_id, exact_hits=FALSE, predict=FALSE) {
    q = paste0('?', ifelse(exact_hits, 'exact_hits', ''),
               '&', ifelse(predict, 'predict', ''))
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/sccmec_primers/', q)
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/sccmec/primer/bulk_by_sample/', q)
        return(submit_post_request(request, format_ids(sample_id), chunk_size=500))
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
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sccmec_subtype_hits(500)
#' get_sccmec_subtype_hits(500, exact_hits=TRUE)
#' get_sccmec_subtype_hits(500, predict=TRUE)
#' get_sccmec_subtype_hits(c(500, 1000, 1500), predict=TRUE)
get_sccmec_subtype_hits <- function(sample_id, exact_hits=FALSE, predict=FALSE) {
    q = paste0('?', ifelse(exact_hits, 'exact_hits', ''),
               '&', ifelse(predict, 'predict', ''))
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/sccmec_subtypes/', q)
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/sccmec/subtype/bulk_by_sample/', q)
        return(submit_post_request(request, format_ids(sample_id), chunk_size=500))
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
get_sccmec_type <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/sccmec_primers/?predict')
    return(submit_get_request(request))
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
get_sccmec_subtype <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/sccmec_subtypes/?predict')
    return(submit_get_request(request))
}

#' get_sccmec_subtypes
#'
#' Retrieve all predicted sccmec subtypes for a given sample.
#'
#' @param sample_id An integer sample ID or A vector of sample IDs
#' @param product_id Optional integer product ID
#' @param cluster_id Optional integer cluster ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sccmec_subtypes(500)
#' get_sccmec_subtypes(c(500, 501, 502)
#' get_sccmec_subtypes(c(500, 501)))
get_sccmec_subtypes <- function(sample_id) {


    data <- c()
    count <- 0
    for (id in sample_id) {
        request <- paste0('/sample/', format_id(id), '/sccmec_subtypes/?predict')
        json_data <- submit_get_request(request)
        data <- append(data, list(json_data))
        if (is.not.null(nrow(json_data))) {
            count <- count + nrow(json_data)

        }
    }

    results <- data.table::rbindlist(data)
    if (count == nrow(results)) {
        return(results)
    } else {
        print(count)
        print(nrow(results))
        return('Error! Count is not equal to number of rows!')
    }

}
