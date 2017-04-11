#' get_sccmec_primers
#'
#' Retrieve sccmec type based on primer hits for a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sccmec_primers(500)
get_sccmec_primers <- function(sample_id) {
    request <-
        request <- paste0('/sample/', format_id(sample_id), '/sccmec_primers/')
    return(submit_get_request(request))
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
