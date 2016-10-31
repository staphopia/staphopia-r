#' get_resistance
#'
#' Retrieve all resistances in the database. Can be filtered based on
#' antibiotic and test.
#'
#' @param antibiotic An antibiotic to search for
#'
#' @param test An antibiotic test method to search for.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_resistance()
#' get_resistance(antibiotic="Vancomycin")
#' get_resistance(antibiotic="Vancomycin", test="Etest")
get_resistance <- function(antibiotic=NULL, test=NULL) {
    request <- '/resistance/'
    if (is.not.null(antibiotic)) {
        if (is.not.null(test)) {
            request <- paste0(request, '?antibiotic=', antibiotic,
                              '&test=', test)
        } else {
            request <- paste0(request, '/?antibiotic=', antibiotic)
        }
    }
    return(submit_get_request(request))
}

#' get_resistance_by_samples
#'
#' Given a list of sample IDs return the resistance phenotypes of each sample.
#'
#' @param sample_ids A vector of sample IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_resistance_by_samples(c(500,501,502))
#' get_resistance_by_samples(c(500,501,502), resistance_id = 1)
get_resistance_by_samples <- function(sample_ids, resistance_id=NULL) {
    request <- '/resistance/bulk_by_sample/'
    if (is.not.null(resistance_id)) {
        request <- paste0(request, '?resistance_id=', resistance_id)
    }

    return(submit_post_request(request, sample_ids, chunk_size=50))
}
