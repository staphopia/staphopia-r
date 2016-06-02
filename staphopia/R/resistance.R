#' get_tags
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
#' get_resistances()
#' get_resistances(anitbiotic="Vancomycin")
#' get_resistances(anitbiotic="Vancomycin", test="Etest")
get_resistance <- function(antibiotic=NULL, test=NULL) {
    request <- NULL
    if (is.not.null(antibiotic)) {
        if (is.not.null(test)) {
            request <- paste0('/resistance/?antibiotic=', antibiotic,
                              '&test=', test)
        } else {
            request <- paste0('/resistance/?antibiotic=', antibiotic)
        }
    } else {
        request <- '/resistance/'
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
    count <- 0
    hits <- c()
    sample_chunk <- split_vector_into_chunks(sample_ids, 100)
    for (chunk in sample_chunk) {

        request <- NULL
        if (is.not.null(resistance_id)) {
            request <- paste0('/resistance/bulk_by_sample/?resistance_id=',
                              resistance_id)

        } else {
            request <- '/resistance/bulk_by_sample/'
        }
        json_data <- submit_post_request(request, list(ids=chunk))
        count <- count + json_data$count
        hits <- append(hits, list(json_data$results))
    }

    results <- data.table::rbindlist(hits)
    if (count == nrow(results)) {
        return(results)
    } else {
        return("Error!")
    }

}
