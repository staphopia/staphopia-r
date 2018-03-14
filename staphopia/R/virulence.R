#' get_virulence_results
#'
#' Retrieve virulences as reported by Ariba for a given sample.
#'
#' @param sample_id An integer sample ID, or vector of sample IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_virulence_results(500)
#' get_virulence_results(c(500, 1000, 1500))
get_virulence_results <- function(sample_id) {
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/virulence/')
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/virulence/ariba/bulk_by_sample/')
        return(submit_post_request(request, sample_id, chunk_size=1000))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}
