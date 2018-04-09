#' get_resistance_results
#'
#' Retrieve resistances as reported by Ariba for a given sample.
#'
#' @param sample_id An integer sample ID, or vector of sample IDs
#' @param summary A boolean to return a summary of the ariba results
#' @param resistance_report A boolean to return a resistance report based on Ariba summary
#' @param cluster_report A boolean to return a cluster report based on Ariba summary
#' @param include_all Return all return summary results whether or not a match exists
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_resistance_results(500)
#' get_resistance_results(c(500, 1000, 1500))
get_resistance_results <- function(sample_id, summary=FALSE, resistance_report=FALSE, cluster_report=FALSE, include_all=FALSE) {
    q = paste0('?', ifelse(summary, 'summary', ''),
               '&', ifelse(resistance_report, 'report', ''),
               '&', ifelse(cluster_report, 'cluster_report', ''),
               '&', ifelse(include_all, 'include_all', ''))

    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/resistance/', q)
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/resistance/ariba/bulk_by_sample/', q)
        return(submit_post_request(request, sample_id, chunk_size=1000))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}
