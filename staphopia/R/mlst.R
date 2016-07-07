#' get_mlst_by_sample
#'
#' Given a list of sample IDs return the MLST (SRST2) for each sample.
#'
#' @param sample_ids A vector of sample IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_mlst_by_sample(c(500,501,502))
get_mlst_by_sample <- function(sample_ids) {
    request <- '/mlst/srst2/bulk_by_sample/'
    return(submit_post_request(request, sample_ids, chunk_size=50))
}
