#' get_sequence_type
#'
#' Retrieve MLST results based on SRST2 for a given sample(s).
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sequence_type(500)
#' get_sequence_type(c(500,501))
get_sequence_type <- function(sample_id) {
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/st/')
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/mlst/bulk_by_sample/')
        return(submit_post_request(request, sample_id, chunk_size=200))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}


#' get_samples_by_st
#'
#' Given a ST, return the sample IDs with MLST (SRST2).
#'
#' @param st A integer MLST
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_samples_by_st(8)
get_samples_bt_st <- function(st) {
    request <- paste0('/sample/?st=', format_id(st))
    return(submit_get_request(request))
}
