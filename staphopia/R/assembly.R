#' get_contig
#'
#' Retrieve assembled contigs by a given contig_id.
#'
#' @param contig_id. An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_contig(500)
get_contig <- function(contig_id.) {
    request <- paste0('/assembly/contig/', format_id(contig_id.), '/')
    return(submit_get_request(request))
}
