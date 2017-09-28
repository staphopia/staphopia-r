#' get_assembly_stats_by_year
#'
#' Retrieve assembled stats by year based on when samples were first made public.
#'
#' @param is_scaffolds Boolean
#' @param is_plasmids Boolean
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_assembly_stats_by_year(is_scaffolds=TRUE, is_plasmids=FALSE)
get_assembly_stats_by_year <- function(is_scaffolds=FALSE, is_plasmids=FALSE) {
    request <- NULL
    if (is_scaffolds && is_plasmids) {
        request <- '/info/assembly_by_year/?is_plasmids&is_scaffolds'
    } else if (is_scaffolds) {
        request <- '/info/assembly_by_year/?is_scaffolds'
    } else if (is_plasmids) {
        request <- '/info/assembly_by_year/?is_plasmids'
    } else {
        request <- '/info/assembly_by_year/'
    }

    return(submit_get_request(request))
}

#' get_sequencing_stats_by_year
#'
#' Retrieve sequencing stats by year based on when samples were first made public.
#'
#' @param is_original Boolean
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sequencing_stats_by_year(is_original=TRUE)
get_sequencing_stats_by_year <- function(is_original=FALSE) {
    request <- NULL
    if (is_original) {
        request <- '/info/sequencing_by_year/?is_original'
    } else {
        request <- '/info/sequencing_by_year/'
    }

    return(submit_get_request(request))
}

#' get_top_sequence_types
#'
#' Retrieve top X (default 10) sequence sequence types.
#'
#' @param total Integer
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_top_sequence_types(20)
get_top_sequence_types <- function(total=10) {
    request <- paste0('/top/', total, '/sequence_types/')
    return(submit_get_request(request))
}
