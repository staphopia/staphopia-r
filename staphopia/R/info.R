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


#' get_submission_by_year
#'
#' Retrieve the public submissions by year.
#'
#' @param all Boolean, include all available sample, not just the processed ones
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_submission_by_year()
get_submission_by_year <- function(all = FALSE) {
    request <- NULL
    if (all) {
        request <- '/info/submission_by_year/?all'
    } else {
        request <- '/info/submission_by_year/'
    }

    return(submit_get_request(request))
}

#' get_rank_by_year
#'
#' Retrieve the rank (Gold Silver Bronze) of public submissions by year.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_rank_by_year()
get_rank_by_year <- function() {
    return(submit_get_request('/info/rank_by_year/'))
}

#' get_st_by_year
#'
#' Retrieve the sequence type assignments of public submissions by year.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_st_by_year()
get_st_by_year <- function() {
  return(submit_get_request('/info/st_by_year/'))
}

#' get_public_cgmlst_patterns
#'
#' Retrieve the cgMLST pattern counts for public samples.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_public_cgmlst_patterns()
get_public_cgmlst_patterns <- function() {
    return(submit_get_request('/info/cgmlst_patterns/'))
}


#' get_publication_links
#'
#' Retrieve counts for how public samples were linked to publications.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_publication_links()
get_publication_links <- function() {
    return(submit_get_request('/info/publication_links/'))
}
