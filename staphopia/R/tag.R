#' get_all_tags
#'
#' Retrieve all Tags in Staphopia.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_all_tags()
get_all_tags <- function() {
    return(submit_get_request('/tag/'))
}

#' get_tag_by_name
#'
#' Retrieve retrieve a tag by its name.
#'
#' @param tag The tag name as a string
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_tag_by_name('my-tag-name')
get_tag_by_name <- function(tag) {
    request <- paste0('/tag/?tag=', tag)
    return(submit_get_request(request))
}

#' get_samples_by_tag
#'
#' Retrieve all samples associated with a given tag.
#'
#' @param tag_id An integer tag ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_samples_by_tag(5)
get_samples_by_tag <- function(tag_id) {
    request <- paste0('/tag/', format_id(tag_id), '/samples/')
    return(submit_get_request(request))
}
