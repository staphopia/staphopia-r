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


#' get_tags
#'
#' Retrieve all Tags associated with a given sample.
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_tags(500)
get_tags <- function(sample_id) {
    request <- paste0('/sample/', format_id(sample_id), '/tags/')
    return(submit_get_request(request))
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
    if (typeof(tag_id) == "integer" | typeof(tag_id) == "double") {
        request <- paste0('/tag/', format_id(tag_id), '/samples/')
        return(submit_get_request(request))
    } else {
        if (typeof(tag_id) == "character") {
            tag <- get_tag_by_name(tag_id)
            if (is.not.null(tag$id)) {
                request <- paste0('/tag/', format_id(tag$id), '/samples/')
                return(submit_get_request(request))
            } else {
                warning(paste0("No tags exist under the tag: ", tag_id),
                        immediate. = TRUE)
            }
        } else {
            warning(paste0("`get_samples_by_tag()` required an integer `id` ",
                           "or character `name`. Recieved: ", tag_id),
                    immediate. = TRUE)
        }
    }
}
