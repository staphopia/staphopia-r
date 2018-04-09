#' get_resistance_results
#'
#' Retrieve resistances as reported by Ariba for a given sample.
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' test_staphopia()
test_staphopia <- function() {
    devtools::test()
}

#' format_id
#'
#' In case scientific notation is turned on, convert it to an integer. This
#' function should not be directly used by the user.
#'
#' @param x A numeric representation of an ID
#'
#' @return ID as an integer.
format_id <- function(x) {
    return(sapply(x, function(x){format(x, scientific=F)}))
}


#' is.not.null
#'
#' A wrapper for "!is.null". This function should not be directly used by the
#' user.
#'
#' @param x
#'
#' @return bool TRUE is not null or FALSE if null.
is.not.null <- function(x) {
    return(!is.null(x))
}

#' split_vector
#'
#' Split a vector into smaller vectors based on the given size. This function
#' should not be directly used by the user.
#'
#' @param x Vector to be split.
#' @param size A number elements to split a vector by.
#'
#' @return bool TRUE is not null or FALSE if null.
split_vector_into_chunks <- function(x, size) {
    return(split(x, ceiling(seq_along(x)/size)))
}

#' is_single_id
#'
#' A wrapper for to validate given id is single and proper type. This function
#' should not be directly used by the user.
#'
#' @param x
#'
#' @return bool TRUE is a single id else FALSE.
is_single_id <- function(x) {
    if (length(x) == 1 & (typeof(x) == 'integer' | typeof(x) == 'double')) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' is_multiple_ids
#'
#' A wrapper for to validate given vector is multiple ids and proper type. This
#' function should not be directly used by the user.
#'
#' @param x
#'
#' @return bool TRUE is multiple ids else FALSE.
is_multiple_ids <- function(x) {
    if (length(x) > 1 & (typeof(x) == 'integer' | typeof(x) == 'double')) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' write_plot
#'
#' A wrapper for to validate given vector is multiple ids and proper type. This
#' function should not be directly used by the user.
#'
#' @param plot_object A ggplot object
#' @param name Basename for the output PDF and PNG files
#' @param height The PDF height of the output (Default: 5)
#' @param width The PDF width of the object (Default: 12)
#'
#' @export
#' @return bool TRUE is multiple ids else FALSE.
write_plot <- function(plot_object, name, height = 5, width = 12) {
    pdf(paste0(name, ".pdf"), width=width, height=height)
    print(plot_object)
    dev_null <- dev.off()

    png(paste0(name, ".png"), width=width*100, height=height*100)
    print(plot_object)
    dev_null <- dev.off()
}
