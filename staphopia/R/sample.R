#' get_sample_by_name
#'
#' Retrieve a sample by its name.
#'
#' @param name An string name
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sample_by_name("ERX389233")
get_sample_by_name <- function(name) {
    return(submit_get_request(paste0('/sample/?name=', name)))
}

#' get_public_samples
#'
#' Retrieve publicly available ENA samples.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_public_samples()
get_public_samples <- function(include_location=FALSE) {
    request <- paste0('/sample/public/')
    if (include_location == TRUE) {
        request <- paste0('/sample/public/?include_location')
    }
    return(submit_get_request(request))
}

#' get_published_samples
#'
#' Retrieve published ENA samples.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_published_samples()
get_published_samples <- function() {
    request <- paste0('/sample/published/')
    return(submit_get_request(request))
}

#' get_unique_st_samples()
#'
#' Retrieve published ENA samples with a unique ST. Disclaimer: Same set of
#' samples is not gauranteed. Use 'tag' if you want a set that does not change.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_unique_st_samples()
get_unique_st_samples <- function() {
    request <- paste0('/sample/unique_st/')
    return(submit_get_request(request))
}

#' get_metadata
#'
#' Retrieve metadata associated with a sample.
#'
#' @param sample_id An integer sample ID, or vector of sample IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_metadata(500)
#' get_metadata(c(500, 501, 1000))
get_metadata <- function(sample_id) {
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/metadata/')
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- '/metadata/bulk_by_sample/'
        return(submit_post_request(request, sample_id, chunk_size=500))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}


#' get_samples_by_indel
#'
#' Given a InDel ID(s) return the samples in which InDel is present.
#'
#' @param indel_id An InDel ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_samples_by_indel(c(5000))
get_samples_by_indel <- function(indel_id) {
    if (is_single_id(indel_id)) {
        request <- paste0('/variant/indel/', format_id(indel_id), '/samples/')
        return(submit_get_request(request))
    } else if (is_multiple_ids(indel_id)) {
        request <- '/variant/indel/bulk_samples/'
        return(submit_post_request(request, indel_id, chunk_size=100))
    } else {
        warning('indel_id is not the expected type (integer(s) or double(s))')
    }
}

#' get_samples_by_snp
#'
#' Given a SNP ID(s) return the samples in which SNP is present.
#'
#' @param snp_id An SNP ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_samples_by_snp(c(5000))
get_samples_by_snp <- function(snp_id) {
    if (is_single_id(snp_id)) {
        request <- paste0('/variant/snp/', format_id(snp_id), '/samples/')
        return(submit_get_request(request))
    } else if (is_multiple_ids(snp_id)) {
        request <- '/variant/snp/bulk_samples/'
        return(submit_post_request(request, snp_id, chunk_size=100))
    } else {
        warning('snp_id is not the expected type (integer(s) or double(s))')
    }
}
