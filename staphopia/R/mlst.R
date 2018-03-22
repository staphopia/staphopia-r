#' get_sequence_type
#'
#' Retrieve MLST results based on Ariba, Mentalist and BLAST for a given sample(s).
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

#' get_mlst_blast_results
#'
#' Retrieve MLST BLAST results for a given sample(s).
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_mlst_blast_results(500)
#' get_mlst_blast_results(c(500,501))
get_mlst_blast_results <- function(sample_id) {
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/st_blast/')
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/mlst/blast_by_sample/')
        return(submit_post_request(request, sample_id, chunk_size=1000))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}


#' get_mlst_allele_matches
#'
#' Retrieve MLST allele match counts for a given sample(s).
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_mlst_allele_matches(500)
#' get_mlst_allele_matches(c(500,501))
get_mlst_allele_matches <- function(sample_id) {
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/st_allele/')
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/mlst/allele_by_sample/')
        return(submit_post_request(request, sample_id, chunk_size=1000))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}

#' get_cgmlst
#'
#' Retrieve cgMLST results based on Mentalist for a given sample(s).
#'
#' @param sample_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_cgmlst(500)
#' get_cgmlst(c(500,501))
get_cgmlst <- function(sample_id) {
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/cgmlst/')
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/cgmlst/bulk_by_sample/')
        return(submit_post_request(request, sample_id, chunk_size=200))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}


#' cgmlst_distance
#'
#' Using output from 'get_cgmlst' determine the pairwise hamming distance between each sample.
#'
#' @param cgmlst_data_frame Data Frame from 'get_cgmlst' function
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' cgmlst_distance(cgmlst_data_frame)
cgmlst_distance <- function(df) {
    n = nrow(df)
    col = ncol(df)
    m <- matrix(nrow=n, ncol=n)
    # Use first column (sample_id) as row and col names
    rownames(m) <- df$sample_id
    colnames(m) <- df$sample_id
    # TODO: Improve speed (~2 minutes for 49 samp)
    for (i in 1:n) {
        if (i %% 10 == 0) {
            print(paste0("Working on ", i, " of ", n))
        }
        for (j in i:n) {
            m[j, i] <- m[i, j] <- sum(as.numeric(as.vector(df[i,2:col])) != as.numeric(as.vector(df[j,2:col])))
        }
    }
    return(m)
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
get_samples_by_st <- function(st) {
    request <- paste0('/sample/?st=', format_id(st))
    return(submit_get_request(request))
}
