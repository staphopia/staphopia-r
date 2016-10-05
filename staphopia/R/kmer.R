#' get_kmers_by_sequence
#'
#' Given a list of sample IDs and a seqeunce return 31-mers and counts.
#'
#' @param sample_ids A vector of sample IDs
#' @param sequence A DNA sequence
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_kmers_by_sequence(c(500,501,502), 'ATGC')
get_kmers_by_sequence <- function(sample_ids, sequence) {
    request <- '/kmer/kmer_test/'
    return(submit_get_request(request))
    #return(submit_post_request(
    #    request, sample_ids, extra_data=list(sequence=sequence)
    #))
}


#' rev_comp
#'
#' Reverse complement an input sequence.
#'
#' @param sequence A DNA sequence
#'
#' @return Reverse complemented sequence as a string.
rev_comp <- function(sequence) {
    return(as.character(
        Biostrings::reverseComplement(Biostrings::DNAString(sequence))
    ))
}


#' split_sequence_into_kmers
#'
#' Split a seqeunce into a set of k-mers.
#'
#' @param sequence A DNA sequence
#' @param k A value of 'k' to split sequence into. Default is 31.
#' @param reverse_complement Option to reverse complement k-mers or not.
#'
#' @export
#' @return A vecter of k-mers.
split_sequence_into_kmers <- function(sequence, k=31, reverse_complement=TRUE) {
    n <- nchar(sequence) - k + 1
    kmers <- substring(sequence, 1:n, 1:n + k - 1)
    if (reverse_complement == TRUE) {
        kmers <- c(kmers, as.character(sapply(kmers, rev_comp)))
    }
    return(unique(kmers))
}
