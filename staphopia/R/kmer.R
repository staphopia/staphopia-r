#' get_kmers_by_sequence
#'
#' Given a list of sample IDs and a seqeunce return 31-mers and counts.
#'
#' @param sample_ids A vector of sample IDs
#' @param sequence A DNA sequence
#' @param reverse_complement Option to reverse complement k-mers or not.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_kmers_by_sequence(c(500,501,502), 'ATGC')
get_kmers_by_sequence <- function(sample_ids, sequence, reverse_complement=TRUE) {
    tables <- list()
    partitions <- get_kmer_partitions()
    kmers <- split_sequence_into_kmers(sequence,
                                       reverse_complement=reverse_complement)
    for (kmer in kmers) {
        partition <- tolower(partitions[[ stringr::str_sub(kmer, start=-7) ]])
        table = paste0('kmer_', partition)
        if (!(table %in% names(tables))) {
            tables[[ table ]] <- c(kmer)
        } else {
            tables[[ table ]] <- c(kmer)
        }
    }
    results <- c()
    count <- 0
    for (table in names(tables)) {
        request <- '/kmer/by_partition/'
        url <- build_url(request)
        json_data <- post_request(
            url,
            list(
                ids=sample_ids,
                extra=list(partition=table, kmers=tables[[ table ]])
            )
        )
        count <- count + json_data$count
        results <- append(results, list(json_data$results))
        #Sys.sleep(0.0)
    }

    results <- data.table::rbindlist(results)
    if (count == nrow(results)) {
        return(results)
    } else {
        return('Error! Count is not equal to number of rows!')
    }
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
#' Split a sequence into a set of k-mers.
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


#' get_kmer_partitions
#'
#' Get partitions in which kmers are split into.
#'
#' @export
#' @return A list of partitions.
get_kmer_partitions <- function(sequence, k=31, reverse_complement=TRUE) {
    request <- '/kmer/partitions/?format=json'
    return(submit_get_request(request))
}

substr_right <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
}
