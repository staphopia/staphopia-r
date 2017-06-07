#' get_sequence_quality
#'
#' Retrieve sequence quality metrics for a given sample. By default only the
#' post processed stats are returned. Use the `original_stats` parameter to
#' return the stats based on the raw FASTQ file.
#'
#' @param sample_id An integer sample ID, or vector of sample IDs
#' @param original_stats Return stats based on raw FASTQ. BOOL (Default FALSE)
#' @param qual_per_base Include quality per base stats. BOOL (Default FALSE)
#' @param read_lengths Include read length distribution. BOOL (Default FALSE)
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_sequence_quality(500)
#' get_sequence_quality(500, original_stats=TRUE)
#' get_sequence_quality(500, qual_per_base=TRUE)
#' get_sequence_quality(500, read_lengths=TRUE)
#' get_sequence_quality(c(500, 1000, 1500))
get_sequence_quality <- function(sample_id, original_stats=FALSE,
                                 qual_per_base=FALSE, read_lengths=FALSE) {
    q = paste0('?', ifelse(original_stats, 'original', ''),
               '&', ifelse(qual_per_base, 'bases', ''),
               '&', ifelse(read_lengths, 'lengths', ''))

    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/qc/', q)
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/sequence-quality/bulk_by_sample/', q)
        return(submit_post_request(request, sample_id, chunk_size=500))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}
