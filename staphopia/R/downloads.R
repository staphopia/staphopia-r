#' write_contigs_to_fasta
#'
#' @param sample_id
#' @param fasta_directory
#' @export write_contigs_to_fasta
#' @return none
write_contigs_to_fasta <- function(sample_id,fasta_directory) {
    library(Biostrings)
    contigs <- get_contigs(sample_id)
    sa_contigs <- DNAStringSet(contigs$sequence)
    names(sa_contigs) <- contigs$name
    path_name <- paste(fasta_directory,a,".fasta",sep="")
    writeXStringSet(sa_contigs, path_name, append=FALSE, format="fasta")
}
