#' write_contigs_to_fasta
#'
#' @param sample_id (numeric)
#' @param fasta_directory (string)
#' @param filt_contigs (numeric) (all contigs with coverage lower than this are removed)
#' 
#' @export
#' @return none
write_contigs_to_fasta <- function(sample_id,fasta_directory, filt_contigs = 2) {
    library(Biostrings)
    contigs <- get_contigs(sample_id)
    contigs <- filter(contigs, is_plasmids == FALSE)
    #adds contig and strain id
    ctabl <- cbind(contigs$sample,contigs$id,as_data_frame(matrix(unlist(strsplit(contigs$name,"_")), byrow = TRUE, ncol = 6)))
    keepers <- ctabl$V6 > filt_contigs
    keepers_names <- sapply(which(keepers), function(x) paste(as.character(ctabl[x,]),collapse = "_"))
    sa_contigs <- DNAStringSet(contigs$sequence[keepers])
    names(sa_contigs) <- keepers_names
    path_name <- paste(fasta_directory,sample_id,".fasta",sep="")
    writeXStringSet(sa_contigs, path_name, append=FALSE, format="fasta")
}


##get plasmids
