#' write_contigs_to_fasta
#'
#' @param sample_id (numeric)
#' @param fasta_directory (string)
#' @param filt_contigs (numeric) (all contigs with coverage lower than this are removed)
#' @param return_plasmids (if TRUE return a data_frame summarizing the plasmidSPADES header lines)
#' @export
#' @return data.frame (optional)
write_contigs_to_fasta <- function(sample_id,fasta_directory, filt_contigs = 2, return_plasmids = TRUE) {
    library(Biostrings)
    library(dplyr)
    contigs <- get_contigs(sample_id)
    plasmids <- filter(contigs, is_plasmids == TRUE)
    contigs <- filter(contigs, is_plasmids == FALSE)
    #adds contig and strain id
    ctabl <- cbind(contigs$sample,contigs$id,as_data_frame(matrix(unlist(strsplit(contigs$name,"_")), byrow = TRUE, ncol = 6)))
    keepers <- as.numeric(ctabl$V6) > filt_contigs
    keepers_names <- sapply(which(keepers), function(x) paste(as.character(ctabl[x,]),collapse = "_"))
    sa_contigs <- DNAStringSet(contigs$sequence[keepers])
    names(sa_contigs) <- keepers_names
    path_name <- paste(fasta_directory,sample_id,".fasta",sep="")
    writeXStringSet(sa_contigs, path_name, append=FALSE, format="fasta")
    if (nrow(plasmids) > 0 && return_plasmids == TRUE) {
      plastabl <- plasmid_meta(plasmids)
      #plastabl <- cbind(sample_id,plastabl)
      return(plastabl)
    }
}


##get plasmids

#' plasmid_meta
#'
#' @param contig_data(df)
#'
#' @return data.frame
#' @export
#'
#' @examples
plasmid_meta <- function(contig_data){
    library(dplyr)
    contig_data <- filter(contig_data, is_plasmids == TRUE)
    pldf <- do.call(rbind.data.frame, strsplit(contig_data$name,"_"))
    colnames(pldf) <- c("NODE","node_id","len","length","cov","coverage","comp","comp_id")
    pldf$length <- as.numeric(as.character(pldf$length))
    pldf$coverage <- as.numeric(as.character(pldf$coverage))
    pltabl <- group_by(pldf,comp_id) %>% summarise(sum(length),sum(coverage*length)/sum(length)) %>% as.data.frame()
    #colnames(pltabl) <- c("component","bp","av_coverage")
    return(pltabl)
}
