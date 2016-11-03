#' write_contigs_to_fasta
#'
#' @param sample_id (numeric)
#' @param fasta_directory (string)
#' @param filt_contigs (numeric) (all contigs with coverage lower than this are removed)
#' @param return_plasmids (if TRUE return a data_frame summarizing the plasmidSPADES header lines)
#'
#' @return data.frame (optional)
#' @export
#'
#' @examples
#' write_contigs_to_fasta(500)
#' write_contigs_to_fasta(500, output_dir='~/')
write_contigs_to_fasta <- function(sample_id, output_dir='./', filt_contigs = 2,
                                   return_plasmids = TRUE) {
    assembly <- get_contigs(sample_id)
    plasmids <- dplyr::filter(assembly, is_plasmids == TRUE)
    contigs <- dplyr::filter(assembly, is_plasmids == FALSE)
    #adds contig and strain id
    contig_tbl <- cbind(
        contigs$sample,
        contigs$id,
        as.data.frame(
            matrix(unlist(strsplit(contigs$name, "_")), byrow = TRUE, ncol = 6)
        )
    )

    keepers <- as.numeric(contig_tbl$V6) > filt_contigs
    sa_contigs <- Biostrings::DNAStringSet(contigs$sequence[keepers])
    names(sa_contigs) <- sapply(
        which(keepers),
        function(x) paste(as.character(contig_tbl[x,]), collapse="_")
    )
    fasta_out <- paste(output_dir, sample_id, "-contigs.fasta", sep="")
    Biostrings::writeXStringSet(
        sa_contigs, fasta_out, append=FALSE, format="fasta"
    )

    if (nrow(plasmids) > 0 && return_plasmids == TRUE) {
        plasmids_tbl <- plasmid_meta(plasmids)
        plasmids_tbl <- cbind(sample_id, plasmids_tbl)
        return(plasmids_tbl)
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
#' plasmid_meta(get_contigs(500))
plasmid_meta <- function(contigs){
    plasmids <- dplyr::filter(contigs, is_plasmids == TRUE)
    plasmids_df <- do.call(rbind.data.frame, strsplit(plasmids$name,"_"))
    colnames(plasmids_df) <- c("NODE","node_id","len","length","cov","coverage","comp","comp_id")
    plasmids_df$length <- as.numeric(as.character(plasmids_df$length))
    plasmids_df$coverage <- as.numeric(as.character(plasmids_df$coverage))
    plasmids_tbl <- dplyr::group_by(plasmids_df, comp_id) %>% dplyr::summarise(
        sum(length), sum(coverage * length)/sum(length)
    ) %>% as.data.frame()
    colnames(plasmids_tbl) <- c("component","bp","avg_coverage")

    return(plasmids_tbl)
}
