#' write_contigs_to_fasta
#'
#' @param sample_id (numeric)
#' @param fasta_directory (string) (ending in '/')
#' @param filt_contigs (numeric) (all contigs with coverage lower than this are removed)
#'
#' @export
#'
#' @examples
#' write_contigs_to_fasta(500)
#' write_contigs_to_fasta(500, output_dir='~/')
write_contigs_to_fasta <- function(sample_id, output_dir='./', filt_contigs = 2) {
    assembly <- get_assembly(sample_id)
    contigs <- dplyr::filter(assembly, is_plasmids == FALSE)
    #adds contig and strain id
    contig_tbl <- cbind(
        contigs$sample,
        contigs$id,
        as.data.frame(
            matrix(unlist(strsplit(contigs$name, "_")), byrow = TRUE, ncol = 6)
        )
    )
    keepers <- as.numeric(as.character(contig_tbl$V6)) > filt_contigs
    contig_tbl$V6 <- as.character(contig_tbl$V6)
    contig_tbl$V4 <- as.character(contig_tbl$V4)
    sa_contigs <- Biostrings::DNAStringSet(contigs$sequence[keepers])
    names(sa_contigs) <- sapply(
        which(keepers),
        function(x) paste(as.character(contig_tbl[x,c(1,2,6,8)]), collapse="_")
    )
    fasta_out <- paste(output_dir,"sample_" ,sample_id, "_contigs.fasta", sep="")
    Biostrings::writeXStringSet(
        sa_contigs, fasta_out, append=FALSE, format="fasta"
    )
}

#' write_plasmids_to_fasta
#'
#' @param sample_id (numeric)
#' @param output_dir (string) (ending in '/')
#' @param filt_contigs (numeric) (all contigs with coverage lower than this are removed; default = 2)
#'
#' @return (data.frame) (summary of combined plasmid components)
#' @export
#'
#' @examples write_plasmids_to_fasta(500, output_dir='~/')
write_plasmids_to_fasta <- function(sample_id, output_dir='./', filt_contigs = 2) {
  assembly <- get_assembly(sample_id)
  contigs <- dplyr::filter(assembly, is_plasmids == TRUE)
  if (nrow(contigs) > 0 ) {
    contig_tbl <- cbind(
      contigs$sample,
      contigs$id,
      as.data.frame(
        matrix(unlist(strsplit(contigs$name, "_")), byrow = TRUE, ncol = 8)
      )
    )
    keepers <- as.numeric(as.character(contig_tbl$V6)) > filt_contigs
    contig_tbl$V6 <- as.character(contig_tbl$V6)
    contig_tbl$V4 <- as.character(contig_tbl$V4)
    sa_contigs <- Biostrings::DNAStringSet(contigs$sequence[keepers])
    # 4 fields in names - sample_id, contig_id, length, coverage
    names(sa_contigs) <- sapply(
      which(keepers),
      function(x) paste(as.character(contig_tbl[x,c(1,2,6,8)]), collapse="_")
    )
    fasta_out <- paste(output_dir,"sample_" ,sample_id, "_plasmids.fasta", sep="")
    Biostrings::writeXStringSet(
      sa_contigs, fasta_out, append=FALSE, format="fasta"
    )

    plasmids_tbl <- plasmid_meta(contigs)
    plasmids_tbl <- cbind(sample_id, plasmids_tbl)
    return(plasmids_tbl)
  }

}


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


#' write_proteins_2_fasta
#'
#' @param sample (single sample_id)
#' @param output_dir (with following '/', e.g: "./myproteins/")
#'
#' @return write_proteins_2_fasta
#' @export
#'
#' @examples write_proteins_2_fasta(500,"./protein_seqs/"
#' @examples output fasta header is of the format <sample>_<staphopia proteinid>_<product id>_<cluster id>_productname
write_proteins_2_fasta <- function(sample,output_dir = "./"){
  #output header is <sample>_<id>_productid_clusterid_productname
  genes <- get_genes(sample_id = sample)
  AAs <- genes[genes$aa > 0,]
  proteins <- Biostrings::AAStringSet(AAs$aa)
  pr_names <- sapply(1:nrow(AAs), function(x) paste(sample,AAs$id[x],AAs$product_id[x],AAs$cluster_id[x],AAs$product[x],sep = "_"))
  names(proteins) <- pr_names
  fasta_out <- paste(output_dir, "sample_",sample, "_aa.fasta", sep="")
  Biostrings::writeXStringSet(
    proteins, fasta_out, append=FALSE, format="fasta"
  )
}
