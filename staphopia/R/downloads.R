#' write_assembly
#'
#' Write the assembled contigs to a FASTA file for a given sample.
#'
#' Example headers:
#' >500|ERX105420|1 coverage=38.394969 length=225174 analysis_version=112017
#' >500|ERX105420|1|plasmid coverage=50.715860 length=41605 analysis_version=112017
#'
#' @param sample_id An integer sample ID
#' @param output_dir A string, directory to output fasta file(s) to. Default: ./
#' @param plasmids Return plasmid stats. BOOL (Default FALSE)
#' @param coverage A numeric value. All contigs with coverage lower than this are removed. Default: 2
#'
#' @return write_assembly
#' @export
#'
#' @examples write_assembly(500,"./protein_seqs/"
#' @examples output fasta header is of the format <sample>_<staphopia proteinid>_<product id>_<cluster id>_productname
write_assembly <- function(sample_id, output_dir = "./", plasmids = FALSE, coverage = 2) {
    q = paste0('?', ifelse(plasmids, 'plasmids', ''))
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/contigs/', q)
        contigs = submit_get_request(request)
        contigs = contigs[contigs$coverage >= coverage,]

        sa_contigs <- Biostrings::DNAStringSet(contigs$sequence)
        names(sa_contigs) <- contigs$header
        fasta_out <- paste0(output_dir, "/sample-", sample_id,
                            ifelse(plasmids, '-plasmids', '') ,".fna")
        Biostrings::writeXStringSet(
            sa_contigs,
            fasta_out,
            append=FALSE,
            format="fasta"
        )
        print(paste0('Wrote ', nrow(contigs), ' contigs to ', fasta_out))
    } else {
        warning('sample_id is not the expected single (integer or double)')
    }
}


#' write_genes
#'
#' Write the predicted genes (DNA) to a FASTA file for a given sample.
#'
#' Example headers:
#' >500|ERX105420_00001|1678|YP_005744112.1 [product=histidinol-phosphate aminotransferase] [name=none] [note=none]
#' 500|ERX105420_00001|1678|YP_005744112.1 ---> sample_id|locus_tag|product_id|inference
#'
#' @param sample_id An integer sample ID
#' @param output_dir A string, directory to output fasta file(s) to. Default: ./
#' @param include_rna Include RNAs in the output (Default TRUE)
#'
#' @return write_genes
#' @export
#'
#' @examples write_genes(500)
#' @examples write_genes(500, include_rna = FALSE)
write_genes <- function(sample_id, output_dir = "./", include_rna = TRUE) {
    if (is_single_id(sample_id)) {
        genes <- submit_get_request(paste0('/sample/', format_id(sample_id), '/genes/'))
        if (include_rna == FALSE) {
            genes <- genes[genes$type == 'CDS',]
        }

        sa_genes <- Biostrings::DNAStringSet(genes$dna)
        names(sa_genes) <- genes$header
        fasta_out <- paste0(output_dir, "/sample-", sample_id,".ffn")
        Biostrings::writeXStringSet(
            sa_genes, fasta_out, append=FALSE, format="fasta"
        )
        print(paste0('Wrote ', nrow(genes), ' genes to ', fasta_out))
    } else {
        warning('sample_id is not the expected single (integer or double)')
    }
}


#' write_proteins
#'
#' Write the predicted proteins (AA) to a FASTA file for a given sample.
#'
#' Example headers:
#' >500|ERX105420_00001|1678|YP_005744112.1 [product=histidinol-phosphate aminotransferase] [name=none] [note=none]
#' 500|ERX105420_00001|1678|YP_005744112.1 ---> sample_id|locus_tag|product_id|inference
#'
#' @param sample_id An integer sample ID
#' @param output_dir A string, directory to output fasta file(s) to. Default: ./
#'
#' @return write_proteins
#' @export
#'
#' @examples write_proteins(500)
write_proteins <- function(sample_id, output_dir = "./") {
    if (is_single_id(sample_id)) {
        genes <- submit_get_request(paste0('/sample/', format_id(sample_id), '/genes/'))
        genes <- genes[genes$type == 'CDS',]
        sa_proteins <- Biostrings::AAStringSet(genes$aa)
        names(sa_proteins) <- genes$header
        fasta_out <- paste0(output_dir, "/sample-", sample_id,".faa")
        Biostrings::writeXStringSet(
            sa_proteins, fasta_out, append=FALSE, format="fasta"
        )
        print(paste0('Wrote ', nrow(genes), ' proteins to ', fasta_out))
    } else {
        warning('sample_id is not the expected single (integer or double)')
    }
}
