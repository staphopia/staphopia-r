#' get_indels
#'
#' Retrieve all InDels present in a given sample(s).
#'
#' @param sample_id An integer sample ID, or vector of sample IDs
#' @param annotation_id Filter results based on an annotation ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_indels(500)
#' get_indels(c(500,501))
get_indels <- function(sample_id, annotation_id = NULL) {
    q = ""
    if (is.not.null(annotation_id)) {
        q = paste0("?annotation_id=", annotation_id)
    }
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/indels/', q)
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/variant/indel_bulk_by_sample/', q)
        return(submit_post_request(request, sample_id, chunk_size=5))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}


#' get_snps
#'
#' Retrieve all SNPs present in a given sample(s).
#'
#' @param sample_id An integer sample ID, or vector of sample IDs
#' @param annotation_id An integer annotation ID to filter snps on
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_snps(500)
#' get_snps(c(500,501))
get_snps <- function(sample_id, annotation_id=NULL) {
    q = ""
    if (is.not.null(annotation_id)) {
        q = paste0("?annotation_id=", annotation_id)
    }
    if (is_single_id(sample_id)) {
        request <- paste0('/sample/', format_id(sample_id), '/snps/', q)
        return(submit_get_request(request))
    } else if (is_multiple_ids(sample_id)) {
        request <- paste0('/variant/snp_bulk_by_sample/', q)
        return(submit_post_request(request, sample_id, chunk_size=5))
    } else {
        warning('sample_id is not the expected type (integer(s) or double(s))')
    }
}


#' get_variant_annotation
#'
#' Get annotation info associated with a variant.
#'
#' @param id An integer annotation ID
#' @param locus_tag An string locus_tag to filter annotaions on
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_variant_annotation(7)
#' get_variant_annotation(locus_tag="SA_RS00145")
get_variant_annotation <- function(id=NULL, locus_tag=NULL) {
    if (is.not.null(id)) {
        request <- paste0('/variant/annotation/', format_id(id), '/')
        return(submit_get_request(request))
    } else if (is.not.null(locus_tag)) {
        request <- paste0('/variant/annotation/?locus_tag=', format_id(locus_tag))
        return(submit_get_request(request))
    } else {
        return(NULL)
    }
}


#' get_snps_in_bulk
#'
#' Given a list of SNP IDs return information about each SNP.
#'
#' @param snp_ids A vector of sample IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_snps_in_bulk(c(5000,5001,5002))
get_snps_in_bulk <- function(snp_ids) {
    unique_snps <- unique(snp_ids)
    request <- '/variant/snp/bulk/'
    return(submit_post_request(request, unique_snps, chunk_size=5000))
}

#' create_snp_matrix
#'
#' Given a data frame with 'sample_id' and 'snp_id' return an SNP matrix that
#' can be used conduct a GWAS.
#'
#' @param snps A data frame returned from 'get_snps_by_samples'
#' @param samples A data frame of samples including sample_id and sample_tag
#''
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' create_snp_matrix(snps, samples)
create_snp_matrix <- function(snps, samples) {
    # Add columns 'sample_tag' and 'reference_position' to snps
    snps <- data.table:::merge.data.table(
        data.table::data.table(snps, key=c("snp_id", "sample_id")),
        data.table::data.table(samples[, c("sample_id", "sample_tag")],
                               key="sample_id"),
        by="sample_id",
        all.x=TRUE
    )

    # Create presence/absence table
    melted_dtl <- data.table::melt(snps, id.vars="reference_position",
                       measure.vars = "sample_tag")
    counts <- data.table::dcast.data.table(
        melted_dtl, reference_position ~ value, fun.aggregate=length
    )

    counts <- data.table::setDF(counts[, 2:ncol(counts), with=FALSE],
                                rownames = counts$reference_position)

    # Remove rows that are all 0's or all 1's
    counts <- counts[rowSums(counts == 0) != ncol(counts),]
    counts <- counts[rowSums(counts == 1) != ncol(counts),]

    return(counts)
}


#' get_variant_gene_sequence
#'
#' Given a list of Sample IDs and Annotation IDs return the SNP substituted
#' gene sequences. Sequences are concatenated.
#'
#' @param sample_ids A vector of sample IDs
#' @param annotation_ids A vector of sample IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_variant_gene_sequence(c(5000, 5001, 5002), c(2, 3, 4))
get_variant_gene_sequence <- function(sample_ids, annotation_ids, chunk_size=5, debug=FALSE) {
    results <- c()
    count <- 0
    i <- 1
    total <- length(sample_ids)
    save_reference <- TRUE
    url <- build_url('/variant/annotation/generate_variant_sequence/')
    if (debug == TRUE & length(annotation_ids) > 20) {
        print("This might take a while... go for a walk!")
    }
    for (sample in split_vector_into_chunks(sample_ids, chunk_size)) {
        json_data <- post_request(
            url,
            list(
                ids=sample,
                extra=list(annotation_ids=annotation_ids,
                           save_reference=save_reference)
            )
        )
        count <- count + json_data$count
        results <- append(results, list(json_data$results))
        if (debug == TRUE) {
            n = i * chunk_size
            if (n > total) {
                n = total
            }
            print(paste0("Retrieved ", i * chunk_size, " of ", total,
                         " samples. (Took: ", json_data$took, ")"))
        }
        i <- i + 1
        save_reference <- FALSE
        Sys.sleep(0.1)
    }

    results <- data.table::rbindlist(results)
    if (count == nrow(results)) {
        return(results)
    } else {
        return('Error! Count is not equal to number of rows!')
    }
}
