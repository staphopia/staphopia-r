#' get_snps_by_sample
#'
#' Given a list of sample IDs return the SNPs present in each sample.
#'
#' @param sample_ids A vector of sample IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_snps_by_sample(c(500,501,502))
get_snps_by_sample <- function(sample_ids) {
    request <- '/variant/snp/bulk_by_sample/'
    return(submit_post_request(request, sample_ids, chunk_size=5))
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
#''
#' @param samples A data frame of samples including sample_id and sample_tag
#'
#' @param snp_info A data frame returned from 'get_snps_in_bulk'
#''
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' create_snp_matrix(snps, snp_info)
create_snp_matrix <- function(snps, samples, snp_info) {
    # Add columns 'sample_tag' and 'reference_position' to snps
    snps <- data.table:::merge.data.table(
        data.table:::merge.data.table(
            data.table::data.table(snps, key=c("snp_id", "sample_id")),
            data.table::data.table(samples[, c("sample_id", "sample_tag")],
                                   key="sample_id"),
            by="sample_id",
            all.x=TRUE
        ),
        data.table::data.table(snp_info[, c("id", "reference_position")],
                               key="id"),
        by.x="snp_id",
        by.y="id"
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
