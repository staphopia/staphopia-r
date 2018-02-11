#' linear_regression
#'
#' Test for significance between a phenotype and SNP. This function should not
#' be called by the user.'
#'
#' @param phenotype A vector of phenotypes per sample
#'
#' @param snp A vector of SNP present (1) or absent (0) per sample
#'
#' @param name Name of the SNP being tested
#'
#' @return Results from linear regression.
linear_regression <- function (phenotype, snp, position) {
    lr <- lm(phenotype ~ snp)
    pval <- summary(lr)$coefficients[2,4]
    return(list(
        pval = pval,
        logp = -1 * log10(pval),
        freq = sum(snp) / length(snp),
        position = as.numeric(position)
    ))
}

#' gwas_lr
#'
#' Test for significance between a phenotype and SNPs. This function should not
#' be called by the user.'
#'
#' @param snps A vector of SNPs present (1) or absent (0) per sample
#'
#' @param phenotypes A vector of phenotypes per sample
#'
#' @param names Name of the SNPs being tested
#'
#' @return A dataframe of linear regression results.
gwas_lr <- function(snps, phenotype, names) {
    results <- lapply(
        1:nrow(snps),
        function(x) linear_regression(phenotype, as.integer(snps[x,]), names[x])
    )
    return(data.frame(
        position=sapply(1:length(results), function(x) results[[x]]$position),
        pval=sapply(1:length(results), function(x) results[[x]]$pval),
        freq=sapply(1:length(results), function(x) results[[x]]$freq),
        logp=sapply(1:length(results), function(x) results[[x]]$logp)
    ))
}

#' run_gwas
#'
#' Given a list of sample IDs return the resistance phenotypes of each sample.
#'
#' @param snp_matrix A vector of SNPs present (1) or absent (0) per sample
#'
#' @param mic A vector of phenotypes per sample
#'
#' @param mode GWAS test to run.
#'
#' @return Results from GWAS
#' @export
#'
#' @examples
#' run_gwas(snp_matrix, phenotype)
run_gwas <- function(snp_matrix, phenotype, mode="lr") {
    if (mode =="lr") {
        return(gwas_lr(snp_matrix, phenotype, rownames(snp_matrix)))
    }
}
