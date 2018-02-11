# These are functions to be reveiwed for removal
# Review the following:
#   gwas.R
#   kmer.R
#   heatmap.R
#   plots.R

#' get_contig
#'
#' Retrieve assembled contigs by a given contig_id.
#'
#' @param contig_id An integer sample ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_contig(500)
get_contig <- function(contig_id) {
    request <- paste0('/assembly/contig/', format_id(contig_id), '/')
    return(submit_get_request(request))
}

#' get_gene_cluster
#'
#' Retrieve gene cluster for a given cluster_id.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_gene_cluster(500)
get_gene_cluster <- function(cluster_id) {
    request <- paste0('/gene/cluster/', format_id(cluster_id), '/')
    return(submit_get_request(request))
}

#' get_gene_clusters
#'
#' Retrieve gene cluster for a set of sample ids.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_gene_clusters(500:505)
get_gene_clusters <- function(sample_ids) {
    request <- '/gene/cluster/clusters_by_samples/'
    return(submit_post_request(request, sample_ids, chunk_size=50))
}


#' get_gene_cluster_counts
#'
#' Retrieve gene cluster counts for a set of sample ids.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_gene_cluster_counts(500:505)
get_gene_cluster_counts <- function(sample_ids) {
    request <- '/gene/cluster/cluster_counts_by_samples/'
    return(submit_post_request(request, sample_ids, chunk_size=1000))
}

#' get_gene_products
#'
#' Retrieve all gene products in Staphopia.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_gene_products()
get_gene_products <- function(term=FALSE) {
    if (term == FALSE) {
        return(submit_get_request('/gene/product/'))
    } else {
        return(submit_get_request(paste0('/gene/product/?term=', term)))
    }
}


#' get_gene_product
#'
#' Retrieve gene product infor for a given ID.
#'
#' @param product_id An integer product ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_gene_product(500)
get_gene_product <- function(product_id) {
    return(submit_get_request(paste0('/gene/product/', product_id, '/')))
}

#' get_assembly_stats_by_year
#'
#' Retrieve assembled stats by year based on when samples were first made public.
#'
#' @param is_scaffolds Boolean
#' @param is_plasmids Boolean
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_assembly_stats_by_year(is_scaffolds=TRUE, is_plasmids=FALSE)
get_assembly_stats_by_year <- function(is_scaffolds=FALSE, is_plasmids=FALSE) {
    request <- NULL
    if (is_scaffolds && is_plasmids) {
        request <- '/info/assembly_by_year/?is_plasmids&is_scaffolds'
    } else if (is_scaffolds) {
        request <- '/info/assembly_by_year/?is_scaffolds'
    } else if (is_plasmids) {
        request <- '/info/assembly_by_year/?is_plasmids'
    } else {
        request <- '/info/assembly_by_year/'
    }

    return(submit_get_request(request))
}

#' get_variant_counts
#'
#' Given a list of Sample IDs return SNP/InDel counts.
#'
#' @param sample_ids A vector of sample IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_variant_counts(c(5000,5001,5002))
get_variant_counts <- function(sample_ids) {
    request <- '/variant/count/bulk_by_sample/'
    return(submit_post_request(request, sample_ids, chunk_size=500))
}


#' get_samples_by_snp
#'
#' Given a list of snp id return the samples in which snp is present.
#'
#' @param snp_id A snp ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_samples_by_snp(c(500,501,502))
get_samples_by_snp <- function(snp_id) {
    request <- paste0('/variant/snp/', snp_id, '/samples/')
    return(submit_get_request(request))
}


#' get_samples_by_indel
#'
#' Given a InDel ID return the samples in which InDel is present.
#'
#' @param indel_id An InDel ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_samples_by_indel(c(5000))
get_samples_by_indel <- function(indel_id) {
    request <- paste0('/variant/indel/', indel_id, '/samples/')
    return(submit_get_request(request))
}
