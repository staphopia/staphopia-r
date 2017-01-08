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
    return(submit_get_request(paste0('/gene/product/', product_id)))
}

#' get_genes
#'
#' Retrieve all predicted genes for a given sample.
#'
#' @param sample_id An integer sample ID or A vector of sample IDs
#' @param product_id Optional integer product ID
#' @param cluster_id Optional integer cluster ID
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_genes(500)
#' get_genes(c(500, 501, 502)
#' get_genes(c(500, 501), cluster_id=2499)
#' get_genes(c(500, 501), product_id=179)
get_genes <- function(sample_id, product_id=NULL, cluster_id=NULL) {
    data <- c()
    count <- 0
    for (id in sample_id) {
        request <- NULL
        if (is.not.null(product_id) && is.not.null(cluster_id)){
            request <- paste0('/sample/', format_id(id),
                              '/genes/?product_id=', product_id,
                              '&cluster_id=', cluster_id)
        } else if (is.not.null(product_id)) {
            request <- paste0('/sample/', format_id(id),
                              '/genes/?product_id=', product_id)
        } else if (is.not.null(cluster_id)) {
            request <- paste0('/sample/', format_id(id),
                              '/genes/?cluster_id=', cluster_id)
        } else {
            request <- paste0('/sample/', format_id(id), '/genes/')
        }
        json_data <- submit_get_request(request)
        data <- append(data, list(json_data))
        if (is.not.null(nrow(json_data))) {
            count <- count + nrow(json_data)

        }
    }

    results <- data.table::rbindlist(data)
    if (count == nrow(results)) {
        return(results)
    } else {
        print(count)
        print(nrow(results))
        return('Error! Count is not equal to number of rows!')
    }

}
