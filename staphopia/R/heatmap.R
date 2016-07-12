#' samp_clust
#'
#' In case scientific notation is turned on, convert it to an integer. This
#' function should not be directly used by the user.
#'
#' @param gene.list Parsed JSON response from the get_genes function.
#'
#' @return matrix.
samp_clust<-function(gene.list){
  gene.m <- matrix(unlist(gene.list),ncol=16)
  gene.m <- unique(gene.m[,c(8,10)])
  colnames(gene.m) <- c("sample_id", "cluster_id")
  return(gene.m)
}

#' pangenome_matrix
#'
#' Construct a matrix with samples as rows and gene cluster IDs as columns, 
#' where the values are a binary indicating presence of that gene for that 
#' sample (1) or absence (0).
#'
#' @param tag The tag name as a string
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' pangenome_matrix('my-tag-name')
pangenome_matrix<-function(tag){
  tag <- get_tag_by_name(tag)
  samples <- get_samples_by_tag(tag$id)
  
  gene.samplist <- lapply(samples$sample_id,function(x){samp_clust(get_genes(x))})
  gene.bind <- do.call(rbind, gene.samplist)
  gene.bind <- cbind(gene.bind, value.var = rep(1,nrow(gene.bind)))
  gene.bind <- as.data.frame(gene.bind, stringsAsFactors = F)
  gene.cast <- reshape2::acast(gene.bind, sample_id~cluster_id, fill = 0, value.var = "value.var")
  
  if(sum(duplicated(samples$sample_tag)) > 0){
    samples.dup <- samples[duplicated(samples$sample_tag),]
    samples.uni <- samples[!duplicated(samples$sample_tag),]
    gene.cast <- gene.cast[!rownames(gene.cast) %in% samples.dup$sample_id,]
    rownames(gene.cast) <- samples.uni[order(as.character(samples.uni$sample_id)),"sample_tag"]
  }
  else{
    rownames(gene.cast) <- samples[order(as.character(samples$sample_id)),"sample_tag"]
  }
  
  gene.cast <- matrix(as.numeric(gene.cast), nrow = nrow(gene.cast), dimnames = list(rownames(gene.cast), colnames(gene.cast)))
  gene.cast[gene.cast>1] <- 1
  return(gene.cast)
}

#' pangenome_matrix
#'
#' Construct a matrix with samples as rows and gene cluster IDs as columns, 
#' where the values are a binary indicating presence of that gene for that 
#' sample (1) or absence (0)
#'
#' @param tag The tag name as a string
#'
#' @return A list with total genes (int), number of core genes (int), 
#' number of accessory genes (int), array of core genes (chr)
#' and array of accessory genes (chr) 
#' @export
#'
#' @examples
#' my_matrix.stats<-pangenome_heatmap(my_matrix)
#' my_matrix.stats<-pangenome_heatmap(my_matrix,'Heatmap','Genes','Samples')
#' my_matrix.stats<-pangenome_heatmap(pangenome_matrix('my-tag-name'))
pangenome_heatmap<-function(pg_matrix, main = "Pangenome Matrix Heatmap", xlab = "Cluster ID", ylab = "Samples"){
  heatmap(pg_matrix[,order(colSums(pg_matrix))], Rowv = NA, Colv = NA, scale = "none", main = main, xlab = xlab, ylab = ylab)
}