#' get_resistance
#'
#' Retrieve all resistances in the database. Can be filtered based on
#' antibiotic and test.
#'
#' ASSUME ARIBA RESULTS: MORE SO WHAT ARIBA TESTED
#'
#' @param antibiotic An antibiotic to search for
#'
#' @param test An antibiotic test method to search for.
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_resistance()
#' get_resistance(antibiotic="Vancomycin")
#' get_resistance(antibiotic="Vancomycin", test="Etest")
get_resistance <- function(antibiotic=NULL, test=NULL) {
    request <- '/resistance/'
    if (is.not.null(antibiotic)) {
        if (is.not.null(test)) {
            request <- paste0(request, '?antibiotic=', antibiotic,
                              '&test=', test)
        } else {
            request <- paste0(request, '/?antibiotic=', antibiotic)
        }
    }
    return(submit_get_request(request))
}

#' get_resistance_by_samples
#'
#' Given a list of sample IDs return the resistance phenotypes of each sample.
#'
#' INCLUDE NEGATIVE RESULTS
#'
#' @param sample_ids A vector of sample IDs
#'
#' @return Parsed JSON response.
#' @export
#'
#' @examples
#' get_resistance_by_samples(c(500,501,502))
#' get_resistance_by_samples(c(500,501,502), resistance_id = 1)
get_resistance_by_samples <- function(sample_ids, resistance_id=NULL) {
    request <- '/resistance/bulk_by_sample/'
    if (is.not.null(resistance_id)) {
        request <- paste0(request, '?resistance_id=', resistance_id)
    }

    return(submit_post_request(request, sample_ids, chunk_size=50))
}

#' antibiotic_data
#'
#' @param no_SNPs
#'
#' @return  Dataframe of staphopia SNPs associated with known AR or (no_SNP=F) dataframe of antibiotic resistance loci or
#' @export antibiotic_data
#'
#' @examples AR_data <- antibiotic_data()
antibiotic_data <- function(no_SNPs = TRUE){
  #outputs either dataframe of known S. aureus SNPs based on the Gordon paper
  #OR the staphopia snp IDs based on the N315 reference

  #gyrA: S84L, E88K, G106D, S85P, E88G, E88L
  gyrA_coords <- c(7005,9674)
  gyrA_name <- c("gyrA","Ciprofloxacin")
  gyrA_posns <- c(84,88,106,85,88,88)
  gyrA_alts <- c("L","K","D","P","G","L")
  #note - E88L not represented - maybe look for unusuall number of SNPs in E88 posiiotn

  #grlA (S80F, S80Y, E84K, E84G, E84V, D432G, Y83N, A116E, I45M, A48T, D79V, V41G, S108N)
  grlA_coords <- c(1356325,1358727)
  grlA_name <- c("grlA","Ciprofloxacin")
  grlA_posns <- c(80,80,84,84,84,432,83,116,45,48,79,41,108)
  grlA_alts <- c("F","Y","K","G","V","G","N","E","M","T","V","G","N")

  #grlB: R470D*, E422D*, P451S*, P585S*, D443E*, R444S*
  grlB_coords <- c(1354328,1356325)
  grlB_name <- c("grlB","Ciprofloxacin")
  grlB_posns <- c(470,422,451,585,443,444)
  grlB_alts <- c("D","D","S","S","E","S")

  #fusA A160V*, A376V, A655E, A655P*, A655V*, A67T*, A70V*, A71V*, B434N, C473S*,
  #D189G*, D189V*, D373N*, D463G*, E233Q*, E444K, E444V*, E449K*, F441Y,
  #F652S*, G451V, G452C, G452S, G556S, G617D, G664S, H438N, H457Q, H457Y,
  #L430S*, L456F, L461K, L461S, M161I*, M453I, M651I, P114H, P404L, P404Q, P406L,
  #P478S, Q115L, R464C, R464H, R464S, R659C, R659H, R659L, R659S, R76C*, S416F*,
  #T385N, T387I*, T436I, T656K, V607I, V90A, V90I, Y654N*
  fusA_coords <- c(588492,590573)
  fusA_name <- c("fusA", "Fusidic Acid")
  fusA_posns <- c(160,376,655,655,655,67,70,71,434,473,189,189,373,463,233,444,444,449,441,652,451,452,452,556,617,664,438,457,457,430,456,461,461,161,453,651,114,404,404,406,478,115,464,464,464,659,659,659,659,76,416,385,387,436,656,607,90,90,654)
  fusA_alts <- c("V", "V", "E", "P", "V", "T", "V", "V", "N", "S","G", "V","N", "G", "Q", "K", "V", "K", "Y","S", "V", "C", "S", "S", "D", "S", "N", "Q", "Y","S", "F", "K", "S", "I", "I", "I", "H", "L", "Q", "L","S", "L", "C", "H", "S", "C", "H", "L", "S", "C", "F","N", "I", "I", "K", "I", "A", "I", "N")

  #rpoB: A473T*, A477D, A477T*, A477V, D471G*, D471Y, D550G, H481D, H481N, H481Y,
  #I527F, I527L*, I527 M*, ins 475H, ins G475*, L466S*, M470T*, N474K*, Q456K,
  #Q468K, Q468L, Q468R, Q565R*, R484H, S463P, S464P, S486L, S529L*
  rpoB_coords <- c(579620,583171)
  rpoB_name <- c("rpoB","Rifampin")
  rpoB_posns <- c(473,477,477,477,471,471,550,481,481,481,527,527,527,466,470,474,456,468,468,468,565,484,463,464,486,529)
  rpoB_alts <- c("T", "D", "T", "V", "G", "Y", "G", "D", "N", "Y","F", "L", "M", "S", "T", "K", "K","K", "L", "R", "R", "H", "P", "P", "L", "L")
  #only 24 SNPs - two missing since there were 26 positions
  #need to account for missing insertions

  #dfrB  F99Y, F99S, F99I, H31N, L41F, H150R, L21V*, N60I*
  dfrB_coords <- c(1431906,1432385)
  drfB_name <- c("dfrB","Trimethoprim")
  dfrB_posns <-c(99,99,99,31,41,150,21,60)
  dfrB_alts <-c("Y","S","I","N","F","R","V","I")

  if (no_SNPs) {
    gyrA <- cbind(gyrA_posns,gyrA_alts,gyrA_coords[1],gyrA_coords[2],gyrA_name[1],gyrA_name[2])
    colnames(gyrA) <- c("aa_position","aa_change","N315_start","N315_end","gene","phenotype")
    grlA <- cbind(grlA_posns,grlA_alts,grlA_coords[1],grlA_coords[2],grlA_name[1],grlA_name[2])
    colnames(grlA) <- c("aa_position","aa_change","N315_start","N315_end","gene","phenotype")
    grlB <- cbind(grlB_posns,grlB_alts,grlB_coords[1],grlB_coords[2],grlB_name[1],grlB_name[2])
    colnames(grlB) <- c("aa_position","aa_change","N315_start","N315_end","gene","phenotype")
    fusA <- cbind(fusA_posns,fusA_alts,fusA_coords[1],fusA_coords[2],fusA_name[1],fusA_name[2])
    colnames(fusA) <- c("aa_position","aa_change","N315_start","N315_end","gene","phenotype")
    rpoB <- cbind(rpoB_posns,rpoB_alts,rpoB_coords[1],rpoB_coords[2],rpoB_name[1],rpoB_name[2])
    colnames(rpoB) <- c("aa_position","aa_change","N315_start","N315_end","gene","phenotype")
    dfrB <- cbind(dfrB_posns,dfrB_alts,dfrB_coords[1],dfrB_coords[2],drfB_name[1],drfB_name[2])
    colnames(dfrB) <- c("aa_position","aa_change","N315_start","N315_end","gene","phenotype")
    AR_tab <- as.data.frame(rbind(gyrA,grlA,grlB,fusA,rpoB,dfrB))
    AR_tab$aa_position <- as.numeric(as.character(AR_tab$aa_position))
    AR_tab$aa_change <- as.character(AR_tab$aa_change)
    AR_tab$N315_start <- as.numeric(as.character(AR_tab$N315_start))
    AR_tab$N315_end <- as.numeric(as.character(AR_tab$N315_end))
    return(AR_tab)
  }
  else {
    gyrA_SNPs <- find_SNPs_by_gene(gyrA_coords,gyrA_posns,gyrA_alts,gyrA_name[1],gyrA_name[2])
    grl_SNPs <- find_SNPs_by_gene(grlA_coords,grlA_posns,grlA_alts,grlA_name[1],grlA_name[2])
    grlB_SNPs <- find_SNPs_by_gene(grlB_coords,grlB_posns,grlB_alts,grlB_name[1],grlB_name[2])
    fusA_SNPs <- find_SNPs_by_gene(fusA_coords,fusA_posns,fusA_alts,fusA_name[1],fusA_name[2])
    rpoB_SNPs <- find_SNPs_by_gene(rpoB_coords,rpoB_posns,rpoB_alts,rpoB_name[1],rpoB_name[2])
    dfrB_SNPs <- find_SNPs_by_gene(dfrB_coords,dfrB_posns,dfrB_alts,drfB_name[1],drfB_name[2])
    antibiotic_df <- rbind(gyrA_SNPs,grl_SNPs,grlB_SNPs,fusA_SNPs,rpoB_SNPs,dfrB_SNPs)
    return(antibiotic_df)
  }
}

#' find_SNPs_by_gene
#'
#' @param coords .
#' @param posns .
#' @param alts .
#' @param gene .
#' @param phenotype .
#'
#' @return dataframe of SNPs associated with AR gene
#' @export
find_SNPs_by_gene <- function(coords,posns,alts,gene,phenotype){
  assert_that(length(coords)==2)
  assert_that(coords[1]<coords[2])
  assert_that(length(posns) == length(alts))
  assert_that(max(posns)<(coords[2]-coords[1]))
  assert_that(is.character(alts))
  res <- list()
  SNPs <- get_snps_in_bulk((coords[1]*3):(coords[2]*3))
  y <- lapply(1:length(posns), function(x) filter(SNPs,codon_position %in% posns[x]) %>% filter(alternate_amino_acid == alts[x]) %>% select(id, amino_acid_change))
  z <- do.call(rbind,y)
  z <- cbind(z,gene,phenotype)
  return(z)
}


#' aa_changes_phenotype
#'
#' @param samples (number) sample_id
#' @param product_id (number) product_id
#' @param posns (number) codon posiiotn in gene where changes lead to rsistance
#' @param alts (character) amino acid changes in gene where changes lead to redsistance
#' @param gene (string)
#' @param phenotype(string)
#'
#' @return finds
#' @export aa_changes_phenotype
#'
#' @examples gyrA_cipro_results <- aa_changes_phenotype(s_list,815,gyrA_posns,gyrA_alts,"gyrA","Ciprofloxacin")
aa_changes_phenotype <- function(samples,product_id,posns,alts,gene,phenotype) {
  #example : gyrA_cipro_results <- aa_changes_phenotype(s_list,815,gyrA_posns,gyrA_alts,"gyrA","Ciprofloxacin")
  #this predicts phenotype based on de novo called amino acids - not 100% funciotnal
  #gyrA product_id = 815
  #aa length = 887
  #gyrA_posns <- c(84,88,106,85,88,88)
  #gyrA_alts <- c("L","K","D","P","G","L")
  assert_that(length(posns) == length(alts))
  gtab <- get_genes(sample_id = samples,product_id = product_id)
  res_list <- vector("list", length(samples))
  names(res_list) <- samples
  samp_status <- vector("list", length(samples))
  names(samp_status) <- samples

  aa_len <- nchar(gtab$aa)
  for (q in 1:length(gtab$sample_id)){
    samp_status[[as.character(gtab$sample_id[q])]] <- aa_len[q]
  }
  short <- gtab$sample_id[nchar(gtab$aa) < max(posns)]
  if (not_empty(short)) {
    for (q in 1:length(short)){
      samp_status[[as.character(short[q])]] <- "Truncated"
    }
  }
  dups <- gtab$sample_id[duplicated(gtab$sample_id)]
  if (not_empty(dups)) {
    for (q in 1:length(dups)){
      samp_status[[as.character(dups[q])]] <- "Duplicated"
    }
  }
  missing_ids  <- gtab$sample_id[!(samples %in% gtab$sample_id)]
  if (not_empty(missing_ids)) {
    for (q in 1:length(missing_ids)){
      samp_status[[as.character(short[q])]] <- "Missing"
    }
  }

  goodrows <- which(!(gtab$sample_id %in% unique(dups,short)))

  for (s in goodrows){
    for (p in 1:length(posns)) {
      if (substr(gtab$aa[s],posns[p],posns[p]) == alts[p]) {
        change <- paste(posns[p],alts[p], sep = "")
        res_list[[as.character(gtab$sample_id[s])]] <- c(res_list[[as.character(gtab$sample_id[s])]],change)
      }
    }
  }
  results <- cbind(as.numeric(names(samp_status)),unlist(samp_status,use.names = FALSE),gene,phenotype)
  colnames(results) <- c("sample_id","aa_length","gene","phenotype")
  results2 <- cbind(as.numeric(names(unlist(res_list))),unlist(res_list,use.names = FALSE))
  colnames(results2) <- c("sample_id","aa_change")
  results3 <- merge(results,results2, by = "sample_id", all=T)
  results3$sample_id <- as.numeric(as.character(results3$sample_id))
  return(results3)
}

