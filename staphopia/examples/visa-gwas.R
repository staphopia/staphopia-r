library(staphopia)
library(ggplot2)

# Minimum Allele Frequency
MAF <- 0.05

# Let's get the tag associated with VISA
tag <- get_tag_by_name('visa-gwas-2014')

# Let's get samples associated with tag 'visa-gwas-2014'
samples <- get_samples_by_tag(tag$id)

# Let's pull SNPs associated with these samples
snps <- get_snps_by_samples(samples$sample_id)

# Let's get information for each SNPunique_snps <- unique(snps$snp_id)
unique_snps <- unique(snps$snp_id)
snp_info <- get_snps_in_bulk(unique_snps)

# Let's create a SNP matrix
snp_matrix <- create_snp_matrix(snps, samples, snp_info)

# Now we need a phenotype!
vancomycin <- get_resistance(antibiotic="vancomycin", test="etest")

# Now for all samples
phenotype <- get_resistance_by_samples(samples$sample_id,
                                       resistance_id=vancomycin$id)

# Merge Phenotype and Sample
samples_with_phenotype <- merge(samples, phenotype)
sorted <- samples_with_phenotype[order(samples_with_phenotype$sample_tag),]

# Run Linear Regression GWAS
gwas <- run_gwas(snp_matrix, sorted$value)

# Remove SNPs < 0.05 frequency
gwas <- gwas[gwas$freq >= MAF,]

# Make some plots
manhattan_plot(gwas)
qq_plot(gwas[with(gwas, order(logp)), ])
