context("Testing Single Sample Endpoints")


test_that("We can get the info for a single sample.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_sample/')
    json_file <- 'api/tests/test_sample.json'
    response <- httr::content(httr::GET(url), as="text", encoding = "UTF-8")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the sequencing quality for a single sample.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_quality/')
    json_file <- 'api/tests/test_quality.json'
    response <- httr::content(httr::GET(url), as="text", encoding = "UTF-8")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the assembly stats for a single sample.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_assembly/')
    json_file <- 'api/tests/test_assembly.json'
    response <- httr::content(httr::GET(url), as="text", encoding = "UTF-8")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the assembled contigs for a single sample.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_contig/')
    json_file <- 'api/tests/test_contig.json'
    response <- httr::content(httr::GET(url), as="text", encoding = "UTF-8")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the predicted genes for a single sample.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_gene/')
    json_file <- 'api/tests/test_gene.json'
    response <- httr::content(httr::GET(url), as="text", encoding = "UTF-8")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the predicted InDels for a single sample.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_indel/')
    json_file <- 'api/tests/test_indel.json'
    response <- httr::content(httr::GET(url), as="text", encoding = "UTF-8")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the predicted SNPs for a single sample.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_snp/')
    json_file <- 'api/tests/test_snp.json'
    response <- httr::content(httr::GET(url), as="text", encoding = "UTF-8")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the predicted MLST (SRST2 based) for a single sample.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_mlst_srst2/')
    json_file <- 'api/tests/test_mlst_srst2.json'
    response <- httr::content(httr::GET(url), as="text", encoding = "UTF-8")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the predicted MLST (BLAST based) for a single sample.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_mlst_blast/')
    json_file <- 'api/tests/test_mlst_blast.json'
    response <- httr::content(httr::GET(url), as="text", encoding = "UTF-8")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the SCCmec Primer hits for a single sample.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_sccmec_primer/')
    json_file <- 'api/tests/test_sccmec_primer.json'
    response <- httr::content(httr::GET(url), as="text", encoding = "UTF-8")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the predicted SCCmec type (Primer based) for a single sample.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_sccmec_primer_predict/')
    json_file <- 'api/tests/test_sccmec_primer_predict.json'
    response <- httr::content(httr::GET(url), as="text", encoding = "UTF-8")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})


test_that("We can get the SCCmec subtype hits for a single sample.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_sccmec_subtype/')
    json_file <- 'api/tests/test_sccmec_subtype.json'
    response <- httr::content(httr::GET(url), as="text", encoding = "UTF-8")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the predicted SCCmec subtype (Primer based) for a single sample.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_sccmec_subtype_predict/')
    json_file <- 'api/tests/test_sccmec_subtype_predict.json'
    response <- httr::content(httr::GET(url), as="text", encoding = "UTF-8")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

