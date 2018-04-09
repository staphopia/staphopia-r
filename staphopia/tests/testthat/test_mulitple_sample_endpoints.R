context("Testing Multiple Sample Endpoints")


test_that("We can get the info for multiple samples.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_samples/')
    json_file <- 'api/tests/test_samples.json'
    response <- httr::content(httr::GET(url), as="text")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the sequencing quality for multiple samples.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_qualities/')
    json_file <- 'api/tests/test_qualities.json'
    response <- httr::content(httr::GET(url), as="text")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the assembly stats for multiple samples.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_assemblies/')
    json_file <- 'api/tests/test_assemblies.json'
    response <- httr::content(httr::GET(url), as="text")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the predicted InDels for multiple samples.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_indels/')
    json_file <- 'api/tests/test_indels.json'
    response <- httr::content(httr::GET(url), as="text")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the predicted SNPs for multiple samples.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_snps/')
    json_file <- 'api/tests/test_snps.json'
    response <- httr::content(httr::GET(url), as="text")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the predicted MLST for multiple samples.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_mlsts/')
    json_file <- 'api/tests/test_mlsts.json'
    response <- httr::content(httr::GET(url), as="text")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the SCCmec Primer hits for multiple samples.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_sccmec_primers/')
    json_file <- 'api/tests/test_sccmec_primers.json'
    response <- httr::content(httr::GET(url), as="text")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the predicted SCCmec type (Primer based) for multiple samples.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_sccmec_primers_predict/')
    json_file <- 'api/tests/test_sccmec_primers_predict.json'
    response <- httr::content(httr::GET(url), as="text")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the SCCmec subtype hits for multiple samples.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_sccmec_subtypes/')
    json_file <- 'api/tests/test_sccmec_subtypes.json'
    response <- httr::content(httr::GET(url), as="text")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

test_that("We can get the predicted SCCmec subtype (Primer based) for multiple samples.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_sccmec_subtypes_predict/')
    json_file <- 'api/tests/test_sccmec_subtypes_predict.json'
    response <- httr::content(httr::GET(url), as="text")
    httptest::expect_json_equivalent(jsonlite::fromJSON(response), jsonlite::fromJSON(json_file))
})

