context(paste0("Testing API Connection To ", build_url('')))


test_that("We can connect to the API.", {
    httptest::skip_if_disconnected()
    url <- build_url('/tests/test_status/')
    json_file <- 'api/tests/test_status.json'
    httptest::expect_json_equivalent(httr::content(httr::GET(url)), jsonlite::fromJSON(json_file))
})
