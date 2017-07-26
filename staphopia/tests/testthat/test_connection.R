context("Testing API Connection")


test_that("We can connect to the API.", {
    skip_if_disconnected()
    url <- build_url('/tests/test_status/')
    json_file <- 'api/tests/test_status.json'
    expect_json_equivalent(httr::content(httr::GET(url)), jsonlite::fromJSON(json_file))
})
