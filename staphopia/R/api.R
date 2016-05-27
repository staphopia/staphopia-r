#' submit_get_request
#'
#' @param request
#'
#' @return JSON string
#' @export
#'
#' @examples
#' sample_500 <- submit_get_request('/sample/500')

get_request <- function(url) {
    req <- GET(url, timeout(seconds=20))
    return(fromJSON(content(req, as="text")))
}
submit_get_request <- function(request){
    url <- build_url(request)
    json_data <- get_request(url)
    if (is.not.null(json_data$`next`)) {
        pages <- submit_paginated_request(json_data$`next`)
        data <- c(list(json_data$results), pages)
        json_data$results <- rbindlist(data)
    }
    return(list(count=json_data$count, results=json_data$results))
}

submit_paginated_request <- function(next_page) {
    data <- c()
    while(is.not.null(next_page)) {
        json_data = get_request(next_page)
        next_page <- json_data$`next`
        data <- append(data, list(json_data$results))
    }
    return(data)
}


build_url <- function(request) {
    base_url <- 'https://staphopia.genetics.emory.edu/api'
    return(paste0(base_url, request))
}
