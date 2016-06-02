#' build_url
#'
#' Builds the endpoint url to query the API. This function should not be
#' directly called by the user.
#'
#' @param request
#'
#' @return Full URL endpoint.
build_url <- function(request) {
    base_url <- 'https://staphopia.genetics.emory.edu/api'
    return(paste0(base_url, request))
}

#' get_request
#'
#' Submit a GET query to Staphopia's API. This function should not be directly
#' called by the user.
#'
#' @param url
#'
#' @return Parsed JSON response.
get_request <- function(url) {
    req <- httr::GET(url, httr::timeout(20))
    json_data <- jsonlite::fromJSON(
        httr::content(req, as="text", encoding = "UTF-8")
    )
    json_data$status <- httr::status_code(req)
    return(json_data)
}

#' submit_get_request
#'
#' Prepare a GET query for submission to Staphopia's API. Paginated responses
#' are aggragated and submitted as a single response. This function should
#' not be directly called by the user.
#'
#' @param request
#'
#' @return Parsed JSON response.
submit_get_request <- function(request){
    url <- build_url(request)
    json_data <- get_request(url)
    if (json_data$status != 200) {
        return(json_data)
    } else {
        if (is.not.null(json_data$`next`)) {
            pages <- submit_paginated_request(json_data$`next`)
            data <- c(list(json_data$results), pages)
            json_data <- list(count=json_data$count,
                              results=data.table::rbindlist(data))
        }

        if (is.not.null(json_data$count)) {
            return(json_data$results)
        } else {
            return(json_data)
        }
    }
}

#' submit_paginated_request
#'
#' Automates cycling through all pages of a paginated response. A delay of
#' 200ms is introduced between each query so as to not overload the API. This
#' function should not be directly called by the user.
#'
#' @param next_page
#'
#' @return Parsed JSON response.
submit_paginated_request <- function(next_page) {
    data <- c()
    while(is.not.null(next_page)) {
        json_data = get_request(next_page)
        next_page <- json_data$`next`
        data <- append(data, list(json_data$results))
        Sys.sleep(0.20)
    }
    return(data)
}

#' post_request
#'
#' Submit a POST query to Staphopia's API. This function should not be directly
#' called by the user.
#'
#' @param url
#'
#' @param data A vector of data to be POSTED
#'
#' @return Parsed JSON response.
post_request <- function(url, data) {
    req <- httr::POST(url, body=data, encode="json")
    return(jsonlite::fromJSON(
        httr::content(req, as="text", encoding = "UTF-8")
    ))
}

#' submit_post_request
#'
#' Prepare a POST query for submission to Staphopia's API. Paginated responses
#' are aggregated and submitted as a single response. This function should
#' not be directly called by the user.
#'
#' @param request
#'
#' @return Parsed JSON response.
submit_post_request <- function(request, data) {
    url <- build_url(request)
    json_data <- post_request(url, data)
    if (is.not.null(json_data$`next`)) {
        pages <- submit_paginated_request(json_data$`next`)
        data <- c(list(json_data$results), pages)
        json_data <- list(count=json_data$count,
                          results=data.table::rbindlist(data))
    }
    return(json_data)
}
