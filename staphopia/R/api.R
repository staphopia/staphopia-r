TOKEN_FILE <- '~/.staphopia'
TOKEN_MISSING <- paste0("Variable 'TOKEN' not found in ", TOKEN_FILE,
                        " Unable to continue.")

#' build_url
#'
#' Builds the endpoint url to query the API. This function should not be
#' directly called by the user.
#'
#' @param request
#'
#' @return Full URL endpoint.
build_url <- function(request) {
    if (Sys.info()['nodename'] == "merlin") {
        USE_DEV = TRUE
    } else if (!(exists("USE_DEV"))) {
        USE_DEV = FALSE
    }

    if (USE_DEV == TRUE) {
        base_url <- 'https://chlamy.genetics.emory.edu/api'
    } else {
        base_url <- 'https://staphopia.emory.edu/api'
    }
    return(paste0(base_url, request))
}

#' get_token
#'
#' Read ~/.staphopia for the user's Token.
#'
get_token <- function() {
    if (!file.exists(TOKEN_FILE)) {
        warning(paste0("File: ", TOKEN_FILE, " Does Not Exist"))
    } else {
        source(TOKEN_FILE)
        if (!(exists("TOKEN"))) {
            warning(paste0("TOKEN variable missing in ", TOKEN_FILE))
        }
    }
}

#' get_token_header
#'
#' Return the proper header for token authentication.
#'
get_token_header <- function() {
    return(paste0('Token ', TOKEN))
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
    req <- httr::GET(url, httr::timeout(120),
                     httr::add_headers(Authorization = get_token_header()))
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
    if (!(exists("TOKEN"))) {
        get_token()
    }

    if (exists("TOKEN")) {
        url <- build_url(request)
        json_data <- get_request(url)
        if (json_data$status != 200) {
            if(is.not.null(json_data$detail)) {
                if (json_data$detail == "Invalid token.") {
                    warning(paste0("Please verify the given TOKEN. ", TOKEN, " is not a ",
                                   "valid TOKEN value."), immediate. = TRUE)
                } else {
                    return(json_data)
                }
            } else {
                return(json_data)
            }
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
    } else {
        warning(TOKEN_MISSING)
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
    if (!(exists("TOKEN"))) {
        get_token()
    }

    if (exists("TOKEN")) {
        req <- httr::POST(url, body=data, encode="json",
                          httr::add_headers(Authorization = get_token_header()))

        json_data = jsonlite::fromJSON(
            httr::content(req, as="text", encoding = "UTF-8")
        )
        if (is.not.null(json_data$detail)){
            if (json_data$detail == "Invalid token.") {
                warning(paste0("Please verify the given TOKEN. ", TOKEN, " is not a ",
                               "valid TOKEN value."), immediate. = TRUE)
            } else {
                return(json_data)
            }
        } else {
            return(json_data)
        }
    } else {
        warning(TOKEN_MISSING, call. = FALSE)
    }
}

#' submit_post_request
#'
#' Prepare a POST query for submission to Staphopia's API. By default post
#' requests will be split into a number of smaller requests. This is hopefully
#' avoid timeouts.
#'
#' @param request An API endpoint
#' @param data A vector of data to be POSTED
#' @param chunk_size Optional parameter to determine size of chunks
#' @param format Optional parameter to format ids so as to not use scientific notation
#'
#' @return Parsed JSON response.
submit_post_request <- function(request, data, chunk_size=10, extra_data=FALSE,
                                format=TRUE) {
    url <- build_url(request)
    if (format) {
        data <- format_id(data)
    }
    count <- 0
    results <- c()
    for (chunk in split_vector_into_chunks(data, chunk_size)) {
        if (length(chunk) == 1){
            chunk <- list(chunk)
        }
        json_data <- post_request(url, list(ids=chunk, extra=extra_data))
        count <- count + json_data$count
        results <- append(results, list(json_data$results))
        Sys.sleep(0.20)
    }

    results <- data.table::rbindlist(results)
    if (count == nrow(results)) {
        return(results)
    } else {
        return('Error! Count is not equal to number of rows!')
    }
}
