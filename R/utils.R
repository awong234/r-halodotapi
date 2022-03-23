#' Add header to request
#'
#' The authorization seeks an environment variable called `AUTOCODE_API_KEY` for
#' the authentication token.
#'
#' @param req An http request, derived from `httr2::request`.
#'
#' @return A modified request, with the authentication token embedded in the request header.
#'
#' @importFrom httr2 request req_headers req_url_query req_perform
#'   resp_body_json
add_auth_header = function(req) {
    req = req_headers(req, "Authorization" = sprintf("Bearer %s", Sys.getenv("AUTOCODE_API_KEY")))
    req
}

#' Adjust the service record filter
#'
#' @description
#' The service record API has a few filters that can be passed:
#'
#' * None (all)
#' * matchmade:pvp
#' * matchmade:social
#' * matchmade:ranked
#' * matchmade:bots
#' * custom
#'
#' For convenience, the user inputs one of `'all', 'pvp', 'social', 'ranked', 'bots', 'custom'`
#'
#' @param filter Define the filter to subset the results. A single character
#'   value. This is one of '', 'pvp', 'ranked', 'bots', or 'custom', for
#'   convenience, which is modified by the function to match the API spec.
#'
#' @return A modified character value.
adjust_service_record_filter = function(filter) {
    # Add 'matchmade' if pvp thru bots
    if (filter %in% c("pvp", "social", "ranked", "bots")) {
        filter = paste0("matchmade:", filter)
    }
    if (filter == "all") {
        filter = NULL
    }

    return(filter)
}

#' Replace NULLS in a nested list with NA
#'
#' Converting a nested list to a data frame requires that NULL's be converted to
#' NA values. This is because the implied number of rows for that entry is 0,
#' leading to a mismatch in the number of rows among all entries in the dataset.
#' To resolve, replace the NULL's with NA, which have length.
#'
#' @param x A nested list, possibly with null values
#'
#' @return The same nested list, but with NULL's substituted for NA's
replace_NULL_with_NA = function(x, depth) {
    if (is.list(x)) {
        lapply(X = x, FUN = replace_NULL_with_NA, depth = depth)
    } else {
        if (is.null(x)) {
            rep(NA, depth)
        } else {
            x
        }
    }
}

max_depth = function(x) {
    max(rapply(x, function(x) length(x)))
}
