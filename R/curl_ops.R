#' Create handle for `curl`
#'
#'
#' @importFrom glue glue
#' @importFrom curl new_handle handle_setheaders
create_handle = function(API_KEY = Sys.getenv("CRYPTUM_API_KEY")) {
    stopifnot(
        API_KEY != ""
    )
    handle = curl::new_handle(verbose = TRUE)
    curl::handle_setheaders(handle,
                      'Authorization' = glue::glue("Cryptum-Token {API_KEY}"),
                      'Content-Type' = 'application/json',
                      'Cryptum-API-Version' = '2.3-alpha'
    )
    return(handle)

}
