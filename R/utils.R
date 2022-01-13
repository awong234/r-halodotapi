#' Add header to request
#' @importFrom httr2 request req_headers req_url_query req_perform resp_body_json
add_auth_header = function(req) {
    req = req_headers(req, "Authorization" = sprintf("Bearer %s", Sys.getenv("AUTOCODE_API_KEY")))
    req
}
