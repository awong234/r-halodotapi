#' Service Record API URL
#'
#' Returns the formatted API URL for the player's service record.
#'
#' @param gamertag Character value for the gamertag to view.
#' @importFrom glue glue
api_player_service_record = function(gamertag, type = c("multiplayer", "campaign")) {
    stopifnot(
        is.character(gamertag)
    )
    type = match.arg(type)
    gamertag = URLencode(gamertag)
    url = 'https://halo.api.stdlib.com/infinite@0.3.5/stats/service-record'
    url = paste(url, type, sep = '/')
    req = request(url)
    req = add_auth_header(req)
    req = req_url_query(req, gamertag = gamertag)
    return(req)
}

#' Match Record API URL
#'
#' Returns the formatted API URL for the player's last 100 matches.
#'
#' @param gamertag Character value for the gamertag to view.
#' @param page Numeric value for the page to view. The data returned is paginated,
#' and you will want to use the next page number that is returned in the response.
#' @importFrom httr2 request req_headers req_url_query req_perform resp_body_json
#' @importFrom dplyr bind_rows
#' @importFrom jsonlite flatten
api_player_list_matches = function(gamertag, count = NULL, offset = NULL, mode = NULL) {
    stopifnot(
        length(gamertag) == 1L,
        is.character(gamertag),
        is.numeric(count) | is.null(count),
        is.numeric(offset) | is.null(offset),
        mode %in% c("matchmade", "custom")
    )
    if (is.null(count)) count = 25L
    if (is.null(offset)) offset = 0L
    gamertag = URLencode(gamertag)
    req = request("https://halo.api.stdlib.com/infinite@0.3.3/stats/matches/list/")
    req = add_auth_header(req)
    req = req_url_query(req, gamertag = gamertag, `limit.count` = count, `limit.offset` = offset, mode = mode)
    return(req)
}

#' CSR API URL
#'
#' Returns the formatted API URL for the player's CSR data.
#'
#' Note that solo-duo queue only has input values for 'mnk' and 'controller',
#' whereas open queue only has the 'crossplay' input value.
#'
#' @param gamertag Character value for the gamertag to view.
#' @param season Numeric value for the season to view.
#' @param input Character value, one of 'mnk', 'controller', or 'crossplay' for the specific input type.
#' @param queue Character value, one of 'open', or 'solo-duo' for the queuing method.
#' @importFrom httr2 request
api_player_csr = function(gamertag, season) {
    season = as.integer(season)
    stopifnot(
        length(gamertag) == 1,
        length(season) == 1,
        is.character(gamertag)
    )
    gamertag = URLencode(gamertag)
    req = request('https://halo.api.stdlib.com/infinite@0.3.5/stats/csrs/')
    req = add_auth_header(req)
    req = req_url_query(req, gamertag = gamertag, season = season)
    return(req)
}

#' Get match data
#'
#' Runs through match history for the supplied gamertag.
#'
#' Each response is returned as JSON, which is formatted as a data.frame for the
#' user. The response contains a field for the next page number, which is used
#' until it returns `NULL`.
#'
#' @param gamertag Character value for the gamertag to view.
#' @param one Boolean, if TRUE returns only the first page. Useful for testing.
#' @return A list of two elements, a data frame with medals, and a data frame with statistics.
#' @importFrom httr2 resp_body_json
#' @importFrom futile.logger flog.info flog.debug
#' @importFrom jsonlite flatten
#' @importFrom tidyr unnest
get_player_match_data = function(gamertag, count = NULL, offset = NULL, mode = NULL, one = FALSE) {
    stopifnot(
        length(gamertag) == 1L,
        is.character(gamertag),
        is.numeric(count) | is.null(count),
        is.numeric(offset) | is.null(offset),
        mode %in% c("matchmade", "custom")
    )
    flog.info("Obtaining match data for %s", gamertag)
    if (is.null(count)) count = 25L
    if (is.null(offset)) offset = 0L
    stats_list = list()
    medals = list()
    while (TRUE) {
        flog.debug("offset %d", offset)
        req = api_player_list_matches(gamertag = gamertag, count = count, offset = offset, mode = mode)
        browser()
        resp = req_perform(req)
        resp = resp_body_json(resp, simplifyDataFrame = TRUE)
        offset = resp$paging$offset + count
        if (resp$count == 0L) {
            break
        } else if (one) {
            stats_list = flatten(resp$data)
            medals = unnest(select(stats, match_id = id, player.stats.core.breakdowns.medals),
                            cols = player.stats.core.breakdowns.medals)
            stats$player.stats.core.breakdowns.medals = NULL
            break
        } else {
            stats = flatten(resp$data)
            medals[[i]] = unnest(select(stats, match_id = id, player.stats.core.breakdowns.medals),
                            cols = player.stats.core.breakdowns.medals)
            stats$player.stats.core.breakdowns.medals = NULL
            stats_list[[i]] = stats
            i = i + 1
        }
    }

    data_list = list(
        medals = medals,
        stats  = stats_list
    )

    return(data_list)
}

#' Get CSR data
#'
#' Automatically loops through valid input and queue parameters, aggregating the
#' results together in a data frame.
#'
#' @param gamertag Character value for the gamertag to view.
#' @param season Numeric value for the season to view.
#' @param handle `curl` handle to use, created with curl::new_handle. Used to supply authentication token.
#' @importFrom httr2 req_perform
#' @importFrom jsonlite flatten
#' @importFrom futile.logger flog.info flog.debug
get_player_csr = function(gamertag, season) {
    flog.info("Obtaining CSR for %s", gamertag)
    req = api_player_csr(gamertag = gamertag, season = season)
    resp = req_perform(req)
    resp = resp_body_json(resp, simplifyDataFrame = TRUE)
    out = flatten(resp$data)
    return(out)
}

get_player_service_record = function(gamertag) {
    flog.info("Obtaining service record for %s", gamertag)
    req = api_player_service_record(gamertag = gamertag)
    resp = req_perform(req)
    resp = resp_body_json(resp, simplifyDataFrame = TRUE)
    medals = resp$data$core$breakdowns$medals
    resp$data$core$breakdowns$medals = NULL
    stats = flatten(as.data.frame(resp$data))
    out = list(
        "medals" = medals,
        "stats"  = stats
    )
    return(out)
}

save_one_player_data = function(handle, con, table_name, gamertag, one = FALSE) {
    message("Starting player data fetch")
    data_df = get_player_data(gamertag, handle, one = one)
    if (! dbExistsTable(con, table_name)) {
        old_player_data = data.frame(
            'player' = character(0),
            'id' = character(0)
        )
        dbCreateTable(con, table_name, fields = data_df)
    } else {
        old_player_data = pull_player_data(con, table_name, gamertag)
    }
    data_df = data_df %>% anti_join(old_player_data, by = c("player", "id"))
    dbAppendTable(con, table_name, value = data_df, append = TRUE, overwrite = FALSE)
    return(invisible(NULL))
}

save_player_data = function(handle, con, table_name, gamertags, one) {
    for (g in gamertags) {
        message("Working on ", g)
        save_one_player_data(handle, con, table_name, g, one = one)
    }
}

pull_player_data = function(con, table_name, gamertag) {
    gamertag = paste0("'", gamertag, "'", collapse = ', ')
    d = dbGetQuery(con, glue::glue("select * from {table_name} where player in ({gamertag})"))
    d %>%
        mutate(played_at = as_datetime(played_at))
}

