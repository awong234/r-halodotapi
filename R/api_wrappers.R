#' API URL
#'
#' The base API url. The default version is currently
#' `r pkgenv$latest_supported_version`. This version is retrieved via an R
#' option named `halodotapi.default_version`
#' @returns A character vector with the base API url.
#' @export
api_base_url = function() {
    version = getOption('halodotapi.default_version')
    url = sprintf('https://halo.api.stdlib.com/infinite@%s', version)
    return(url)
}

#' API URLs
#'
#' These functions return the formatted API URL for the corresponding Halodotapi
#' endpoint. They do NOT perform queries, instead returning a valid,
#' authenticated URL ready to be requested.
#'
#' Note that solo-duo queue only has input values for 'mnk' and 'controller',
#' whereas open queue only has the 'crossplay' input value.
#' @param gamertag Character value for the gamertag to view.
#' @param type One of either 'multiplayer' or 'campaign'
#' @param filter One of `'all', 'pvp', 'social', 'ranked', 'bots', 'custom'` to
#'   limit the results. These only affect data returned for the `multiplayer`
#'   type.
#' @param count The number of records to return per paginated result. Defaults
#'   to 25, which is also the upper limit.
#' @param offset The pagination number to offset by. Starts at 0, and continues
#'   in integer values until no more results are available.
#' @param mode One of either 'matchmade' or 'custom'. Defaults to 'matchmade'.
#' @param match.id The 32-character match identifier.
#' @param season Numeric value for the season to view.
#' @param input Character value, one of 'mnk', 'controller', or 'crossplay' for the specific input type.
#' @param queue Character value, one of 'open', or 'solo-duo' for the queuing method.
#' @name api_urls
NULL

#' @rdname api_urls
#' @importFrom glue glue
#' @importFrom httr2 req_url_path_append req_url_query
#' @export
api_player_service_record = function(
    gamertag,
    type = c("multiplayer", "campaign"),
    filter = c('all', 'pvp', 'social', 'ranked', 'bots', 'custom')
) {
    stopifnot(
        is.character(gamertag)
    )
    type = match.arg(type)
    filter = match.arg(filter)
    filter = adjust_service_record_filter(filter)
    gamertag = URLencode(gamertag)
    url = api_base_url()
    req = request(url)
    req = req_url_path_append(req, 'stats/service-record')
    req = req_url_path_append(req, type)
    req = add_auth_header(req)
    req = req_url_query(req, gamertag = gamertag, filter = filter)
    return(req)
}

#' @rdname api_urls
#' @importFrom httr2 request req_headers req_url_query req_perform resp_body_json req_url_path_append
#' @importFrom dplyr bind_rows
#' @importFrom jsonlite flatten
#' @export
api_player_match_history = function(gamertag, count, offset, mode) {
    stopifnot(
        length(gamertag) == 1L,
        is.character(gamertag),
        is.numeric(count) & count <= 25L, # 403 forbidden error results if count > 25.
        is.numeric(offset) & offset >= 0L,
        mode %in% c("matchmade", "custom")
    )
    gamertag = URLencode(gamertag)
    url = api_base_url()
    req = request(url)
    req = req_url_path_append(req, "stats/matches/list")
    req = add_auth_header(req)
    req = req_url_query(req, gamertag = gamertag, `limit.count` = count, `limit.offset` = offset, mode = mode)
    return(req)
}

#' @rdname api_urls
#' @export
api_match_details = function(match.id) {
    stopifnot(
        is.character(match.id)
    )
    url = api_base_url()
    req = request(url)
    req = req_url_path_append(req, "stats/matches/retrieve")
    req = add_auth_header(req)
    req = req_url_query(req, id = match.id)
    return(req)
}

#' @rdname api_urls
#' @importFrom httr2 request req_url_query
#' @export
api_player_csr = function(gamertag, season) {
    season = as.integer(season)
    stopifnot(
        length(gamertag) == 1,
        length(season) == 1,
        is.character(gamertag)
    )
    gamertag = URLencode(gamertag)
    url = api_base_url()
    req = request('https://halo.api.stdlib.com/infinite@0.3.5/stats/csrs/')
    req = add_auth_header(req)
    req = req_url_query(req, gamertag = gamertag, season = season)
    return(req)
}

#' Get match data
#'
#' Queries the match history for the supplied gamertag.
#'
#' Each response is returned as JSON, which is formatted as a data.frame for the
#' user. The response contains a field for the next page number, which is used
#' until it returns `NULL`.
#'
#' Statistics and medals are available for each match.
#'
#' @param gamertag Character value for the gamertag to view.
#' @param one Boolean, if TRUE returns only the first page. Useful for testing.
#' @return A list of two elements, a data frame with medals, and a data frame with statistics.
#' @importFrom httr2 resp_body_json
#' @importFrom futile.logger flog.info flog.debug
#' @importFrom jsonlite flatten
#' @importFrom tidyr unnest
#' @importFrom dplyr select
#' @export
get_player_match_history = function(gamertag, count = 25L, offset = 0L, mode = c("matchmade", "custom"), one = FALSE) {
    stopifnot(
        length(gamertag) == 1L,
        is.character(gamertag),
        is.numeric(count),
        is.numeric(offset)
    )
    mode = match.arg(mode)
    # Counters
    i = 1
    flog.info("Obtaining %s match data for %s", mode, gamertag)
    if (one) flog.debug("Debug mode: returning single page only")
    flog.debug("Returning %d items per page", count)
    # Output objects
    stats_list = list()
    medals_list = list()
    while (TRUE) {
        flog.debug("offset %d", offset)
        req = api_player_match_history(gamertag = gamertag, count = count, offset = offset, mode = mode)
        resp = req_perform(req)
        resp = resp_body_json(resp, simplifyDataFrame = TRUE)
        offset = resp$paging$offset + count
        if (resp$count == 0L) {
            break
        } else {
            stats = flatten(resp$data)
            medals_cols = select(stats, match.id = id, player.stats.core.breakdowns.medals)
            medals = unnest(medals_cols,
                cols = player.stats.core.breakdowns.medals
            )
            # we're removing this item because medals are collected in their own object
            stats$player.stats.core.breakdowns.medals = NULL
            stats_list[[i]] = stats
            medals_list[[i]] = medals
            i = i + 1
            if (one) {
                flog.debug("Breaking out of loop returning single page")
                break
            }
        }
    }
    # Condense into single data frame
    medals_out = bind_rows(medals_list)
    stats_out = bind_rows(stats_list)

    data_list = list(
        medals = medals_out,
        stats  = stats_out
    )

    return(data_list)
}

#' Get match session details
#'
#' This query takes no gamertag input, instead retrieving some high-level
#' information about the match (1 record for several match-level variables), and
#' all players in the match (1 record for each player).
#'
#' There are three items returned:
#'
#' * Match session details
#' * Player details
#' * Player medals
#'
#' The match session details contain information on dimensions related to that
#' specific game session. These include:
#'
#' * Match ID
#' * Play date-time
#' * Game mode
#' * Map name
#'
#' And other dimensions.
#'
#' The player details contain individual performance summaries for the match,
#' for each participant in the match. These are many of the same dimensions as
#' provided in `get_player_match_history` and `get_player_service_record`. This
#' players dataset has a many-to-one relationship with the match ID.
#'
#' The player medals dataset has a many-to-one relationship with the player
#' details, and a many-to-one relationship with the match ID. This dataset includes:
#'
#' * Medal-specific identifier codes
#' * Medal names
#' * Frequency of medals earned
#' * Match ID and gamertag of the recipient.
#'
#' Most datasets include thumbnails of the assets involved.
#'
#' @param match.id The 32-character match identifier, typically retrieved from a player's match history.
#'
#' @return A list of three data frames, containing match session information, player stats, and player medals
#' @export
get_match_details = function(match.id) {
    flog.info("Obtaining match information for match: %s", match.id)
    req = api_match_details(match.id)
    resp = req_perform(req)
    resp = resp_body_json(resp, simplifyDataFrame = TRUE)
    # Several levels of data here. Match-level data includes ID, details, teams, experience, duration
    id = data.frame(match.id = resp$data$id, stringsAsFactors = FALSE)
    experience = data.frame(experience = resp$data$experience, stringsAsFactors = FALSE)
    played_at = data.frame(played_at = resp$data$played_at, stringsAsFactors = FALSE)
    match_details = replace_NULL_with_NA(resp$data$details)
    match_details = as.data.frame(match_details)
    match_level_data = cbind(match.id = id, experience, played_at, match_details)
    # Player details
    player_level_data = flatten(resp$data$players)
    player_level_stats = select(player_level_data, -stats.core.breakdowns.medals)
    player_level_medals = select(player_level_data, stats.core.breakdowns.medals)[[1]]
    names(player_level_medals) = player_level_stats$gamertag
    player_level_medals = bind_rows(player_level_medals, .id = 'gamertag')
    # Assign match ID to player-level data
    player_level_stats = cbind(player_level_stats, match.id = resp$data$id)
    player_level_medals = cbind(player_level_medals, match.id = resp$data$id)
    return(list(
        match_details = match_level_data,
        player_stats = player_level_stats,
        player_medals = player_level_medals
    ))
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
#' @export
get_player_csr = function(gamertag, season) {
    flog.info("Obtaining CSR for %s", gamertag)
    req = api_player_csr(gamertag = gamertag, season = season)
    resp = req_perform(req)
    resp = resp_body_json(resp, simplifyDataFrame = TRUE)
    out = flatten(resp$data)
    return(out)
}

#' Get player service record
#'
#' The player service record is a record of medals, and single overall
#' statistics over the player's lifetime.
#'
#' Examples of these are lifetime totals of:
#'
#' * Kills
#' * Deaths
#' * Assists
#' * Betrayals
#' * Hijacks
#' * Damage dealt/taken
#' * Shots fired/missed/accuracy
#' * KDA/KDR
#' * Matches played
#' * Time played
#' * Score
#'
#' and several more dimensions.
#'
#' @param gamertag Gamertag to query. A single character string.
#' @param type One of either `multiplayer` or `campaign`.
#' @param filter One of `'all', 'pvp', 'social', 'ranked', 'bots', 'custom'`.
#'
#' @return
#' @export
get_player_service_record = function(
    gamertag,
    type = c('multiplayer', 'campaign'),
    filter = c('all', 'pvp', 'social', 'ranked', 'bots', 'custom')
) {
    flog.info("Obtaining %s, %s service record for %s", type, filter, gamertag)
    type = match.arg(type)
    filter = match.arg(filter)
    req = api_player_service_record(gamertag = gamertag, type = type, filter = filter)
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

