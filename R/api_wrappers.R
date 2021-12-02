#' Service Record API URL
#'
#' Returns the formatted API URL for the player's service record.
#'
#' @param gamertag Character value for the gamertag to view.
#' @importFrom glue glue
api_player_service_record = function(gamertag) {
    stopifnot(
        is.character(gamertag)
    )
    gamertag = URLencode(gamertag)
    hapi = glue::glue('https://cryptum.halodotapi.com/games/hi/stats/players/{gamertag}/service-record/global')
    return(hapi)
}

#' Match Record API URL
#'
#' Returns the formatted API URL for the player's last 100 matches.
#'
#' @param gamertag Character value for the gamertag to view.
#' @param page Numeric value for the page to view. The data returned is paginated,
#' and you will want to use the next page number that is returned in the response.
#' @importFrom glue glue
api_player_matches = function(gamertag, page) {
    page = as.integer(page)
    stopifnot(
        length(gamertag) == 1,
        length(page) == 1,
        is.character(gamertag),
        ! is.na(page)
    )
    gamertag = URLencode(gamertag)
    hapi = glue::glue('https://cryptum.halodotapi.com/games/hi/stats/players/{gamertag}/matches?page={page}')
    return(hapi)
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
#' @importFrom glue glue
api_player_csr = function(gamertag, season, input = c('mnk', 'controller', 'crossplay'), queue = c('solo-duo', 'open')) {
    season = as.integer(season)
    stopifnot(
        length(gamertag) == 1,
        length(season) == 1,
        length(input) == 1,
        length(queue) == 1,
        is.character(gamertag),
        is.character(input),
        is.character(queue),
        ! is.na(season)
    )
    gamertag = URLencode(gamertag)
    hapi = glue::glue('https://cryptum.halodotapi.com/games/hi/stats/players/{gamertag}/csr?season={season}&input={input}&queue={queue}')
    return(hapi)
}

#' Get match data
#'
#' Runs through the pagination for the match history for the supplied gamertag.
#' Each response is returned as JSON, which is formatted as a data.frame for the
#' user. The response contains a field for the next page number, which is used
#' until it returns `NULL`.
#'
#' @param gamertag Character value for the gamertag to view.
#' @param handle `curl` handle to use, created with curl::new_handle. Used to supply authentication token.
#' @param one Boolean, if TRUE returns only the first page. Useful if
#' @importFrom dplyr `%>%` mutate bind_rows
#' @importFrom lubridate as_datetime
#' @importFrom curl curl_fetch_memory
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map
#' @importFrom tibble as_tibble
get_player_match_data = function(gamertag, handle, one = FALSE) {
    message("Obtaining data for ", gamertag)
    page = 1
    data_list = list()
    while (! is.null(page)) {
        message("Querying page", page)
        res = curl::curl_fetch_memory(url = api_player_matches(gamertag, page), handle = handle)
        data = jsonlite::fromJSON(rawToChar(res$content), simplifyDataFrame = TRUE)
        data_list[[page]] = data
        page = data$paging$`next`
        if (one) page = NULL
    }

    data_df = as_tibble(bind_rows(map(data_list, 'data'))) %>%
        unnest_df() %>%
        mutate(played_at = as_datetime(played_at),
               dps = stats.damage.dealt / duration.seconds,
               player = gamertag)

    return(data_df)
}

#' Get CSR data
#'
#' @param gamertag Character value for the gamertag to view.
#' @param season Numeric value for the season to view.
#' @param handle `curl` handle to use, created with curl::new_handle. Used to supply authentication token.
#' @importFrom curl curl_fetch_memory
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr `%>%` bind_rows
get_player_csr = function(gamertag, season, handle) {
    message("Obtaining data for ", gamertag)
    vals = data.frame(
        'inputs' = c('mnk', 'controller', 'crossplay'),
        'queues' = c('solo-duo', 'solo-duo', 'open')
    )
    out = data.frame()
    for (i in 1:nrow(vals)) {
        res = curl::curl_fetch_memory(url = api_player_csr(gamertag, season, vals[i, 'inputs'], vals[i, 'queues']), handle = handle)
        data = jsonlite::fromJSON(rawToChar(res$content), simplifyDataFrame = TRUE)
        data_df = data %>% unnest_df()
        if ('statusCode' %in% colnames(data_df)) {
            next
        } else {
            out = bind_rows(out, data_df)
        }
    }

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

