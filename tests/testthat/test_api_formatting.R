library(testthat)

test_that('CSR api formats correctly', {
    test_gamertag = paste0(sample(LETTERS, 5), collapse = '')
    test_season = rpois(n = 1, lambda = 1)
    test_input = sample(c('mnk', 'controller'), size = 1)
    test_queue = 'solo-duo'
    res = api_player_csr(gamertag = test_gamertag, season = test_season, input = test_input, queue = test_queue)
    ressplit = strsplit(res, split = '/')[[1]]
    test_components = c(
        'https:',
        '',
        'cryptum.halodotapi.com',
        'games',
        'hi',
        'stats',
        'players',
        test_gamertag,
        glue::glue('csr?season={test_season}&input={test_input}&queue={test_queue}')
    )
    expect_true(isTRUE(all.equal(ressplit, test_components)))
})

test_that('Match API formats correctly', {
    test_gamertag = paste0(sample(LETTERS, 5), collapse = '')
    res = api_player_list_matches(gamertag = test_gamertag, count = 1, offset = 0)
    expect_s3_class(res, 'httr2_request')
    url = res$url
    split_url = strsplit(url, split = '/')[[1]]
    query = gsub(pattern = "\\?", replacement = "", x = split_url[8])
    split_query = strsplit(query, split = '&')[[1]]
    split_query = strsplit(split_query, split = '=')
    expect_equal(split_query[[1]][2], test_gamertag)
    expect_equal(split_query[[2]][2], '1')
    expect_equal(split_query[[3]][2], '0')
})

test_that('Service record API formats correctly', {
    test_gamertag = paste0(sample(LETTERS, 5), collapse = '')
    res = api_player_service_record(gamertag = test_gamertag)
    ressplit = strsplit(x = res, split = '/')[[1]]
    test_components = c(
        'https:',
        '',
        'cryptum.halodotapi.com',
        'games',
        'hi',
        'stats',
        'players',
        test_gamertag,
        'service-record',
        'global'
    )
    expect_true(all.equal(ressplit, test_components))
})





