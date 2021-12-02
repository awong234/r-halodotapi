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
    test_page = rpois(n = 1, lambda = 1)
    res = api_player_matches(gamertag = test_gamertag, page = test_page)
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
        glue::glue('matches?page={test_page}')
    )
    expect_true(all.equal(ressplit, test_components))
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





