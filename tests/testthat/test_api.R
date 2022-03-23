futile.logger::flog.threshold("DEBUG")

test_that("API player service record is proper class", {
    res = api_player_service_record("Corvus0805")
    expect_true(inherits(res, 'httr2_request'))
})

test_that("API player service record has the correct parameters", {
    res = api_player_service_record("Corvus0805", type = 'multiplayer', filter = 'pvp')
    expect_true(grepl(pattern = 'gamertag=Corvus0805', x = res$url))
    expect_true(grepl(pattern = 'service-record/multiplayer', x = res$url))
    expect_true(grepl(pattern = 'filter=matchmade%3Apvp', x = res$url))
})

test_that("API player service record data returns properly", {
    res = get_player_service_record("Corvus0805", type = "multiplayer", filter = "all")
    expect_length(res, 2)
    expect_true(is.data.frame(res$medals))
    expect_true(is.data.frame(res$stats))
    # Test with filter active
    res_social = get_player_service_record("Corvus0805", type = "multiplayer", filter = "social")
    res_ranked = get_player_service_record("Corvus0805", type = "multiplayer", filter = "ranked")
    expect_true(! identical(res_social, res_ranked))
})

# Match data

test_that("API player match has the proper class and query contents", {
    res = api_player_match_history('Corvus0805', count = 25L, offset = 0L, mode = "matchmade")
    expect_true(inherits(res, 'httr2_request'))
    expect_true(grepl(pattern = 'mode=matchmade', x = res$url))
    expect_true(grepl(pattern = 'gamertag=Corvus0805', x = res$url))
})

test_that("API player match data returns properly", {
    res = get_player_match_history("Corvus0805", one = TRUE)
    expect_length(res, 2)
    expect_true(inherits(res$medals, 'data.frame'))
    expect_true(inherits(res$stats, 'data.frame'))
})

test_that("Match list data returns properly", {
    match.id = '4ba33f1b-caef-492d-ba85-83dd6130558e'
    resp = get_match_details(match.id)
    expect_length(resp, 3)
    expect_true(inherits(resp$match_details, 'data.frame'))
    expect_true(inherits(resp$player_stats, 'data.frame'))
    expect_true(inherits(resp$player_medals, 'data.frame'))
})
