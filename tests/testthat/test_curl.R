library(testthat)

test_that("Handle creates OK", {
    expect_true(Sys.getenv("CRYPTUM_API_KEY") != '')
    handle <<- create_handle()
    expect_true(inherits(handle, 'curl_handle'))
})

test_that("CSR data returns OK", {
    res = get_player_csr('Corvus0805', 1, handle)
    cols = c("data.current.value", "data.current.measurement_matches_remaining",
                 "data.current.tier", "data.current.tier_start", "data.current.sub_tier",
                 "data.current.next_tier", "data.current.next_tier_start", "data.current.next_sub_tier",
                 "data.current.initial_measurement_matches", "data.current.tier_image_url",
                 "data.season.value", "data.season.measurement_matches_remaining",
                 "data.season.tier", "data.season.tier_start", "data.season.sub_tier",
                 "data.season.next_tier", "data.season.next_tier_start", "data.season.next_sub_tier",
                 "data.season.initial_measurement_matches", "data.season.tier_image_url",
                 "data.all_time.value", "data.all_time.measurement_matches_remaining",
                 "data.all_time.tier", "data.all_time.tier_start", "data.all_time.sub_tier",
                 "data.all_time.next_tier", "data.all_time.next_tier_start", "data.all_time.next_sub_tier",
                 "data.all_time.initial_measurement_matches", "data.all_time.tier_image_url",
                 "additional.gamertag", "additional.season", "additional.queue",
                 "additional.input")
    expect_true( all.equal(cols, colnames(res)) )
    expect_true( all(res$additional.gamertag == 'Corvus0805') )
    expect_true( is.numeric(res$data.current.value) )
})

test_that("Match data returns OK", {
    res = get_player_match_data('Corvus0805', handle, one = TRUE)
    cols = c("id", "details.category.name", "details.category.asset.id",
             "details.category.asset.version", "details.category.asset.thumbnail_url",
             "details.map.name", "details.map.asset.id", "details.map.asset.version",
             "details.map.asset.thumbnail_url", "teams.enabled", "teams.scoring",
             "teams.details.id", "teams.details.name", "teams.details.emblem_url",
             "stats.summary.kills", "stats.summary.deaths", "stats.summary.assists",
             "stats.summary.betrayals", "stats.summary.suicides", "stats.summary.vehicles.destroys",
             "stats.summary.vehicles.hijacks", "stats.summary.medals", "stats.damage.taken",
             "stats.damage.dealt", "stats.shots.fired", "stats.shots.landed",
             "stats.shots.missed", "stats.shots.accuracy", "stats.rounds.won",
             "stats.rounds.lost", "stats.rounds.tied", "stats.breakdowns.kills.melee",
             "stats.breakdowns.kills.grenades", "stats.breakdowns.kills.headshots",
             "stats.breakdowns.kills.power_weapons", "stats.breakdowns.assists.emp",
             "stats.breakdowns.assists.driver", "stats.breakdowns.assists.callouts",
             "stats.kda", "stats.kdr", "stats.score", "rank", "outcome", "experience",
             "ranked", "played_at", "duration.seconds", "duration.human",
             "dps", "player")
    expect_true(
        all(cols %in% colnames(res))
    )
})

test_that("Service record returns OK", {
    res = get_player_service_record('Corvus0805', handle)
    cols = c("data.summary.kills", "data.summary.deaths", "data.summary.assists",
             "data.summary.betrayals", "data.summary.suicides", "data.summary.vehicles.destroys",
             "data.summary.vehicles.hijacks", "data.summary.medals", "data.damage.taken",
             "data.damage.dealt", "data.damage.average", "data.shots.fired",
             "data.shots.landed", "data.shots.missed", "data.shots.accuracy",
             "data.breakdowns.kills.melee", "data.breakdowns.kills.grenades",
             "data.breakdowns.kills.headshots", "data.breakdowns.kills.power_weapons",
             "data.breakdowns.assists.emp", "data.breakdowns.assists.driver",
             "data.breakdowns.assists.callouts", "data.breakdowns.matches.wins",
             "data.breakdowns.matches.losses", "data.breakdowns.matches.left",
             "data.breakdowns.matches.draws", "data.kda", "data.kdr", "data.total_score",
             "data.matches_played", "data.time_played.seconds", "data.time_played.human",
             "data.win_rate", "additional.gamertag", "additional.enforcement"
    )
    expect_true(
        all(cols %in% colnames(res))
    )
})
