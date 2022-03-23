test_that("Incorrect version produces message on load", {
    expect_message(withr::with_options(list(halodotapi.default_version = 'x.x.x'), {
        devtools::load_all()
    }), regexp = "Version is not supported!")
})

test_that("Lack of API key produces message on load", {
    expect_message({
        withr::with_envvar(c("AUTOCODE_API_KEY" = ""), {
            devtools::load_all()
        })
    }, regexp = "No Autocode API key found!")
})
