test_that("Can replace NULL with NA in nested list of depth 1", {
    test_list = list(
        first = 1,
        second = list(
            a = NULL,
            b = 3
        )
    )
    depth = max_depth(test_list)
    replaced = replace_NULL_with_NA(test_list, depth = depth)
    expect_true(is.na(replaced$second$a))
})

test_that("Can replace NULL with NA in nested list of depth > 1", {
    test_list = list(
        first = c(1,2),
        second = list(
            a = c(NULL, NULL),
            b = c(3,4)
        )
    )

    depth = max(rapply(test_list, function(x) length(x)))
    replaced = replace_NULL_with_NA(test_list, depth = depth)
    expect_true(identical(replaced$second$a, c(NA, NA)))
})
