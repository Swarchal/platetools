context("set_block")


well_df <- data.frame(well = num_to_well(1:96))


test_that("set_block errors when expected", {
    df_wrong_colname = data.frame(incorrect = num_to_well(1:96))
    expect_error(set_block(df_wrong_colname, "A01", "new_colname", 0.1))
})


test_that("set_block returns warning when expected", {
    expect_warning(set_block(well_df, "A01~A05", "Mg2+", 0.1))
})


test_that("set_block returns expected answer with quotes", {
    p <- set_block(well_df, c("A01~B03", "A05~D05"), "dNTP", 0.25)
    p <- set_block(p, "H12", "dNTP", 0.25)
    ans_index = c(1,  2,  3,  5, 13, 14, 15, 17, 29, 41, 96)
    expected_ans <- rep(NA, 96)
    expected_ans[ans_index] <- 0.25
    expect_equal(p[["dNTP"]], expected_ans)
})


test_that("set_block returns expected answer with 384 well plates", {
    df_384 <- data.frame(well = num_to_well(1:384, plate=384))
    df_384 <- set_block(df_384, "A01~B24", "new_col", 1L)
    expected_ans <- rep(NA, 384)
    expected_ans[1:48] <- 1L
    expect_equal(df_384[["new_col"]], expected_ans)
})


test_that("set_block errors when wells are not present", {
#     well_df_short <- data.frame(well = num_to_well(1:5))
#     expect_error(set_block(well_df_short, "A01~H12", "new_col", 0.1))
})


test_that("set_block returns expected answer with 1536 well plates", {
    #TODO: make this
})


test_that("set_block returns expected answer with vector of co-ordinates", {
    #TODO: clarify the expected input
})


test_that("set_block works with data.tables and tibbles", {
    #TODO: make this
})
