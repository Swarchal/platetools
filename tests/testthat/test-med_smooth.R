context("med_smooth")

# example data

vals <- rnorm(96)
wells <- num_to_well(1:96)
platemap <- plate_map(data = vals, well = wells)
out <- med_smooth(platemap, plate = 96)

vals384 <- rnorm(384)
well384 <- num_to_well(1:384, plate = 384L)
platemap384 <- plate_map(data = vals384,
                         well = well384)
out384 <- med_smooth(platemap384, plate = 384L)

vals1536 <- rnorm(1536)
well1536 <- num_to_well(1:1536, plate = 1536L)
platemap1536 <- plate_map(data = vals1536,
                          well = well1536)
out1536 <- med_smooth(platemap1536, plate = 1536L)


test_that("med_smooth errors when expected",{
    expect_error(med_smooth(iris, plate = 96))
    expect_error(med_smooth(platemap, plate = 1))
})

test_that("med_smooth returns a dataframe",{
    expect_is(out, 'data.frame')
    expect_is(out384, 'data.frame')
    expect_is(out1536, 'data.frame')
})

test_that("return dataframe is correct size",{
    expect_equal(nrow(out), 96L)
    expect_equal(ncol(out), 2L)
    expect_equal(nrow(out384), 384L)
    expect_equal(ncol(out384), 2L)
    expect_equal(nrow(out1536), 1536L)
    expect_equal(ncol(out1536), 2L)
})