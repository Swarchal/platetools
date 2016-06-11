context("fill_plate")

# example data
vals <- rnorm(96)
wells <- num_to_well(1:96)
df <- data.frame(wells, vals)
df_missing <- df[-c(1:10), ]
df_out <- fill_plate(df_missing, "wells")

test_that("returns error when expected",{
    expect_error(fill_plate(df_missing, df_missing$wells))
    expect_error(fill_plate("not_a_df", "string"))
    expect_error(fill_plate(df_missing, "wells", plate = 1))
    expect_error(fill_plate(df_missing, "not_in"))
})

test_that("returns a dataframe",{
    expect_is(fill_plate(df_missing, "wells"), 'data.frame')
})

test_that("returns correct size dataframe",{
    expect_equal(nrow(fill_plate(df_missing, "wells")), 96L)
    expect_equal(ncol(fill_plate(df_missing, "wells")), 2L)
})

test_that("returns same named columns",{
    expect_equal(names(df_out), c("wells", "vals"))
})

test_that("returns the same values",{
    expect_equal(as.numeric(na.omit(df_out$vals)),
		 as.numeric(df_missing$vals),
		 tolerance = 1e-5)
})

# test 384 well plate
vals384 <- rnorm(384)
wells384 <- num_to_well(1:384, plate = 384)
df384 <- data.frame(wells = wells384, vals = vals384)
df384_missing <- df[-c(1:10), ]
df384_out <- fill_plate(df384_missing, "wells", plate = 384)

# test 1536 plate
vals1536 <- rnorm(1536)
wells1536 <- num_to_well(1:1536, plate = 1536)
df384 <- data.frame(wells = wells1536, vals = vals1536)
df384_missing <- df[-c(1:10), ]
df384_out <- fill_plate(df384_missing, "wells", plate = 1536)


