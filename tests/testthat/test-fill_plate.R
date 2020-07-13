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


# test 6 well plate
vals6 <- rnorm(6)
wells6 <- num_to_well(1:6, plate = 6)
df6 <- data.frame(wells = wells6, vals = vals6)
df6_missing <- df[-c(1:2), ]
df6_out <- fill_plate(df6_missing, "wells", plate = 6)

# test 12 well plate
vals12 <- rnorm(12)
wells12 <- num_to_well(1:12, plate = 12)
df12 <- data.frame(wells = wells12, vals = vals12)
df12_missing <- df[-c(1:5), ]
df12_out <- fill_plate(df12_missing, "wells", plate = 12)

# test 24 well plate
vals24 <- rnorm(24)
wells24 <- num_to_well(1:24, plate = 24)
df24 <- data.frame(wells = wells24, vals = vals24)
df24_missing <- df[-c(1:10), ]
df24_out <- fill_plate(df24_missing, "wells", plate = 24)

# test 48 well plate
vals48 <- rnorm(48)
wells48 <- num_to_well(1:48, plate = 48)
df48 <- data.frame(wells = wells48, vals = vals48)
df48_missing <- df[-c(1:10), ]
df48_out <- fill_plate(df48_missing, "wells", plate = 48)

# test 96 well plate
vals96 <- rnorm(96)
wells96 <- num_to_well(1:96, plate = 96)
df96 <- data.frame(wells = wells96, vals = vals96)
df96_missing <- df[-c(1:10), ]
df96_out <- fill_plate(df96_missing, "wells", plate = 96)

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


