context("read_map")

# load data
data(readmap_data)
dat <- readmap_data

# example dataframe
df <- data.frame(well = num_to_well(1:96),
		 val = rnorm(96))

out_verbose <- read_map(df, map = dat)
out <- read_map(df, map = dat, verbose = FALSE)

test_that("adds data to dataframe",{
    expect_is(out, 'data.frame')
    expect_equal(ncol(out), 3L)
    expect_equal(ncol(out_verbose), 5L)
    expect_equal(nrow(out), 96L)
})

test_that("returns correct values",{
    # in this case the values in the platemap
    # are filled with the column numbers
    expect_equal(out_verbose$header, out_verbose$column)
})
