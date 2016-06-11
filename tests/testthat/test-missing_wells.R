context("missing_wells")

# create data with no missing wells
full <- data.frame(val = rnorm(96),
		   well = num_to_well(1:96))

# create data with missing wells
# missing last 6 wells
missing <- data.frame(val = rnorm(90),
		      well = num_to_well(1:90))

out_full <- missing_wells(full, well = 'well')
out_missing <- missing_wells(missing, well = 'well')


test_that("missing_wells errors when expected",{
     expect_error( missing_wells(full, well = TRUE))
     expect_error( missing_wells(full, well = "well", plate = 1))
     expect_error( missing_wells(full, well = full$well))
})

test_that("missing wells doesn't return anything for a full plate",{
    expect_true(length(out_full) == 0L)
})

test_that("missing wells return expected answer",{
    expect_equal(length(out_missing), 6L)
    expect_equal(out_missing, num_to_well(91:96))
})
