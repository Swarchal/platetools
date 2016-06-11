context("plate effects")

# example data
df <- data.frame(well = num_to_well(1:96),
		 val = rnorm(96))

platemap <- plate_map(data = df$val, well = df$well)

test_that("plate_effect returns error when expected",{
    expect_error(plate_effect(platemap, plate = 1))
})

test_that("plate_effect returns a medpolish object",{
    expect_true(class(plate_effect(platemap, 96)) == 'medpolish')
})


# example data
df384 <- data.frame(well = num_to_well(1:384),
		    val = rnorm(384))
platemap384 <- plate_map(data = df384$val,
			 well = df384$well)

test_that("384 plate work as well",{
    expect_true(class(plate_effect(platemap384, 384)) == 'medpolish')
})
