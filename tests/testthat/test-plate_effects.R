context("plate effects")

# example data
df <- data.frame(well = num_to_well(1:96),
		 		 val = rnorm(96))

platemap <- plate_map(data = df$val,
					  well = df$well)

test_that("plate_effect returns error when expected",{
    expect_error(plate_effect(platemap, plate = 1))
})

test_that("plate_effect returns a medpolish object",{
    expect_true(class(plate_effect(platemap, 96)) == 'medpolish')
})


# example data
df384 <- data.frame(well = num_to_well(1:384, plate = 384),
		    		val = rnorm(384))
platemap384 <- plate_map(data = df384$val,
			 			 well = df384$well)

test_that("384 plate works as well", {
    expect_true(class(plate_effect(platemap384, 384)) == 'medpolish')
})


df1536 <- data.frame(well = num_to_well(1:1536, plate = 1536),
					 val = rnorm(1536))

platemap1536 <- plate_map(data = df1536$val,
						  well = df1536$well)

test_that("1536 plate works as well", {
	expect_true(class(plate_effect(platemap1536, 1536)) == 'medpolish')
})