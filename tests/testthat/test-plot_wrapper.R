context("plot_wrapper")

# example data
df96 <- data.frame(well = num_to_well(1:96),
		 val = rnorm(96))

df384 <- data.frame(well = num_to_well(1:384, plate = 384),
		    val = rnorm(384))

platemap96 <- plate_map(data = df96$val,
		      well = df96$well)

platemap384 <- plate_map(data = df384$val,
			 well = df384$well)

out96 <- plt96(platemap96)
out384 <- plt384(platemap384)

test_that("returns ggplot object",{
    expect_is(plt96(platemap96), 'ggplot')
    expect_is(plt384(platemap384), 'ggplot')
})

test_that("returns expected ggplot object",{
    expect_equal(length(out96), 9L) # 9 element list
    expect_equal(names(out96),
		 c('data', 'layers', 'scales', 'mapping',
		   'theme', 'coordinates', 'facet', 'plot_env',
		   'labels'))
    expect_equal(names(out96), names(out384))
})



