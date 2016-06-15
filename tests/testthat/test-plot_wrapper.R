context("plot_wrapper")

# example data
df96 <- data.frame(well = num_to_well(1:96),
		 		   val = rnorm(96))

df384 <- data.frame(well = num_to_well(1:384, plate = 384),
		    		val = rnorm(384))

df1536 <- data.frame(well = num_to_well(1:1536, plate = 1536),
					 val = rnorm(1536))

platemap96 <- plate_map(data = df96$val,
		      			well = df96$well)

platemap384 <- plate_map(data = df384$val,
			 			 well = df384$well)

platemap1536 <- plate_map(data = df1536$val,
						  well = df1536$well)

out96 <- plt96(platemap96)
out384 <- plt384(platemap384)
out1546	<- plt1536(platemap1536)

test_that("returns ggplot object",{
    expect_is(plt96(platemap96), 'ggplot')
    expect_is(plt384(platemap384), 'ggplot')
    expect_is(plt1536(platemap1536), 'ggplot')
})

test_that("returns expected ggplot object",{
    expect_equal(length(out96), 9L) # 9 element list
    expect_equal(names(out96),
		 c('data', 'layers', 'scales', 'mapping',
		   'theme', 'coordinates', 'facet', 'plot_env',
		   'labels'))
    expect_equal(names(out96), names(out384))
})



