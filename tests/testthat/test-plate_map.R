context("plate_map")
set.seed(12321)

data_96 <- data.frame(well = num_to_well(1:96),
		      val = rnorm(96, 100))

data_384 <- data.frame(well = num_to_well(1:384, plate = 384),
		       val = rnorm(384, 100))

out_96 <- plate_map(data = data_96$val, well = data_96$well)
out_384 <- plate_map(data = data_384$val, well = data_384$well)

data_1536 <- data.frame(well = num_to_well(1:1536, plate = 1536),
                        val = rnorm(1536, 100))

out_1536 <- plate_map(data = data_1536$val, well = data_1536$well)

test_that("plate_map returns a dataframe",{
    expect_true(is.data.frame(out_96))
    expect_equal(nrow(out_96), 96L)
    expect_equal(ncol(out_96), 4L)

    expect_true(is.data.frame(out_384))
    expect_equal(nrow(out_384), 384L)
    expect_equal(ncol(out_384), 4L)

    expect_true(is.data.frame(out_1536))
    expect_equal(nrow(out_1536), 1536L)
    expect_equal(ncol(out_1536), 4L)
})

test_that("plate_map returns expected columns",{
    expect_equal(names(out_96), c("well", "Row", "Column", "values"))
    expect_equal(names(out_384), c("well", "Row", "Column", "values"))
    expect_equal(names(out_1536), c("well", "Row", "Column", "values"))
})

test_that("plate_map returns expected values",{
    expect_equal(as.character(out_96$well), num_to_well(1:96))
    expect_equal(as.character(out_384$well), num_to_well(1:384, plate = 384))
    expect_equal(as.character(out_1536$well), num_to_well(1:1536, plate = 1536))

    expect_equal(out_96$Row, rep(1:8, each = 12))
    expect_equal(out_96$Column, rep(1:12, 8))
    expect_equal(out_96$values, data_96$val)

    expect_equal(out_384$Row, rep(1:16, each = 24))
    expect_equal(out_384$Column, rep(1:24, 16))
    expect_equal(out_384$values, data_384$val)

    expect_equal(out_1536$Row, rep(1:32, each = 48))
    expect_equal(out_1536$Column, rep(1:48, 32))
    expect_equal(out_1536$values, data_1536$val)
})


test_that("plate_map_scale returns expected values",{
    out_scale <- plate_map_scale(data = data_96$val,
				 well = data_96$well)
    expect_true(is.data.frame(out_scale))
    expect_equal(names(out_scale), c("well", "Row", "Column", "values"))
    expect_equal(mean(out_scale$values), 0, tolerance = 1e-5)
})



data_grid <- rbind(data_96, data_96)
data_grid$plate_id <- rep(c("plate_1", "plate_2"), each = 96)
out_grid <- plate_map_grid(data = data_grid$val,
			   well = data_grid$well,
			   plate_id = data_grid$plate_id)

test_that("plate_map_grid returns column of plate ids",{
    expect_true(is.data.frame(out_grid))
    expect_equal(names(out_grid),
		 c("well", "Row", "Column", "values", "plate_label"))
})


test_that("plate_map_grid_scale creates a dataframe", {

    # create test data
    vals <- c(rnorm(96), rnorm(96, mean = 10))
    wells <- rep(num_to_well(1:96), 2)
    plate_id <- rep(c("plate_1", "plate_2"), each = 96)

    out_each <- plate_map_grid_scale(data = vals,
                                well = wells,
                                plate_id = plate_id,
                                each = TRUE)


    out_not_each <- plate_map_grid_scale(data = vals,
                                well = wells,
                                plate_id = plate_id,
                                each = FALSE)

    expect_is(out_each, "data.frame")
    expect_is(out_not_each, "data.frame")
    expect_equal(nrow(out_each), length(vals))
    expect_equal(nrow(out_not_each), length(vals))
    expect_equal(ncol(out_each), ncol(out_not_each))
})

test_that("plate_map_grid_scale argument each works", {

    # create test data
    vals <- c(rnorm(96), rnorm(96, mean = 10))
    wells <- rep(num_to_well(1:96), 2)
    plate_id <- rep(c("plate_1", "plate_2"), each = 96)

    out_each <- plate_map_grid_scale(data = vals,
                                well = wells,
                                plate_id = plate_id,
                                each = TRUE)


    out_not_each <- plate_map_grid_scale(data = vals,
                                well = wells,
                                plate_id = plate_id,
                                each = FALSE)

    # check that it's actually scaling the values
    expect_equal(mean(out_not_each$values), 0, tolerance = 1e-3)
    expect_equal(sd(out_not_each$values), 1, tolerance = 1e-3)

    # split scaled separately data (each = TRUE) into to
    # and confirm each one has a mean of zero

    # because R
    out_each$plate_label <- as.factor(out_each$plate_label)

    sub1 <- out_each[out_each$plate_label == "plate_1", ]
    sub2 <- out_each[out_each$plate_label == "plate_2", ]

    expect_equal(mean(sub1$values), 0, tolerance = 1e-3)
    expect_equal(sd(sub1$values), 1, tolerance = 1e-3)
    expect_equal(mean(sub2$values), 0, tolerance = 1e-3)
    expect_equal(sd(sub2$values), 1, tolerance = 1e-3)
})


test_that("plate_map_multiple returns expected", {
		vals1 <- rnorm(96)
		vals2 <- rnorm(96)
		wells <- num_to_well(1:96)

		df <- data.frame(wells, vals1, vals2)

		out <- plate_map_multiple(df[, c("vals1", "vals2")], well = df$wells)

        expect_is(out, "data.frame")
        expect_equal(nrow(out), 96L, tolerance = 1e-5)
        expect_equal(ncol(out), 5L, tolerance = 1e-5)
        expect_equal(as.numeric(vals1), as.numeric(df[, "vals1"]))
        expect_equal(as.numeric(vals2), as.numeric(df[, "vals2"]))

})