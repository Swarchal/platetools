context("well_to_num")

input <- c("A01", "A02", "A03")

test_that("well_to_num returns numbers",{
    expect_true(is.numeric(well_to_num(input)))
    expect_equal(well_to_num("P24", plate = 384), 384L)
})


test_snake <- c("A10", "A11", "A12", "B01")
test_snake_384 <- c("A24", "B24")


test_that("snake works as expected",{
    expect_equal(well_to_num(test_snake), c(10, 11, 12, 13))
    expect_equal(well_to_num(test_snake, style = "snake"),
		 c(10, 11, 12, 24))

    expect_equal(well_to_num(test_snake_384, plate = 384),
		 c(24, 48))

    expect_equal(well_to_num(test_snake_384, plate = 384, style = "snake"),
		 c(24, 25))
})

