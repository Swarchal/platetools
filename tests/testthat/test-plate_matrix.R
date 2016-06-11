context("plate_matix")

# example data
x384 <- 1:384
wells384 <- num_to_well(1:384, plate = 384)
out384 <- plate_matrix(data = x384, well = wells384, plate = 384)

x96 <- 1:96
wells96 <- num_to_well(1:96)
out96 <- plate_matrix(data = x96, well = wells96)

x1536 <- 1:1536
wells1536 <- num_to_well(x1536, plate = 1536)
out1536 <- plate_matrix(data = x1536, well = wells1536, plate = 1536)

test_that("returns error when expected",{
    expect_error(plate_matrix(data = x384,
			      well = wells384,
			      plate = 1)
    )
})

test_that("returns a matrix",{
    expect_is(out384, 'matrix')
    expect_is(out96, 'matrix')
})

test_that("returns correct size matrix",{
    expect_equal(prod(dim(out384)), 384L)
    expect_equal(ncol(out384), 24L)
    expect_equal(nrow(out384), 16L)

    expect_equal(prod(dim(out96)), 96L)
    expect_equal(ncol(out96), 12L)
    expect_equal(nrow(out96), 8L)

    expect_equal(prod(dim(out1536)), 1536L)
    expect_equal(ncol(out1536), 48L)
    expect_equal(nrow(out1536), 32L)
})

test_that("returns expected values",{
    expect_equal(matrix(1:384, ncol = 24, nrow = 16, byrow = TRUE), out384)
    expect_equal(matrix(1:96, ncol = 12, nrow = 8, byrow = TRUE), out96)
    expect_equal(matrix(1:1536, ncol = 48, nrow = 32, byrow = TRUE), out1536)
})
