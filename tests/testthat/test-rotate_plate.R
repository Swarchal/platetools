context("rotate_plate")

mat <- matrix(1:100, nrow = 10)
out <- rotate_plate(mat)

test_that("returns expected values",{
    expect_is(out, 'matrix')
    expect_equal(dim(out), dim(mat))
    expect_equal(mat[1,1], 1L)
    expect_equal(out[1,1], 100L)
    expect_equal(rotate_plate(out), mat)
})
