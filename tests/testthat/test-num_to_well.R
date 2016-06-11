context("num_to_well")

p96 <- num_to_well(1:96)

p384 <- num_to_well(1:384, plate = 384)

p1536 <- num_to_well(1:1536, plate = 1536)

test_that("returns errors when expected",{
    expect_error(num_to_well("string"))
    expect_error(num_to_well(c("a", "b", "c")))
})

test_that("returns expected values",{
    expect_true(is.vector(p96))
    expect_true(is.vector(p384))
    expect_equal(length(p96), 96L)
    expect_equal(length(p384), 384L)
    expect_equal(p96[1], "A01")
    expect_equal(p96[length(p96)], "H12")
    expect_equal(p384[384], "P24")
    expect_equal(p1536[1], "A01")
    expect_equal(p1536[1536], "FF48")
})
