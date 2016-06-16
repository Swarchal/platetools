context("basic tests for plot functions")


# example data
# single plate
vals96 <- rnorm(96)
wells96 <- num_to_well(1:96, plate = 96)

vals384 <- rnorm(384)
wells384 <- num_to_well(1:384, plate = 384)

vals1536 <- rnorm(1536)
wells1536 <- num_to_well(1:1536, plate = 1536)


test_that("return ggplot object", {

        expect_is(b_map(data = vals96,
                        well = wells96),
                        'ggplot')

        expect_is(b_map(data = vals384,
                        well = wells384,
                        plate = 384),
                        'ggplot')

        expect_is(b_map(data = vals1536,
                        well = wells1536,
                        plate = 1536),
                        'ggplot')

       #############################

        expect_is(hit_map(data = vals96,
                          well = wells96),
                          'ggplot')

        expect_is(hit_map(data = vals384,
                          well = wells384,
                          plate = 384),
                          'ggplot')

        expect_is(hit_map(data = vals1536,
                          well = wells1536,
                          plate = 1536),
                          'ggplot')

        #############################

        expect_is(raw_map(data = vals96,
                          well = wells96),
                          'ggplot')

        expect_is(raw_map(data = vals384,
                          well = wells384,
                          plate = 384),
                          'ggplot')

        expect_is(raw_map(data = vals1536,
                          well = wells1536,
                          plate = 1536),
                          'ggplot')

        #############################

        expect_is(z_map(data = vals96,
                        well = wells96),
                        'ggplot')

        expect_is(z_map(data = vals384,
                        well = wells384,
                        plate = 384),
                        'ggplot')

        expect_is(z_map(data = vals1536,
                        well = wells1536,
                        plate = 1536),
                        'ggplot')


})



test_that("returns error when incorrect plate is passed", {

    expect_warning(b_map(data = vals384,
                       well = wells384,
                       plate = 96))

    expect_warning(b_map(data = vals1536,
                       well = wells1536,
                       plate = 384))

    expect_error(z_map(data = vals96,
                       well = wells96,
                       plate = 1))

       ############################

    expect_warning(hit_map(data = vals384,
                         well = wells384,
                         plate = 96))

    expect_warning(hit_map(data = vals1536,
                         well = wells1536,
                         plate = 384))

    expect_error(hit_map(data = vals96,
                         well = wells96,
                         plate = 1))

    #############################

    expect_warning(raw_map(data = vals384,
                           well = wells384,
                           plate = 96))

    expect_warning(raw_map(data = vals1536,
                           well = wells1536,
                           plate = 384))

    expect_error(raw_map(data = vals96,
                         well = wells96,
                         plate = 1))

    #############################

    expect_warning(z_map(data = vals384,
                         well = wells384,
                         plate = 96))

    expect_warning(z_map(data = vals1536,
                         well = wells1536,
                         plate = 384))

    expect_error(z_map(data = vals96,
                       well = wells96,
                       plate = 1))
    })