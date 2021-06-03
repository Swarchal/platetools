context("basic tests for plot functions")


# example data
# single plate
vals6 <- rnorm(6)
wells6 <- num_to_well(1:6, plate = 6)

vals12 <- rnorm(12)
wells12 <- num_to_well(1:12, plate = 12)

vals24 <- rnorm(24)
wells24 <- num_to_well(1:24, plate = 24)

vals48 <- rnorm(48)
wells48 <- num_to_well(1:48, plate = 48)

vals96 <- rnorm(96)
wells96 <- num_to_well(1:96, plate = 96)

vals384 <- rnorm(384)
wells384 <- num_to_well(1:384, plate = 384)

vals1536 <- rnorm(1536)
wells1536 <- num_to_well(1:1536, plate = 1536)


test_that("return ggplot object", {

        expect_is(b_map(data = vals6,
                        well = wells6,
                        plate = 6),
                        'ggplot')

        expect_is(b_map(data = vals12,
                        well = wells12,
                        plate = 12),
                        'ggplot')

        expect_is(b_map(data = vals24,
                        well = wells24,
                        plate = 24),
                        'ggplot')

        expect_is(b_map(data = vals48,
                        well = wells48,
                        plate = 48),
                        'ggplot')

        expect_is(b_map(data = vals96,
                        well = wells96,
                        plate = 96),
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

        expect_is(bhit_map(data = vals6,
                          well = wells6,
                          plate = 6),
                          'ggplot')

        expect_is(bhit_map(data = vals12,
                          well = wells12,
                          plate = 12),
                          'ggplot')

        expect_is(bhit_map(data = vals24,
                          well = wells24,
                          plate = 24),
                          'ggplot')

        expect_is(bhit_map(data = vals48,
                          well = wells48,
                          plate = 48),
                          'ggplot')

        expect_is(bhit_map(data = vals96,
                          well = wells96,
                          plate = 96),
                          'ggplot')

        expect_is(bhit_map(data = vals384,
                          well = wells384,
                          plate = 384),
                          'ggplot')

        expect_is(bhit_map(data = vals1536,
                          well = wells1536,
                          plate = 1536),
                          'ggplot')

       #############################

        expect_is(hit_map(data = vals6,
                          well = wells6,
                          plate = 6),
                          'ggplot')

        expect_is(hit_map(data = vals12,
                          well = wells12,
                          plate = 12),
                          'ggplot')

        expect_is(hit_map(data = vals24,
                          well = wells24,
                          plate = 24),
                          'ggplot')

        expect_is(hit_map(data = vals48,
                          well = wells48,
                          plate = 48),
                          'ggplot')

        expect_is(hit_map(data = vals96,
                          well = wells96,
                          plate = 96),
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

        expect_is(raw_map(data = vals6,
                          well = wells6,
                          plate = 6),
                          'ggplot')

        expect_is(raw_map(data = vals12,
                          well = wells12,
                          plate = 12),
                          'ggplot')

        expect_is(raw_map(data = vals24,
                          well = wells24,
                          plate = 24),
                          'ggplot')

        expect_is(raw_map(data = vals48,
                          well = wells48,
                          plate = 48),
                          'ggplot')

        expect_is(raw_map(data = vals96,
                          well = wells96,
                          plate = 96),
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

        expect_is(z_map(data = vals6,
                        well = wells6,
                        plate = 6),
                        'ggplot')

        expect_is(z_map(data = vals12,
                        well = wells12,
                        plate = 12),
                        'ggplot')

        expect_is(z_map(data = vals24,
                        well = wells24,
                        plate = 24),
                        'ggplot')

        expect_is(z_map(data = vals48,
                        well = wells48,
                        plate = 48),
                        'ggplot')

        expect_is(z_map(data = vals96,
                        well = wells96,
                        plate = 96),
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



### separate tests for PC functions

test_that("PC functions return ggplot object", {
    ## data for PC functions
    df6 <- data.frame(well = num_to_well(1:6, plate=6),
                      vals1 = rnorm(6),
                      vals2 = rnorm(6))

    df12 <- data.frame(well = num_to_well(1:12, plate=12),
                       vals1 = rnorm(12),
                       vals2 = rnorm(12))

    df24 <- data.frame(well = num_to_well(1:24, plate=24),
                       vals1 = rnorm(24),
                       vals2 = rnorm(24))

    df48 <- data.frame(well = num_to_well(1:48, plate=48),
                       vals1 = rnorm(48),
                       vals2 = rnorm(48))

    df96 <- data.frame(well = num_to_well(1:96),
                       vals1 = rnorm(1:96),
                       vals2 = rnorm(1:96))

    df384 <- data.frame(well = num_to_well(1:384, plate = 384),
                        vals1 = rnorm(1:384),
                        vals2 = rnorm(1:384))

    df1536 <- data.frame(well = num_to_well(1:1536, plate = 1536),
                        vals1 = rnorm(1:1536),
                        vals2 = rnorm(1:1536))


    expect_is(pc_map(data = df6[, 2:3],
                     well = df6$well,
                     plate = 6),
              'ggplot')

    expect_is(pc_map(data = df12[, 2:3],
                     well = df12$well,
                     plate = 12),
              'ggplot')

    expect_is(pc_map(data = df24[, 2:3],
                     well = df24$well,
                     plate = 24),
              'ggplot')

    expect_is(pc_map(data = df48[, 2:3],
                     well = df48$well,
                     plate = 48),
              'ggplot')


    expect_is(pchit_map(data = df96[, 2:3],
                        well = df96$well,
                        plate = 96),
              'ggplot')


    expect_is(pc_map(data = df384[, 2:3],
                     well = df384$well,
                     plate = 384),
              'ggplot')

    expect_is(pchit_map(data = df384[, 2:3],
                        well = df384$well,
                        plate = 384),
              'ggplot')

    expect_is(pc_map(data = df1536[, 2:3],
                     well = df1536$well,
                     plate = 1536),
              'ggplot')

    expect_is(pchit_map(data = df1536[, 2:3],
                        well = df1536$well,
                        plate = 1536),
              'ggplot')


})
