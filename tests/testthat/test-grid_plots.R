context("basic tests for grid plot functions")

# test data set
 df96_1 <- data.frame(well = num_to_well(1:96),
   vals = rnorm(96),
   plate = 1)
 df96_2 <- data.frame(well = num_to_well(1:96),
   vals = rnorm(96),
   plate = 2)
 df96 <- rbind(df96_1, df96_2)


 df384_1 <- data.frame(well = num_to_well(1:384, plate = 384),
   vals = rnorm(384),
   plate = 1)
 df384_2 <- data.frame(well = num_to_well(1:384, plate = 384),
   vals = rnorm(384),
   plate = 2)
 df384 <- rbind(df384_1, df384_2)


 df1536_1 <- data.frame(well = num_to_well(1:1536, plate = 1536),
   vals = rnorm(384),
   plate = 1)
 df1536_2 <- data.frame(well = num_to_well(1:1536, plate = 1536),
   vals = rnorm(1536),
   plate = 2)
 df1536 <- rbind(df1536_1, df1536_2)


test_that("return ggplot object", {

     expect_is(b_grid(data = df96$vals,
         well = df96$well,
         plate_id = df96$plate,
         plate = 96),
         'ggplot')


     expect_is(hit_grid(data = df96$vals,
         well = df96$well,
         plate_id = df96$plate,
         plate = 96),
         'ggplot')


     expect_is(raw_grid(data = df96$vals,
         well = df96$well,
         plate_id = df96$plate,
         plate = 96),
         'ggplot')


     expect_is(z_grid(data = df96$vals,
         well = df96$well,
         plate_id = df96$plate,
         plate = 96),
         'ggplot')


         ###############

     expect_is(b_grid(data = df384$vals,
         well = df384$well,
         plate_id = df384$plate,
         plate = 384),
         'ggplot')


     expect_is(hit_grid(data = df384$vals,
         well = df384$well,
         plate_id = df384$plate,
         plate = 384),
         'ggplot')


     expect_is(raw_grid(data = df384$vals,
         well = df384$well,
         plate_id = df384$plate,
         plate = 384),
         'ggplot')


     expect_is(z_grid(data = df384$vals,
         well = df384$well,
         plate_id = df384$plate,
         plate = 384),
         'ggplot')


         #################

     expect_is(b_grid(data = df1536$vals,
         well = df1536$well,
         plate_id = df1536$plate,
         plate = 1536),
         'ggplot')


     expect_is(hit_grid(data = df1536$vals,
         well = df1536$well,
         plate_id = df1536$plate,
         plate = 1536),
         'ggplot')


     expect_is(raw_grid(data = df1536$vals,
         well = df1536$well,
         plate_id = df1536$plate,
         plate = 1536),
         'ggplot')


     expect_is(z_grid(data = df1536$vals,
         well = df1536$well,
         plate_id = df1536$plate,
         plate = 1536),
         'ggplot')

})


### test for PC functions that need multiple columns

# create test data

df01 <- data.frame(well = num_to_well(1:96),
                   plate = 1,
                   vals1 = rnorm(1:96),
                   vals2 = rnorm(1:96))

df02 <- data.frame(well = num_to_well(1:96),
                   plate = 2,
                   vals1 = rnorm(1:96),
                   vals2 = rnorm(1:96))

df <- rbind(df01, df02)

out <- pc_grid(data = df[, 3:4],
               well = df$well,
               plate_id = df$plate,
               plate = 96)


out_hit <- pchit_grid(data = df[, 3:4],
                      well = df$well,
                      plate_id = df$plate,
                      plate = 96)

test_that("pc_grid returns ggplot object", {
    expect_is(out, 'ggplot')
})

test_that("pchit_grid returns ggplot object", {
    expect_is(out_hit, 'ggplot')
})