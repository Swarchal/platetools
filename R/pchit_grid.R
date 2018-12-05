#' Plots multiple heatmaps identifying hits from the first principal component
#'
#' Converts numerical values, well labels, and plate labels into multiple heatmaps
#' of plates, with z-scored principal components coloured dependent on a specified
#' threshold of standard deviations above or below the average.
#'
#' @param data Numerical values, either a dataframe or a matrix
#' @param well Vector of well identifers e.g "A01"
#' @param plate_id Vector of plate identifiers e.g "Plate_1"
#' @param ... additional arguments to `platetools::hit_grid()`
#' @return ggplot plot
#'
#' @import ggplot2
#' @importFrom stats prcomp
#'
#' @export
#'
#' @examples
#' df01 <- data.frame(
#'   well = num_to_well(1:96),
#'   plate = 1,
#'   vals1 = rnorm(1:96),
#'   vals2 = rnorm(1:96))
#'
#' df02 <- data.frame(
#'   well = num_to_well(1:96),
#'   plate = 2,
#'   vals1 = rnorm(1:96),
#'   vals2 = rnorm(1:96))
#'
#' df <- rbind(df01, df02)
#'
#' pchit_grid(data = df[,3:4],
#'            well = df$well,
#'            plate_id = df$plate,
#'            plate = 96)

pchit_grid <- function(data, well, plate_id, ...) {
    pca_data <- prcomp(data) # pca of data
    pc1 <- pca_data$x[,1] # take first principal component
    pc_hit_grid <- hit_grid(pc1, well = well, plate_id = plate_id, ...)
    return(pc_hit_grid)
}
