#' Plots a heatmap identifying hits from the first principal component
#'
#' Converts numerical values  and plate labels intoa plate heatmap
#' with z-scored principal components coloured dependent on a specified
#' threshold of standard deviations above or below the average.
#'
#' @param data Numerical values, either a dataframe or a matrix
#' @param well Vector of well identifers e.g "A01"
#' @param plate Number of wells in complete plate (96, 384 or 1536)
#' @param threshold Threshold of +/- standard deviations form the average
#'     to determine a hit
#' @param palette RColorBrewer palette
#'
#' @return ggplot plot
#'
#' @import ggplot2
#' @import dplyr
#' @import RColorBrewer
#'
#' @export
#'
#' @examples
#' v1 <- rnorm(1:96)
#' v2 <- rnorm(1:96)
#' v3 <- rnorm(1:96)
#' wells <- num_to_well(1:96)
#' df <- data.frame(wells, v1, v2, v3)
#'
#'
#' pchit_map(data = df[, 2:4],
#'           well = df$wells,
#'           threshold = 1.5)


pchit_map <- function(data, well,
                      plate = 96,
                      threshold = 2,
                      palette = "Spectral"){

    pca_data <- prcomp(data) # pca of data
    pc1 <- pca_data$x[,1] # take first principal component

    pc_hit_map <- hit_map(
        pc1,
        well,
        plate,
        threshold,
        palette)

    return(pc_hit_map)

}
