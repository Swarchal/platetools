#' Plots multiple platemaps as a heatmap of the first principal component.
#'
#' Converts multivariate data and well labels into a heatmap of the first
#' principal component in the form of a grid of platemaps.
#'
#' @param data Numerical values be transformed, scaled and plotted as a colour
#' @param well Vector of well identifiers e.g "A01"
#' @param plate_id Vector of plate labels or identifiers e.g "plate_1"
#' @param ncols Number of columns to plot multiple platemaps
#' @param plate Number of wells in complete plate (96, 384 or 1536)
#' @param ... additional arguments to be passed to z_grid
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
#' pc_grid(data = df[, 3:4],
#'     well = df$well,
#'     plate_id = df$plate,
#'     plate = 96)


pc_grid <- function(data, well,
                    plate_id,
                    ncols = 2,
                    plate = 96,
                    ...){

      pca_data <- prcomp(data) # pca of data
      pc1 <- pca_data$x[, 1] # take first principal component

      pc_grid <- z_grid(pc1,
			well,
			plate_id,
			ncols,
			plate,
			...)

      return(pc_grid)
}
