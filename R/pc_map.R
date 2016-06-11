#' Principal component heatmap in a plate layout
#'
#' Takes the values and well identifiers, calculates the first principal
#' component, scales and plots the component as a heatmap in the form of
#' a 96 or 384-well plate. A way to quickly show variation of
#' multi-parametric data within a plate.
#'
#' @param data Vector of numerical data to calculate the first principal component
#' @param well Vector of well identifiers e.g "A01"
#' @param plate Number of wells in complete plate (96, 384 or 1536
#'
#' @return gplot plot
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   well = num_to_well(1:96),
#'   vals1 = rnorm(1:96),
#'   vals2 = rnorm(1:96))
#'
#' pc_map(data = df[, 2:3],
#'        well = df$well,
#'        plate = 384)

pc_map <- function(data, well,
    plate = 96){

    pca_data <- prcomp(data) # pca of data
    pc1 <- pca_data$x[,1] # take first principal component

    plot_pc_map <- z_map(pc1, well, plate)
    return(plot_pc_map)

}
