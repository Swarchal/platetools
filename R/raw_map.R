#' Plots a platemap with heatmap of raw values
#'
#' Converts numerical values and  well labels into multiple plate heatmaps
#'
#' @param data Numerical values to be plotted
#' @param well Vector of well identifiers e.g "A01"
#' @param plate Number of wells in complete plate (96, 384 or 1536)
#' @param ... additional parameters to plot wrappers
#'
#' @import ggplot2
#'
#' @return ggplot plot
#'
#' @export
#'
#' @examples
#' df <- data.frame(vals = rnorm(1:384),
#'   well = num_to_well(1:384, plate = 384))
#'
#' raw_map(data = df$vals,
#'         well = df$well,
#'         plate = 384)

raw_map <- function(data, well, plate = 96, ...) {

    check_plate_input(well, plate)

    # transform well labels into row-column values
    platemap <- plate_map(data, well)

    if (plate == 96){
    plt <- plt96(platemap, ...) + theme_bw()

    } else if (plate == 384){
    plt <- plt384(platemap, ...) + theme_bw()

    } else if (plate == 1536L){
    plt <- plt1536(platemap, ...) + theme_bw()

    } else{
    stop("Invalid argument for 'plate'. \nOption: 96, 384 or 1546",
        call. = FALSE)
    }

    return(plt)

}
