#' Plots a platemap with heatmap of scaled values
#'
#' Converts numerical values and  well labels into multiple plate heatmaps
#'
#' @param data Numerical values to be plotted
#' @param well Vector of well identifiers e.g "A01"
#' @param plate Number of wells in complete plate (96, 384 or 1536))
#'
#' @return ggplot plot
#'
#' @import ggplot2
#' @import dplyr
#' @import RColorBrewer
#' @export
#'
#' @examples
#' df <- data.frame(vals = rnorm(1:384),
#'   well = num_to_well(1:384, plate = 384))
#'
#' z_map(data = df$vals,
#'       well = df$well,
#'       plate = 384)


z_map <- function(data, well,
    plate = 96){

    stopifnot(is.vector(data))


    if (length(well) > plate) {
        stop("Invalid plate selection. The data given has more rows than number of wells. \nAre you sure argument 'plate' is correct for the number of wells in your data? \nnote: Default is set to a 96-well plate.",
            call. = FALSE)
    }

    platemap <- plate_map_scale(data, well)

    if (plate == 96L){
        # produce a plate map in ggplot (96-well format)
        plt <- plt96(platemap) +
            theme_bw()

    } else if (plate == 384L){
        # produce a plate map in ggplot (384-well format)
        plt <- plt384(platemap) +
            theme_bw()

    } else if (plate == 1536L) {
	plt <- plt1536(platemap) +
	    theme_bw()

    } else stop("Not a valid plate format. Enter either 96, 384 or 1536.",
		call. = FALSE)


    return(plt)
}
