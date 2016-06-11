#' Plots multiple platemaps with heatmap of raw values
#' 
#' Converts numerical values. well labels, and plate labels into multiple 
#' plate heatmaps
#' 
#' @param data Numerical values to be plotted
#' @param well Vector of well identifiers e.g "A01"
#' @param plate_id Vector of plate identifiers e.g "Plate_1"
#' @param ncols Number of columns to display multiple heatmaps
#' @param plate Number of wells in complete plate (96, 384 or 1536)
#' 
#' @return ggplot plot
#'
#' @import dplyr
#' @import ggplot2
#' @import RColorBrewer
#
#' @export
#'
#' @examples
#' df01 <- data.frame(well = num_to_well(1:96),
#'   vals = rnorm(96),
#'   plate = 1)
#' 
#' df02 <- data.frame(well = num_to_well(1:96),
#'   vals = rnorm(96),
#'   plate = 2)
#' 
#' df <- rbind(df01, df02)
#' 
#' raw_grid(data = df$vals,
#'     well = df$well,
#'     plate_id = df$plate,
#'     plate = 96)


raw_grid <- function(data, well,
                     plate_id,
                     ncols = 2,
                     plate = 96){
  
    ## multiple platemap plots in a single figure using facet_wrap

    stopifnot(is.vector(data))

    # transform well labels into row-column values for a 96-well plate
    # need to include plate_id labels into this dataframe
    platemap <- plate_map_grid(data, well, plate_id)
  
    if (plate == 96) {
	# produce a plate map in ggplot (96-well format)
	plt<- plt96(platemap) +
	    theme_bw() + 
	    theme(panel.margin.x = unit(1, "lines"), 
	    panel.margin.y = unit(0.5, "lines")) + # increase spacing between facets
	    facet_wrap(~plate_label,
			ncol = ncols,
			scales = 'free')

    } else if (plate == 384L){
	# produce a plate map in ggplot (384-well format)
	plt <- plt384(platemap) +
	    theme_bw() +
	    theme(panel.margin.x = unit(1, "lines"), 
	    panel.margin.y = unit(0.5, "lines")) + # increase spacing between facets
	    facet_wrap(~plate_label,
			ncol = ncols,
			scales = 'free')
	
    } else if (plate == 1536L){
	plt <- plt1536(platemap) +
	    theme_bw() +
	    theme(panel.margin.x = unit(1, "lines"), 
	    panel.margin.y = unit(0.5, "lines")) + # increase spacing between facets
	    facet_wrap(~plate_label,
			ncol = ncols,
			scales = 'free')
	
    } else stop("Invalid argument for 'plate'. \nOption: 96 or 384.",
            call. = FALSE)

    return(plt)
  
}
