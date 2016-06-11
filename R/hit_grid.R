#' Plots multiple platemaps with and identifies hits
#'
#' Converts numerical values and well labels into 'hits' in the form of
#' multiple plate maps. Hits are calculated as wells above or below a
#' specified number of standard deviations from the overall average
#'
#' @param data Numerical values to be scaled and plotted
#' @param well Vector of well identifiers. e.g "A01"
#' @param plate_id Vector of plate identifiers e.g "Plate_1"
#' @param threshold Numerical value of standard deviations from the mean
#'  for a well to be classified as a 'hit'. Default it +/- 2 SD
#' @param ncols Number of columns in the grid of plates
#' @param plate Number of wells in the complete plates (96, 384 or 1536)
#' @param each boolean, if true scales each plate individually, if false will
#'     scale the pooled values of \code{data}
#' @param palette RColorBrewer palette
#'
#' @return ggplot plot
#'
#' @import dplyr
#' @import ggplot2
#' @import RColorBrewer
#'
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
#' hit_grid(data = df$vals,
#'     well = df$well,
#'     plate_id = df$plate,
#'     plate = 96,
#'     each = FALSE)


hit_grid <- function(data, well,
                   plate_id,
                   threshold = 2,
                   ncols = 2,
                   plate = 96,
                   each = FALSE,
                   palette = "Spectral"){

    # normalised across entire range of values
    platemap <- plate_map_grid_scale(data, well, plate_id, each)
    platemap$hit <- NA

    # calculate whether values are beyond the threshold; defined as hit or null
    for (row in 1:nrow(platemap)){
        if (platemap[row, 'values'] > threshold){platemap$hit[row] <- "hit"
        } else  if (platemap[row, 'values'] < (-1 * threshold)){platemap$hit[row] <- "neg_hit"
        } else {platemap$hit[row] <- "null"}
    }

    # change name of hit to values
    # plt96 and plt384 are colouring points by value, in this case needs to be hit
    platemap$actual_vales <- platemap$values
    platemap$values <- platemap$hit

    # RColorBrewerPallette
    my_cols <- brewer.pal(3, palette)
    my_colours <- c(hit = my_cols[1], neg_hit = my_cols[3], null = my_cols[2])


    if (plate == 96L){
      # produce a 96-well plate map layout in ggplot
      plt <- plt96(platemap) +
          scale_fill_manual("hit", values = my_colours) +
          theme_bw() +
          theme(panel.margin.x = unit(1, "lines"),
          panel.margin.y = unit(0.5, "lines")) + # increase spacing between facets
          facet_wrap(~plate_label,
                     ncol = ncols,
                     scales = 'free')
      } else if (plate == 384L){
      # produce a 384-well plate map layout in ggplot
      plt <- plt384(platemap) +
          scale_fill_manual("hit", values = my_colours) +
          theme_bw() +
          theme(panel.margin.x = unit(1, "lines"),
          panel.margin.y = unit(0.5, "lines")) + # increase spacing between facets
          facet_wrap(~plate_label,
                     ncol = ncols,
                     scales = 'free')
    } else if (plate == 1536L){
	plt <- plt1536(platemap) + 
	    scale_fill_manual("hit", values = my_colours) +
	    theme_bw() +
	    theme(panel.margin.x = unit(1, "lines"),
	    panel.margin.y = unit(0.5, "lines")) # increase spacing between facets
	    facet_wrap(~plate_label,
			ncol = ncols,
			scales = "free")
    } else stop("Not a valid plate format. Enter either 96, 384 or 1536.", call. = FALSE)

  return(plt)
}
