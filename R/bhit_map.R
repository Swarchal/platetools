#' Platemap to identify 'hits' following a B-score normalisation
#'
#' Produces a platemap with colours indicating wells above or below selected threshold
#' after normalising for systematic plate effects via B-score smooth. The threshold is
#' definined calculated from a z-score, i.e plus or minus standard deviations from the
#' plate mean.
#'
#' @param data Vector of numerical values
#' @param well Vector of well identifiers, e.g "A01"
#' @param plate Number of wells in whole plate (96, 384 or 1536)
#' @param threshold Standard deviations from the plate average to indicate a hit.
#'      default is set to +/- 2 SD.
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
#' df <- data.frame(vals = rnorm(384),
#'    well = num_to_well(1:384, plate = 384))
#'
#' bhit_map(data = df$vals,
#'    well = df$well,
#'    plate = 384,
#'    threshold = 3)

bhit_map <- function(data, well,
                     plate = 96,
                     threshold = 2,
                     palette = "Spectral"){

    # need to transform columns of wellID and data into
    # matrix corresponding to well positions:
    platemap <- plate_map(data, well)

    # ensure data is ordered properly before passing to matrix()
    platemap <- platemap[order(platemap$Row, platemap$Column), ]

    if (length(well) > plate){
        warning("Invalid plate selection. The data given has more rows then number of wells. \nAre you sure argument 'plate' is correct for the number of wells in your data? \nnote: Default is a 96-well plate.",
                call. = FALSE)
    }
    if (plate > 2 * length(well)){
        warning("Invalid plate selection. The data given has more rows then number of wells. \nAre you sure argument 'plate' is correct for the number of wells in your data? \nnote: Default is a 96-well plate.",
                call. = FALSE)
    }
    if (plate == 96L){
        # transform into 12*8 matrix (96-well plate)
        # fills matrix in a row-wise fashion i.e, A01, A02 ...
        mat_plate_map <- matrix(data,
                                nrow = 8,
                                ncol = 12,
                                byrow = TRUE)
    } else if (plate == 384L){
        # transform into 24*16 matrix (384-well plate)
        # fills matrix in a row-wise fashion, i.e A01, A02 ...
        mat_plate_map <- matrix(data,
                                nrow = 16,
                                ncol = 24,
                                byrow = TRUE)
    } else if (plate == 1536L){
	mat_plate_map <- matrix(data,
				nrow = 32,
				ncol = 24,
				byrow = TRUE)
    } else{
        stop("Not a plate format. \nArgument 'plate' should be 96, 384 or 1536.",
             call. = FALSE)
    }

    # median polish of the data
    data_pol <- medpolish(mat_plate_map,
                          na.rm = TRUE)

    # transpose of residual matrix (as counts in column-wise fashion)
    # now well numbers correspond i.e t_out[12] = A12, t_out[13] = B01
    t_out <- t(data_pol$residuals)

    # 1:96 elements of residuals corresponding to 1:96 of num_to_well
    # produce dataframe of two columns
    df <- NULL

    for (num in 1:length(t_out)){
        df$residual[num] <- t_out[num]
        df$well[num] <- num_to_well(num, plate = plate)
    }

    df <- as.data.frame(
        cbind("well" = df$well,
              "residual" = df$residual))
    # change residuals from factor to numeric
    df$residual <- as.numeric(as.character(df$residual))

    platemap$values <- scale(df$residual)
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

    if (plate == 96){
        # produce a 96-well plate map layout in ggplot
        plt <- plt96(platemap) +
            scale_fill_manual("hit", values = my_colours) +
            theme_bw()
    } else if (plate == 384){
        # produce a 384-well plate map layout in ggplot
        plt <- plt384(platemap) +

            scale_fill_manual("hit", values = my_colours) +
            theme_bw()
    } else if (plate == 1536L){
	plt <- plt1536(platemap) + 
	    scale_fill_manual("hit", values = my_colours) +
	    theme_bw()
    } else stop("Not a valid plate format. Either 96 or 384.", call. = FALSE)

    if (length(well) > plate) {
        stop("Invalid plate selection. The data given has more rows than the number of wells. \nAre you sure argument 'plate' is correct for the number of wells in your data? \nnote: Default is set to a 96-well plate.")
    }

    return(plt)
}
