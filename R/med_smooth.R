#' 2-way median smooth
#'
#' Given a platemap produced by \code{plate_map}, will return
#' a dataframe with after values have been transformed into
#' a matrix mirroring the plate structure and undergoing a
#' 2-way median polish to remove row or column effects
#'
#' @param platemap dataframe produced by \code{plate_map}
#' @param plate numeric, number of wells in plate, either 96 or 384
#'
#' @return A dataframe consisting of two column, wellID and
#'         polished numeric values
#'
#' @importFrom stats medpolish
#'
#' @export


med_smooth <- function(platemap, plate){

    if (plate == 96L){
        # transform into 12*8 matrix (96-well plate)
        # fills matrix in a row-wise fashion i.e, A01, A02 ...
        mat_plate_map <- matrix(platemap$values,
                                nrow = 8,
                                ncol = 12,
                                byrow = TRUE)
    } else if (plate == 384L){
        # transform into 24*16 matrix (384-well plate)
        # fills matrix in a row-wise fashion, i.e A01, A02 ...
        mat_plate_map <- matrix(platemap$values,
                                nrow = 16,
                                ncol = 24,
                                byrow = TRUE)
    } else if (plate == 1536){
        mat_plate_map <- matrix(platemap$values,
                                nrow = 32,
			                    ncol = 48,
			                    byrow = TRUE)
    } else {
        stop("Not a plate format. \nArgument 'plate', should be 96 or 384.",
             call. = FALSE)
    }

    # median polish of the data
    data_pol <- medpolish(mat_plate_map,
                          na.rm = TRUE,
                          trace.iter = FALSE)

    # transpose of residual matrix (as counts in column-wise fashion)
    # now well numbers correspond i.e t_out[12] = A12, t_out[13] = B01
    t_out <- t(data_pol$residuals)

    # elements of residuals corresponding to elements of num_to_well
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

    return(df)

}

#' Two way-median smooth on a plate map
#'
#' Given a platemap produced by \code{plate_map}, this will perform
#' a two way median smooth, and return the results of \code{medpolish}.
#' Useful for row and column effects, as well as the raw residuals.
#'
#' @param platemap platemap produced by \code{plate_map}
#' @param plate, integer either 96 or 384
#' @export


plate_effect <- function(platemap, plate){
    if (plate == 96L){
	mat_plate_map <- matrix(platemap$values,
				nrow = 8,
				ncol = 12,
				byrow = TRUE)
    } else if (plate == 384L){
	mat_plate_map <- matrix(platemap$values,
				nrow = 16,
				ncol = 24,
				byrow = TRUE)
    } else if (plate == 1536L) {
	mat_plate_map <- matrix(platemap$values,
				nrow = 32,
				ncol = 48,
				byrow = TRUE)
    } else {
	stop("Not a plate format")
    }
    data_pol <- medpolish(mat_plate_map,
			   na.rm = TRUE,
			   trace.iter = FALSE)

    return(data_pol)
}
