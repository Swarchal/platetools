#' Fill in missing wells
#'
#' Fills in missing wells with rows of NA values. Useful for any functions
#' that require a complete plate such as `b_score`.
#'
#' @param df dataframe
#' @param plate Number of wells in complete plate (96, 384 or 1536)
#' @param well Column containing well identifiers i.e "A01"
#'
#' @return dataframe
#'
#' @export
#'
#' @examples
#' vals <- rnorm(96) ; wells <- num_to_well(1:96)
#' df <- data.frame(wells, vals)
#' df_missing <- df[-c(1:10), ]
#' fill_plate(df_missing, "wells")

fill_plate <- function(df, well, plate = 96) {


    # TODO: if passed the column name that's not a string,
    # should be able to subset anyway
    if (!is.character(well)) {
        stop("'well' should be a string of the column name", call. = FALSE)
    }

    # check inputs
    if (!is.data.frame(df)) {
        stop("'df' needs to be a dataframe", call. = FALSE)
    }

    # check plate is a valid plate format
    accepted_plates <- c(6L, 12L, 24L, 48L, 96L, 384L, 1536L)
    if (plate %in% accepted_plates == FALSE) {
        stop("'plate' needs to be either 6, 12, 24, 48, 96, 384 or 1536", call. = FALSE)
    }

    well_col <- df[, well]

    # all wells in a complete plate
    complete_plate <- num_to_well(1:plate, plate=plate)

    # wells in complete plate that are not in 'well'
    missing_indices <- which(complete_plate %!in% well_col)
    missing_wells <- complete_plate[missing_indices]

    missing_df <- data.frame(missing_wells) # dataframe of just missing wells
    names(missing_df) <- eval(substitute(well)) # name column after original well column
    filled_df <- rbind_fill(df, missing_df) # rbind, fill rows with NAs
    return(filled_df)
}


# useful 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))


rbind_fill <- function(x, y) {
    # fill in missing column in y with NA values
    if (nrow(y) == 0) {
        return(x)
    }
    x_names <- colnames(x)
    y_names <- colnames(y)
    for (col_name in x_names) {
        # if column not found in y, then add it as all NAs
        if (col_name %!in% y_names) {
            y[, col_name] <- NA
        }
    }
    # now the two dataframes should be able to be rbind together
    rbind(x, y)
}
