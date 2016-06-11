#' Returns wells that are missing from a complete plate
#' 
#' Returns a vector of wells that are missing from a complete plate.
#' 
#' @param df dataframe
#' @param plate Number of wells in complete plate (96 or 384)
#' @param well Column containing well identifiers i.e "A01"
#' 
#' @return vector of missing wells
#'
#' @export
#'
#' @examples 
#' vals <- rnorm(96) ; wells <- num_to_well(1:96)
#' df <- data.frame(vals, wells)
#' df_missing <- df[-c(1:10), ]
#' missing_wells(df_missing, "wells")

missing_wells <- function(df, well, plate = 96){
    
    # useful 'not in' function
    '%!in%' <- function(x,y)!('%in%'(x,y))
    
    # TODO: if passed the column name that's not a string,
    # should be able to subset
    if (!is.character(well)){
        stop("'well' should be a string of the column name",
             call. = FALSE)
    }
    
    # check inputs
    if (!is.data.frame(df)){
        stop("'df' needs to be a dataframe", call. = FALSE)
    }
    
    # check plate is a valid plate format
    accepted_plates <- c(96, 384)
    if (plate %in% accepted_plates == FALSE){
        stop("'plate' needs to be either 96 or 384", call. = FALSE)
    }
    
    # check that well is a column in df
    if (well %in% colnames(df) == FALSE){
        stop(paste(well, "is not a column in", substitute(df)),
             call. = FALSE)
    }
    
    well_col <- df[, well]
    
    # all wells in a complete plate
    complete_plate <- num_to_well(1:plate)
    
    # wells in complete plate that are not in 'well'
    missing_indices <- which(complete_plate %!in% well_col)
    missing_wells <- complete_plate[missing_indices]
    return(missing_wells)
}