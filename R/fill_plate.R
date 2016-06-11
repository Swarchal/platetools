#' Fill in missing wells
#' 
#' Fills in missing wells with rows of NA values. Useful for any functions 
#' that require a complete plate such as `b_score`.
#' 
#' @param df dataframe
#' @param plate Number of wells in complete plate (96, 384 or 1536)
#' @param well Column containing well identifiers i.e "A01"
#' 
#' @importFrom plyr rbind.fill
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

fill_plate <- function(df, well, plate = 96){
    
    # useful 'not in' function
    '%!in%' <- function(x,y)!('%in%'(x,y))
    
    # TODO: if passed the column name that's not a string,
    # should be able to subset anyway
    if (!is.character(well)){
        stop("'well' should be a string of the column name",
             call. = FALSE)
    }
    
    # check inputs
    if (!is.data.frame(df)){
        stop("'df' needs to be a dataframe", call. = FALSE)
    }
    
    # check plate is a valid plate format
    accepted_plates <- c(96L, 384L, 1536L)
    if (plate %in% accepted_plates == FALSE){
        stop("'plate' needs to be either 96, 384 or 1536", call. = FALSE)
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
    
    missing_df <- data.frame(missing_wells) # dataframe of just missing wells
    names(missing_df) <- eval(substitute(well)) # name column after original well column
    filled_df <- rbind.fill(df, missing_df) # rbind, fill rows with NAs
    return(filled_df)
    
}
