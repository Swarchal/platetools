#' creates dataframe of row,column,data from wellID and data
#'
#' internal function
#'
#' @param data numeric data to be used as colour scale
#' @param well alpha-numeric well IDs, e.g 'A01'
#' @return dataframe
#' @export

plate_map <- function(data, well){

    platemap <- as.data.frame(well)
    names(platemap)[1] <- "well"

    # if not a 1536 well plate, we can just convert well labels to numbers
    if (!is_1536(well)){
        platemap$Row <-  as.numeric(match(toupper(substr(well, 1,1)), LETTERS))
        platemap$Column <- as.numeric(substr(well, 2, 5))
    } else {
        # if a 1536 plate cannot convert the well labels directly to row and column
        # values as we have double well ID's, i.e 'AA' isn't a number
        Row <- as.numeric(match(toupper(substr(well, 1, 1)), LETTERS))
        add_to_row <- ifelse(nchar(as.character(well)) == 4, 26, 0)
        Row <- Row + add_to_row
        platemap$Row <- Row
        platemap$Column <-  as.numeric(substr(well, nchar(as.character(well)) - 1, 5))

    }
    platemap['values'] <- data

    return(platemap)
}


#' creates dataframe of row, column, and scaled data from well IDs
#'
#' internal function
#'
#' @param data numeric data to be used as colour scale
#' @param well alpha-numeric well IDs, e.g 'A01'
#' @return dataframe
#' @export
#'
plate_map_scale <- function(data, well){
    df <- plate_map(data, well)
    df$values <- scale(df$values)
    return(df)
}

#' creates dataframe of row, column, plate_id from data regarding wellIDs
#'
#' internal function
#'
#' @param data numerical data to be used as colour scale
#' @param well alpha-numeric wellIDs, e.g 'A01'
#' @param plate_id plate identifers e.g 'plate_1'
#' @return dataframe
#' @export

plate_map_grid <- function(data, well, plate_id){
    df <- plate_map(data, well)
    df$plate_label <- plate_id
    return(df)
}

#' creates dataframe of row, column, plate_id from data regarding wellIDs
#'
#' internal function
#'
#' @param data numerical data to be used as colour scale
#' @param well alpha-numeric wellIDs, e.g 'A01'
#' @param plate_id plate identifers e.g 'plate_1'
#' @param each boolean, if true scales each plate individually, if false will
#'     scale the pooled values of \code{data}
#' @return dataframe
#' @export

plate_map_grid_scale <- function(data, well, plate_id, each){
    df <- plate_map_grid(data, well, plate_id)
    if (each == FALSE) {
        df$values <- scale(df$values)
    } else if (each == TRUE) {
        split_df <- split(df, df$plate_label)
        df <- do.call(
            rbind,
            Map(function(x) {scale(x$values) -> x$values; x}, split_df)
        )
    }
    return(df)
}



#' row, column for multiple features
#'
#' Generates a dataframe for multiple features, given a wellID column and multiple
#' features
#'
#' @param data vector or dataframe of numeric data
#' @param well vector of alphanumeric well IDs e.g 'A01'

plate_map_multiple <- function(data, well){
    platemap <- as.data.frame(well)
    platemap$Row <- as.numeric(match(toupper(substr(well, 1, 1)), LETTERS))
    platemap$Column <- as.numeric(substr(well, 2, 5))
    platemap_out <- data.frame(platemap, data)
    return(platemap_out)
}


#' internal 1536 plate function for plate_map
#'
#' @param well vector of alphanumeric well labels

is_1536 <- function(well){
  # check if contains double character well labels
  two_letters <- do.call(paste0,expand.grid(LETTERS,LETTERS))
  any(grepl(paste(two_letters, collapse = '|'), well, ignore.case = TRUE))
}
