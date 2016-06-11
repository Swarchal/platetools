#' 2 way median polish
#' 
#' 2 way median polish to remove plate effects such as row/column/edge effects.
#' Given a dataframe containing alpha-numeric wellIDs and numerical values,
#' this \code{b_score} will return a dataframe of the same structure after
#' a two-way median smooth.
#' 
#' @param data numeric data, either a vector or dataframe column
#' @param well alpha-numeric wellIDs. e.g 'A01'
#' @param plate numeric, number of wells within a plate, either 96 or 384
#' 
#' @export
#' 
#' @examples
#' df <- data.frame(well = num_to_well(1:96),
#'                  vals = rnorm(96))
#' 
#' b_score(data = df$vals,
#'         well = df$well,
#'         plate = 96)

b_score <- function(data, well, plate){

    platemap <- plate_map(data, well)

    med_smooth(platemap, plate)
}