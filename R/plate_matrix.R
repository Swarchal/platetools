#' plate layout matrix from well IDs
#'
#' Given a dataframe of alpha-numeric well IDs e.g ("A01"), and values,
#' this function will produce a matrix in the form of a plate layout.
#'
#' @param data vector of data to be placed in matrix
#' @param well vector of alphanumeric well IDs. e.g ("A01")
#' @param plate number of wells in plate (96 or 384)
#' 
#' @return matrix
#' 
#' @export
#' 
#' @examples 
#' a <- 1:96
#' wells <- num_to_well(1:96)
#' plate_matrix(data = a, well = wells)
#' 
#' x <- rnorm(384)
#' wells <- num_to_well(1:384, plate = 384)
#' plate_matrix(data = x, well = wells, plate = 384)


plate_matrix <- function(data, well, plate = 96){
    
    platemap <- plate_map(data, well)
    
    # fill plate incase of missing values
    filled_plate <- fill_plate(df = platemap, well = "well")
    
    # create empty matrix
    if (plate == 96L) {
        mat <- matrix(NA, nrow = 8, ncol = 12)
    } else if (plate == 384L){
        mat <- matrix(NA, nrow = 16, ncol = 24)
    } else if (plate == 1536L){
	mat <- matrix(NA, nrow = 32, ncol = 48)
    } else {
        stop("incorrect plate size -- 96, 384 or 1536 only")
    }
    
    # fill mat with values (header) from Row and Column index
    for (i in 1:nrow(filled_plate)){
        row <- filled_plate[i, 'Row']
        col <- filled_plate[i, 'Column']
        val <- filled_plate[i, 'values']
        mat[row, col] <- val
    }
    
    return(mat)
}
