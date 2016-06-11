#' Converts numbers to well labels
#' 
#' Converts numerical values to corresponding alpha-numeric well labels
#' for 96, 384 or 1536 well plates.
#' 
#' @param x Vector of numbers to be converted
#' @param plate Number of wells in complete plate (96 or 384)
#' 
#' @return Vector of alpha-numeric well labels
#'
#' @export
#'  
#' @examples
#' num_to_well(1:96)
#' num_to_well(1:96, plate = 384)
#' 
#' nums <- c(1:10, 20:40, 60:96)
#' num_to_well(nums)

num_to_well <- function(x, plate = 96){
    
    stopifnot(is.numeric(x))

    if (plate == 96L){
	rows <- LETTERS[1:8]
	columns <- 1:12
    } else if (plate == 384L){
	rows <- LETTERS[1:16]
	columns <- 1:24 
    } else if (plate == 1536L){
	first_26 <- LETTERS[1:26]
	last_6 <- vector(length = 6)
	for (i in 1:6){
	    last_6[i] <- paste(LETTERS[rep(i, 2)],
		    collapse = "")
	}
	rows <- c(first_26, last_6)
	columns <- 1:48
    } else stop("Plate needs to be 96, 384 or 1536")
    
    # columns then rows for normal row-wise counting  
    combinations <-  expand.grid(columns, rows)
    # but then have to reverse order to print in the normal way
    out <- paste0(combinations[x, 2], formatC(combinations[x, 1], width = 2, flag = "0"))
    return(out)
}
