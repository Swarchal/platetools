#' rotates matrix by 180 degrees
#' 
#' If someone (no names) puts in a plate upside down, this function 
#' will rotate a plate matrix produced by \code{plate_matrix} to be
#' the correct way up. I.e if A01 is in the bottom right hand corner rather
#' than the top left.
#' 
#' @param m matrix
#' @return matrix
#' @export

rotate_plate <- function(m){
    m[nrow(m):1, ncol(m):1]
}