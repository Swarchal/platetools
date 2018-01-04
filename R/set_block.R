#' Set values in rectangular areas of a plate
#'
#' Updates a table representing a multiwell plate, by setting a given value
#' for all wells in a block or a list of blocks defined by the well coordinates
#' of their upper-left and bottom-right corners.
#'
#' This function is tested with a table in \code{\link{data.frame}} format,
#' but it should also work with the \code{\link[S4Vectors]{DataFrame}},
#' \code{\link[data.table]{data.table}} and
#' \code{\link[tibble]{tibble}} formats.
#'
#' @param plate A table representing a multiwell plate, with one column
#'        named \dQuote{well} representing the well identifiers.
#' @param block Coordinates of a rectangular block (such as \dQuote{A01~B02}),
#'        or a vector of coordinates.
#' @param what A column name in the table.
#' @param value The value to set.
#'
#' @return Returns the \sQuote{\code{plate}} table, where the values for
#' the wells indicated in the blocks have been updated.
#'
#' @examples
#' p <- data.frame(well = num_to_well(1:96))
#' head(p)
#'
#' p <- set_block(p, c("A01~B02", "A05~D05"), "dNTP", 0.25)
#' p <- set_block(p,   "A03",                 "dNTP", 0.50)
#' head(p)
#'
#' # Be careful with the column names
#' p <- set_block(p, "A01~H12", "Mg2+", 3.0)
#' head(p)
#'
#'\dontrun{
#' # Chained updates with magrittr
#' p %<>%
#'   setBlock("A01~C04", "dNTP", 0.5) %>%
#'   setBlock("A01~C04", "Mg",   3.0)
#' }
#'
#' @seealso \code{\link{num_to_well}}
#'
#' @author Charles Plessy
#'
#' @export

set_block <- function(plate, block, what, value) {

    if (! "well" %in% colnames(plate))
        stop(dQuote("plate"), " data frame misses ", dQuote("well"), " column containing well IDs.")

    if (what != make.names(what))
        warning("Column name not syntactically correct.  See ", dQuote("?make.names"), ".")

    # First, define a function that works on a single block.
    set_block_ <- function(plate, block) {
        plateRows  <- gsub("[[:digit:]]+", "", plate$well) %>% factor
        plateCols  <- gsub("[[:alpha:]]+", "", plate$well) %>% as.numeric %>% factor
        startWell  <- sub("~.*", "", block)
        endWell    <- sub(".*~", "", block)
        startRow   <- substr(startWell, 1,1)
        endRow     <- substr(endWell,   1,1)
        startCol   <- substr(startWell, 2,3) %>% as.numeric
        endCol     <- substr(endWell,   2,3) %>% as.numeric
        targetRows <- LETTERS[seq(which(LETTERS == startRow), which(LETTERS == endRow))]
        targetCols <- seq(startCol, endCol)
        plate[plateRows %in% targetRows & plateCols %in% targetCols, what] <- value
        plate
    }

    # Then, apply it to every blocks supplied.
    Reduce(set_block_, block, plate)
}
