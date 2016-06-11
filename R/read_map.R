#' Annotates dataframe with metadata in a platemap matrix
#'
#' Annotates a dataframe containined well identifiers with metadata in the
#' form of a platemap matrix, matching the existing well-labels to the well
#' position in the platemap
#'
#' @param data existing daatframe, with wellIDs under the column name of 'well'
#' @param map Matrix of metadata to be added to the dataframe, N.B NO MISSING WELLS!
#' @param verbose Boolean, if TRUE will add row and column numbers to dataframe
#' @param new_col_name What to call the added metadata
#'
#' @import dplyr
#' @importFrom utils read.csv
#'
#' @export
#'
#' @return dataframe with new column named after `new_col_name`


read_map <- function (data, map,
                      verbose = TRUE,
                      new_col_name = "header"){

    if (is.character(map) == TRUE) {
	map <- as.matrix(read.csv(map, header = FALSE))
    }
    map <- as.matrix(map)
    platemap <- mutate(data,
	row = as.numeric(match(toupper(substr(data$well,1, 1)), LETTERS)),
        column = as.numeric(substr(data$well, 2, 5)))

    platemap$header <- NULL

    for (i in 1:nrow(data)) {
	platemap$header[i] <- as.vector(with(platemap,
                                         map[[row[i],column[i]]]))
    }

    names(platemap)[ncol(platemap)] <- new_col_name

    if (verbose == FALSE) {
	platemap$row <- NULL
	platemap$column <- NULL
	return(platemap)
    } else if (verbose == TRUE) {
	return(platemap)
    } else stop("Argument 'verbose' requires a boolean.", call. = FALSE)
}
