#' Plots multiple b-scored normalised platemaps
#'
#' Transforms numerical values using the b-score normalisation process to
#' account for row and column effects. Uses well and plate labels to plot the
#' normalised values in the form of microtitre plates. Works for 96, 384 and
#' 1536 well plates.
#'
#' @param data Numerical values to be plotted
#' @param well Vector of well identifiers e.g "A01"
#' @param plate Number of wells in complete plate (96, 384 or 1536)
#' @param plate_id Vector of plate identifiers e.g "Plate_1"
#' @return ggplot plot
#'
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#' df01 <- data.frame(well = num_to_well(1:96),
#'   vals = rnorm(96),
#'   plate = 1)
#'
#' df02 <- data.frame(well = num_to_well(1:96),
#'   vals = rnorm(96),
#'   plate = 2)
#'
#' df <- rbind(df01, df02)
#'
#' b_grid(data = df$vals,
#'     well = df$well,
#'     plate_id = df$plate,
#'     plate = 96)

b_grid <- function(data, well, plate_id, plate = 96) {

    stopifnot(is.vector(data))

    # need to group_by plate_id, median polish, then return data.frame
    # that can be passed to ggplot and use raw_grid

    platemap <- plate_map_grid(data, well, plate_id)

    # force to factor
    platemap$plate_label <- as.factor(platemap$plate_label)

    # split by plate_id
    platemap_split <- split(platemap, platemap$plate_label)

    # apply med_smooth to each dataframe, split by plate_id
    med_smooth_list <- lapply(platemap_split, function(x){
        med_smooth(x, plate = plate)
    })

    # list to dataframe
    med_smooth_df <- list_to_dataframe(med_smooth_list,
                                       col_name = "plate_label")

    raw_grid(data = med_smooth_df$residual,
             well = med_smooth_df$well,
             plate_id = med_smooth_df$plate_label)
}


#' Converts list to a dataframe in a sensible way
#'
#' Given a list of dataframes with the same columns, this function will row bind
#' them together, and if passed a \code{col_name} arguement, will produce a
#' column containing their original element name
#'
#' @param l list of dataframes to be converted into single dataframe
#' @param col_name (optional) name of column to put element names under
#'
#' @return dataframe
#'


list_to_dataframe <- function(l, col_name = NULL){

  # check l is a list
  if (!is.list(l)) stop(paste(l , "needs to be a list"))

  # if col_name is a string, will create a new column from the element names
  # within the list
  if (!is.null(col_name)){
    # create column from list name
    for (name in names(l)){
      l[[name]][col_name] <- name
    }
  }

  # create data frame from list
  out_df <- do.call(rbind, l)
  rownames(out_df) <- NULL
  return(out_df)
}
