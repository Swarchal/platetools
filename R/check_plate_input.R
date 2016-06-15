#' checks plate input for dodgy well plate combinations
#'
#' @param plate integer, number of wells in full plate
#' @param well vector of well labels

check_plate_input <- function(well, plate) {
    if (length(well) > plate){
        warning("Invalid plate selection. The data given has more rows then number of wells.\nAre you sure argument 'plate' is correct for the number of wells in your data?\nnote: Default is a 96-well plate.",
                call. = FALSE)
    }
    if (plate > 2 * length(well)){
        warning("Invalid plate selection. The data given has more rows then number of wells.\nAre you sure argument 'plate' is correct for the number of wells in your data?\nnote: Default is a 96-well plate.",
                call. = FALSE)
    }
}