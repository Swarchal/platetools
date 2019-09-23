#' checks plate input for dodgy well plate combinations
#'
#' @param plate integer, number of wells in full plate
#' @param well vector of well labels

check_plate_input <- function(well, plate) {
    if (length(well) > plate){
        warning(paste("Warning: Invalid plate selection. The well data given (",length(well),") has more entries then number of wells in the selected plate (",plate,").\nAre you sure argument 'plate' is correct for the number of wells in your data?\nnote: Default is a 96-well plate.",
                call. = FALSE)
    }
    if (plate > 2 * length(well)){
        warning("Warning: Your well label count (",length(well),") covers less than half the selected plate(",plate,").\nAre you sure argument 'plate' is correct for the number of wells in your data?\nnote: Default is a 96-well plate.",
                call. = FALSE)
    }
}
