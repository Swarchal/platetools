#' Plots a heatmap of b-score normalised values in a plate layout
#'
#' Transforms numerical values using the b-score normalisation process to account
#' for row and column effects. Uses well labels to plot the normalised values in
#' the form of a microtitre plate. Works for 6, 12, 24, 48, 96, 384 or 1536 well plates
#'
#' @param data Numerical values in the form of a vector to be normalised
#' @param well Vector of well identifiers, e.g "A01"
#' @param plate integer, 6, 12, 24, 48, 96, 384 or 1536
#' @param normalise Boolean, if TRUE then the residual values will be divded by
#'                 the plate median absolute deviation as per Malo et al.
#' @param eps real number greater than 0. A tolerance for divergence
#' @param maxiter int, the maximum number of iterations
#' @param trace.iter Boolean, should progress in convergence be reported?
#' @param na.rm Boolean, should missing values be removed?
#' @param ... additional parameters to plot wrappers
#' @return ggplot plot
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' df <- data.frame(well = num_to_well(1:96),
#' vals = rnorm(96))
#'
#' b_map(data = df$vals,
#'      well = df$well,
#'      plate = 96)
#'
#' df_384 <- data.frame(
#'          well = num_to_well(1:384, plate = 384),
#'          vals = rnorm(384))
#'
#' b_map(data = df_384$vals,
#'      well = df_384$well,
#'      plate = 384)


 b_map <- function(data, well, normalise = FALSE, plate = 96, eps = 0.01,
                   maxiter = 10, trace.iter = FALSE, na.rm = TRUE, ...) {

    stopifnot(is.vector(data))

    # need to transform columns of wellID and data into
    # matrix corresponding to well positions:
    platemap <- plate_map(data, well)
    # ensure data is ordered properly before passing to matrix()
    platemap <- platemap[order(platemap$Row, platemap$Column), ]

    check_plate_input(well, plate)

    df <- med_smooth(platemap = platemap, plate = plate, eps = eps,
                     maxiter = maxiter, trace.iter = trace.iter, na.rm = na.rm)

    if (normalise) {
        # divide by the plate median absolute deivation
        df$residual <- df$residual / stats::mad(df$residual)
    }

    df$values <- scale(df$residual)
    platemap <- plate_map(df$values, df$well)

    # produce a plate map in ggplot (96-well format)
    if (plate == 6L) {
        plt <- plt6(platemap, ...) + theme_bw()
    } else if (plate == 12L) {
        plt <- plt12(platemap, ...) + theme_bw()
    } else if (plate == 24L) {
        plt <- plt24(platemap, ...) + theme_bw()
    } else if (plate == 48L) {
        plt <- plt48(platemap, ...) + theme_bw()
    } else if (plate == 96L){
        plt <- plt96(platemap, ...) + theme_bw()
    } else if (plate == 384L) {
        plt <- plt384(platemap, ...) + theme_bw()
    } else if (plate == 1536L) {
        plt <- plt1536(platemap, ...) + theme_bw()
    } else {
        stop("Invalid argument for `plate`. \nOptions: 6, 12, 24, 48, 96, 384 or 1536.",
             call. = FALSE)
    }

    return(plt)
}
