#' Platemap to identify 'hits' following a B-score normalisation
#'
#' Produces a platemap with colours indicating wells above or below selected threshold
#' after normalising for systematic plate effects via B-score smooth. The threshold is
#' definined calculated from a z-score, i.e plus or minus standard deviations from the
#' plate mean.
#'
#' @param data Vector of numerical values
#' @param well Vector of well identifiers, e.g "A01"
#' @param plate Number of wells in whole plate (96, 384 or 1536)
#' @param threshold Standard deviations from the plate average to indicate a hit.
#'      default is set to +/- 2 SD.
#' @param palette RColorBrewer palette
#' @param eps real number greater than 0. A tolerance for divergence
#' @param maxiter int, the maximum number of iterations
#' @param trace.iter Boolean, should progress in convergence be reported?
#' @param na.rm Boolean, should missing values be removed?
#' @param ... additional parameters to plot wrappers
#'
#' @return ggplot plot
#'
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
#'
#' @export
#'
#' @examples
#' df <- data.frame(vals = rnorm(384),
#'    well = num_to_well(1:384, plate = 384))
#'
#' bhit_map(data = df$vals,
#'    well = df$well,
#'    plate = 384,
#'    threshold = 3)

bhit_map <- function(data, well, plate = 96, threshold = 2,
                     palette = "Spectral", eps = 0.01, maxiter = 10,
                     trace.iter = FALSE, na.rm = TRUE, ...){

    # need to transform columns of wellID and data into
    # matrix corresponding to well positions:
    platemap <- plate_map(data, well)

    # ensure data is ordered properly before passing to matrix()
    platemap <- platemap[order(platemap$Row, platemap$Column), ]

    check_plate_input(well, plate)
    if (plate == 6L) {
        nrow <- 3
        ncol <- 2
    } else if (plate == 12L) {
        nrow <- 4
        ncol <- 3
    } else if (plate == 24L) {
        nrow <- 6
        ncol <- 4
    } else if (plate == 48L) {
        nrow <- 8
        ncol <- 6
    } else if (plate == 96L) {
        nrow <- 8
        ncol <- 12
    } else if (plate == 384L) {
        nrow <- 16
        ncol <- 24
    } else if (plate == 1536L) {
        nrow <- 32
        ncol <- 48
    } else {
       stop("Not a plate format.\nArgument 'plate' should be 6, 12, 24, 48, 96, 384 or 1536.",
             call. = FALSE)
    }
    mat_plate_map <- matrix(data,
                            nrow = nrow,
                            ncol = ncol,
                            byrow = TRUE)

    # median polish of the data
    data_pol <- medpolish(mat_plate_map, eps = eps, maxiter = maxiter,
                          trace.iter = trace.iter, na.rm = na.rm)

    # transpose of residual matrix (as counts in column-wise fashion)
    # now well numbers correspond i.e t_out[12] = A12, t_out[13] = B01
    t_out <- t(data_pol$residuals)

    # 1:96 elements of residuals corresponding to 1:96 of num_to_well
    # produce dataframe of two columns
    df <- NULL

    for (num in 1:length(t_out)) {
        df$residual[num] <- t_out[num]
        df$well[num] <- num_to_well(num, plate = plate)
    }

    df <- as.data.frame(cbind("well" = df$well, "residual" = df$residual))
    # change residuals from factor to numeric
    df$residual <- as.numeric(as.character(df$residual))

    platemap$values <- scale(df$residual)[, ]
    platemap$hit <- NA

    # calculate whether values are beyond the threshold; defined as hit or null
    for (row in 1:nrow(platemap)) {
        if (!is.finite(platemap[row, "values"])) {
            platemap$hit[row] <- NaN
        } else if (platemap[row, 'values'] > threshold) {
            platemap$hit[row] <- "hit"
        } else if (platemap[row, 'values'] < (-1*threshold)) {
            platemap$hit[row] <- "neg_hit"
        } else {
            platemap$hit[row] <- "null"
        }
    }

    # change name of hit to values
    # plt96 and plt384 are colouring points by value, in this case needs to be hit
    platemap$actual_vales <- platemap$values
    platemap$values <- platemap$hit

    # RColorBrewerPallette
    my_cols <- brewer.pal(3, palette)
    my_colours <- c(hit = my_cols[1], neg_hit = my_cols[3], null = my_cols[2])

    if (plate == 6L){
        # produce a 6-well plate map layout in ggplot
        plt <- plt6(platemap, ...) +
            scale_fill_manual("hit", values = my_colours) +
            theme_bw()
    } else if (plate == 12L){
        # produce a 12-well plate map layout in ggplot
        plt <- plt12(platemap, ...) +
            scale_fill_manual("hit", values = my_colours) +
            theme_bw()
    } else if (plate == 24L){
        # produce a 24-well plate map layout in ggplot
        plt <- plt24(platemap, ...) +
            scale_fill_manual("hit", values = my_colours) +
            theme_bw()
    } else if (plate == 48L){
        # produce a 48-well plate map layout in ggplot
        plt <- plt48(platemap, ...) +
            scale_fill_manual("hit", values = my_colours) +
            theme_bw()
    } else if (plate == 96L){
        # produce a 96-well plate map layout in ggplot
        plt <- plt96(platemap, ...) +
            scale_fill_manual("hit", values = my_colours) +
            theme_bw()
    } else if (plate == 384L) {
        # produce a 384-well plate map layout in ggplot
        plt <- plt384(platemap, ...) +
            scale_fill_manual("hit", values = my_colours) +
            theme_bw()
    } else if (plate == 1536L) {
        plt <- plt1536(platemap, ...) +
           scale_fill_manual("hit", values = my_colours) +
           theme_bw()

    } else {
        stop("Not a valid plate format. Either 6, 12, 24, 48, 96, 384 or 1536.", call. = FALSE)
    }
    return(plt)
}
