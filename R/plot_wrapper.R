#' ggplot plate object
#'
#' internal function
#'
#' @param platemap platemap dataframe produced by \code{plate_map}
#' @param size int, size parameter for ggplot2::geom_point
#' @param shape int, shape parameter for ggplot2::geom_point
#' @return ggplot object
#' @import ggplot2
#' @export

plt6 <- function(platemap, size = 50, shape = 21){
    shape <- parse_shape(shape)
    ylim <- c(2.5, 0.5)
    if (is_old_ggplot()) {
        ylim <- rev(ylim)
    }
    ggplot(data = platemap, aes_string(x = "Column", y = "Row")) +
        geom_point(data = expand.grid(seq(1, 3), seq(1, 2)),
                   aes_string(x = "Var1", y = "Var2"),
                   color = "grey90", fill = "white", shape = shape, size = size-4, alpha = 0.1) +
        geom_point(aes_string(fill = "values"), colour = "gray20", shape = shape, size = size) +
        coord_fixed(ratio = (4.75 / 3) / (3 / 2), xlim = c(0.5, 3.5), ylim = ylim) +
        scale_y_reverse(breaks = seq(1, 2), labels = LETTERS[1:2]) +
        scale_x_continuous(position = "top", breaks = seq(1, 3)) +
        xlab("") +
        ylab("")
}


#' ggplot plate object
#'
#' internal function
#'
#' @param platemap platemap dataframe produced by \code{plate_map}
#' @param size int, size parameter for ggplot2::geom_point
#' @param shape int, shape parameter for ggplot2::geom_point
#' @return ggplot object
#' @import ggplot2
#' @export

plt12 <- function(platemap, size = 38, shape = 21){
    shape <- parse_shape(shape)
    ylim <- c(3.5, 0.5)
    if (is_old_ggplot()) {
        ylim <- rev(ylim)
    }
    ggplot(data = platemap, aes_string(x = "Column", y = "Row")) +
        geom_point(data = expand.grid(seq(1, 4), seq(1, 3)),
                   aes_string(x = "Var1", y = "Var2"),
                   color = "grey90", fill = "white", shape = shape, size = size-4, alpha = 0.1) +
        geom_point(aes_string(fill = "values"), colour = "gray20", shape = shape, size = size) +
        coord_fixed(ratio = (5.5 / 4) / (4 / 3), xlim = c(0.5, 4.5), ylim = ylim) +
        scale_y_reverse(breaks = seq(1, 3), labels = LETTERS[1:3]) +
        scale_x_continuous(position = "top", breaks = seq(1, 4)) +
        xlab("") +
        ylab("")
}


#' ggplot plate object
#'
#' internal function
#'
#' @param platemap platemap dataframe produced by \code{plate_map}
#' @param size int, size parameter for ggplot2::geom_point
#' @param shape int, shape parameter for ggplot2::geom_point
#' @return ggplot object
#' @import ggplot2
#' @export

plt24 <- function(platemap, size = 26, shape = 21){
    shape <- parse_shape(shape)
    ylim <- c(4.5, 0.5)
    if (is_old_ggplot()) {
        ylim <- rev(ylim)
    }
    ggplot(data = platemap, aes_string(x = "Column", y = "Row")) +
        geom_point(data = expand.grid(seq(1, 6), seq(1, 4)),
                   aes_string(x = "Var1", y = "Var2"),
                   color = "grey90", fill = "white", shape = shape, size = size-4, alpha = 0.1) +
        geom_point(aes_string(fill = "values"), colour = "gray20", shape = shape, size = size) +
        coord_fixed(ratio = (8 / 6) / (5 / 4), xlim = c(0.5, 6.5), ylim = ylim) +
        scale_y_reverse(breaks = seq(1, 4), labels = LETTERS[1:4]) +
        scale_x_continuous(position = "top", breaks = seq(1, 6)) +
        xlab("") +
        ylab("")
}


#' ggplot plate object
#'
#' internal function
#'
#' @param platemap platemap dataframe produced by \code{plate_map}
#' @param size int, size parameter for ggplot2::geom_point
#' @param shape int, shape parameter for ggplot2::geom_point
#' @return ggplot object
#' @import ggplot2
#' @export

plt48 <- function(platemap, size = 18, shape = 21){
    shape <- parse_shape(shape)
    ylim <- c(6.5, 0.5)
    if (is_old_ggplot()) {
        ylim <- rev(ylim)
    }
    ggplot(data = platemap, aes_string(x = "Column", y = "Row")) +
        geom_point(data = expand.grid(seq(1, 8), seq(1, 6)),
                   aes_string(x = "Var1", y = "Var2"),
                   color = "grey90", fill = "white", shape = shape, size = size-4, alpha = 0.1) +
        geom_point(aes_string(fill = "values"), colour = "gray20", shape = shape, size = size) +
        coord_fixed(ratio = (9 / 8) / (7 / 6), xlim = c(0.5, 8.5), ylim = ylim) +
        scale_y_reverse(breaks = seq(1, 6), labels = LETTERS[1:6]) +
        scale_x_continuous(position = "top", breaks = seq(1, 8)) +
        xlab("") +
        ylab("")
}


#' ggplot plate object
#'
#' internal function
#'
#' @param platemap platemap dataframe produced by \code{plate_map}
#' @param size int, size parameter for ggplot2::geom_point
#' @param shape int, shape parameter for ggplot2::geom_point
#' @return ggplot object
#' @import ggplot2
#' @export

plt96 <- function(platemap, size = 10, shape = 21){
    shape <- parse_shape(shape)
    ylim <- c(8.5, 0.5)
    if (is_old_ggplot()) {
        ylim <- rev(ylim)
    }
    ggplot(data = platemap, aes_string(x = "Column", y = "Row")) +
        geom_point(data = expand.grid(seq(1, 12), seq(1, 8)),
                   aes_string(x = "Var1", y = "Var2"),
                   color = "grey90", fill = "white", shape = shape, size = size-4, alpha = 0.1) +
        geom_point(aes_string(fill = "values"), colour = "gray20", shape = shape, size = size) +
        coord_fixed(ratio = (13 / 12) / (9 / 8), xlim = c(0.5, 12.5), ylim = ylim) +
        scale_y_reverse(breaks = seq(1, 8), labels = LETTERS[1:8]) +
        scale_x_continuous(position = "top", breaks = seq(1, 12)) +
        xlab("") +
        ylab("")
}

#' ggplot plate object
#'
#' internal function
#'
#' @param platemap platemap dataframe produced by \code{plate_map}
#' @param size int, size parameter for ggplot2::geom_point
#' @param shape int, shape parameter for ggplot2::geom_point
#' @return ggplot object
#' @import ggplot2
#' @export

plt384 <- function(platemap, size = 5, shape = 22){
    shape <- parse_shape(shape)
    ylim <- c(16.5, 0.5)
    if (is_old_ggplot()) {
        ylim <- rev(ylim)
    }
    ggplot(data = platemap, aes_string(x = "Column", y = "Row")) +
        geom_point(data = expand.grid(seq(1, 24), seq(1, 16)),
                   aes_string(x = "Var1", y = "Var2"),
                   color = "grey90", fill = "white", shape = shape, size = size-2, alpha = 0.1) +
        geom_point(aes_string(fill = "values"), colour = "gray20", shape = shape, size = size) +
        coord_fixed(ratio = (24.5 / 24) / (16.5 / 16), xlim = c(0.5, 24.5), ylim = ylim) +
        scale_y_reverse(breaks = seq(1, 16), labels = LETTERS[1:16]) +
        scale_x_continuous(position = "top", breaks = seq(1, 24)) +
        xlab("") +
        ylab("")
}

#' ggplot plate object
#'
#' internal function
#'
#' @param platemap platemap dataframe produced by \code{plate_map}
#' @param size int, size parameter for ggplot2::geom_point
#' @param shape int, shape parameter for ggplot2::geom_point
#' @return ggplot object
#' @import ggplot2
#' @export


plt1536 <- function(platemap, size = 3.5, shape = 22){
    shape <- parse_shape(shape)
    ylim <- c(32.5, 0.5)
    if (is_old_ggplot()) {
        ylim <- rev(ylim)
    }
    ggplot(data = platemap, aes_string(x = "Column", y = "Row")) +
    geom_point(data = expand.grid(seq(1, 48), seq(1, 32)),
               aes_string(x = "Var1", y = "Var2"),
               color = "grey90", fill = "white", shape = shape, size = size-1.5, alpha = 0.1) +
    geom_point(aes_string(fill = "values"), colour = "gray20", shape = shape, size = size) +
    coord_fixed(ratio = (48.25 / 48) / (32.25 / 32), xlim = c(0.5, 48.5), ylim = ylim) +
    scale_y_reverse(breaks = seq(1, 32)) +
    scale_x_continuous(position = "top", breaks = seq(1, 48)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)) +
    xlab("") +
    ylab("")
}


parse_shape <- function(shape) {
    if (is.numeric(shape)) {
        return(shape)
    } else if (is.character(shape)) {
        # R is the worst
        # why is it `tolower` and not `to.lower` or `toLower` ????
        if (tolower(shape) == "circle") {
            return(21L)
        } else if (tolower(shape) == "round") {
            return(21L)
        } else if (tolower(shape) == "square") {
            return(22L)
        } else {
            stop("invalid parameter for shape")
        }
    } else {stop("invalid parameter for shape")}
}


#' change legend title
#'
#' Change the legend title. This can be done in ggplot but there
#' are a million incomprehensible ways to do it.
#'
#' @param title, string new title
#' @return ggplot object
#' @import ggplot2
#' @export

legend_title <- function(title) {
    return(labs(fill = title))
}


#' check ggplot2 version
#'
#' after ggplot2 v3.3.0, using scale_y_reverse() also reverses the order
#' of the ylim arguments in coord_fixed()

is_old_ggplot <- function() {
    # there's no sensible way to do this in R?!
    return(utils::packageVersion("ggplot2")<"3.3.0")
}
