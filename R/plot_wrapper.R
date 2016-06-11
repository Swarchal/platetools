#' ggplot plate object
#'
#' internal function
#'
#' @param platemap platemap dataframe produced by \code{plate_map}
#' @return ggplot object
#' @import ggplot2
#' @export

plt96 <- function(platemap){
    ggplot(data = platemap, aes_string(x = "Column", y = "Row")) +
        geom_point(data = expand.grid(seq(1, 12), seq(1, 8)),
                   aes_string(x = "Var1", y = "Var2"),
                   color = "grey90", fill = "white", shape = 21, size = 6, alpha = 0.1) +
        geom_point(aes_string(fill = "values"), colour = "gray20", shape = 21, size = 10) +
        coord_fixed(ratio = (13 / 12) / (9 / 8), xlim = c(0.5, 12.5), ylim = c(0.5, 8.5)) +
        scale_y_reverse(breaks = seq(1, 8), labels = LETTERS[1:8]) +
        scale_x_continuous(breaks = seq(1, 12)) +
        xlab("") +
        ylab("")
}

#' ggplot plate object
#'
#' internal function
#'
#' @param platemap platemap dataframe produced by \code{plate_map}
#' @return ggplot object
#' @import ggplot2
#' @export

plt384 <- function(platemap){
    ggplot(data = platemap, aes_string(x = "Column", y = "Row")) +
        geom_point(data = expand.grid(seq(1, 24), seq(1, 16)),
                   aes_string(x = "Var1", y = "Var2"),
                   color = "grey90", fill = "white", shape = 22, size = 3, alpha = 0.1) +
        geom_point(aes_string(fill = "values"), colour = "gray20", shape = 22, size = 5) +
        coord_fixed(ratio = (24.5 / 24) / (16.5 / 16), xlim = c(0.5, 24.5), ylim = c(0.5, 16.5)) +
        scale_y_reverse(breaks = seq(1, 16), labels = LETTERS[1:16]) +
        scale_x_continuous(breaks = seq(1, 24)) +
        xlab("") +
        ylab("")
}

#' ggplot plate object
#'
#' internal function
#' @param platemap platemap dataframe produced by \code{plate_map}
#' @return ggplot object
#' @import ggplot2
#' @export


plt1536 <- function(platemap){
    ggplot(data = platemap, aes_string(x = "Column", y = "Row")) +
	geom_point(data = expand.grid(seq(1, 48), seq(1, 32)),
               aes_string(x = "Var1", y = "Var2"),
		       color = "grey90", fill = "white", shape = 22, size = 2, alpha = 0.1) +
	geom_point(aes_string(fill = "values"), colour = "gray20", shape = 22, size = 3.5) +
	coord_fixed(ratio = (48.25 / 48) / (32.25 / 32), xlim = c(0.5, 48.5), ylim = c(0.5, 32.5)) +
	scale_y_reverse(breaks = seq(1, 32)) +
	scale_x_continuous(breaks = seq(1, 48)) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)) +
    xlab("") +
    ylab("")
}
