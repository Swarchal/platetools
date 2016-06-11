#' Plots distributions per well in a plate layout
#'
#' Produces distribution plots facetted in a plate-layout format.
#'
#' @param well vector of alphanumeric wellIDs e.g 'A01'
#' @param data numeric vector
#'
#' @import ggplot2
#'
#' @export

#' @return ggplot plot


dist_map <- function(well, data){

    localenv <- environment()

    platemap <- plate_map(data, well)

    plt <- ggplot(data = platemap,
                  aes_string(x = "values"),
                  environment = localenv) +
        geom_density(alpha = 0.6,
                     fill = "gray80") +
        facet_grid(Row ~ Column) +
        theme_bw() +
        theme(axis.text.x=element_text(angle = -90, hjust = 0)) # rot. x-axis lab

    return(plt)
}
