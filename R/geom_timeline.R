#' Timeline of Earthquakes
#'
#' \code{geom_timeline} shows a timeline of NOAA Significant earthquakes,
#' plotting individual countries along the y-axis and dates along the x-axis.
#' The size of the points is relatative to the earthquakes' magnitude, and
#' the color is related to the total number of deaths.
#'
#' It is highly recommended that this geom is not used in isolation. For
#' best results, use it with the \code{\link{eq_timeline}} wrapper function.
#'
#' @param mapping See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param data See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param stat See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param position See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param na.rm See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param show.legend See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param inherit.aes See \code{ggplot2} \code{\link[ggplot2]{layer}}
#' @param ... other arguments passed on to \code{\link{layer}}.
#'
#' @section Aesthetics:
#' \code{geom_timeline} undertands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'  \item \strong{x}: recommend \code{DATE}
#'  \item y: recommend \code{COUNTRY}
#'  \item size: recommend \code{EQ_PRIMARY}
#'  \item color: recommend \code{TOTAL_DEATHS}
#'  \item alpha
#'  \item shape
#'  \item stroke
#'  \item fill
#' }
#' @export
#'
#' @examples
#' library(dplyr); library(ggplot2)
#' quakes <- eq_load_clean_data()
#'
#' quakes %>%
#'   dplyr::filter(COUNTRY %in% c('USA', 'UK')) %>%
#'   dplyr::filter(DATE > '2000-01-01') %>%
#'   ggplot() +
#'   geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
#'                     size = EQ_PRIMARY)) +
#'   scale_size_continuous(name = 'Richter scale value') +
#'   scale_color_continuous(name = '# of Deaths') +
#'   theme_eq()
geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
    ggplot2::layer(
      geom = GeomTimeline,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
}

#' GeomTimeline
#'
#' See \code{\link{geom_timeline}} for description.
#'
#' @format NULL
#' @usage NULL
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
  required_aes = c('x'),

  default_aes = ggplot2::aes(
    y = 0,
    size = 1,
    color = 'grey50',
    alpha = 0.5,
    shape = 19,
    stroke = 0.5,
    fill = NA
  ),

  draw_key = ggplot2::draw_key_point,

  draw_panel = function(data, panel_scales, coord) {

    coords <- coord$transform(data, panel_scales)

    # resize the coordinates' size to more reasonable values
    coords$size <-
      coords$size / max(coords$size) * 1.5

    ## Construct a grid grob
    grid::pointsGrob(
      x = coords$x,
      y = coords$y,
      pch = coords$shape,
      gp = grid::gpar(
        col = coords$colour,
        alpha = coords$alpha,
        cex = coords$size
      )
    )
  }
)
