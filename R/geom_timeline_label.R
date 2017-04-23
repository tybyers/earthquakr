#' Label Largest Earthquakes on Timeline
#'
#' \code{geom_timeline_label} works best when used with
#' \code{\link{geom_timeline}}, labeling the top \code{n} earthquakes, by
#' magnitude, with a specified label field.  By default, the labels are for
#' the top 5 earthquakes for each country specified, however, the user may
#' adjust this with the \code{n_max} aesthetic.
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
#' \code{geom_timeline_label} undertands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'  \item \strong{x}: recommend \code{DATE}
#'  \item \strong{label}: recommend \code{LOCATION_NAME}
#'  \item \strong{magnitude}: recommend \code{EQ_PRIMARY}
#'  \item y: recommend \code{COUNTRY}
#'  \item n_max: default 5. Top \code{n} earthquakes to label,
#'        sorted by magnitude.
#'  \item color
#'  \item linetype
#'  \item size
#'  \item alpha
#' }
#'
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
#'   geom_timeline_label(aes(x = DATE, y = COUNTRY, magnitude = EQ_PRIMARY,
#'                          label = LOCATION_NAME, n_max = 5)) +
#'   scale_size_continuous(name = 'Richter scale value') +
#'   scale_color_continuous(name = '# of Deaths') +
#'   theme_eq()
geom_timeline_label <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      geom = GeomTimelineLabel,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }

#' GeomTimelineLabel
#'
#' See \code{\link{geom_timeline_label}} for description.
#'
#' @format NULL
#' @usage NULL
#' @export
GeomTimelineLabel <-
  ggplot2::ggproto(
    "GeomTimelineLabel",
    ggplot2::Geom,
    required_aes = c('x', 'label', 'magnitude'),

    default_aes = ggplot2::aes(
      n_max = 5,
      y = 0,
      color = 'grey50',
      size = 0.5,
      linetype = 1,
      alpha = NA
    ),

    draw_key = ggplot2::draw_key_point,

    draw_panel = function(data, panel_scales, coord) {

      n_max <- data$n_max[1]

      # get to n earthquakes by magnitude
      data <- data %>%
        dplyr::mutate(magnitude = magnitude / max(magnitude) * 1.5) %>%
        dplyr::group_by(group) %>%
        dplyr::top_n(n_max, magnitude)

      # grob for vertical line
      data$xend <- data$x
      data$yend <- data$y + 0.1
      g1 <- ggplot2::GeomSegment$draw_panel(unique(data), panel_scales, coord)

      # grob for text label
      data$y <- data$yend + 0.03
      data$angle <- 45
      data$fontface <- 9
      data$lineheight <- 2
      data$hjust <- 'left'
      data$vjust <- 'top'
      data$family <- 'sans'
      data$size <- 3
      data$colour <- 'black'
      g2 <- ggplot2::GeomText$draw_panel(unique(data), panel_scales, coord)

      ggplot2:::ggname('geom_timeline_label', grid::grobTree(g1, g2))
    }
  )
