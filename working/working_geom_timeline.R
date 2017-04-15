library(ggplot2)
library(grid)
library(tidyverse)
quakes <- eq_load_clean_data()

GeomTimeline <- ggplot2::ggproto(
  "GeomTimeline",
  ggplot2::Geom,
  required_aes = c('x'),
  default_aes = ggplot2::aes(
    size = 1,
    color = 'grey50',
    alpha = 0.5,
    shape = 19,
    y = 0,
    stroke = 1,
    fill = 'black'
  ),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_scales, coord) {
    ## Transform the data first
    #browser()
    # pull the y-coord down a bit -- don't want it centered
    #ranges <- coord$range(panel_scales)
    #data$y <- data$y - ranges$y[1]
    #panel_scales$y.major_source <- panel_scales$y.major_source - ranges$y[1]
    #panel_scales$y.major <- panel_scales$y.major -
    #  ranges$y[1] / (ranges$y[2] - ranges$y[1])
    #panel_scales$y.range[2] <- panel_scales$y.range[2] +
    #  0.5 * (panel_scales$y.range[2] - panel_scales$y.range[1])

    coords <- coord$transform(data, panel_scales)

    # resize the coordinates' size to more reasonable values
    coords$size <-
      coords$size / max(coords$size) * 1.5

    #browser()
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


geom_timeline <-
  function(mapping = NULL,
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

timeline_example <- function(country, label_n = 0) {

  qs <- eq_load_clean_data() %>%
    filter(COUNTRY %in% country)

  eq_timeline(qs, '2000-01-01', '2016-01-01', label_n)
}

theme_eq <- function(base_size = 11,
                     base_family = 'sans') {
  eq <- (
    ggplot2::theme_minimal(base_size = base_size,
                           base_family = base_family) +
      theme(
        legend.position = 'bottom',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(),
        axis.line.x = element_line()
      )
  )

  eq
}


eq_timeline <- function(df, xmin = '2000-01-01', xmax = '2018-01-01',
                        label_n = 0) {

  df <- df %>%
    dplyr::filter(DATE >= xmin, DATE <= xmax)

  if (label_n == 0) {
    p <- df %>%
      ggplot2::ggplot() +
      geom_timeline(aes(x = DATE, y = COUNTRY,
                        color = DEATHS, size = EQ_PRIMARY)) +
      ggplot2::scale_size_continuous(name = 'Richter scale value') +
      ggplot2::scale_color_continuous(name = '# of Deaths') +
      theme_eq()
  } else {
    p <- df %>%
      ggplot2::ggplot() +
      geom_timeline(aes(x = DATE, y = COUNTRY,
                        color = DEATHS, size = EQ_PRIMARY)) +
      geom_timeline_label(aes(
        x = DATE,
        y = COUNTRY,
        magnitude = EQ_PRIMARY,
        label = LOCATION_NAME,
        n_max = label_n
      )) +
      scale_size_continuous(name = 'Richter scale value') +
      scale_color_continuous(name = '# of Deaths') +
      theme_eq()
  }

  print(p)
}
