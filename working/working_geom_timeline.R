library(ggplot2); library(grid); library(tidyverse)
quakes <- eq_load_clean_data()

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                   required_aes = c('x'),
                   default_aes = ggplot2::aes(size = 1, color = 'grey50',
                     alpha = 0.5, shape = 19, y = 0, stroke = 1,
                     fill = 'grey'
                   ),
                   draw_key = ggplot2::draw_key_point,
                   draw_panel = function(data, panel_scales, coord) {
                     ## Transform the data first
                     #browser()

                     coords <- coord$transform(data, panel_scales)
                     #coords <- coords %>%
                     # rename(`Richter scale value` = EQ_PRIMARY)

                     ## Let's print out the structure of the 'coords' object
                     str(coords)

                     #browser()
                     ## Construct a grid grob
                     grid::pointsGrob(
                       x = coords$x,
                       y = coords$y,
                       pch = coords$shape,
                       gp = grid::gpar(
                         color = unique(coords$colour),
                         alpha = coords$alpha,
                         size = coords$size,
                         fontsize = coords$fontsize
                       )
                     )
                   }
)


geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
         position = "identity", na.rm = FALSE,
         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data,
    stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

eq_data_transform <- function(data) {
  browser()
  data
}
timeline_example <- function(country) {
  qs <- eq_load_clean_data() %>%
    dplyr::filter(COUNTRY %in% country)

  # maybe need to clean this in the clean_data function, but
  # test it out here first
  qs <- qs %>%
    dplyr::mutate(DEATHS = as.numeric(DEATHS),
                  EQ_PRIMARY = as.numeric(EQ_PRIMARY))
  #browser()
  #ggplot(qs) + geom_timeline(aes(xmin = as.Date('2000-01-01'),
  #                                   xmax = as.Date('2016-01-01'),
  #                               fill = DEATHS, x = DATE,
  #                               size = EQ_MAG_ML
  #                               ))

  #ggplot(qs) + geom_timeline(aes(x = DATE))   # works ok
  #ggplot(qs) + geom_timeline(aes(x = DATE, xmin = as.Date('2000-01-01'),
  #                               xmax = as.Date('2016-01-01')))

  # maybe the x-min and xmax dates should be decided as scale_x...

  # ggplot(qs) + geom_timeline(aes(x = DATE, y = COUNTRY,
  #                                color = DEATHS, size = EQ_PRIMARY)) +
  #   scale_x_date(limits = c(as.Date('2000-01-01'), as.Date('2016-01-01'))) +
  #   theme_minimal()

  # ggplot(qs) + geom_timeline(aes(x = DATE, y = COUNTRY,
  #                                color = DEATHS, size = EQ_PRIMARY,
  #                                xmin = as.Date('2000-01-01'),
  #                                xmax = as.Date('2016-01-01'))) +
  #   #scale_x_date(limits = c(as.Date('2000-01-01'), as.Date('2016-01-01'))) +
  #   theme_minimal()

  eq_timeline(qs, '2000-01-01', '2016-01-01')
}

eq_timeline <- function(df, xmin, xmax) {
  p <- df <- df %>%
    filter(DATE >= xmin, DATE <= xmax) %>%
    ggplot() + geom_timeline(aes(x = DATE, y = COUNTRY, color = DEATHS,
                                 size = EQ_PRIMARY)) +
    scale_size_continuous(name = 'Richter scale value', position = 'bottom') +
    scale_color_continuous(name = '# of Deaths') +
    theme_minimal()
  print(p)
}
