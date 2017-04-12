library(ggplot2); library(grid)
quakes <- eq_load_clean_data()

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                   required_aes = c('x'),
                   default_aes = ggplot2::aes(fill = DEATHS, size = EQ_MAG_ML,
                                              color = 'black',
                     alpha = 0.5, shape = 19,
                     y = 0
                   ),
                   draw_key = ggplot2::draw_key_point,
                   draw_panel = function(data, panel_scales, coord) {
                     ## Transform the data first
                     browser()


                     coords <- coord$transform(data, panel_scales)

                     ## Let's print out the structure of the 'coords' object
                     str(coords)

                     #browser()
                     ## Construct a grid grob
                     grid::pointsGrob(
                       x = coords$x,
                       y = coords$y,
                       pch = coords$shape,
                       gp = grid::gpar(
                         fill = unique(coords$DEATHS),
                         alpha = coords$alpha
                         #size = coords$EQ_MAG_ML
                       )
                     )
                   }
)

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
         position = "identity", na.rm = FALSE,
         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

timeline_example <- function(country) {
  qs <- eq_load_clean_data() %>%
    dplyr::filter(COUNTRY == country)

  # maybe need to clean this in the clean_data function, but
  # test it out here first
  qs <- qs %>%
    dplyr::mutate(DEATHS = as.numeric(DEATHS),
                  EQ_MAG_ML = as.numeric(EQ_MAG_ML))
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

  ggplot(qs) + geom_timeline(aes(x = DATE, fill = DEATHS, size = EQ_MAG_ML)) +
    scale_x_date(limits = c(as.Date('2000-01-01'), as.Date('2016-01-01'))) +
    theme_minimal()
}
