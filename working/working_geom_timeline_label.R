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
      ## Transform the data first
      #browser()
      n_max <- data$n_max[1]

      # pull the y-coord down a bit -- don't awant it centered
      #ranges <- coord$range(panel_scales)
      #data$y <- data$y - ranges$y[1]

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

      ggplot2:::ggname('geom_timeline_label', grobTree(g1, g2))
    }
  )


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
