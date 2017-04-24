#' Earthquake Timeline Theme
#'
#' Makes your earthquakr timeline plots look pretty.
#'
#' @param base_size Default \code{11}, base size of text
#' @param base_family Default \code{sans}, text family
#'
#' @export
#'
#' @examples
#' library(dplyr); library(ggplot2)
#' quakes <- eq_load_clean_data()
#'
#' quakes %>% dplyr::filter(COUNTRY %in% c('USA', 'UK')) %>%
#'   dplyr::filter(DATE > '2000-01-01') %>%
#'   ggplot2::ggplot() +
#'   geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
#'                     size = EQ_PRIMARY)) +
#'   ggplot2::scale_size_continuous(name = 'Richter scale value') +
#'   ggplot2::scale_color_continuous(name = '# of Deaths') +
#'   theme_eq()
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
