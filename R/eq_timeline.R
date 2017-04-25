#' Produce Earthquake Timeline Plots
#'
#' \code{eq_timeline} is a function to easily create a timeline plot with or
#' without labels using NOAA Significant Earthquake data.
#'
#' This function is a wrapper function that makes it easy to create a timeline
#' plot of the NOAA Significant earthquakes for a given country or countries. It
#' also allows the user to specify (or not!) the number of earthquakes to label.
#' This function makes it easy to use the recommended defaults for
#' \code{\link{geom_timeline}}, \code{\link{geom_timeline_label}}, and
#' \code{\link{theme_eq}}. The benefit of using this function is that it
#' saves the user a significant amount of typing and data filtering, though
#' it gives up control in specifics of how the timelines are displayed.
#'
#' @param df The cleaned earthquake data to timeline. See
#'   \code{\link{eq_load_clean_data}} for more information.
#' @param countries Default \code{USA}.  May be a single country or a character
#'   vector of countries (countries stacked on y-axis).
#' @param date_min Default \code{'2000-01-01'}. Minimum date for timeline in
#'   \code{YYYY-mm-dd} format. May pass in a \code{Date}, a \code{POSIXct}, or
#'   \code{character} vector which can be coerced to a \code{Date}.
#' @param date_max Default \code{'2018-01-01'}. Maximum date for timeline in
#'   \code{YYYY-mm-dd} format. May pass in a \code{Date}, a \code{POSIXct}, or
#'   \code{character} vector which can be coerced to a \code{Date}.
#' @param label_n Default \code{0}. If \code{label_n == 0}, no labels are
#'   printed on the timeline. Otherwise, the number of labels specified as the
#'   value for this parameter are printed. Suggest keeping this number less
#'   than or equal to 10.
#'
#' @return A \code{ggplot2} object, showing the earthquake timeline.
#'
#' @export
#'
#' @examples
#' # An example with a single country and no labels, showing two
#' # different classes of date* parameters being passed in.
#' library(ggplot2); library(dplyr)
#' quakes <- eq_load_clean_data()
#' quakes %>% eq_timeline(countries = 'USA', date_min = as.Date('1995-01-01'),
#'                       date_max = as.POSIXct('2015-01-01'), label_n = 0)
#'
#' # An example with multiple countries and 5 labels per country.
#' quakes <- eq_load_clean_data()
#' quakes %>% eq_timeline(countries = c('JAPAN', 'INDONESIA', 'HAITI'),
#'                       date_min = '2000-01-01', date_max = '2017-01-01',
#'                       label_n = 5)
eq_timeline <- function(df,
                        countries = 'USA',
                        date_min = '2000-01-01',
                        date_max = '2018-01-01',
                        label_n = 0) {

  countries <- toupper(countries)

  df <- df %>%
    dplyr::filter_(~DATE >= date_min,
                   ~DATE <= date_max,
                   ~COUNTRY %in% countries)

  if (label_n == 0) {
    p <- df %>%
      ggplot2::ggplot() +
      geom_timeline(aes_(x = ~DATE,
                         y = ~COUNTRY,
                         color = ~TOTAL_DEATHS,
                         size = ~EQ_PRIMARY)) +
      ggplot2::scale_size_continuous(name = 'Richter scale value') +
      ggplot2::scale_color_continuous(name = '# of Deaths') +
      theme_eq()
  } else {
    p <- df %>%
      ggplot2::ggplot() +
      geom_timeline(aes_(x = ~DATE,
                        y = ~COUNTRY,
                        color = ~TOTAL_DEATHS,
                        size = ~EQ_PRIMARY)) +
      geom_timeline_label(aes_(x = ~DATE,
                               y = ~COUNTRY,
                               magnitude = ~EQ_PRIMARY,
                               label = ~LOCATION_NAME,
                               n_max = label_n
      )) +
      scale_size_continuous(name = 'Richter scale value') +
      scale_color_continuous(name = '# of Deaths') +
      theme_eq()
  }

  p
}
