#' Clean Earthquake Data
#'
#' \code{eq_clean_data} takes a data frame of NOAA earthquake data and cleans it
#' up for further analysis.
#'
#' This function takes a data frame of NOAA earthquake data, which may be
#' obtained from NOAA's
#' \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{Significant
#' EarthQuake Database}, or loaded into the envirnoment with \code{data(quakes)}
#' (April 1, 2017 snapshot), and cleans it up for further analysis.  In
#' particular, it creates a \code{DATE} feature by combing the \code{YEAR},
#' \code{MONTH}, and \code{DAY} features, and makes sure the \code{LATITUDE} and
#' \code{LONGITUDE} features are of type \code{numeric}.  Also converts the
#' \code{DEATHS}, \code{TOTAL_DEATHS}, and \code{EQ_PRIMARY} (Richter scale
#' value) to type \code{numeric}.
#'
#' For the \code{DATE} variable, note that R does not handle years before 1CE
#' correctly (see documentation for \code{\link[base]{as.Date}}). This function
#' has attempted to make an approximation of those dates, but the user should
#' handle with care.  Also, for some events the month and day are not available,
#' so those are approximated by January and 01, respectively.
#'
#' @param data A data frame of NOAA significant earthquake data, similar to what
#'   can be loaded with \code{data(quakes)}. At a minimum, the data needs to
#'   have \code{YEAR}, \code{MONTH}, \code{DAY}, \code{LATITUDE},
#'   \code{LONGITUDE}, \code{DEATHS}, \code{TOTAL_DEATHS}, and \code{EQ_PRIMARY}
#'   features.
#'
#' @return A \code{tbl_df} with the same supplied data, but with
#'   \code{LATITUDE}, \code{LONGITUDE}, \code{DEATHS}, \code{TOTAL_DEATHS}, and
#'   \code{EQ_PRIMARY} converted from \code{character} to \code{numeric}, and a
#'   new column \code{DATE}.
#'
#' @export
#'
#' @examples
#'
#' data(quakes)
#' quakes_clean <- eq_clean_data(quakes)
eq_clean_data <- function(data) {

  required_vars <- c('LONGITUDE', 'LATITUDE', 'MONTH', 'DAY', 'YEAR',
                     'DEATHS', 'TOTAL_DEATHS', 'EQ_PRIMARY')
  purrr::map(required_vars, function(rv) {
    if(!(rv %in% names(data))) {
      stop('Missing required variable: ', rv)
    }
  })

  data <- data %>%
    dplyr::mutate_(
      LONGITUDE = ~as.numeric(LONGITUDE),
      LATITUDE = ~as.numeric(LATITUDE),
      MONTH = ~ifelse(is.na(MONTH), 1, MONTH),
      DAY = ~ifelse(is.na(DAY), 1, DAY),
      DEATHS = ~as.numeric(DEATHS),
      TOTAL_DEATHS = ~as.numeric(TOTAL_DEATHS),
      EQ_PRIMARY = ~as.numeric(EQ_PRIMARY))

  # as.Date doesn't handle BCE dates, so have to estimate it as best as possible
  data <- data %>%
    dplyr::mutate_(DATE = ~purrr::pmap(list(YEAR, MONTH, DAY),
                                      function(y, m, d) {
      if (y < 0) {
        # numeric date for first DAY of CE
        first_date <-
          as.numeric(as.Date('0 1 1', '%Y %m %d', origin = '1970-01-01'))
        # take the negative date and "mirror" it to CE Jan 1st
        mirror_date <-
          as.Date(paste(y * -1 - 1, 1, 1, sep = '-'), '%Y-%m-%d',
                  origin = '1970-01-01')
        dt <- as.numeric(mirror_date) - first_date

        # time to end of year
        end_of_year_dt <-
          as.numeric(as.Date(paste(y * -1 + 2, 12, 31, sep = '-'), '%Y-%m-%d',
                             origin = '1970-01-01')) -
          as.numeric(as.Date(paste(y * -1 + 2, m, d, sep = '-'), '%Y-%m-%d',
                             origin = '1970-01-01'))
        date <-
          as.Date(first_date - dt - end_of_year_dt + 1, origin = '1970-01-01')
      } else {
        date <- as.Date(paste(y, m, d, sep = '-'), '%Y-%m-%d')
      }
      date
    })) %>%
    dplyr::mutate_(DATE = ~unlist(DATE), # date comes out of purrr::map as numeric
                  DATE = ~as.Date(DATE, origin = '1970-01-01'))

  data
}

#' Clean Earthquake Location Field
#'
#' \code{eq_location_clean} takes a data frame of NOAA earthquake data and cleans
#' the \code{LOCATION_NAME} field.
#'
#' This function cleans the \code{LOCATION_NAME} observation in a NOAA
#' Significant Earthquakes data set.  It removes the country name (of form
#' \code{COUNTRY:}) from the \code{LOCATION_NAME} data (unless the country is
#' the only name present in that field), and converts the remainder of the field
#' to Title Case. This will make the location name easier to read when plotting
#' and mapping.
#'
#' @param data A data frame of NOAA significant earthquake data, similar to what
#'   can be loaded with \code{data(quakes)}. At a minimum, the data needs to
#'   have \code{LOCATION_NAME} and \code{COUNTRY} features to use this function.
#'
#' @return A \code{tbl_df} with the same supplied data, but with the
#'   \code{LOCATION_NAME} variable cleaned up to remove the country name that is
#'   supplied in the \code{COUNTRY} variable (unless the country name is the
#'   \strong{only} word in the \code{LOCATION_NAME} observation), and convert
#'   the remainder of the words in \code{LOCATION_NAME} to Title Case.
#'
#' @export
#'
#' @examples
#' data(quakes)
#' quakes_loc_clean <- eq_location_clean(quakes)
eq_location_clean <- function(data) {

  required_vars <- c('COUNTRY', 'LOCATION_NAME')
  purrr::map(required_vars, function(rv) {
    if(!(rv %in% names(data))) {
      stop('Missing required variable: ', rv)
    }
  })

  data <- data %>%
    dplyr::mutate_(
      LOCATION_NAME =
        ~purrr::map2_chr(COUNTRY, LOCATION_NAME,
                        function(COUNTRY, LOCATION_NAME) {
                          gsub(paste0(COUNTRY, ":"), '', LOCATION_NAME)
                        }),
      LOCATION_NAME = ~stringr::str_trim(LOCATION_NAME),
      LOCATION_NAME = ~stringr::str_to_title(LOCATION_NAME)
    )

  data
}

#' Load Clean Earthquakes Data
#'
#' \code{eq_load_clean_data} is a shortcut to loading and cleaning the NOAA
#' \code{\link{quakes}} data supplied with this package.
#'
#' This function is a quick shortcut to load the \code{\link{quakes}} data
#' supplied with this package.  If the April 1, 2017 cutoff is sufficient for
#' you, use this function to save a few keystrokes.
#'
#' @return A \code{tbl_df} of NOAA Significant Earthquakes Data, cleaned with
#'   the functions \link{eq_clean_data} and \link{eq_location_clean}.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' quakes1 <- eq_load_clean_data()
#' # is equivalent to:
#' quakes2 <- quakes %>% eq_clean_data() %>% eq_location_clean()
eq_load_clean_data <- function() {
  eq <- get('quakes')
  df <- eq %>%
    eq_clean_data() %>%
    eq_location_clean()
  df
}
