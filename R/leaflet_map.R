#' Interactive Earthquake Map
#'
#' \code{eq_map} creates an interactive \code{\link[leaflet]{leaflet}} map
#' showing the location of earthquakes in the given \code{quakes} data set.
#'
#' This function shows an interactive map of the location of the earthquakes in
#' the given \code{\link{quakes}} data.  The size of the circles are
#' proportional to the magnitude of the earthquakes (in the \code{EQ_PRIMARY})
#' variable. The map is interactive, and when you click on a link, the popup
#' shows the annotation as specified by the \code{annot_col} variable.
#'
#' @param data The \code{\link{quakes}} data frame. For best results, you should
#'   filter to a single country or nearby countries, and preferably filter down
#'   to a relatively narrow date range (maybe several decades at most).
#' @param annot_col The column to use for the popup annotation. You may use a
#'   single column; otherwise create a more useful label first by using the
#'   \code{\link{eq_create_label}} function.
#'
#' @seealso \code{\link{eq_create_label}} to create a more useful popup
#'   annotation in the \code{quakes} data frame.
#'
#' @return Returns an interactive \code{leaflet} map.
#' @export
#'
#' @examples
#' library(lubridate); library(dplyr)
#'
#' ## use 'DATE' as annotation
#' quakes <- eq_load_clean_data() %>%
#'  dplyr::filter(COUNTRY == 'JAPAN') %>%
#'  dplyr::filter(lubridate::year(DATE) >= 2000)
#'
#' eq_map(quakes, annot_col = 'DATE')
#'
#'
#' ## Create a popup_text variable and use that for the label
#' quakes <- eq_load_clean_data() %>%
#'  dplyr::filter(COUNTRY == 'JAPAN') %>%
#'  dplyr::filter(lubridate::year(DATE) >= 2000) %>%
#'  dplyr::mutate(popup_text = eq_create_label(.))
#'
#' eq_map(quakes, annot_col = 'popup_text')
eq_map <- function(data, annot_col = 'DATE') {
  data <- data %>%
    dplyr::mutate_(popup_col = as.name(annot_col))

  m <- leaflet::leaflet(data = data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lng = ~ LONGITUDE,
      lat = ~ LATITUDE,
      radius = ~ EQ_PRIMARY,
      weight = 1,
      popup = ~ as.character(popup_col)
    )

  m
}

#' Popup Label for Earthquake Map
#'
#' \code{eq_create_label} creates a more descriptive and HTML-formatted popup
#' label to be used in \code{\link{eq_map}}.
#'
#' This function creates a vector of HTML-formatted labels using supplied
#' \code{\link{quakes}} data.  The function creates lines of the format
#' \strong{Label:} \code{ value} from the following variables in the \code{quakes}
#' data set:
#' \itemize{
#'   \item DATE
#'   \item LOCATION_NAME (as cleaned in the \code{\link{eq_location_clean}}
#'   function).
#'   \item EQ_PRIMARY (earthquake magnitude)
#'   \item TOTAL_DEATHS
#' }
#' Any of the above variables with missing/NA values are skipped in the label.
#'
#' @param data The \code{\link{quakes}} data frame.
#'
#' @return A vector with the HTML-formatted labels. You should include this
#' vector with the data frame that is sent to \code{\link{eq_map}}.
#'
#' @export
#'
#' @examples
#' library(dplyr); library(lubridate)
#' ## Create a popup_text variable and use that for the label
#' quakes <- eq_load_clean_data() %>%
#'  dplyr::filter(COUNTRY == 'JAPAN') %>%
#'  dplyr::filter(lubridate::year(DATE) >= 2000) %>%
#'  dplyr::mutate(popup_text = eq_create_label(.))
#'
#' eq_map(quakes, annot_col = 'popup_text')
eq_create_label <- function(data) {
  labeling <- function(ln, mag, td, dt) {
    label <- ''
    if (!(is.na(dt))) {
      label <- paste0(label,
                      '<b>Date:</b> ',
                      as.Date(dt, origin = '1970-01-01'),
                      '<br/>')
    }
    if (!(is.na(ln))) {
      label <- paste0(label, '<b>Location:</b> ', ln, '<br/>')
    }
    if (!(is.na(mag))) {
      label <- paste0(label, '<b>Magnitude:</b> ', mag, '<br/>')
    }
    if (!(is.na(td))) {
      label <- paste0(label, '<b>Total Deaths:</b> ', td, '<br/>')
    }
    label
  }

  data <- data %>%
    dplyr::mutate_(popup_text = ~purrr::pmap_chr(
      list(LOCATION_NAME, EQ_PRIMARY,
           TOTAL_DEATHS, DATE),
      labeling
    ))

  data$popup_text
}
