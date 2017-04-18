#' Title
#'
#' @param data
#' @param annot_col
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
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
    dplyr::mutate(popup_text = purrr::pmap_chr(
      list(LOCATION_NAME, EQ_PRIMARY,
           TOTAL_DEATHS, DATE),
      labeling
    ))

  data$popup_text
}
