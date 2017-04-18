# eq_map
library(leaflet)


eq_map <- function(df, annot_col = 'DATE') {

  df <- df %>%
    dplyr::mutate_(popup_col = as.name(annot_col))

  m <- leaflet::leaflet(data = df) %>%
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

eq_create_label <- function(df) {
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

  df <- df %>%
    dplyr::mutate(
      popup_text = purrr::pmap_chr(list(LOCATION_NAME, EQ_PRIMARY,
                                        TOTAL_DEATHS, DATE), labeling))

  df$popup_text
}


map_example <- function(country, annot_col = 'DATE') {
  qs <- eq_load_clean_data() %>%
    filter(COUNTRY %in% country) %>%
    filter(lubridate::year(DATE) >= 2000)

  eq_map(qs, annot_col)
}

map_example_popup <-  function(country, annot_col = 'popup_text') {
  qs <- eq_load_clean_data() %>%
    filter(COUNTRY %in% country) %>%
    filter(lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.))

  eq_map(qs, annot_col)
}
