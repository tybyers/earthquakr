context('test map is created')

test_that('map is created ', {
  qs <- eq_load_clean_data()

  qs <- qs %>%
    dplyr::filter(COUNTRY == 'JAPAN') %>%
    dplyr::filter(lubridate::year(DATE) >= 2000)

  map <- eq_map(qs, annot_col = 'DATE')

  expect_s3_class(map, 'leaflet')
  expect_s3_class(map, 'htmlwidget')
})
