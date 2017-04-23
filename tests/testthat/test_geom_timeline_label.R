context('geom_timeline_label')

test_that('geom_timeline_label works, using 2 countries', {
  quakes <- eq_load_clean_data()

  p <- quakes %>% dplyr::filter(COUNTRY %in% c('INDONESIA', 'HAITI')) %>%
    dplyr::filter(DATE > '2000-01-01') %>%
    ggplot() +
    geom_timeline_label(aes(x = DATE, y = COUNTRY, label = LOCATION_NAME,
                            magnitude = EQ_PRIMARY))

  expect_s3_class(p, 'ggplot')

  out <- layer_data(p)

  expect_length(unique(out$y), 2)

  expect_equal(sum(out$group == out$y), nrow(out))

})
