context('geom_timeline')

test_that('geom_timeline works, allows 2 countries', {
  quakes <- eq_load_clean_data()

  p <- quakes %>% dplyr::filter(COUNTRY %in% c('RUSSIA', 'JAPAN')) %>%
    dplyr::filter(DATE > '2000-01-01') %>%
    ggplot() +
    geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
                      size = EQ_PRIMARY))

  expect_s3_class(p, 'ggplot')

  out <- layer_data(p)

  expect_length(unique(out$y), 2)

  expect_equal(sum(out$group == out$y), nrow(out))

})
