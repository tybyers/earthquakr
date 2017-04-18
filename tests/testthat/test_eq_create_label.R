context('test popup labels created correctly')

test_that('popup labels create correctly', {
  qs <- eq_load_clean_data()
  cols_start <- ncol(qs)

  qs <- qs %>%
    dplyr::filter(COUNTRY == 'IRAN') %>%
    dplyr::filter(lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.))

  expect_is(qs$popup_text, 'character')
  expect_equal(cols_start, ncol(qs) - 1)

})
