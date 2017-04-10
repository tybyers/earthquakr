context('test shortcut method to load and clean data in one shot')

test_that('eq_load_clean_data loads data correctly', {

  quick_quakes <- eq_load_clean_data()

  expect_is(quick_quakes$LATITUDE, 'numeric')
  expect_is(quick_quakes$LONGITUDE, 'numeric')

  # the "slow" way
  slow_quakes <- quakes %>%
    eq_clean_data() %>%
    eq_location_clean()

  expect_equal(sum(quick_quakes$LOCATION_NAME == slow_quakes$LOCATION_NAME,
                   na.rm = TRUE),
               sum(!(is.na(quick_quakes$LOCATION_NAME))))

  expect_equal(sum(quick_quakes$DATE == slow_quakes$DATE),
               nrow(quick_quakes))
})
