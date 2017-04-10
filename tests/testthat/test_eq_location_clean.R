context('clean location data')

df <- quakes

test_that('basic location name cleaning function works', {
  df_clean <- eq_location_clean(df)

  expect_is(df_clean$LOCATION_NAME, 'character')

  # is it in title case?
  expect_equal(df_clean$LOCATION_NAME,
               df_clean$LOCATION_NAME %>%  # transform first to lower then title
                 stringr::str_to_lower() %>%
                 stringr::str_to_title())

  # did we remove the "Country:" from location name?
  expect_equal(purrr::map2(df_clean$COUNTRY, df_clean$LOCATION_NAME,
                               function(cty, loc) {
                                 grep(paste0(cty, ":"), loc)
                               }) %>% unlist() %>% sum(), 0)
})

test_that('missing variables throw an error', {

  expect_error(eq_location_clean(df %>% dplyr::select(-COUNTRY)),
               'Missing required variable: COUNTRY')

  expect_error(eq_location_clean(df %>% dplyr::select(-LOCATION_NAME)),
               'Missing required variable: LOCATION_NAME')

  # remove a column we don't care much about
  expect_s3_class(eq_location_clean(df %>% dplyr::select(-MINUTE)), 'tbl_df')
})
