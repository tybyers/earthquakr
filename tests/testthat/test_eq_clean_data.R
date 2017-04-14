context('clean data')

# load quakes from package
df <- quakes

test_that('basic function works correctly', {

  df_clean <- eq_clean_data(df)

  expect_equal(ncol(df) + 1, ncol(df_clean))
  expect_equal(nrow(df), nrow(df_clean))
  expect_is(df_clean$DATE, 'Date')
  expect_is(df_clean$LATITUDE, 'numeric')
  expect_is(df_clean$LONGITUDE, 'numeric')
  expect_is(df_clean$DEATHS, 'numeric')
  expect_is(df_clean$EQ_PRIMARY, 'numeric')

})

test_that('throws error if do not have right columns', {

  expect_error(eq_clean_data(df %>% dplyr::select(-YEAR)),
               'Missing required variable: YEAR')
  expect_error(eq_clean_data(df %>% dplyr::select(-MONTH)),
               'Missing required variable: MONTH')
  expect_error(eq_clean_data(df %>% dplyr::select(-DAY)),
               'Missing required variable: DAY')
  expect_error(eq_clean_data(df %>% dplyr::select(-LATITUDE)),
               'Missing required variable: LATITUDE')
  expect_error(eq_clean_data(df %>% dplyr::select(-LONGITUDE)),
               'Missing required variable: LONGITUDE')
  expect_error(eq_clean_data(df %>% dplyr::select(-DEATHS)),
               'Missing required variable: DEATHS')
  expect_error(eq_clean_data(df %>% dplyr::select(-EQ_PRIMARY)),
               'Missing required variable: EQ_PRIMARY')

  # remove a column we don't care much about
  expect_is(eq_clean_data(df %>% dplyr::select(-MINUTE)), 'tbl_df')
})
