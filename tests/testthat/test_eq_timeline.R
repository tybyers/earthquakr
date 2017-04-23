context('eq_timeline shortcut')

test_that('geom_timeline works, allows 2 countries', {
  quakes <- eq_load_clean_data()

  p1 <- eq_timeline(quakes)
  expect_s3_class(p1, 'ggplot')
  out1 <- layer_data(p1)
  expect_length(p1$layers, 1)
  expect_length(unique(out1$group), 1)
  #expect_true()

  # can handle lowercase country names and 5 labels:
  p2 <- eq_timeline(quakes, countries = c('USA', 'canada'), label_n = 5)
  expect_length(p2$layers, 2)
  out2 <- layer_data(p2, 2)
  expect_true('label' %in% names(out2))

  # date range change works
  p3 <- eq_timeline(quakes, countries = c('USA', 'canada'),
                    date_min = '1950-01-01', label_n = 5)
  out3 <- layer_data(p3, 2)
  expect_gt(nrow(out3), nrow(out2))

})


