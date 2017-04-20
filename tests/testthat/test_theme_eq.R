context('theme_eq')

test_that('theme_eq adds some of the expected theme elements', {

  p <- ggplot2::qplot(1:4, 1:4)
  p <- p + theme_eq()

  expect_equal(p$theme$legend.position, 'bottom')
  expect_equal(p$theme$axis.line.x, element_line())

})
