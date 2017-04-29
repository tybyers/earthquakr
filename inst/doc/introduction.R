## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 5,
                      fig.align = 'center')

## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github('tybyers/earthquakr')
#  library(earthquakr)

## ----message = FALSE-----------------------------------------------------
library(earthquakr) 
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

## ------------------------------------------------------------------------
quakes <- earthquakr::quakes # loads quakes data with data set
quakes <- quakes %>%
  eq_clean_data() %>%
  eq_location_clean()
tail(quakes)

## ------------------------------------------------------------------------
quakes <- eq_load_clean_data()
tail(quakes)

## ----message = FALSE-----------------------------------------------------
filename <- system.file('extdata', 'earthquakes.txt', package = 'earthquakr')
quakes_from_raw <- readr::read_delim(filename, delim = '\t')
quakes_from_raw_clean <- quakes_from_raw %>%
  eq_clean_data() %>%
  eq_location_clean()
tail(quakes_from_raw_clean)

## ------------------------------------------------------------------------
quakes <- eq_load_clean_data()

## ------------------------------------------------------------------------
quakes %>%
  dplyr::filter(COUNTRY == 'USA') %>%
  dplyr::filter(DATE > '2000-01-01') %>%
  ggplot() +
  geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
                    size = EQ_PRIMARY)) +
  scale_size_continuous(name = 'Richter scale value') +
  scale_color_continuous(name = '# of Deaths')

## ------------------------------------------------------------------------
quakes %>%
  dplyr::filter(COUNTRY %in% c('USA', 'UK')) %>%
  dplyr::filter(DATE > '2000-01-01') %>%
  ggplot() +
  geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
                    size = EQ_PRIMARY)) +
  scale_size_continuous(name = 'Richter scale value') +
  scale_color_continuous(name = '# of Deaths')

## ------------------------------------------------------------------------
quakes %>%
  dplyr::filter(COUNTRY %in% c('NEW ZEALAND', 'SOUTH AFRICA')) %>%
  dplyr::filter(DATE > '2000-01-01', DATE < '2015-01-01') %>%
  ggplot() +
  geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
                    size = EQ_PRIMARY)) +
  geom_timeline_label(aes(x = DATE, y = COUNTRY, magnitude = EQ_PRIMARY,
                         label = LOCATION_NAME, n_max = 5)) +
  scale_size_continuous(name = 'Richter scale value') +
  scale_color_continuous(name = '# of Deaths')

## ------------------------------------------------------------------------
quakes %>%
  dplyr::filter(COUNTRY %in% c('NEW ZEALAND', 'SOUTH AFRICA')) %>%
  dplyr::filter(DATE > '2000-01-01', DATE < '2015-01-01') %>%
  ggplot() +
  geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
                    size = EQ_PRIMARY)) +
  geom_timeline_label(aes(x = DATE, y = COUNTRY, magnitude = EQ_PRIMARY,
                         label = LOCATION_NAME, n_max = 5)) +
  scale_size_continuous(name = 'Richter scale value') +
  scale_color_continuous(name = '# of Deaths') +
  theme_eq()

## ------------------------------------------------------------------------
quakes %>% eq_timeline(countries = 'NEW ZEALAND', 
                       date_min = as.Date('1995-01-01'),
                       date_max = as.POSIXct('2015-01-01'), 
                       label_n = 0)

## ------------------------------------------------------------------------
quakes %>% eq_timeline(countries = c('NEW ZEALAND', 'HAITI'),
                       date_min = '2000-01-01', 
                       date_max = '2015-01-01',
                       label_n = 5)

## ------------------------------------------------------------------------
quakes %>%
  dplyr::filter(COUNTRY == 'JAPAN') %>%
  dplyr::filter(lubridate::year(DATE) >= 2000) %>%
  eq_map(annot_col = 'DATE')

## ------------------------------------------------------------------------
quakes %>%
  dplyr::filter(COUNTRY == 'MEXICO') %>%
  dplyr::filter(lubridate::year(DATE) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = 'popup_text')

