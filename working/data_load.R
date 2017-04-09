library(dplyr); library(readr); library(stringr)
quakes <- readr::read_delim('working/earthquakes.txt', delim = '\t')

names(quakes) <- tolower(names(quakes))
quakes <- quakes %>%
  mutate(longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    month = ifelse(is.na(month), 1, month),
    day = ifelse(is.na(day), 1, day))

# as.Date doesn't handle BCE dates, so have to estimate it as best as possible
quakes <- quakes %>%
  mutate(date = purrr::pmap(list(year, month, day), function(y, m, d) {
    if(y < 0) {
      # numeric date for first day of CE
      first_date <- as.numeric(as.Date('0 1 2', '%Y %m %d', origin = '1970-01-01'))
      # take the negative date and "mirror" it to CE Jan 1st
      mirror_date <- as.Date(paste(y * -1 - 1, 1, 1, sep = '-'), '%Y-%m-%d',
                             origin = '1970-01-01')
      dt <- as.numeric(mirror_date) - first_date

      # time to end of year
      end_of_year_dt <- as.numeric(as.Date(paste(y * -1 + 2, 12, 31, sep = '-'), '%Y-%m-%d',
                                           origin = '1970-01-01')) -
        as.numeric(as.Date(paste(y * -1 + 2, m, d, sep = '-'), '%Y-%m-%d',
                           origin = '1970-01-01'))
      date <- as.Date(first_date - dt - end_of_year_dt - 1, origin = '1970-01-01')
    } else {
      date <- as.Date(paste(y, m, d, sep = '-'), '%Y-%m-%d')
    }
    date
    })
  ) %>%  # date comes out of purrr::map as numeric
  mutate(date = unlist(date),
         date = as.Date(date, origin = '1970-01-01'))

## do the country name cleanup
quakes <- quakes %>%
  mutate(loc_clean =  # this needs to be its own function eq_loc_clean per directions...
           purrr::map2_chr(country, location_name, function(country, location_name) {
             gsub(paste0(country, ":"), '', location_name) }),
         loc_clean = stringr::str_to_title(loc_clean)
  )
