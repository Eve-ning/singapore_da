# Pre Main is the place to preliminarily process data gathered from the reader

# Coerce string of date to actual date
wbt$wbt_date = as.Date(wbt$wbt_date)

# Rename Columns
wbt %<>%
  rename(
    date = wbt_date,
    time = wbt_time,
    value = wet_bulb_temperature
  )

wbt$date.year <- year(wbt$date)
wbt$date.month <- month(wbt$date)
wbt$date.day <- day(wbt$date)