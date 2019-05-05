# Pre Main is the place to preliminarily process data gathered from the reader

# Coerce string of date to actual date
wbt$wbt_date = as.Date(wbt$wbt_date)

# Rename Columns
wbt %<>%
  rename(
    dates = wbt_date,
    times = wbt_time,
    values = wet_bulb_temperature
  )

wbt$dates.year <- year(wbt$dates)
wbt$dates.month <- month(wbt$dates)
wbt$dates.day <- day(wbt$dates)