wbt.path <- "C:/Users/johnc/Documents/Projects/Data/singapore/wbt/main.csv"

wbt <- read.csv(wbt.path) 

library(ggplot2)
library(magrittr)
library(directlabels)
library(scales)
library(dplyr)

# Convert Char to Date, then to Numeric
wbt$wbt_date = as.numeric(as.Date(wbt$wbt_date))

# Merge both Date and Time
wbt$wbt_datetime = wbt$wbt_date + wbt$wbt_time/24

wbt %<>%
  rename(
    date = wbt_date,
    time = wbt_time,
    datetime = wbt_datetime,
    value = wet_bulb_temperature
  )

wbt 
# Aggregate by Date, too much variation by hour

ggplot(head(wbt,1000)) +
  aes(x = datetime,
      y = value) +
  geom_point() +
  geom_line()
