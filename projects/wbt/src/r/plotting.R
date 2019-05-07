
# This generates the period delta plots
wbt.plot.delta <- function(df,
                           year.start = 1982,
                           year.end = 2019,
                           filename = "period.png",
                           smooth = 0.5,
                           smoothing = T,
                           scaling = 8) {
  # Filter dates here
  df$dates %<>%
    as.Date()
  
  df %<>%
    subset(dates.year >= year.start &
           dates.year <= year.end)
  
  p <- ggplot(df) +
    aes(x = as.Date(dates),
        y = delta) +
    geom_line(color = 'blue') +
    labs(x = "Dates",
         y = "WBT Delta") +
    scale_x_date(breaks = pretty_breaks(n=10))
  
  # Smoothing is optional
  if (smoothing) {
    p <- p + stat_smooth(color = 'red',
                         span = smooth,
                         se = F,
                         size = 0.5,
                         linetype = 'dashed')
  }
  
  ggsave(paste("../img/", filename, sep = ""),
         plot = p,
         width = scaling * (year.end - year.start),
         height = 8,
         dpi = 150,
         units="cm",
         limitsize = F)
  
  
}

# This generates the plot comparing the loess to the original dataset
wbt.plot.loess.comp <- function(df) {
  
  p <- ggplot(df) +
    aes(x = as.Date(dates),
        y = values) +
    geom_line(color = 'blue') +
    geom_line(aes(x = as.Date(dates),
                  y = pred),
              color = 'red',
              linetype = 'dotted',
              size = 1) +
    labs(x = "Dates",
         y = "WBT")
  
  ggsave("../img/loess_est.png",
         width = 23,
         height = 15,
         units = "cm",
         dpi = 150)
}
