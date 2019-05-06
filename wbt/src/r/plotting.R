wbt.plot.delta <- function(wbt) {
  
  p <- ggplot(wbt) +
    aes(x = as.Date(dates),
        y = delta) +
    geom_line(color = 'blue') +
    labs(x = "Dates",
         y = "WBT Delta")
  
  ggsave("../img/period.png",
         plot = p,
         width = 30,
         height = 4,
         dpi = 150,
         units="cm")
}

wbt.plot.loess.comp <- function(wbt) {
  
  ggplot(wbt) +
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
}
