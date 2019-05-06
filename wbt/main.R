source("libs.R") # Load Libraries
source("reader.R") # Read Source
source("preview.R") # Grab preview functions

wbt.fn.month.loess <- function() {
  
  # Aggregate by Month
  month.mean <-
    aggregate(cbind(values, dates) ~ dates.year + dates.month,
              data = wbt,
              mean)
  
  # Create a local regression model
  pred <- predict(
    loess(values ~ dates,
          data = month.mean,
          span = 0.5)
  )
  
  # CBind with original df
  month.mean <- cbind(month.mean,
                      pred)
  
  # Grab delta
  month.mean$delta = month.mean$values - month.mean$pred
  
  return(month.mean)
}

wbt.delta <- wbt.fn.month.loess()

ggplot(wbt.delta) +
  aes(x = as.Date(dates),
      y = values) +
  geom_point() +
  geom_line()

