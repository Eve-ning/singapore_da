# This creates a loess model of the data
wbt.normalize <- function(df) {
  
  # Aggregate by Month
  df <-
    aggregate(cbind(values, dates) ~ dates.year + dates.month,
              data = df,
              mean)
  
  # Create a local regression model
  pred <- predict(
    loess(values ~ dates,
          data = df,
          span = 0.5)
  )
  
  # CBind with original df
  df <- cbind(df, pred)
  
  # Grab delta
  df$delta = df$values - df$pred
  
  return(df)
}
