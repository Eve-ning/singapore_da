# Preview is where put most source previewing functions
# In other words, we view the source data visually here

create.preview <- function(df) {
  list(
    # This will give the preview for a moving average of 
    # a specified value
    monthly = function(val) {
      if (val <= 0) {
        stop("val must be positive and non-zero")
      }
      # Group by year and month
      wbt.month.mean <-
        aggregate(cbind(value, date) ~ date.year + date.month,
                  data = wbt,
                  mean)
      
      wbt.month.mean$date %<>% 
        as.Date()
      
      ggplot(wbt.month.mean) +
        aes(x = date,
            y = value) +
        geom_ma(n = val, colour = 'blue', linetype = 'solid') 
    },
    yearly = function(val) {
      if (val <= 0) {
        stop("val must be positive and non-zero")
      }
      
      # Group by Year
      wbt.year.mean <-
        aggregate(cbind(value, date) ~ date.year,
                  data = wbt,
                  mean)
      
      wbt.year.mean$date %<>% 
        as.Date()
      
      ggplot(wbt.year.mean) +
        aes(x = date,
            y = value) +
        geom_ma(n = 1, colour = 'blue', linetype = 'solid') 
      
      
    }
  )
}