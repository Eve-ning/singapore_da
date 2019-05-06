source("libs.R") # Load Libraries
source("reader.R") # Read Source
source("preview.R") # Grab preview functions

source("modelling.R") # This will grab all the modelling functions
source("plotting.R")

wbt <- wbt.normalize(wbt)

# Generates the loess comparison plot
wbt.plot.loess.comp(wbt)

# Generate for Everything
wbt.plot.delta(wbt,
               filename = "period.png",
               scaling = 1.2,
               smoothing = F)

# Generate for 2015
wbt.plot.delta(wbt,
               year.start = 2015,
               filename = "2015.png",
               smooth = 0.3,
               scaling = 8)

# Generate for 2017
wbt.plot.delta(wbt,
               year.start = 2017,
               filename = "2017.png",
               smooth = 0.5,
               scaling = 8)

summary(wbt$delta)
wbt <- wbt[order(wbt$dates),]

y = wbt$delta
x = wbt$dates
sde <- spectrum(wbt$delta)
per <- 1/sde$freq[sde$spec == max(sde$spec)]


ggplot(wbt) +
  aes(x = dates,
      y = delta) +
  geom_point() +
  geom_line(data = reslm.pred, aes(x = dates,
                                   y = val))
  
lines(fitted(reslm)~wbt$delta,col=4,lty=2)


save.image('workspace.RData')
