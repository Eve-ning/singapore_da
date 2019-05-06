source("libs.R") # Load Libraries
source("reader.R") # Read Source
source("preview.R") # Grab preview functions

source("modelling.R") # This will grab all the modelling functions
wbt <- wbt.normalize(wbt)

source("plotting.R")

wbt.plot.loess.comp(wbt)

# Generate for Everything
wbt.plot.delta(wbt,
               filename = "period.png",
               smooth = 0.04,
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
