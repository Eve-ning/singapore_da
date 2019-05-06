source("libs.R") # Load Libraries
source("reader.R") # Read Source
source("preview.R") # Grab preview functions

source("modelling.R") # This will grab all the modelling functions
wbt <- wbt.normalize(wbt)

source("plotting.R")

wbt.plot.loess.comp(wbt)
wbt.plot.delta(wbt)
