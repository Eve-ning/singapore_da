require(magrittr)
require(stringr)
require(magrittr)
require(directlabels)
require(grid)
require(scales)
require(dplyr)
require(reshape2)
require(ggplot2)

# Load in data
enrolment.path <- "C:\\Users\\johnc\\Documents\\Projects\\Data\\singapore\\enrolment\\2017\\main.csv"
enrolment.raw <- read.csv(enrolment.path, header = T, sep = ',', stringsAsFactors = T)

# Treat the data

# The Sex provided are in MF and F, we should replace MF's with M
{
  enrolment.MF <- subset(enrolment.raw, sex == "MF") # We extract MF
  enrolment.F <- subset(enrolment.raw, sex == "F")   # We extract F
  
  # Subtract off the numeric columns (4 - 6)
  enrolment.M <- enrolment.MF[,4:6] - enrolment.F[,4:6]
    
  # Column Bind back the non-numeric columns (1 - 3)
  
  # We bind via .F instead of binding to .F 
  # because it automatically fixes the issue of numeric columns
  # appearing before the non-numeric ones
  enrolment.M <- enrolment.F[,1:3] %>% 
    cbind(enrolment.M) %>% 
    mutate(sex = "M")
  
  # Join back .M and .F
  enrolment.df <- enrolment.M %>% 
    rbind(enrolment.F)
  
  # Clear unused data
  rm(enrolment.MF, enrolment.M, enrolment.F)
  
  # As an additional step to fixing the dataset,
  # we melt the numeric columns to their respective type
  enrolment.melt <- enrolment.df %>% 
    melt(value.name = "persons",
         variable.name = "type",
         id.vars = 1:3)
}

# enrolment.sum
{
  enrolment.sum <- enrolment.melt %>%
    group_by(year, type) %>% 
    summarise(persons = sum(persons))
  
  enrolment.sum.p <- ggplot(enrolment.sum) +
    aes(x = factor(year),
        y = persons,
        colour = type) +
    geom_point() +
    geom_line(aes(group = type)) +
    geom_dl(
      aes(label = sprintf("%sK", round(persons/1000, digits = 0))),
      method = list(cex =0.7,
                    dl.trans(x=x+0.2, y=y-0.2))
    )
  
  ggsave("../img/enrolment_sum.png",
         plot = enrolment.sum.p,
         width = 23,
         height = 15,
         dpi = 150,
         units = "cm")
  rm(enrolment.sum, enrolment.sum.p)
}

# enrolment.intake
{
  enrolment.intake <- enrolment.df
  enrolment.intake <-
    aggregate(cbind(intake, enrolment, graduates) ~ year + course, data = enrolment.intake, sum)
  enrolment.intake$intake_rate = enrolment.intake$intake / enrolment.intake$enrolment
  
  # Note that graduate rate is not viable as the intake population
  # is not the same as the graduate population in the same year.
  # enrolment.intake$graduate_rate = enrolment.intake$graduates / enrolment.intake$intake
  
  enrolment.intake.p <- ggplot(enrolment.intake) +
    aes(x = year,
        y = intake,
        colour = course) +
    geom_point() +
    geom_line(aes(group = course)) +
    scale_x_discrete(expand = expand_scale(add = c(0.3,5))) +
    geom_dl( # Draw Labels
      aes(label = course),
      method = list(last.bumpup,
                    cex = 0.8,
                    dl.trans(x=x+0.2)
      )) +
    theme(legend.position = "none", # Remove Legends
          plot.margin = unit(c(1,2,1,2),"cm")
    ) 
  
  ggsave("../img/enrolment_intake.png",
         plot = enrolment.intake.p,
         width = 30,
         height = 15,
         dpi = 150,
         units = "cm")
  
  rm(enrolment.intake.p)
  
  enrolment.intake_rate.p <- ggplot(enrolment.intake) +
    aes(x = year,
        y = intake_rate,
        colour = course) +
    geom_point() +
    geom_line(aes(group = course)) +
    scale_x_discrete(expand = expand_scale(add = c(0.3,5))) +
    geom_dl( # Draw Labels
      aes(label = course),
      method = list(last.bumpup,
                    cex = 0.8,
                    dl.trans(x=x+0.2)
      )) +
    theme(legend.position = "none", # Remove Legends
          plot.margin = unit(c(1,2,1,2),"cm")
    ) 
  
  ggsave("../img/enrolment_intake_rate.png",
         plot = enrolment.intake_rate.p,
         width = 23,
         height = 15,
         dpi = 150,
         units = "cm")
  
  rm(enrolment.intake_rate.p)
}

save.image(file = "enrolment.RData")