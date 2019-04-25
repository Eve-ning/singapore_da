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
  enrolment <- enrolment.M %>% 
    rbind(enrolment.F)
  
  # Clear unused data
  rm(enrolment.MF, enrolment.M, enrolment.F)
  
  # As an additional step to fixing the dataset,
  # we melt the numeric columns to their respective type
  enrolment.melt -> enrolment %>% 
    melt(value.name = "persons",
         variable.name = "type",
         id.vars = 1:3)
}

# enrolment.sum
{
  enrolment.sum <- enrolment.melt %>%
    group_by(year, type) %>% 
    summarise(persons = sum(persons))
  
  ggplot(enrolment.sum) +
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
  
  enrolment.sum <- enrolment %>%
    group_by(year, type) %>% 
    summarise(persons = sum(persons))
  
  ggsave("../img/enrolment_sum.png")
  rm(enrolment.sum)
}

  
}

save.image(file = "enrolment.RData")