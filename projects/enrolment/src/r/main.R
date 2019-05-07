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
 
# --- Some useful lists we will calculate globally

enrolment.courses = unique(enrolment.df$course)

# ---

# enrolment.sum
fn.sum <- function(){
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
    ) +
    labs(y = "Students",
         x = "Year")
  
  ggsave("../img/enrolment_sum.png",
         plot = enrolment.sum.p,
         width = 23,
         height = 15,
         dpi = 150,
         units = "cm")
}

# enrolment.intake
fn.intake <- function(){
  # We need to combine both M and F
  enrolment.intake <<-
    aggregate(cbind(intake, enrolment, graduates) ~ year + course, data = enrolment.df, sum)

  # We will need to filter
  enrolment.intake.fil <- 
    subset(enrolment.intake,
      # We want to automatically set this as the last year
      # if there are newer data
      year == max(year) & 
      intake > 750 # However, this is best done manually
      )['course']
  
  enrolment.intake.fil <- subset(enrolment.intake,
                                 course %in% enrolment.intake.fil$course)
  
  enrolment.intake.p <- ggplot(enrolment.intake.fil) +
    aes(x = factor(year),
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
    ) +
    labs(y = "Intake",
         x = "Year")
  
  ggsave("../img/enrolment_intake.png",
         plot = enrolment.intake.p,
         width = 30,
         height = 15,
         dpi = 150,
         units = "cm")
}

# enrolment.intake.rate
fn.intake.rate <- function(){
  enrolment.intake$intake_rate <- enrolment.intake$intake / enrolment.intake$enrolment
  enrolment.intake_rate = enrolment.intake[c("year", "course", "intake_rate")]
  for (crs in enrolment.courses){
    
    enrolment.intake.crs <- subset(enrolment.intake_rate,
                                   course == crs)
    enrolment.intake.crs[is.na(enrolment.intake.crs)] <- 0
    
    enrolment.intake.crs.p <- ggplot(enrolment.intake.crs) +
      aes(x = year, 
          y = intake_rate) +
      geom_area(aes(group = course),
                alpha = 0) + 
      stat_smooth(
        geom = 'area',
        method = 'loess',
        span = 0.25,
        fill = 'red',
        alpha = 0.5) +
      scale_x_continuous(breaks = pretty_breaks()) +
      ggtitle(crs) +
      ylim(c(0, 0.7)) +
      labs(y = "Intake Rate",
           x = "Year") +
      theme_minimal()
    
    enrolment.intake.crs.p
    
    ggsave(paste("../img/frags/intake_rate/", crs, ".png", sep = ""),
           plot = enrolment.intake.crs.p,
           width = 23,
           height = 10,
           dpi = 150,
           units = "cm")
  }
}

save.image(file = "enrolment.RData")