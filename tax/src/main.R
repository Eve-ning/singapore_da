require(ggplot2)
require(stringr)
require(magrittr)
require(directlabels)
require(grid)
require(scales)
require(plyr)
require(reshape2) # dcast

# Load in data
tax.path <- "C:\\Users\\johnc\\Documents\\Projects\\Data\\singapore\\tax\\2017\\main.csv"
tax <- read.csv(tax.path, header = T, sep = ',', stringsAsFactors = T)

# Sort Levels correctly
{
  tax.levels <- data.frame(levels(tax$assessed_income_group)) # Access Levels
  
  tax.levels.spl <-
    tax.levels$levels.tax.assessed_income_group. %>%
      str_split_fixed(pattern = " ", n = 3) # Split Levels by space
  
  tax.levels.spl <- tax.levels.spl[,1] # Grab first col
  
  tax.levels.spl %<>%  
    gsub(pattern = ",", replacement = "") %>% 
    as.numeric # Replace commas, then map to numeric. e.g. "1,000" -> "1000" -> 1000
  
  tax.levels <- cbind(tax.levels, tax.levels.spl) # Join both df to sort
  
  tax.levels <- tax.levels[order(tax.levels$tax.levels.spl),] # Sort by numeric
  
  tax$assessed_income_group %<>%  # Apply to main df
    factor(levels = paste(tax.levels$levels.tax.assessed_income_group.))
}

# Drop all non-Tax residents
tax.resident = subset(tax, tax$resident_type == "Tax Resident") 

#### STATIC ANALYSIS

# Draw Plot
if (!file.exists("img/tax_plot.png")){
  ggplot(tax.resident,
         aes(x = factor(year_of_assessment), # We use factor to force all x labels
             number_of_taxpayers,
             colour = assessed_income_group) # Color by Levels
         ) +
    geom_point() + # Add Points
    geom_line(aes(group = assessed_income_group)) + # Add Lines
    ylab("Number of Tax Payers (*1000)") + # Y Label
    xlab("Year of Assessment") + # X Label
    ggtitle(label = "Number of Taxpayers By Income Groups",
            subtitle = "From \"Taxable Individuals by Assessable Income Group, Annual\", data.gov.sg") +
    scale_y_continuous(labels = function(y)y/1000.0) + # Scale Y Axis by /1000
    scale_x_discrete(expand = expand_scale(add = c(0.3,5))) + 
    theme(legend.position = "none", # Remove Legends
          plot.margin = unit(c(1,2,1,2),"cm")
          ) + 
    geom_dl( # Draw Labels
      aes(label = sprintf("$%s/y", assessed_income_group)),
      method = list(last.bumpup,
                    cex = 1,
                    dl.trans(x=x+0.2)
                    )) 
  # Save Plot
  ggsave("img/tax_plot.png",width = 23, height = 15, dpi = 100, units="cm")
}

tax.resident %<>% 
  group_by(year_of_assessment) %>% 
  mutate(dist = round(number_of_taxpayers / sum(number_of_taxpayers) * 100, digits = 2))

# Create distribution animation
if (!file.exists("img/dist.gif")) { 
  p <- ggplot(tax.resident) + # Create bar graph
    aes(x = assessed_income_group, y=dist) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90)) +
    xlab("Income Group") +
    ylab("Distribution of Taxpayers") +
    ggtitle("Income Group Distribution from 2004 to 2017") +
    geom_text(aes(label = percent(dist/100)), nudge_y = 1) 

  p
  anim <- p +
    transition_time(time = year_of_assessment) +
    labs(title = "Year: {frame_time}",
         subtitle = "Income Group Distribution from 2004 to 2017")
  
  animate(anim, fps = 15, duration = 12)
  anim_save("img/dist.gif")
}

#### GROWTH ANALYSIS

# Calculate Growth
tax.resident.grw <- ddply(tax.resident,"assessed_income_group",transform,
                          growth=c(NA, exp(diff(log(number_of_taxpayers)))-1))

# Select important columns only
tax.resident.grw %<>% 
  select(year_of_assessment,
         assessed_income_group,
         growth)

# Replace NA with 0
tax.resident.grw[is.na(tax.resident.grw)] <- 0 


# Make Wide Table
{
  # Unstack on Year
  tax.resident.grw.wide <- tax.resident.grw %>%  
    dcast(formula = assessed_income_group ~ year_of_assessment, value.var = "growth")
  
  # Convert to percent and round
  tax.resident.grw.wide %<>% 
    mutate_if(is.numeric, round, digits = 3) %>% 
    mutate_if(is.numeric, percent, trim = T)
}

# Save Environment
# RMD Requires data from Wide Table
save.image("tax.RData")
