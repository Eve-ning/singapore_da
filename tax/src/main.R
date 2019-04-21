require(ggplot2)
require(stringr)
require(magrittr)
require(directlabels)
require(grid)

# Load in data
tax.path <- "C:\\Users\\johnc\\Documents\\Projects\\Data\\singapore\\tax\\main.csv"
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

# Draw Plot
ggplot(tax.resident,
       aes(x = factor(year_of_assessment), # We use factor to force all x labels
           number_of_taxpayers,
           colour = assessed_income_group) # Color by Levels
       ) +
  geom_point() + # Add Points
  geom_line(aes(group = assessed_income_group)) + # Add Lines
  ylab("Number of Tax Payers (*1000)") + # Y Label
  xlab("Year of Assessment") + # X Label
  ggtitle(label = "Taxable Individuals by Assessable Income Group, Annual",
          subtitle = "By Income Groups") +
  scale_y_continuous(labels = function(y)y/1000.0) + # Scale Y Axis by /1000
  scale_x_discrete(expand = expand_scale(add = c(0.3,2))) + 
  theme(legend.position = "none", # Remove Legends
        plot.margin = unit(c(1,2,1,2),"cm")
        ) + 
  geom_dl( # Draw Labels
    aes(label = sprintf("$%s/y", comma(assessed_income_group))),
    method = list(last.bumpup,
                  cex = 1.1,
                  dl.trans(x=x+0.2)
                  )) 


            