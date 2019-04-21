require(ggplot2)
require(stringr)
require(magrittr)

# Load in data
tax.path <- "C:\\Users\\johnc\\Documents\\Projects\\Data\\singapore\\tax\\main.csv"
tax <- read.csv(tax.path, header = T, sep = ',', stringsAsFactors = T)


# Grab levels to sort them correctly
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

tax.resident = subset(tax, tax$resident_type == "Tax Resident") # Drop all non-Tax residents

tax.resident.ends = subset(tax,
                           year_of_assessment == max(tax$year_of_assessment) |
                             year_of_assessment == min(tax$year_of_assessment))


ggplot(tax.resident, aes(year_of_assessment, number_of_taxpayers, colour = assessed_income_group)) +
  geom_point() + geom_line(aes(group = assessed_income_group)) +
  ylab("Number of Tax Payers (*1000)") + xlab("Year of Assessment") +
  scale_y_continuous(labels = function(y)y/1000.0)



            