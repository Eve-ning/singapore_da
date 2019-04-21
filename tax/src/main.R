require(ggplot2)

tax.path <- "C:\\Users\\johnc\\Documents\\Projects\\Data\\singapore\\tax\\main.csv"

tax <- read.csv(tax.path, header = T, sep = ',', stringsAsFactors = T)

tax.levels = levels(tax$assessed_income_group)

tax.resident = subset(tax, tax$resident_type == "Tax Resident")

ggplot(tax.resident, aes(year_of_assessment, number_of_taxpayers, colour = assessed_income_group)) +
  geom_point() + geom_line(aes(group = assessed_income_group)) +
  ylab("Number of Tax Payers (*1000)") + xlab("Year of Assessment") +
  scale_y_continuous(labels = function(y)y/1000.0) +
  geom_text(aes(label=round(number_of_taxpayers/1000.0,0)), vjust = -1.5)
            