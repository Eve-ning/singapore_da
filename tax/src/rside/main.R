require(ggplot2)

tax.path <- "C:\\Users\\johnc\\Documents\\Projects\\Data\\singapore\\tax\\main.csv"

tax <- read.csv(tax.path, header = T, sep = ',', stringsAsFactors = T)

tax.levels = levels(tax$assessed_income_group)

ggplot(tax, aes(year_of_assessment, number_of_taxpayers, colour = assessed_income_group)) + geom_point()
            