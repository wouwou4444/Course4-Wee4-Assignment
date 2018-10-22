##########################################################
##########################################################
############         Question 4
library(ggplot2)
library(reshape2)

### Download data
sourcefile <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
destfile <- "./dataEPA.zip"

getwd()
download.file(url = sourcefile, destfile = destfile)
unzip(destfile)

dir(getwd())

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#########################################################

# Based on section 5 of https://www.epa.gov/sites/production/files/2015-07/documents/aerr_final_rule.pdf
# we can assume that EI.sector contain the information on Fuel combustion *** coal
indices_coal <- grep(pattern = "fuel.*coal", x = SCC$EI.Sector, ignore.case = TRUE)
scc_coal <- SCC[indices_coal,]

NEI_1999_2008 <- subset(NEI, year %in% c(1999, 2008))
table(NEI_1999_2008$year)
names(NEI_1999_2008)
names(SCC)
NEI_1999_2008_coal <- merge(NEI_1999_2008, scc_coal, by = "SCC")
table(NEI_1999_2008_coal$year)

dev.copy(png, file = "plot4.png")
ggplot(NEI_1999_2008_coal, aes(x=year, y=Emissions/1000)) +
  theme_minimal() +
  ylim(0,600) +
  ylab("Total Emissions(thousands)") +
  labs(title="Emissions in USA for Combustion Fuel based on coal") +
  stat_summary(fun.y = sum, geom ='line')
dev.off()
