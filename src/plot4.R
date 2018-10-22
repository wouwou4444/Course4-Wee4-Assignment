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


#### OTher solutions or submissions

NEI <- readRDS("SummarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# Plot total PM25 emissions from coal combustion-related sources for the years 1999, 2002, 2005 and 2008
combsubset <- SCC[grepl("Comb", SCC$EI.Sector),]
coalcombsubset <- combsubset[grepl("Coal",combsubset$EI.Sector),]
both <- intersect(NEI$SCC,coalcombsubset$SCC)
coalsubset <- subset(NEI, SCC %in% both)
totalcoalbyyear <- aggregate(Emissions ~ year, coalsubset, sum)
barplot(height = totalcoalbyyear$Emissions, names.arg = totalcoalbyyear$year,xlab= "Year", ylab="PM2.5 Emissions in Tons")
title(main="Coal Combustion-Related Emissions in the U.S.")
dev.copy(png, file= "plot4.png")
dev.off()
