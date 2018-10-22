### Download data
sourcefile <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
destfile <- "./dataEPA.zip"

getwd()
download.file(url = sourcefile, destfile = destfile)
unzip(destfile)

dir(getwd())

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#### Question 1
names(NEI)

table(NEI$year)
head(table(NEI$SCC, NEI$year), 100)

# with base system
total_emission_by_year <- tapply(NEI$Emissions, NEI$year, sum)

dev.copy(png, file = "plot1.png")
plot(names(total_emission_by_year), 
     total_emission_by_year/1000000, 
     main = "Total Emissions in USA(1999 to 2008)",
     xlab = "year",
     ylab="Emissions(Millions of PM)",
     ylim = c(0,8),
     pch = 19,
     lwd = 5)
dev.off()


##########################################################
# with ggplot2
# ggplot(NEI, 
#        aes(x= year, 
#            y= Emissions/1000000)) + 
#   theme_minimal()+
#   stat_summary(fun.y = sum, na.rm = TRUE, geom ='point')


### Other Solutions
###


library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)


getwd()

if (!file.exists("data")){
  dir.create("data")
}

# 1) download the source data file
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
#download.file(url, destfile = "./data/Dataset.zip", mode = "wb")
rm(url)
# 2) Unzip file using unzip command

#unzip("./data/Dataset.zip", exdir = "./data")

# 3) Read RDS files 
NEI <- read_rds("./data/summarySCC_PM25.rds")
SCC <- read_rds("./data/Source_Classification_Code.rds")


#============================================================================
#=================Question 1=================================================

Total_Emissions_Year <- NEI %>% 
  group_by(year) %>%
  summarise(Total_Emissions = sum(Emissions))

plot(Total_Emissions ~ year, data = Total_Emissions_Year, 
     xaxt = "n",
     type = "b",
     col = "blue",
     ylab = "Total Emissions",
     xlab ="Year",
     main = "PM2.5 Emissions from 1999 to 2008")

axis(side = 1, at = c(1999,2002,2005, 2008))
dev.copy(png, file = "plot1.png")
dev.off()

rm(Total_Emissions_Year)