##########################################################
##########################################################
###########    Question 6
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

## motor vehicle selection is based on on-road vehicles
# Mobile - On-Road
names(NEI)
names(SCC)
NEI_baltimore_losangeles <- subset(NEI,fips %in% c("24510","06037") & year %in% c(1999, 2008))
# NEI_baltimore_losangeles <- subset(NEI,fips == "24510" )
dim(NEI_baltimore_losangeles)
table(NEI_baltimore_losangeles$year, NEI_baltimore_losangeles$fips)

indices <- grep(pattern = "Mobile - On-Road", x= SCC$EI.Sector, ignore.case = TRUE)
SCC_motor_vehicle <- SCC[indices,]

# Merge/Join
NEI_baltimore_losangeles_motor_vehicle <- merge(NEI_baltimore_losangeles, SCC_motor_vehicle, by = "SCC")
dim(NEI_baltimore_losangeles_motor_vehicle)
table(NEI_baltimore_losangeles_motor_vehicle$year, NEI_baltimore_losangeles_motor_vehicle$fips)

# compute total emission for baltimore by year and source type
total_emission_by_year_balt_la_motor_vehicle <- data.frame(tapply(NEI_baltimore_losangeles_motor_vehicle$Emissions, 
                                                                  INDEX = list(NEI_baltimore_losangeles_motor_vehicle$fips,
                                                                               NEI_baltimore_losangeles_motor_vehicle$year
                                                                  ),
                                                                  sum))

total_emission_by_year_balt_la_motor_vehicle$city <- rownames(total_emission_by_year_balt_la_motor_vehicle)
colnames(total_emission_by_year_balt_la_motor_vehicle) <- c('1999','2008','city')

df_baltimore_la <- melt(total_emission_by_year_balt_la_motor_vehicle, id=c("city"))
df_baltimore_la <- melt(total_emission_by_year_balt_la_motor_vehicle, id=c("city"), 
                        measure.vars = c("1999","2008"), 
                        value.name = "emission")
names(df_baltimore_la) <- c("city","year", "emissions")

dev.copy(png, file = "plot6.png")
ggplot(df_baltimore_la, aes(x=year, y=emissions, colour = city)) +
  geom_line(aes(group = city)) + 
  theme_minimal() +
  ylim(0,4200) +
  ylab("Total Emissions") +
  labs(title="Emissions Balt vs LA for Motor Vehicle") 
dev.off()


#################
# Other solution based on tidyverse
install.packages("tidyverse")

library(tidyverse)

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
NEI.filename <- "summarySCC_PM25.rds"
SCC.filename <- "Source_Classification_Code.rds"

if (!NEI.filename %in% dir() | !SCC.filename %in% dir()) {
  download.file(url, destfile = "data.zip")
  unzip("data.zip")
}

NEI <- readRDS(NEI.filename)

emissions <- NEI %>%
  filter(fips %in% c("24510","06037"), type == "ON-ROAD") %>%
  group_by(year, fips) %>%
  summarise(total = sum(Emissions)) %>%
  spread(year, total)

change.balt <- with(filter(emissions, fips == "24510"), `2008` - `1999`)
change.la <- with(filter(emissions, fips == "06037"), `2008` - `1999`)
changes <- c(Baltimore = change.balt, LA = change.la)

png(filename="plot6.png")
barplot(changes, 
        ylab = "Change in Emissions (tons)", 
        main = "Change in Motor Vehicle Emissions from 1999 to 2008")
dev.off()