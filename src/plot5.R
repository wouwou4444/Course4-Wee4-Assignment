##########################################################
##########################################################
###########    Question 5
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
NEI_baltimore <- subset(NEI,fips == 24510 & year %in% c(1999, 2008))
dim(NEI_baltimore)
table(NEI_baltimore$year)
head(table(NEI_baltimore$SCC, NEI_baltimore$year), 100)

indices <- grep(pattern = "Mobile - On-Road", x= SCC$EI.Sector, ignore.case = TRUE)
SCC_motor_vehicle <- SCC[indices,]

# Merge/Join
NEI_baltimore_motor_vehicle <- merge(NEI_baltimore, SCC_motor_vehicle, by = "SCC")
dim(NEI_baltimore_motor_vehicle)
table(NEI_baltimore_motor_vehicle$year)

# compute total emission for baltimore by year and source type
total_emission_by_year_balt_motor_vehicle <- tapply(NEI_baltimore_motor_vehicle$Emissions, 
                                                    INDEX = list(NEI_baltimore_motor_vehicle$year),
                                                    sum)

df_baltimore <- melt(total_emission_by_year_balt_motor_vehicle, id=c("1999","2008"))
names(df_baltimore) <- c("year", "emissions")

dev.copy(png, file = "plot5.png")
ggplot(df_baltimore, aes(x=year, y=emissions)) +
  geom_line() + 
  theme_minimal() +
  ylim(0,400) +
  ylab("Total Emissions") +
  labs(title="Emissions in Balt for Motor Vehicle") 
dev.off()
