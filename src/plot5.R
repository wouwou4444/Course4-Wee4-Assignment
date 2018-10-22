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

#### Other solutions and submissions

#============================================================================
#=================Question 5=================================================

SCC_Vehicle <- SCC[grep("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE),]

NEI_Vehicle <- inner_join(NEI,SCC_Vehicle, by = "SCC")


Total_Vehicle_Emissions_Year <- NEI_Vehicle %>% 
  filter(fips=="24510") %>%
  group_by(year) %>%
  summarise(Total_Emissions = sum(Emissions))


ggplot(Total_Vehicle_Emissions_Year, aes(x = year, y = Total_Emissions)) +
  geom_point(size = 2) + geom_line(col = "blue", size = 1) +
  labs(title="Total Vehicle Related" ~ PM[2.5] ~ "Emission in Baltimore City, Maryland", x ="Year", y = "Total Emissions in Tons")



dev.copy(png, file = "plot5.png")
dev.off()

rm(NEI_Vehicle)
rm(SCC_Vehicle)
rm(Total_Vehicle_Emissions_Year)


#############

library(dplyr)
library(ggplot2)
library(stringr)

url1 <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
destfile1 <- "destfile.zip"

if(!file.exists(destfile1)) {
  download.file(url1, 
                destfile = destfile1, 
                method = "curl")
  unzip(destfile1, exdir = ".")
}


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

vehicle.scc <- SCC[grep("[Vv]eh", SCC$Short.Name), ]

emissions.motor.baltimore <- NEI %>% 
  subset(fips == "24510" & NEI$SCC %in% vehicle.scc$SCC) %>%
  merge(y = vehicle.scc, by.x = "SCC", by.y = "SCC") %>%
  group_by(year) %>%
  summarize(Vehicle.Emissions.Type = sum(Emissions, na.rm = TRUE))


emissions.motor.baltimore.plot <- ggplot(emissions.motor.baltimore, aes(year, Vehicle.Emissions.Type)) +
  geom_point(color = "red", 
             size = 4, 
             alpha = 1/3) + 
  xlab("Year") +
  ylab("Total Emissions [Tons]") +
  ggtitle("Total Annual Vehicle Emissions in Baltimore City")

emissions.motor.baltimore.plot