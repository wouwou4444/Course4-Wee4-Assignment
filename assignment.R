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

##########################################################
##########################################################
####            Question 2
names(NEI)

# subset data for Baltimore city
NEI_baltimore <- subset(NEI,fips == 24510 & year %in% c(1999, 2008))
dim(NEI_baltimore)
table(NEI_baltimore$year)
head(table(NEI_baltimore$SCC, NEI_baltimore$year), 100)

# compute total emission foro baltimore by year
total_emission_by_year_balt <- tapply(NEI_baltimore$Emissions, NEI_baltimore$year, sum)
total_emission_by_year_balt

dev.copy(png, file = "plot2.png")
plot(names(total_emission_by_year_balt), 
     total_emission_by_year_balt, 
     main = "Total Emissions in Baltimore(1999,2008)",
     xlab = "year",
     ylab="Emissions",
     ylim = c(0,4000),
     pch = 19,
     lwd = 5)
dev.off()

##########################################################
##########################################################
####              Question 3
library(ggplot2)
library(reshape2)

names(NEI)

# subset data for Baltimore city
NEI_baltimore <- subset(NEI,fips == 24510 & year %in% c(1999, 2008))
dim(NEI_baltimore)
table(NEI_baltimore$year)
head(table(NEI_baltimore$SCC, NEI_baltimore$year), 100)

# compute total emission for baltimore by year and source type
total_emission_by_year_balt <- tapply(NEI_baltimore$Emissions, 
                                      INDEX = list(NEI_baltimore$type, NEI_baltimore$year),
                                      sum)

total_emission_by_year_balt

df_baltimore <- melt(total_emission_by_year_balt, id=c("1999","2008"))
names(df_baltimore) <- c("type", "year", "emission")

dev.copy(png, file = "plot3.png")
ggplot(as.data.frame(df_baltimore), 
       aes(x= year, y = emission,
           colour = type), 
       xlab = "year",
       ylab="Emissions"     ) + 
  labs(title = "Total Emissions by source type in Baltimore(1999,2008)") +
  geom_line() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14, color = "red" ))
dev.off()


##########################################################
##########################################################
############         Question 4
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

##########################################################
##########################################################
###########    Question 5

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
  ylab("Total Emissions(thousands)") +
  labs(title="Emissions in Balt for Motor Vehicle") 
dev.off()

##########################################################
##########################################################
###########    Question 6

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
  ylab("Total Emissions(thousands)") +
  labs(title="Emissions in Balt for Motor Vehicle") 
dev.off()


