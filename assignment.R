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
plot(names(total_emission_by_year), 
     total_emission_by_year/1000000, 
     main = "Total Emissions in USA(1999 to 2008)",
     xlab = "year",
     ylab="Emissions(Millions of PM)",
     ylim = c(0,8),
     pch = 19,
     lwd = 5)

par(mar=c(0.2,4,0.2,4))


##########################################################
# with ggplot2
# ggplot(NEI, 
#        aes(x= year, 
#            y= Emissions/1000000)) + 
#   theme_minimal()+
#   stat_summary(fun.y = sum, na.rm = TRUE, geom ='point')

#### Question 2
names(NEI)

NEI_baltimore <- subset(NEI,fips == 24510 & year %in% c(1999, 2008))
dim(NEI_baltimore)
table(NEI_baltimore$year)
head(table(NEI_baltimore$SCC, NEI_baltimore$year), 100)

# with base system
total_emission_by_year_balt <- tapply(NEI_baltimore$Emissions, NEI_baltimore$year, sum)
total_emission_by_year_balt
plot(names(total_emission_by_year_balt), 
     total_emission_by_year_balt, 
     main = "Total Emissions in Baltimore(1999,2008)",
     xlab = "year",
     ylab="Emissions",
     ylim = c(0,4000),
     pch = 19,
     lwd = 5)

#### Question 3
names(NEI)

NEI_baltimore <- subset(NEI,fips == 24510 & year %in% c(1999, 2008))
dim(NEI_baltimore)
table(NEI_baltimore$year)
head(table(NEI_baltimore$SCC, NEI_baltimore$year), 100)

# with base system
total_emission_by_year_balt <- tapply(NEI_baltimore$Emissions, 
                                      INDEX = list(NEI_baltimore$type, NEI_baltimore$year),
                                      sum)
# total_emission_by_year_balt <- tapply(NEI_baltimore$Emissions, 
#                                       INDEX = list(NEI_baltimore$year, NEI_baltimore$type),
#                                       sum)

library(reshape2)
library(ggplot2)
dcast(total_emission_by_year_balt)
total_emission_by_year_balt

df_baltimore <- melt(total_emission_by_year_balt, id=c("1999","2008"))
names(df_baltimore) <- c("type", "year", "emission")


ggplot(as.data.frame(df_baltimore), 
       aes(x= year, y = emission,
           colour = type), 
       xlab = "year",
       ylab="Emissions"     ) + 
  labs(title = "Total Emissions by source type in Baltimore(1999,2008)") +
  geom_line() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14, color = "red" ))

# Question 4
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
ggplot(NEI_1999_2008_coal, aes(x=year, y=Emissions/1000)) +
  theme_minimal() +
  ylim(0,600) +
  ylab("Total Emissions(thousands)") +
  labs(title="Emissions in USA for Combustion Fuel based on coal") +
  stat_summary(fun.y = sum, geom ='line')
