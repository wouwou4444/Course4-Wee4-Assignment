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
