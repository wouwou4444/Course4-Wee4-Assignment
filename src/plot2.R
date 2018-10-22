##########################################################
##########################################################
####            Question 2

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