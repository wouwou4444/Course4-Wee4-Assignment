##########################################################
##########################################################
####              Question 3
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
