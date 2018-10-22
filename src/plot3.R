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

########
# Other solution using Tidyverse
# install.packages("tidyverse")
library(tidyverse)

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
NEI.filename <- "summarySCC_PM25.rds"
SCC.filename <- "Source_Classification_Code.rds"

if (!NEI.filename %in% dir() | !SCC.filename %in% dir()) {
  download.file(url, destfile = "data.zip")
  unzip("data.zip")
}

NEI <- readRDS(NEI.filename)

png(filename = "plot3.png")
NEI %>% 
  filter(fips == "24510", year %in% c("1999", "2008")) %>%
  group_by(year, type) %>%
  summarize(total = sum(Emissions)) %>%
  ggplot(aes(type, total, fill = as.factor(year))) + 
  geom_col(position = "dodge") +
  labs(x = "Emission Type",
       y = "Total emissions (tons)", 
       title = "Baltimore emissions by type",
       fill = "Year")
dev.off()

#### Other solution

## Question 3
if(!exists("NEI")){
  NEI <- readRDS("./data/summarySCC_PM25.rds")
}
if(!exists("SCC")){
  SCC <- readRDS("./data/Source_Classification_Code.rds")
}

library(ggplot2)

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999 2008 for Baltimore City? 
# Which have seen increases in emissions from 1999 2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

# 24510 is Baltimore, see plot2.R
subsetNEI <- NEI[NEI$fips=="24510", ]

aggregatedTotalByYearAndType <- aggregate(Emissions ~ year + type, subsetNEI, sum)



png("plot3.png", width=640, height=480)
g <- ggplot(aggregatedTotalByYearAndType, aes(year, Emissions, color = type))
g <- g + geom_line() +
  xlab("year") +
  ylab(expression('Total PM'[2.5]*" Emissions")) +
  ggtitle('Total Emissions in Baltimore City, Maryland (fips == "24510") from 1999 to 2008')
print(g)
dev.off()



#####
#### other solution
# 3. Which of the four sources have seen decreases in emissions from 1999-2008 for Baltimore City?
  library(ggplot2)
temp = melt(NEI.Baltimore, id = c("year", "type"), measure.vars = "Emissions")
temp = dcast(temp, year+type~variable, sum)
ggplot(data = temp, aes(x = year, y = Emissions, group = type, col = type)) + 
  geom_line() + geom_point(size = 2, shape = 21, fill = "white") + 
  xlab("Year") + ylab("Emissions") + ggtitle("Emissions from PM2.5 in the Baltimore City")



