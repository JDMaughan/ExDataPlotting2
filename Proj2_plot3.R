## Exploratory Data Analysis Course Project 2, plot 3
##
library(dplyr)
library(ggplot2)
NEIFile <- "./Data/summarySCC_PM25.rds"
SCCFile <- "./Data/Source_Classification_Code.rds"

## If either NEI File or SCC File aren't present, download and unzip the original data
if (!file.exists(NEIFile) || !file.exists(SCCFile)) {
        download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", 
                      destfile = "./Data/exdata-data-NEI_data.zip", 
                      mode = "wb")
        unzip("./Data/exdata-data-NEI_data.zip", overwrite = TRUE, exdir = ".")
} 

## Read the two files if they haven't been read into the environment
if (!nrow(NEI) == 6497651){
        NEI <- readRDS(NEIFile)   
}
if (!nrow(SCC) == 11717){
        SCC <- readRDS(SCCFile)
}

## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
## which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
## Which have seen increases in emissions from 1999–2008? 
## Use the ggplot2 plotting system to make a plot answer this question.
BaltimoreCity <- filter(NEI, fips == "24510") 

balEmissionByType <- group_by(BaltimoreCity, type, year) %>%
                summarize(AnnualTotalForType = sum(Emissions))

qplot(balEmissionByType$year, balEmissionByType$AnnualTotalForType, data=balEmissionByType,
        facets = . ~ type, 
        geom = c("point", "smooth"), method = "lm", geom_smooth(fill=NA),
        margins = TRUE,
      ##  ylim = c(0, 2500),
        xlab = "Year", 
        ylab = "PM2.5 Emissions (in tons)",
        main = "Baltimore City Emissions by Type\n")

## Copy graphic device to a .png file
dev.copy(png, file = "P2_plot3.png", width = 800, height = 600)
dev.off()
