## Exploratory Data Analysis Course Project 2, plot 1
##
library(dplyr)
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

## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
## Using the base plotting system, make a plot showing the total PM2.5 emission from 
## all sources for each of the years 1999, 2002, 2005, and 2008.

aggTotEmissionByYear <- aggregate(Emissions ~ year, NEI, FUN = "sum")

plot(aggTotEmissionByYear, 
     type = "l", 
     xlab = "Year", 
     ylab = "Emmissions (in tons)",
     main = "Total Emissions by Year")

## Copy graphic device to a .png file
dev.copy(png, file = "P2_plot1.png", width = 800, height = 800)
dev.off()


