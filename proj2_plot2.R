## Exploratory Data Analysis Course Project 2, plot 2
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

## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
## (fips == "24510") from 1999 to 2008? 
## Use the base plotting system to make a plot answering this question.
BaltimoreCounty <- filter(NEI, fips == "24510") 

balEmissionByYear <- aggregate(Emissions ~ year, BaltimoreCounty, FUN = "sum")

plot(balEmissionByYear, 
     type = "l", 
     xlab = "Year", 
     ylab = "Emmissions (in tons)",
     main = "Total Emissions by Year \n Baltimore City: FIPS 24510",
     col = "red",
     lwd = 3)

## Copy graphic device to a .png file
dev.copy(png, file = "P2_plot2.png", width = 480, height = 480)
dev.off()