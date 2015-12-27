## Exploratory Data Analysis Course Project 2, plot 6
##
library(dplyr)
library(ggplot2)
library(grid)

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

## Compare emissions from motor vehicle sources in Baltimore City with emissions from 
## motor vehicle sources in Los Angeles County, California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?
## 
## Assumption: For this plot, "motor vehicle sources" are assumed to be of Type "ONROAD".   Aircraft, locomotives,
## and marine vessels were in NONROAD before 2008 but now are in ONROAD from 2008 onward. 
## (http://www3.epa.gov/ttn/chief/net/2008inventory.html#inventorydoc)
## 
## Filter SCC data for only motor vehicle sources
SCC.motorVehicleDetails <- filter(SCC, grepl("Mobile", SCC$EI.Sector, ignore.case=T))

## Create two data frames, one for Baltimore city (fips == 24510) and one for Los Angeles County (fips = 06037)
## Filter NEI data where 
## 1) fips = "24510"/"06037" --> Baltimore City/Los Angeles County and 
## 2) Type == ON-ROAD
## 
NEIBalt <- filter(NEI, fips == "24510" & type == "ON-ROAD")
NEIBalt$City <- "Baltimore City"
NEILAC <- filter(NEI, fips == "06037" & type == "ON-ROAD")
NEILAC$City <- "Los Angeles County"

## filter NEI data where NEI.SCC matches SCC.SCC        
NEIBaltMotorVeh <- filter(NEIBalt, NEIBalt$SCC %in% SCC.motorVehicleDetails$SCC)
NEILACMotorVeh <- filter(NEILAC, NEILAC$SCC %in% SCC.motorVehicleDetails$SCC)

NEIBoth <- rbind(NEIBaltMotorVeh, NEILACMotorVeh)

## Group by Year and FIPS, Sum the Emissions, and make Factor "year"  with initial caps.
NEIBothByFIPS <- group_by(NEIBoth, year, City) %>%
        summarize(AnnualEmission = sum(Emissions)) %>%
        mutate(Year = year)

## Create base ggplot object to add layers to
g <- ggplot(NEIBothByFIPS, aes(Year, AnnualEmission))

## Add layers and customize for a  line chart
g + geom_point(size = 4) +
        geom_line(size = 1) +
        geom_smooth(method = "lm", size = 1, se = T) +
        facet_grid(. ~ City) +
        xlab("Year") +
        ylab("Annual Emissions (in tons)") +
        ggtitle("Motor Vehicle Emissions\n Baltimore City vs. Los Angeles County") +
        scale_y_continuous(breaks = seq(0,5000,1000)) 

## Copy graphic device to a .png file
dev.copy(png, file = "P2_plot6.png", width = 480, height = 480)
dev.off()