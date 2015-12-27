## Exploratory Data Analysis Course Project 2, plot 5
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

## How have emissions from *motor vehicle* sources changed from 1999–2008 in Baltimore City?
## Assumption: For this plot, "motor vehicle sources" are assumed to be of Type "ONROAD".   Aircraft, locomotives,
## and marine vessels were in NONROAD before 2008 but now are in ONROAD from 2008 onward. 
## (http://www3.epa.gov/ttn/chief/net/2008inventory.html#inventorydoc)
## Filter SCC data for only motor vehicle sources
SCC.motorVehicleDetails <- filter(SCC, grepl("Mobile", SCC$EI.Sector, ignore.case=T))

## Filter NEI data where 
## 1) fips = "24510" --> Baltimore City and
## 2) Type == ON-ROAD
## 
NEIBalt <- filter(NEI, fips == "24510" & type == "ON-ROAD")

## filter NEI data where NEI.SCC matches SCC.SCC        
NEIBaltMotorVeh <- filter(NEIBalt, NEIBalt$SCC %in% SCC.motorVehicleDetails$SCC)
        

## Group by Year and Type, Sum the Emissions, and make Factor "year"  with initial caps.
NEIBaltMotorVehByYear <- group_by(NEIBaltMotorVeh, year) %>%
        summarize(AnnualEmission = sum(Emissions)) %>%
        mutate(Year = year)

## Sample plot using qplot
# qplot(NEICoalCombustByYear$year, NEICoalCombustByYear$AnnualEmission/10^6, data = NEICoalCombustByYear,
#         col = NEICoalCombustByYear$type,
#         geom = c("point", "smooth"), method = "lm",
#         margins = TRUE,
#         xlab = "Year",
#         ylab = "PM2.5 Emissions (in Millions of tons)",
#         main = "Coal Combustion-Related Emissions"),
#         legend()

## Create base ggplot object to add layers to
g <- ggplot(NEIBaltMotorVehByYear, aes(Year, AnnualEmission))

## Add layers and customize
g + geom_point(size = 4) +
        geom_line(size = 1) +
        geom_smooth(method = "lm", size = 1, se = F) +
        xlab("Year") +
        ylab("Annual Emissions") +
        ggtitle("Motor Vehicle Emissions\n Baltimore City") +
        theme(plot.margin = unit(c(1,1,2,1),"cm")) + 
        scale_y_continuous(breaks = seq(0,1000,50)) 

## Copy graphic device to a .png file
dev.copy(png, file = "P2_plot5.png", width = 480, height = 480)
dev.off()