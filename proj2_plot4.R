## Exploratory Data Analysis Course Project 2, plot 4
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

## Across the United States, how have emissions from coal
## combustion-related sources changed from 1999–2008?
## 
## Filter SCC data for only coal cumbustion-related records.
coalCombust <- filter(SCC, grepl("^fuel comb -(.*)- coal$", SCC$EI.Sector, ignore.case=T))

## Filter NEI data where NEI.SCC matches SCC.SCC
NEICoalCombust <- filter(NEI, NEI.SCC %in% coalCombust$SCC)

## Group by Year and Type, Sum the Emissions, and make Factor "Type" with initial caps.
NEICoalCombustByYear <- group_by(NEICoalCombust, year, type) %>%
        summarize(AnnualEmission = sum(Emissions)) %>%
        mutate(Type = type)

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
g <- ggplot(NEICoalCombustByYear, aes(year, AnnualEmission/10^6))

## Add layers and customize
g + geom_point(size = 4) +
        geom_smooth(method = "lm", size = 1, se = F)  +
        facet_grid(. ~ Type) +
        xlab("Year") +
        ylab("Annual Emissions\n (in Millions of Tons)") +
        ggtitle("Coal Combustion-Related Emissions") +
        theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks = seq(1,7,1)) 

## Copy graphic device to a .png file
dev.copy(png, file = "P2_plot4.png", width = 800, height = 600)
dev.off()