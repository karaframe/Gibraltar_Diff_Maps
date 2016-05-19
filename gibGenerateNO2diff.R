
library(lattice)
library(openair)
library(reshape2)
library(importr)
library(maptools)
library(threadr) # load this package before dplyr
library(leaflet)
library(dplyr)
library(rgdal)
library(raster)
library(rgeos)
library(lubridate)
library(webshot)
library(htmlwidgets)

# Set-up ---------------------------
# Set working directory
# setwd("G:/Gibraltar_policy/Gibraltar2015/5_data_digest/1_r")
setwd("C:/Gibraltar_diffusion")

# Load a heap of set-up code done by others
source("gibDataDigestPreamble.R")

# Get data ---------------------------
# Read the NO2 diffusion tube meta data from the database
NO2meta_data <- search_database(database = "gibraltar") %>%
                          filter(data_table == "md_measurement", is.na(date_ended), variable == "no2")

# NO2 diffusion tube data in the Gibraltar archive is aggegated (average of triplicate measurement)  
NO2diff <- import_measures(database = "gibraltar", site = NO2meta_data$site, 
                                variable = "no2", start = "2005-01-01", end = paste0(YEAR, "-12-31"), extra = TRUE)
NO2diff <- filter(NO2diff, period_name == "multiday") # ensures hourly (auto) data is dropped
NO2diff <- subset(NO2diff, select = c(date, date_end, site, site_name, value))

colnames(NO2diff)[which(names(NO2diff) == "value") ] <- "no2"
colnames(NO2diff)[which(names(NO2diff) == "date") ] <- "date_start"

extra <- function(mydata) {
  ## the sequence of dates
  expanded <- data.frame(date = seq(mydata$date_start, mydata$date_end, by = "day"))
  expanded[names(mydata)] <- mydata
  expanded
}

## add extra dates - go through each line at a time
NO2diff <- lapply(1:nrow(NO2diff), function (x) extra(NO2diff[x, ]))
NO2diff <- do.call(rbind.fill, NO2diff)


# merge with NO2meta_data
NO2diff <- merge(NO2diff, NO2meta_data, by = "site")
# add a YEAR column based on final date
NO2diff <- within(NO2diff, year <- as.numeric(format(date_end, "%Y")))
# only select fields for plotting
NO2diff <- subset(NO2diff, select = c(site, site_name.x, latitude, longitude, year, no2))
colnames(NO2diff)[which(names(NO2diff) == "site_name.x") ] <- "site_name"
colnames(NO2diff)[which(names(NO2diff) == "site") ] <- "site_id"

# annualDiffs in NO2
annualDiff_NO2 <- ddply(NO2diff, .(site_id, site_name, year), numcolwise(mean), na.rm = TRUE)
class(annualDiff_NO2$no2) <- "integer"

# write a .csv file for annualDiff_NO2
write.csv(annualDiff_NO2, "annualDiff_NO2.csv")


# make a spatial dataframe with traffic data for each hour
sp_NO2_hour <- SpatialPointsDataFrame(NO2_hour[,1:2], NO2_hour,  # longitude, latitude
                                      proj4string=CRS("+init=epsg:4326")) 
plot(sp_NO2_hour)


writeOGR(sp_NO2_hour, sprintf('gibraltar_AVG_NO2_h%02d',i),
         "AVG_NO2", driver = "ESRI Shapefile",
         overwrite_layer = TRUE)





# Figure 14 ---------------------------
# Run Figure 14 here --------> change from googlemap to leaflet
# Apply the plot formatting
fontsize <- trellis.par.get("fontsize")
fontsize$text <- 18 # this should make the text legible
trellis.par.set("fontsize" = fontsize)

Figure14 <- GoogleMapsPlot(subset(annualDiff, year == YEAR), latit = "latitude",
                           pollutant = "no2", cex.range = 4,
                           xlim = c(-5.365, -5.340), ylim = c(36.1090, 36.1525),
                           longit = "longitude",  maptype = "terrain", col = "jet",
                           key.header = "Annual mean no2 (ug m-3)", key.footer = "", 
                           key.position = "top", breaks = 6,
                           plot.style = c("ticks", "border"),
                           xlab = expression("Longitude (" * degree * "E)"),
                           ylab =  expression("Latitude (" * degree * "N)"),
                           fit = "all", height = 1,
                           space = "top", limits = c(0, no2.daily.limit*2), verbose  = 0,
                           labels = list(labels = "no2", col = "black", font = 1, cex = 0.5))

## add id field
annualDiff$id <- paste0("2005-", as.character(as.numeric(YEAR)-1))
ids <- which(annualDiff$year == YEAR)
annualDiff$id[ids] <- as.character(YEAR)
annualDiff <- melt(annualDiff, measure.vars = "no2")
annualDiff <- dcast(annualDiff, ... ~ id + variable)
annualDiff <- ddply(annualDiff, .(site_id), numcolwise(mean), na.rm = TRUE)
annualDiff$diff <- round(annualDiff[, 6] - annualDiff[ , 5], 1)










# Figure 15 ---------------------------
# Run Figure 15 here --------> change from googlemap to leaflet
# Apply the plot formatting
fontsize <- trellis.par.get("fontsize")
fontsize$text <- 18 # this should make the text legible
trellis.par.set("fontsize" = fontsize)

Figure15 <- GoogleMapsPlot(annualDiff, latit = "latitude", pollutant = "diff", cex.range = 4,
                           xlim = c(-5.365, -5.340), ylim = c(36.1090, 36.1525),
                           longit = "longitude",  maptype = "terrain", col = "jet",
                           key.header = "Difference in annual mean no2 from long-term mean (ug m-3)", 
                           key.footer = "", key.position = "top", breaks = 6,
                           plot.style = c("ticks", "border"), 
                           xlab = expression("Longitude (" * degree * "E)"),
                           ylab =  expression("Latitude (" * degree * "N)"),
                           fit = "all", height = 1,
                           space = "top", limits = c(-17, 17), verbose  = 0,
                           labels = list(labels = "diff", col = "black", font = 1, cex = 0.5))

# Generate CSV containing summary stats
# !!! Need to check whether there are pre-calculated stats for the non-auto measurements !!!
# remove the year column from annualDiff before writing to file as this is confusing
annualDiffcsv <- annualDiff[ , -which(names(annualDiff) == "year")]

meassage(paste("Writing ", YEAR, "_summary_non_auto_NO2.csv to ", csvDir,"...", sep = ""))

write.csv(annualDiff, file = file.path(csvDir, paste(YEAR, "_summary_non_auto_NO2.csv", sep = "")))

# Generate pre-calculated annual means -> this only generates annual means for the auto sites
# theStatistic <- c("annual_mean")
# thePollutant <- "no2"

# theData <- import_stats(database = "gibraltar", site = unique(NO2meta_data$site),
                        # statistic = theStatistic,
                        # variable = thePollutant, 
                        # start = "2005-01-01",
                        # end = paste0(YEAR, "-01-01"), extra = TRUE)


