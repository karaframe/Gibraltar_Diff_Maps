# Set-up ---------------------------
# Set working directory
# setwd("G:/Gibraltar_policy/Gibraltar2015/5_data_digest/1_r")
setwd("C:/Gibraltar_diffusion")

# Load a heap of set-up code done by others
source("gibDataDigestPreamble.R")
# YEAR <- "2016"

# Get data ---------------------------
# Read the Bz diffusion tube meta data from the database
Bzmeta_data <- search_database(database = "gibraltar") %>%
  filter(data_table == "md_measurement", is.na(date_ended), variable == "benzene")

# Bz diffusion tube data in the Gibraltar archive is aggegated (average of triplicate measurement)  
Bzdiff <- import_measures(database = "gibraltar", site = Bzmeta_data$site, 
                           variable = "benzene", start = "2005-01-01", end = paste0(YEAR, "-12-31"), extra = TRUE)
Bzdiff <- filter(Bzdiff, period_name == "multiday") # ensures hourly (auto) data is dropped
Bzdiff <- subset(Bzdiff, select = c(date, date_end, site, site_name, value))

colnames(Bzdiff)[which(names(Bzdiff) == "value") ] <- "benzene"
colnames(Bzdiff)[which(names(Bzdiff) == "date") ] <- "date_start"

extra <- function(mydata) {
  ## the sequence of dates
  expanded <- data.frame(date = seq(mydata$date_start, mydata$date_end, by = "day"))
  expanded[names(mydata)] <- mydata
  expanded
}

## add extra dates - go through each line at a time
Bzdiff <- lapply(1:nrow(Bzdiff), function (x) extra(Bzdiff[x, ]))
Bzdiff <- do.call(rbind.fill, Bzdiff)


# merge with Bzmeta_data
Bzdiff <- merge(Bzdiff, Bzmeta_data, by = "site")
# add a YEAR column based on final date
Bzdiff <- within(Bzdiff, year <- as.numeric(format(date_end, "%Y")))
# only select fields for plotting
Bzdiff <- subset(Bzdiff, select = c(site, site_name.x, latitude, longitude, year, benzene))
colnames(Bzdiff)[which(names(Bzdiff) == "site_name.x") ] <- "site_name"
colnames(Bzdiff)[which(names(Bzdiff) == "site") ] <- "site_id"

# annualDiffs in Bz
annualDiff <- ddply(Bzdiff, .(site_id, site_name, year), numcolwise(mean), na.rm = TRUE)
annualDiff$benzene <- round(annualDiff$benzene, 1)

# Figure 41 ---------------------------
# Run Figure 41 here --------> change from googlemap to leaflet
# Apply the plot formatting
fontsize <- trellis.par.get("fontsize")
fontsize$text <- 18 # this should make the text legible
trellis.par.set("fontsize" = fontsize)

Figure41 <- GoogleMapsPlot(subset(annualDiff, year == YEAR), latit = "latitude",
                          xlim = c(-5.365, -5.340), ylim = c(36.1090, 36.1525),
                          pollutant = "benzene", cex.range = 4,
                          longit = "longitude",  maptype = "terrain", col = "jet",
                          key.header = "Annual mean benzene (ug m-3)", key.footer = "", 
                          key.position = "top", breaks = 5,
                          plot.style = c("ticks", "border"),
                          xlab = expression("Longitude (" * degree * "E)"),
                          ylab =  expression("Latitude (" * degree * "N)"),
                          fit = "all", height = 1,
                          space = "top", limits = c(0, bz.amean), verbose  = 0,
                          labels = list(labels = "benzene", col = "black", font = 1, cex = 0.5))

# add id field
annualDiff$id <- paste0("2005-", as.character(as.numeric(YEAR)-1))
ids <- which(annualDiff$year == YEAR)
annualDiff$id[ids] <- as.character(YEAR)
annualDiff <- melt(annualDiff, measure.vars = "benzene")
annualDiff <- dcast(annualDiff, ... ~ id + variable)
annualDiff <- ddply(annualDiff, .(site_id), numcolwise(mean), na.rm = TRUE)
annualDiff$diff <- round(annualDiff[, 6] - annualDiff[ , 5], 1)

# Figure 42 ---------------------------
# Run Figure 42 here --------> change from googlemap to leaflet
# Apply the plot formatting
fontsize <- trellis.par.get("fontsize")
fontsize$text <- 18 # this should make the text legible
trellis.par.set("fontsize" = fontsize)

Figure42 <- GoogleMapsPlot(annualDiff, pollutant = "diff", cex.range = 4,
                           xlim = c(-5.365, -5.340), ylim = c(36.1090, 36.1525),
                           latit = "latitude", longit = "longitude",
                           maptype = "terrain", col = "jet",
                           key.header = "Difference in annual mean benzene from long-term mean (ug m-3)", key.footer = "", 
                           key.position = "top", breaks = 4,
                           plot.style = c("ticks", "border"),
                           xlab = expression("Longitude (" * degree * "E)"),
                           ylab =  expression("Latitude (" * degree * "N)"),
                           fit = "all", height = 1,
                           space = "top", limits = c(-3, 3), verbose  = 0,
                           labels = list(labels = "diff", col = "black", font = 1, cex = 0.5))

