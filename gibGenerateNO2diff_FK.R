
library(lattice)
library(openair)
library(reshape2)
library(importr)
library(threadr)
library(maptools)
library(lubridate)
library(leaflet)
library(dplyr)
library(webshot)
library(htmlwidgets)

# Set-up ---------------------------
# Set working directory
# setwd("G:/Gibraltar_policy/Gibraltar2015/5_data_digest/1_r")
setwd("C:/Gibraltar_diffusion")
# setwd("C:/RICARDO-AEA/Gibraltar_diffusion")

# Load a heap of set-up code done by others
source("gibDataDigestPreamble.R")
# YEAR = 2015
no2.daily.limit = 40

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
# NO2diff <- subset(NO2diff, select = c(site, site_name.x, longitude, latitude, year, no2))
colnames(NO2diff)[which(names(NO2diff) == "site_name.x") ] <- "site_name"
colnames(NO2diff)[which(names(NO2diff) == "site") ] <- "site_id"

# annualDiffs in NO2
annualDiff <- ddply(NO2diff, .(site_id, site_name, year), numcolwise(mean), na.rm = TRUE)
class(annualDiff$no2) <- "integer"

write.csv(annualDiff, "annualDiff_NO2.csv")


# Figure 14 ---------------------------
# Run Figure 14 here --------> change from googlemap to leaflet
# Apply the plot formatting
fontsize <- trellis.par.get("fontsize")
fontsize$text <- 18 # this should make the text legible
trellis.par.set("fontsize" = fontsize)

Figure14 <- GoogleMapsPlot(subset(annualDiff_NO2, year == YEAR), latit = "latitude",
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







##### Make a Leaflet Map FUNCTION ##########################################################################
############################################################################################################





##### Make a Leaflet Map ###################################################################################
############################################################################################################
# Figure 14 ---------------------------

  
# subset only selected YEAR
annualDiff_YEAR <- filter(annualDiff, year == YEAR)

# define a colorbar
MIN_NO2 = 0
MAX_NO2 = no2.daily.limit*2

# MIN_NO2 = min(annualDiff_YEAR$no2)-1
# MAX_NO2 = max(annualDiff_YEAR$no2)+1

pal_NO2 <- colorNumeric(
  palette = c("#0000ff", "#0000ff", "#ffa500", "#ffff00", "#7f0000", "#7f0000"),
 # domain = sp_annualDiff_NO2$no2
  c(MIN_NO2, MAX_NO2)
)

# green: "#7fe67f"

popup_NO2 <- paste0("<strong><i>", 
                    annualDiff_YEAR$site_name,
                    "</i></strong><br>Annual Mean NO<sub>2</sub>: <strong> ", annualDiff_YEAR$no2, " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")


#Sys.setenv(https_proxy="https://harproxy02:3128")
Sys.setenv(http_proxy="http://harproxy02:3128")


map <- leaflet(data = annualDiff_YEAR[,]) %>%
  addTiles() %>% 
  setView(-5.35, 36.130, 15) %>%
#  addPopups(-5.34, 36.160, content,
#            options = popupOptions(closeButton = FALSE)) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("Thunderforest.Transport", group = "Thunderforest") %>%
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
  addProviderTiles("Hydda.Base", group = "Hydda_Base") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addCircleMarkers(lng= ~longitude, lat= ~latitude, 
                   popup = popup_NO2,
             weight = 3, radius=9, 
             color = pal_NO2(annualDiff_YEAR$no2), stroke = FALSE, fillOpacity = 1,
             group= "Diffusion_NO2") %>%
  addLegend("bottomright", pal = pal_NO2, values = c(MIN_NO2, MAX_NO2),      # values = sp_annualDiff_NO2$no2,
            title = "<strong>Annual Mean<br>NO<sub>2</sub>(<font face=symbol>m</font>g/m<sup>3</sup>):",
            labFormat = labelFormat(prefix = ""), labels = "black",
            opacity = 1) %>%
  addLayersControl(
    baseGroups = c("Thunderforest", "Hydda_Full", "Hydda_Base", "Satellite", "Toner Lite"),
    overlayGroups = c("Diffusion_NO2"),
    options = layersControlOptions(collapsed = TRUE)) 
map



## create a .png figure for the report and one ineractive html file
saveWidget(map, paste("NO2_Diffusion_",YEAR, ".html", sep = ""), selfcontained = FALSE)
webshot(paste("NO2_Diffusion_",YEAR, ".html", sep = ""), file = paste(YEAR, "_", "Figure_14.png", sep = ""), 
        vwidth = 900, vheight = 1300, cliprect = 'viewport')


############################################################################################################
############################################################################################################



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







##### Make a Leaflet Map ###################################################################################
############################################################################################################
# Figure 15 ---------------------------

# annualDiff <- annualDiff%>%
#   left_join(annualDiff_YEAR[1:2], "site_id")

sites_info <- annualDiff_YEAR %>%
  dplyr::select(site_id,
                site_name)

annualDiff <- annualDiff%>%
  left_join(sites_info, "site_id")

# define a colorbar
# MIN_diff = -17
# MAX_diff = +17

MIN_diff = min(annualDiff$diff)
MAX_diff = max(annualDiff$diff)

pal_diff <- colorNumeric(
  palette =  c("#0000ff", "#0000ff", "#ffa500", "#ffff00", "#7f0000", "#7f0000"),
  # domain = sp_annualDiff_NO2$no2
  c(MIN_diff, MAX_diff)
)


popup_diff <- paste0("<strong><i>", 
                    annualDiff$site_name,
                    "</i></strong><br>Annual difference:<strong> ", annualDiff$diff, " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")


map <- leaflet(data = annualDiff[,]) %>%
  addTiles() %>% 
  setView(-5.35, 36.130, 15) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("Thunderforest.Transport", group = "Thunderforest") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
  addProviderTiles("Hydda.Base", group = "Hydda_Base") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  
  addCircleMarkers(lng= ~longitude, lat= ~latitude, 
                   popup = popup_diff,
                   weight = 3, radius=9, 
                   color = pal_diff(annualDiff$diff), stroke = FALSE, fillOpacity = 1,
                   group= "difference") %>%
  addLegend("bottomright", pal = pal_diff, values = c(MIN_diff, MAX_diff),   
            title = "<strong>Difference in annual mean NO<sub>2</sub><br>from long-term mean (<font face=symbol>m</font>g/m<sup>3</sup>):",
            labFormat = labelFormat(prefix = ""), labels = "black",
            opacity = 1) %>%
  addLayersControl(
    baseGroups = c("Thunderforest", "Hydda_Full", "Hydda_Base", "Satellite", "Toner Lite"),
    overlayGroups = c("difference"),
    options = layersControlOptions(collapsed = TRUE)) 
map



## create a .png figure for the report and one ineractive html file
saveWidget(map, paste("diff_NO2_Diffusion_",YEAR, ".html", sep = ""), selfcontained = FALSE)
webshot(paste("diff_NO2_Diffusion_",YEAR, ".html", sep = ""), file = paste(YEAR, "_", "Figure_15.png", sep = ""), 
        vwidth = 900, vheight = 1300, cliprect = 'viewport')


############################################################################################################
############################################################################################################


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


