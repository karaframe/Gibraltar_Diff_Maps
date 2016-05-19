# Set-up ---------------------------
# Set working directory
# setwd("G:/Gibraltar_policy/Gibraltar2015/5_data_digest/1_r")
setwd("C:/Gibraltar_diffusion")

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


# Load a heap of set-up code done by others
source("gibDataDigestPreamble.R")
bz.amean = 5

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

write.csv(annualDiff, "annualDiff_Benzene.csv")


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



##### Make a Leaflet Map ###################################################################################
############################################################################################################
# Figure 41 ---------------------------

# subset only YEAR = 2015
annualDiff_YEAR <- filter(annualDiff, year == YEAR)

# define a colorbar
# MIN_BZ = 0
# MAX_BZ = bz.amean

MIN_BZ = min(annualDiff_YEAR$benzene)-0.5
MAX_BZ = max(annualDiff_YEAR$benzene)+0.5

pal_BZ <- colorNumeric(
  palette = c("#0000ff", "#0000ff", "#ffa500", "#ffff00", "#7f0000", "#7f0000"),
  c(MIN_BZ, MAX_BZ)
)


popup_BZ <- paste0("<strong><i>", 
                    annualDiff_YEAR$site_name,
                    "</i></strong><br>Annual Mean benzene: <strong>",annualDiff_YEAR$benzene, " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")


map <- leaflet(data = annualDiff_YEAR[,]) %>%
  addTiles() %>% 
  setView(-5.35, 36.130, 15) %>%
  #  addPopups(-5.34, 36.160, content,
  #            options = popupOptions(closeButton = FALSE)) %>%
  addProviderTiles("Thunderforest.Transport", group = "Thunderforest") %>%
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
  addProviderTiles("Hydda.Base", group = "Hydda_Base") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addCircleMarkers(lng= ~longitude, lat= ~latitude, 
                   popup = popup_BZ,
                   weight = 3, radius=9, 
                   color = pal_BZ(annualDiff_YEAR$benzene), stroke = FALSE, fillOpacity = 1,
                   group= "Diff_BZ") %>%
  addLegend("bottomright", pal = pal_BZ, values = c(MIN_BZ, MAX_BZ),    
            title = "<strong>Annual Mean<br> benzene (<font face=symbol>m</font>g/m<sup>3</sup>):",
            labFormat = labelFormat(prefix = ""), labels = "black",
            opacity = 1) %>%
  addLayersControl(
    baseGroups = c("Thunderforest", "Hydda_Full", "Hydda_Base", "Satellite", "Toner Lite"),
    overlayGroups = c("Diff_NO2"),
    options = layersControlOptions(collapsed = TRUE)) 
map



## create a .png figure for the report and one ineractive html file
saveWidget(map, paste("BZ_Diffusion_",YEAR, ".html", sep = ""), selfcontained = FALSE)
webshot(paste("BZ_Diffusion_",YEAR, ".html", sep = ""), file = paste(YEAR, "_", "Figure_41.png", sep = ""), 
        vwidth = 900, vheight = 1300, cliprect = 'viewport')


############################################################################################################
############################################################################################################


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




##### Make a Leaflet Map ###################################################################################
############################################################################################################
# Figure 42 ---------------------------

annualDiff <- annualDiff%>%
  left_join(annualDiff_YEAR[1:2], "site_id")

# define a colorbar
# MIN_diff = -3
# MAX_diff = +3

MIN_diff = min(annualDiff$diff)-0.3
MAX_diff = max(annualDiff$diff)+0.3


pal_diff <- colorNumeric(
  palette = c("#0000ff", "#0000ff", "#ffa500", "#ffff00", "#7f0000", "#7f0000"),
  c(MIN_diff, MAX_diff)
)




popup_diff <- paste0("<strong><i>", 
                     annualDiff$site_name,
                     "</i></strong><br>Annual difference:<strong> ", annualDiff$diff, " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")


map <- leaflet(data = annualDiff[,]) %>%
  addTiles() %>% 
  setView(-5.35, 36.130, 15) %>%
  addProviderTiles("Thunderforest.Transport", group = "Thunderforest") %>%
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
  addProviderTiles("Hydda.Base", group = "Hydda_Base") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addCircleMarkers(lng= ~longitude, lat= ~latitude, 
                   popup = popup_diff,
                   weight = 3, radius=9, 
                   color = pal_diff(annualDiff$diff), stroke = FALSE, fillOpacity = 1,
                   group= "diff") %>%
  addLegend("bottomright", pal = pal_diff, values = c(MIN_diff, MAX_diff),   
            title = "<strong>Difference in annual mean benzene<br>from long-term mean (<font face=symbol>m</font>g/m<sup>3</sup>):",
            labFormat = labelFormat(prefix = ""), labels = "black",
            opacity = 1) %>%
  addLayersControl(
    baseGroups = c("Thunderforest", "Hydda_Full", "Hydda_Base", "Satellite", "Toner Lite"),
    overlayGroups = c("diff"),
    options = layersControlOptions(collapsed = TRUE)) 
map



## create a .png figure for the report and one ineractive html file
saveWidget(map, paste("diff_BZ_Diffusion_",YEAR, ".html", sep = ""), selfcontained = FALSE)
webshot(paste("diff_BZ_Diffusion_",YEAR, ".html", sep = ""), file = paste(YEAR, "_", "Figure_42.png", sep = ""), 
        vwidth = 900, vheight = 1300, cliprect = 'viewport')

############################################################################################################
############################################################################################################

