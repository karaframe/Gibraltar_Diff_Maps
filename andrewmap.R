

# Load packages required to run the script
stopifnot(require(reshape2))
stopifnot(require(leaflet))
stopifnot(require(plyr))
stopifnot(require(dplyr))
stopifnot(require(webshot))
stopifnot(require(htmlwidgets))

Sys.setenv(https_proxy="https://harproxy02:3128")

#######------------------------########################################################
# .....start the function here ########################################################
andrewmap <- function (long, lat, zoom, background, data, YEAR, variable) {
  
# subset a selected YEAR
data_YEAR <- filter(data, year == YEAR)
# names(data_YEAR)[names(data_YEAR) == variable] <- "variable"
  
# define a colorbar for NO2
if (variable == "no2") {
  variable_name = "NO<sub>2</sub>"
  MIN = 0
  MAX = 40 * 2
}


# define a colorbar for NOx
if (variable == "nox") {
  variable_name = "NOx"
  MIN = 0
  MAX = 40 * 5
}


# define a colorbar for PM10
if (variable == "pm10") {
  variable_name = "PM<sub>10</sub>"
  MIN = 0
  MAX = 50
}


# define a colorbar for PM2.5
if (variable == "pm25") {
  variable_name = "PM<sub>2.5</sub>"
  MIN = 0
  MAX = 25
}


# define a colorbar for SO2
if (variable == "so2") {
  variable_name = "SO<sub>2</sub>"
  MIN = 0
  MAX = 125
}


# define a colorbar for CO
if (variable == "co") {
  variable_name = "CO"
  MIN = 0
  MAX = 10
}


# define a colorbar for Ozone O3
if (variable == "o3") {
  variable_name = "O<sub>3</sub>"
  MIN = 0
  MAX = 120
}


# define a colorbar for benzene
if (variable == "benzene") {
  variable_name = "benzene"
  MIN = 0
  MAX = 5
}


# define a colorbar for difference data
if (variable == "diff") {
  variable_name = "diff"
  MIN = min(data_YEAR[[variable]])
  MAX = max(data_YEAR[[variable]])
}
    
# define color palette for the variable
pal_variable <- colorNumeric(
  palette = c(
    "#0000ff", "#0000ff", "#ffa500", "#ffff00", "#7f0000", "#7f0000"
  ),
  c(MIN, MAX)
)


# define popup for the iteractive map
popup_variable <- paste0(
  "<strong><i>",
  data_YEAR$site_name,
  "</i></strong><br>Annual Mean"," ",variable_name, ": <strong> ", data_YEAR[[variable]], " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)"
)

  

# leaflet map for the pollutant

map <- leaflet(data = data_YEAR[,]) %>%
  addTiles() %>%
  setView(long, lat, zoom) %>%
  addProviderTiles("Thunderforest.Transport", group = "Thunderforest") %>%
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
  addProviderTiles("Hydda.Base", group = "Hydda_Base") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addCircleMarkers(
    lng = ~ longitude, lat = ~ latitude,
    popup = popup_variable,
    weight = 3, radius = 10,
    color = pal_variable(data_YEAR[[variable]]), stroke = FALSE, fillOpacity = 1,
    group = variable
  ) %>%
  addLegend(
    "bottomright", pal = pal_variable, values = c(MIN, MAX),
    title = paste(
      "<strong>Annual Mean<br>", variable_name ,"(<font face=symbol>m</font>g/m<sup>3</sup>):"
    ),
    labFormat = labelFormat(prefix = ""), labels = "black",
    opacity = 1
  ) %>%
  addLayersControl(
    # baseGroups = background,
    baseGroups = c("Thunderforest", "Hydda_Full", "Hydda_Base", "Satellite", "Toner Lite"),
    overlayGroups = c(variable),
    options = layersControlOptions(collapsed = TRUE)
  )

## create a .png figure for the pollutant and an ineractive html file
saveWidget(map, paste(variable, "_", YEAR, "_map.html", sep = ""), selfcontained = FALSE)
webshot(paste(variable, "_", YEAR, "_map.html", sep = ""), file = paste(variable, "_", YEAR, "_map.png", sep = ""), 
        vwidth = 900, vheight = 1300, cliprect = 'viewport')
map

}

