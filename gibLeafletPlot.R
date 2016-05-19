gibmap <- leaflet() %>%
       addTiles() %>%  # Add default OpenStreetMap map tiles
       addMarkers(lng=-5.352902, lat=36.139194, popup="Gibraltar")
gibmap  # Print the map


# How to put dots on a Leaflet map with R
# http://trendct.org/2015/06/26/tutorial-how-to-put-dots-on-a-leaflet-map-with-r/
  
# Leaflet - Lines and Shapes
# https://rstudio.github.io/leaflet/shapes.html

# Leaflet - Colors
# https://rstudio.github.io/leaflet/colors.html


# Tiles to use:
#   
# .	Thunderforest.Transport
# .	Hydda.Full
# .	Hydda.Base
# .	Map.Box

thePlot <- leaflet(annualDiff) %>% addProviderTiles("Hydda.Base") %>%  
  setView(lng = mean(c(-5.365, -5.340)), lat = mean(c(36.1090, 36.1525)), zoom = 14) %>%
  addCircles(~longitude, ~latitude, popup = annualDiff$no2, weight = 3, radius = 40, stroke = TRUE, color = "red") 