

# Set working directory-----------------------------------------------------
setwd("C:/Gibraltar_diffusion")
# setwd("G:/Gibraltar_policy/Gibraltar2015/5_data_digest/1_r/Leaflet_function")
# setwd("C:/RICARDO-AEA/Gibraltar_diffusion")

source("difference.R")
source("andrewmap.R")


# Load some sample data
annualDiff_NO2 <- read.csv("annualDiff_NO2.csv")[-1]
# annualDiff_benzene <- read.csv("annualDiff_benzene.csv")[-1]
# diff_NO2 <- read.csv("annual_Difference_2015_no2.csv")[-1]
# diff_benzene <- read.csv("annual_Difference_2015_benzene.csv")[-1]

##### example fro the "andrewmap" and the "difference" functions

# variable = "no2", "benzene", "diff"
# data = annualDiff_NO2; annualDiff_benzene; diff_NO2 etc...
# long = -5.35
# lat = 36.130
# zoom = 15
# year = 2015  
# background = "Thunderforest" or"Hydda_Full" or "Hydda_Base" or "Satellite" or "Toner Lite"

map <- andrewmap(-5.35, 36.130, 15, "Thunderforest", annualDiff_NO2, 2015, "no2")
diff <- difference(annualDiff_NO2, 2015, "no2")
