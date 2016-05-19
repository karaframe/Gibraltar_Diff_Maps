#
#
# Before running this script:
#
# (1) ensure that uploadMetalsCsv.R has been run - this loads the metals measurements for the reporting year
#     to the Gib RData url (http://hornbeam.harwell.aeat.com/gibraltar/openair/R_data/)
#
# (2) ensure that uploadADMSmet.R has been run - this loads the ADMS format met obs for the airport
#     to the Gib RData url (http://hornbeam.harwell.aeat.com/gibraltar/openair/R_data/)
#
# These scripts can be found in /Gibraltar_policy/gib_r
#
#
#######################################################################################################
#                                                                                                     #
# Produces a series of standard, multi-pollutant plots required for the Annual Gibraltar Data Digest  #
#                                                                                                     #
# JJNL : 16/05/2012                                                                                   #
# updated by JJNL : 22/04/2013                                                                        #
# updated by RFM : 20/05/2014                                                                         #
# 2015 running was handled by Stuart Grange                                                           #
#######################################################################################################       
#
#
# =====
# Unix
# =====
#
# To run this function type:
# source("/ash-projects/gibraltar/Gibraltar_policy/Gibraltar2011/data_digest/1_r/gibDataDigest.R")
# at the cmd line and then: gibDataDigest(), to run the script.
# Note, typing: gibDataDigest (no brackets), will just print the script to the console window.
#
#
#
# ========
# Windows
# ========
#
# To run this function type:
# source("G:/Gibraltar_policy/Gibraltar2011/data_digest/1_r/gibDataDigest.R")
# at the cmd line and then: gibDataDigest(), to run the script.
# Note, typing: gibDataDigest (no brackets), will just print the script to the console window.
#
#
# Define the source and storage directories based on the OS:
#

if ( .Platform$OS.type == "unix" ) { root = "/ash-projects/gibraltar"
} else { root = "G:" }


# gibDataDigest2013 = function()
#   
#   
# { # start of function


options("warn" = -1) # If warn is negative all warnings are ignored
# If warn is one, warnings are printed as they occur


# Set the reporting YEAR
YEAR = as.character(as.numeric(format(Sys.Date(), "%Y"))-1)
year = YEAR 
# YEAR = 2011


# Set the directories
home = list.files(file.path(root, "Gibraltar_policy", paste("Gibraltar", YEAR, sep = "")), 
                  pattern = ".*data_digest.*", full.names = TRUE)

PlotDir = file.path(home, "1_plots")

# If the PlotDir doesn't exist, create it
if(!file.exists(PlotDir)){dir.create(PlotDir)}

pngDir = file.path(PlotDir, "1_png")

# If the PlotDir doesn't exist, create it
if(!file.exists(pngDir)){dir.create(pngDir)}

csvDir = file.path(home, "1_csv")

# If the csvDir doesn't exist, create it
if(!file.exists(csvDir)){dir.create(csvDir)}


## Set A4 paper dimensions in inches
# A4 = 210 x 297 mm
A4height = (29.7/2.54)
A4width = (21.0/2.54)


# Load packages required to run the script
stopifnot(require(openair))
stopifnot(require(plyr))
stopifnot(require(xtable))
stopifnot(require(RgoogleMaps))
stopifnot(require(gridExtra))
stopifnot(require(RCurl))
stopifnot(require(ggplot2))
stopifnot(require(reshape2))
stopifnot(require(lattice))
stopifnot(require(zoo))
stopifnot(require(importr))
stopifnot(require(dplyr))

# Set pollutant colours and limits
LV.colour = "red"

no2.colour = "green4"
no2.daily.limit = 40

nox.colour = "orange"
nox.limit = no2.daily.limit*5

pm10.colour = "dark blue"
pm10.daily.limit = 50
pm10.annual.limit = 40
pm10.percentile = 90

pm25.colour = "lime green"
pm25.stage1.LV = 25 # same as PM2.5 Target Value
pm25.stage2.LV = 20

so2.colour = "goldenrod3"
so2.daily.limit = 125

co.ram.colour = "spring green"
co.ram.limit = 10

o3.lto.colour = "magenta"
o3.lto.limit = 120

bz.colour = "maroon4"
bz.amean = 5

# source("~/Rscripts/importGIB.R")
source(paste(file.path(root, "Gibraltar_policy", "gib_r"), .Platform$file.sep, "importGIB.R", sep = ""))
source(paste(file.path(root, "Gibraltar_policy", "gib_r"), .Platform$file.sep, "importGIB2.R", sep = ""))
source(paste(file.path(root, "Gibraltar_policy", "gib_r"), .Platform$file.sep, "timePlotGib.R", sep = ""))
source(paste(file.path(root, "Gibraltar_policy", "gib_r"), .Platform$file.sep, "timePlotGibSS.R", sep = ""))
source(paste(file.path(root, "Gibraltar_policy", "gib_r"), .Platform$file.sep, "timePlotGibPM25.R", sep = ""))
source(paste(file.path(root, "Gibraltar_policy", "gib_r"), .Platform$file.sep, "aqStatsGib.R", sep = ""))
source(paste(home, .Platform$file.sep, "1_r", .Platform$file.sep, "CloseFigure.R", sep = ""))


