

# Load packages required to run the script
stopifnot(require(reshape2))
stopifnot(require(leaflet))
stopifnot(require(plyr))
stopifnot(require(dplyr))
stopifnot(require(webshot))
stopifnot(require(htmlwidgets))

Sys.setenv(https_proxy="https://harproxy02:3128")

# .....start the function here ######################
difference <- function (data, YEAR, variable) {
  # build the difference variable
  data$id <- paste0("2005-", as.character(as.numeric(YEAR) - 1))
  ids <- which(data$year == YEAR)
  data$id[ids] <- as.character(YEAR)
  data <- melt(data, measure.vars = variable)
  data <- dcast(data, ... ~ id + variable)
  data <- ddply(data, .(site_name), numcolwise(mean), na.rm = TRUE)
  data$diff <- round(data[, 6] - data[, 5], 1)
  names(data)[names(data) == paste0(YEAR, "_", variable)] <- variable
  data$year <- YEAR
  write.csv(data, paste0("annual_Difference_",YEAR,"_",variable,".csv"))
}
