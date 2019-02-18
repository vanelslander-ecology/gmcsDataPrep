library(data.table)
library(raster)
library(dplyr)
library(climatebcAPI)
#Filter by BC
temp <- grep(pattern = "^BC", x = mySimOut$PSPgis$MeasureID)
#subset points
BCLoc <- mySimOut$allLocSF[temp,]

#For climatebcAPI
#1. Need to convert it to a data.frame with elevation  and lat long included....
#2. Need to rename the ID fields to ID1 and ID2.
BCLoc <- sf::as_Spatial(from = BCLoc)
BCLoc$lon <- BCLoc@coords[,1]
BCLoc$lat <- BCLoc@coords[,2]

ElevBC <- raster("C:/Ian/Data/Elevation/GMTED2010N50W150_150/50n150w_20101117_gmted_med150.tif")
names(ElevBC) <- "el" #This is required by climateBC

BCLoc <- raster::extract(x = ElevBC, y = BCLoc, method = "simple", fun = "mean", na.rm = TRUE, sp = TRUE)

tempBC <- as.data.frame(BCLoc)
tempBC$ID1 <- tempBC$MeasureID
tempBC$ID2 <- tempBC$OrigPlotID1

# write.csv(tempBC, "C:/Ian/PracticeDirectory/tempBioSim.csv")
# Have not had success with BioSim yet
x <- tempBC
temp <- data.frame("ID1" = x[1], "ID2" = x[2], "lon" = x[7], "lat" = x[8], "el" = x[9])
per1 <- myClimatebcAPI(temp, ysm = "Y", period = "Year_1960.ann")


climBC <- function(x){
  temp <- data.frame("ID1" = x[1], "ID2" = x[2], "lon" = x[7], "lat" = x[8], "el" = x[9])

  per1 <- myClimatebcAPI(temp, ysm = "Y", period = "Year_1960.ann")

  #For Yong's model, get the CMD and MAT. This should eventually be the mean of the period
  #e.g. for a site sampled in 1990 and 2000, the mean of cmi_1990-2000
  temp$CMD <- per1$CMD
  temp$MAT <- per1$MAT
  }

out <- apply(tempBC, 1, FUN = climBC)

clm <- climatebcAPI::climatebcAPI(tempBC, ysm = "Y")
#Defaults to