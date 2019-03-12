library(SpaDES)
library(raster)
setPaths(cachePath = "cache",
         modulePath = "../",
         inputPath = "inputs",
         outputPath = "outputs")
getPaths() # shows where the 4 relevant paths are
paths <- getPaths()
times <- list(start = 2011, end = 2030)

parameters <- list(
  #.progress = list(type = "text", interval = 1), # for a progress bar
  ## If there are further modules, each can have its own set of parameters:
  #module1 = list(param1 = value1, param2 = value2),
  #module2 = list(param1 = value1, param2 = value2)
)
ecoDistricts <- shapefile("C:/Ian/Git/scfm/modules/scfmLandcoverInit/data/ecodistricts_shp/Ecodistricts/ecodistricts.shp")
studyArea <- ecoDistricts[ecoDistricts$DISTRICT_I %in% c(387, 390, 372),] #3 small contiguous ecodistricts in the RIA


modules <- list("gmcsDataPrep", "PSP_Clean")
objects <- list("studyAreaLarge" = shapefile("C:/Ian/Campbell/RIA/GIS/RIA_StudyArea/RIA_StudyArea_Valid.shp"),
                "rasterToMatch" = raster("C:/Ian/Campbell/RIA/Land-R/rasterToMatch.tif"),
                "studyArea" = studyArea)

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects)

mySimOut <- spades(mySim)
