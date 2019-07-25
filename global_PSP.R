library(SpaDES)
library(raster)
library(magrittr)

setPaths(cachePath = "cache",
         modulePath = "../",
         inputPath = "inputs",
         outputPath = "outputs")
getPaths() # shows where the 4 relevant paths are
paths <- getPaths()
times <- list(start = 2011, end = 2013)

parameters <- list(
  #.progress = list(type = "text", interval = 1), # for a progress bar
  ## If there are further modules, each can have its own set of parameters:
  #module1 = list(param1 = value1, param2 = value2),
  #module2 = list(param1 = value1, param2 = value2)
)

# studyArea <- shapefile("C:/Ian/PracticeDirectory/scfm/RIA_studyArea.shp")
# rasterToMatch <- raster("C:/Ian/PracticeDirectory/scfm/RIA_studyAreaRas.tif")
NWT.url <- "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU"
studyArea <- prepInputs(url = NWT.url,
                   destinationPath = "C:/Ian/PracticeDirectory",
                   userTags = "edeSA",
                   omitArgs = c("destinationPath"))
rasterToMatch <- raster("C:/Ian/PracticeDirectory/rasterToMatch.tif")



studyAreaPSP <- shapefile("C:/Ian/Data/Canada Ecosystem/Ecozones/ecozones.shp") %>%
  spTransform(., CRSobj = crs(rasterToMatch))

modules <- list("gmcsDataPrep", "PSP_Clean")
objects <- list("studyAreaPSP" = studyAreaPSP,
                "rasterToMatch" = rasterToMatch,
                "studyArea" = studyArea)

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects)

mySimOut <- spades(mySim)
