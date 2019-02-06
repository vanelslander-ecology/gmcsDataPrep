
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "gmcsDataPrep",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = c(person(c("Ian", "MS"), "Eddy", email = "ian.eddy@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.4", gmcsDataPrep = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "gmcsDataPrep.Rmd"),
  reqdPkgs = list('data.table', 'sf'),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = 'PSPmeasure', objectClass = 'data.table', desc = "PSP data for individual measures", sourceURL = NA),
    expectsInput(objectName = 'PSPplot', objectClass = 'data.table', desc = "PSP data for each plot", sourceURL = NA),
    expectsInput(objectName = 'PSPgis', objectClass = 'data.table', desc = "PSP plot data as sf object", sourceURL = NA),
    expectsInput(objectName = 'studyAreaLarge', objectClass = 'SpatialPolygonsDataFrame', desc = "this area will be used to subset PSP plots before building the statistical model. Must be extensive enough to yield approrpriate sample size", sourceURL = NA),
    expectsInput(objectName = "PSPclimData", objectClass = "data.table", desc = "climate data for each PSP",
                 url = "https://drive.google.com/file/d/1PD_Fve2iMpzHHaxT99dy6QY7SFQLGpZG/view?usp=sharing")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "LME_GM_model", objectClass = "model object", desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.gmcsDataPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "gmcsDataPrep", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "gmcsDataPrep", "save")
    },

    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
 #Crop points to studyArea
  browser()
  tempSA <- spTransform(x = sim$studyAreaLarge, CRSobj = crs(sim$PSPgis)) %>%
    st_as_sf(.)
  PSP_sa <- sim$PSPgis[tempSA,]

  #Filter other PSP datasets
  PSPmeasure <- sim$PSPmeasure[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]
  PSPplot <- sim$PSPplot[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]

  #Filter by 3+ repeat measures - this info is only in PSPmeasure
  repeats <- PSPmeasure[, .(measures = .N), by = .(OrigPlotID1, TreeNumber)] %>%
    .[measures > 2,]
  PSPmeasure <- PSPmeasure[OrigPlotID1 %in% repeats$OrigPlotID1,]
  PSPplot <- PSPplot[OrigPlotID1 %in% repeats$OrigPlotID1,]
  PSP_sa <- PSP_sa[PSP_sa$OrigPlotID1 %in% repeats$OrigPlotID1,]
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("PSPmeasure", sim)) {
    message("You have not supplied PSP data. Consider running module 'PSP_Clean'. Generating simulated data")
    sim$PSPmeasure <- data.table(MeasureID = "ABPSMature_1", OrigPlotID1 = 'AB1',
                                 OrigPlotID2 = "1", MeasureYear = 1960, TreeNumber = 1,
                                 Species = "PB", DBH = 39.4, Height = NA)
  }

  if (!suppliedElsewhere("PSPplot", sim)) {
    sim$PSPplot <- data.table(MeasureID = "ABPSMature_1", OrigPlotID1 = 'AB1',
                                 MeasureYear = 1960, Longitude = -116.8351, Latitude = 54.40416,
                                 Zone = 11, Easting = 510704.3, Northing = 762, PlotSize = 0.1012,
                              baseYear = 1960, baseSA = 76)

  }

  if (!suppliedElsewhere("PSPgis", sim)){
    sim$PSPgis <- st_as_sf(x = sim$PSPplot, coords = c("Longitude", "Latitude"),
                           crs = "+proj=longlat +datum=WGS84")
  }

  if (!suppliedElsewhere("studyAreaLarge", sim)) {
    message("studyAreaLarge not supplied. Using random polygon in Alberta")
    sim$studyAreaLarge <- LandR::randomStudyArea(size = 1e12)
    #This is silly
  }

  if (!suppliedElsewhere("PSPclimData", sim)) {

   sim$PSPclimData <- prepInputs(targetFile = "climateNA_PSPel_1920-2017YT.csv",
                                 url = extractURL("PSPclimData"),
                                 destinationPath = dPath,
                                 fun = "read.csv")
  }

  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
