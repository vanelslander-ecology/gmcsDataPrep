
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
  documentation = list("README.txt", "gmcsDataPrep.Rmd", "PredictiveEcology/pemisc@development"),
  reqdPkgs = list('data.table', 'sf', 'sp', 'raster'),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, desc = "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("studyPeriod", "numeric", c(1958, 2011), NA, NA, desc = "The years by which to compute climate normals and subset sampling plot data. Must be a vector of at least length 2"),
    defineParameter("minDBH", "numeric", 10, 0, NA, desc = "The minimum DBH allowed. Each province uses different criteria for monitoring trees, so absence of entries < min(DBH) does not equate to absence of trees."),
    defineParameter("useHeight", "logical", TRUE, NA, NA, desc = "Should height be used to calculate biomass (in addition to DBH)"),
    defineParameter("biomassModel", "character", "Lambert2005", NA, NA, desc =  "The model used to calculate biomass from DBH. Can be either 'Lambert2005' or 'Ung2008'")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = 'PSPmeasure', objectClass = 'data.table', desc = "PSP data for individual measures", sourceURL = NA),
    expectsInput(objectName = 'PSPplot', objectClass = 'data.table', desc = "PSP data for each plot", sourceURL = NA),
    expectsInput(objectName = 'PSPgis', objectClass = 'data.table', desc = "PSP plot data as sf object", sourceURL = NA),
    expectsInput(objectName = 'studyAreaLarge', objectClass = 'SpatialPolygonsDataFrame', desc = "this area will be used to subset PSP plots before building the statistical model.", sourceURL = NA),
    expectsInput(objectName = "PSPclimData", objectClass = "data.table", desc = "climate data for each PSP",
                 sourceURL = "https://drive.google.com/open?id=1PD_Fve2iMpzHHaxT99dy6QY7SFQLGpZG")
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
  if (length(P(sim)$studyPeriod) < 2) {
    stop("Please supply P(sim)$studyPeriod of length 2 or greater")
  }

  tempSA <- spTransform(x = sim$studyAreaLarge, CRSobj = crs(sim$PSPgis)) %>%
    st_as_sf(.)
  PSP_sa <- sim$PSPgis[tempSA,] %>% #Find how to cache this. '[' did not work
    setkey(., OrigPlotID1)
  #Restrict climate variables to only thosee of interest.. should be param
  PSPclimData <- sim$PSPclimData[,.("OrigPlotID1" = ID1, Year, CMD, MAT)]

  #Filter other PSP datasets to those in study Area
  PSPmeasure <- sim$PSPmeasure[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]
  PSPplot <- sim$PSPplot[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]
  PSPclimData <- PSPclimData[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]

  #length(PSPclimData)/length(PSP_sa) should always yield a whole number.
  #Filter data by study period
  PSPmeasure <- PSPmeasure[MeasureYear > min(P(sim)$studyPeriod) & MeasureYear < max(P(sim)$studyPeriod),]
  PSPplot <- PSPplot[MeasureYear > min(P(sim)$studyPeriod) & MeasureYear < max(P(sim)$studyPeriod),]
  PSPclimData[Year > min(P(sim)$studyPeriod) & Year < max(P(sim)$studyPeriod),]

  #Join data (should be small enough by now)
  PSPmeasure <- PSPmeasure[PSPplot, on = c('MeasureID', 'OrigPlotID1', 'MeasureYear')]
  PSPmeasure[, c('Longitude', 'Latitude', 'Easting', 'Northing', 'Zone'):= NULL]

  #Filter by > 30 trees at first measurement (P) to ensure forest.
  forestPlots <- PSPmeasure[MeasureYear == baseYear, .(measures = .N), OrigPlotID1] %>%
    .[measures >= 30,]
  PSPmeasure <- PSPmeasure[OrigPlotID1 %in% forestPlots$OrigPlotID1,]

  #Filter by 3+ repeat measures - must be last filter criteria. Removes the most.
  repeats <- PSPmeasure[, .(measures = .N), by = .(OrigPlotID1, TreeNumber)] %>%
    .[measures > 2,]
  PSPmeasure <- PSPmeasure[OrigPlotID1 %in% repeats$OrigPlotID1,]
  PSPplot <- PSPplot[OrigPlotID1 %in% PSPmeasure$OrigPlotID1,]


  #Restrict to trees > 10 DBH (P) This gets rid of some big trees. Some 15 metres tall
  PSPmeasure <- PSPmeasure[DBH >= P(sim)$minDBH,]

  #Calculate Climate Means
  mCMD <- PSPclimData[OrigPlotID1 %in% PSPmeasure$OrigPlotID1, .("mCMD" = mean(CMD)), OrigPlotID1]
  mMAT <- PSPclimData[OrigPlotID1 %in% PSPmeasure$OrigPlotID1, .("mMAT" = mean(MAT)), OrigPlotID1]

  PSPplot <- PSPplot[mCMD, on = "OrigPlotID1"]
  PSPplot <- PSPplot[mMAT, on = "OrigPlotID1"]

  #Calculate biomass
  #TODO: What are these units? tonnes? 1
  tempOut <- pemisc::biomassCalculation(species = PSPmeasure$newSpeciesName,
                                                   DBH = PSPmeasure$DBH,
                                                   height = PSPmeasure$Height,
                                                   includeHeight = P(sim)$useHeight,
                                                   equationSource = P(sim)$biomassModel)
  message("No biomass estimate possible for these species: ")
  print(tempOut$missedSpecies)
  PSPmeasure$biomass <- tempOut$biomass
  PSPmeasure <- PSPmeasure[biomass != 0]
  #Must remove 0 before calculating mortality and growth.

  plotSppChange <- lapply(unique(PSPmeasure$OrigPlotID1), FUN = function(x, m = PSPmeasure, p = PSPplot, clim = PSPclimData){
    #for each plot...
    #sort by year. Calculate the changes in biomass, inc unobserved growth and mortality
    m <- m[OrigPlotID1 %in% x,] #subset data by plot
    p <- p[OrigPlotID1 %in% x,]

    #p has duplicates due to plotID2. Must remove.
    p <- p[, MeasureID := NULL]
    p <- p[!duplicated(p)]
    clim <- clim[OrigPlotID1 %in% x,]
    p <- setkey(p, MeasureYear) #Order by year
    m <- setkey(m, TreeNumber)
    periods <- nrow(p) - 1

    #Preallocate dataframe
    # out <- data.frame("PlotNumber"= character(periods),
    #                   "CensusPeriod" = character(periods),
    #                   "annG" = numeric(periods),
    #                   "annM" = numeric(periods),
    #                   "annB" = numeric(periods),
    #                   "ATA" = numeric(periods),
    #                   "ACMD" = numeric(periods), stringsAsFactors = FALSE)

    #For each interval
    periodSums <- lapply(1:periods, FUN = function(i, M = m, P = p, Clim = clim){

      #Calculate climate variables
      ATA <- mean(Clim$MAT[Clim$Year >= p$MeasureYear[i] &
                             Clim$Year <= p$MeasureYear[i+1]]) - p$mMAT[1]
      ACMD <- mean(Clim$CMD[Clim$Year >= p$MeasureYear[i] &
                              Clim$Year <= p$MeasureYear[i+1]]) - p$mCMD[1]

      period <- paste0(P$MeasureYear[i], "-", P$MeasureYear[i+1])
      m1 <- M[MeasureYear == P$MeasureYear[i]]
      m2 <- M[MeasureYear == P$MeasureYear[i + 1]]
      censusLength <- P$MeasureYear[i + 1] - P$MeasureYear[i]
      year <- round(sum(P$MeasureYear[i] + P$MeasureYear[i + 1])/2, digits = 0)
      living1 <- m1[m1$TreeNumber %in% m2$TreeNumber]
      living2 <- m2[m2$TreeNumber %in% m1$TreeNumber]
      dead <- m1[!m1$TreeNumber %in% m2$TreeNumber]
      newborn <- m2[!m2$TreeNumber %in% m1$TreeNumber]

      #Find observed annual changes in mortality and growth
      living2$origBiomass <- living1$biomass
      living <- living2[, .(growth =  sum(biomass - origBiomass)/censusLength), by = Species] %>%
        setkey(., Species)

      newborn <- newborn[, .(newGrowth = sum(biomass)/(censusLength/2)), by = Species] %>%
        setkey(., Species)
      #measure from census midpoint for new seedlings
      dead <- dead[, .(mortality = sum(biomass)/censusLength), by = Species] %>%
        setkey(., Species)

      #Find unobserved growth and mortality.
      #Not necessary when summing by species, b/c we can't assign species for unobserved trees
      #Unobserved growth and mortality = ~1% of observed, so climate influences on this are trivial.
      #Leaving this in nonetheless, in case we change methods
      #Unobserved recruits U = N * R * M * L
      #N = # of trees with DBH between 10 and 15
      # N <- nrow(m2[DBH <= 15]) #TODO ask Yong if this is m2, or total
      # #R = number of recruits between two successive censuses (trees in t2 not in t1)/census length
      # R <- nrow(newborn)/censusLength/N #I am not 100% sure if we divide by N or total stems in plot
      # #M = Mortality rate, number of trees with DBH between 10 and 15 that died between two census/interval length
      # M <- nrow(dead[DBH <= 15,])/censusLength/N
      # #L = census interval length
      #
      # #Next calculate the median growth of the 10-15 DBH class, assume they grew to the midpoint.
      # UnobservedR <- N * R * M * censusLength
      # UnobservedM <- UnobservedR * median(m2$biomass[m2$DBH <= 15])/censusLength/2
      # #assume unobserved trees died at midpoint. I think this overestimates growth and mortality
      # totalM <- UnobservedM + observedMortality
      # totalG <- UnobservedM + observedGrowth

      changes <- merge(newborn, living, all = TRUE) %>%
        merge(., dead, all = TRUE)
      changes[is.na(changes)] <- 0
      changes <- changes[, .("netGrowth" = sum(newGrowth, growth), mortality), by = Species]
      changes <- changes[, .("species" = Species, "netBiomass" = (netGrowth - mortality), "growth" = netGrowth, mortality)]

      changes$period <- period
      changes$ACMD <- ACMD
      changes$ATA <- ATA
      changes$OrigPlotID1 <- p$OrigPlotID1[1]
      changes$year <- year
      changes$standAge <- p$baseSA[1]
      setcolorder(changes, c("OrigPlotID1", "period", "species", "growth",
                             "mortality", "netBiomass", "ACMD", "ATA", "standAge"))

      return(changes)
    })

    periodSums <- data.table::rbindlist(periodSums)
    return(periodSums)
    })

  sim$PSPmodelData <- rbindlist(plotSppChange)
  browser()
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
  }

  if (!suppliedElsewhere("PSPclimData", sim)) {
   sim$PSPclimData <- prepInputs(targetFile = file.path(dPath, "climateNA_PSPel_1920-2017YT.csv"),
                                 url = extractURL("PSPclimData"),
                                 destinationPath = dPath,
                                 fun = "data.table::fread")
  }

  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
