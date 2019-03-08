
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
  reqdPkgs = list('data.table', 'sf', 'sp', 'raster', "nlme"),
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
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame", desc = "this area will be used to crop climate rasters", sourceURL = NA)
    expectsInput(objectName = 'studyAreaLarge', objectClass = 'SpatialPolygonsDataFrame', desc = "this area will be used to subset PSP plots before building the statistical model.", sourceURL = NA),
    expectsInput(objectName = "PSPclimData", objectClass = "data.table", desc = "climate data for each PSP",
                 sourceURL = "https://drive.google.com/open?id=1PD_Fve2iMpzHHaxT99dy6QY7SFQLGpZG"),
    expectsInput(objectName = "ATAstack", objectClass = "RasterStack", desc = "annual projected mean annual temperature anomalies", sourceURL = "https://drive.google.com/open?id=1mNdLnQv09N0mf5e5v8D8rIfsnLovpdyE"),
    expectsInput(objectName = "CMDstack", objectClass = "RasterStack", desc = "annual projected mean climate moisture deficit", sourceURL = "https://drive.google.com/open?id=1qt-9vNf5fpcprojD9Y9nhuh7oPfW235m"),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer", desc = "template raster for ATA and CMD")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "PSPmodelData", objectClass = "data.table", desc = "PSP growth mortality calculations"),
    createsOutput(objectName = 'CMD', objectClass = "RasterLayer", desc = "climate moisture deficit at time(sim), resampled using rasterToMatch"),
    createsOutput(objectName = 'ATA', objectClass = "RasterLayer", desc = "annual temperature anomaly, resampled using rasterToMatch")
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
      sim <- scheduleEvent(sim, time(sim), eventType = "prepRasters")
    },

    prepRasters = {
      sim$ATA <- resampleStacks(stack = sim$ATAstack, time = time(sim), isATA = TRUE)
      sim$CMD <- resampleStacks(stack = sim$CMDstack, time = time(sim))
      sim <- scheduleEvent(sim, time(sim) + 1, eventTYpe = "prepRasters")
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

  #stupid-catch
  if (length(P(sim)$studyPeriod) < 2) {
    stop("Please supply P(sim)$studyPeriod of length 2 or greater")
  }

  sim$PSPmodelData <- Cache(prepModelData, studyAreaLarge = sim$studyAreaLarge,
                                    PSPgis = sim$PSPgis,
                                    PSPmeasure = sim$PSPmeasure,
                                    PSPplot = sim$PSPplot,
                                    PSPclimData = sim$PSPclimData,
                                    useHeight = P(sim)$useHeight,
                                    biomassModel = P(sim)$biomassModel,
                                    studyPeriod = P(sim)$studyPeriod,
                                    minDBH = P(sim)$minDBH,
                            userTags = c("gmcsDataPrep", "prepModelData"))

  return(invisible(sim))
}

prepModelData <- function(studyAreaLarge, PSPgis, PSPmeasure, PSPplot,
                          PSPclimData, useHeight, biomassModel,
                          studyPeriod, minDBH) {
  #Crop points to studyArea
  tempSA <- spTransform(x = studyAreaLarge, CRSobj = crs(PSPgis)) %>%
    st_as_sf(.)
  message("Filtering PSPs to study Area...")
  PSP_sa <- PSPgis[tempSA,] %>% #Find how to cache this. '[' did not work
    setkey(., OrigPlotID1)
  message(paste0("There are "), nrow(PSP_sa), " PSPs in your study area")
  #Restrict climate variables to only thosee of interest.. should be param
  PSPclimData <- PSPclimData[,.("OrigPlotID1" = ID1, Year, CMD, MAT)]

  #Filter other PSP datasets to those in study Area
  PSPmeasure <- PSPmeasure[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]
  PSPplot <- PSPplot[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]
  PSPclimData <- PSPclimData[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]

  #length(PSPclimData)/length(PSP_sa) should always yield a whole number.
  #Filter data by study period
  message("Filtering by study period...")
  PSPmeasure <- PSPmeasure[MeasureYear > min(studyPeriod) &
                             MeasureYear < max(studyPeriod),]
  PSPplot <- PSPplot[MeasureYear > min(studyPeriod) &
                       MeasureYear < max(studyPeriod),]
  PSPclimData[Year > min(studyPeriod) & Year < max(studyPeriod),]

  #Join data (should be small enough by now)
  PSPmeasure <- PSPmeasure[PSPplot, on = c('MeasureID', 'OrigPlotID1', 'MeasureYear')]
  PSPmeasure[, c('Longitude', 'Latitude', 'Easting', 'Northing', 'Zone'):= NULL]

  #Filter by > 30 trees at first measurement (P) to ensure forest.
  message("Filtering by min. 30 trees in earliest measurement")
  forestPlots <- PSPmeasure[MeasureYear == baseYear, .(measures = .N), OrigPlotID1] %>%
    .[measures >= 30,]
  PSPmeasure <- PSPmeasure[OrigPlotID1 %in% forestPlots$OrigPlotID1,]
  PSPplot <- PSPplot[OrigPlotID1 %in% PSPmeasure$OrigPlotID1,]
  repeats <- PSPplot[, .(measures = .N), by = OrigPlotID1]
  message(paste0("There are "), nrow(repeats), " PSPs with min. 30 trees at earliest measurement")

  #Filter by 3+ repeat measures - must be last filter criteria. Removes the most.
  #Some plots share ID but have different trees so simple count of plots insufficient to find repeat measures
  message("Filtering by at least 3 repeat measures per plot")
  repeats <- PSPplot[, .(measures = .N), by = .(OrigPlotID1, MeasureYear)]
  actualCount <- repeats[, .(measures = .N), by = OrigPlotID1]
  actualCount <- actualCount[measures > 2]
  PSPmeasure <- PSPmeasure[OrigPlotID1 %in% actualCount$OrigPlotID1,]
  PSPplot <- PSPplot[OrigPlotID1 %in% PSPmeasure$OrigPlotID1,]
  message(paste0("There are "), nrow(actualCount), " PSPs with min. 3 repeat measures")

  #Restrict to trees > 10 DBH (P) This gets rid of some big trees. Some 15 metres tall
  PSPmeasure <- PSPmeasure[DBH >= minDBH,]

  #Calculate Climate Means
  mCMD <- PSPclimData[OrigPlotID1 %in% PSPmeasure$OrigPlotID1, .("mCMD" = mean(CMD)), OrigPlotID1]
  mMAT <- PSPclimData[OrigPlotID1 %in% PSPmeasure$OrigPlotID1, .("mMAT" = mean(MAT)), OrigPlotID1]

  PSPplot <- PSPplot[mCMD, on = "OrigPlotID1"]
  PSPplot <- PSPplot[mMAT, on = "OrigPlotID1"]

  #Calculate biomass
  tempOut <- pemisc::biomassCalculation(species = PSPmeasure$newSpeciesName,
                                        DBH = PSPmeasure$DBH,
                                        height = PSPmeasure$Height,
                                        includeHeight = useHeight,
                                        equationSource = biomassModel)
  message("No biomass estimate possible for these species: ")
  print(tempOut$missedSpecies)
  PSPmeasure$biomass <- tempOut$biomass
  PSPmeasure <- PSPmeasure[biomass != 0]
  #might as well drop species with no biomass match

  #Iterate over all plots, keeping OrigPlotID2s unique
  TrueUniques <- PSPmeasure[, .N, by = c("OrigPlotID1", "OrigPlotID2")]

  pSppChange <- lapply(1:nrow(TrueUniques), FUN = function(x, m = PSPmeasure, p = PSPplot, clim = PSPclimData){
    x <- TrueUniques[x,]
    #Duplicate plots arise from variable 'stand' (OrigPlotID2) that varied within the same plot.
    #Tree No. is not unique between stands, which means the same plot can have duplicate trees.
    #sort by year. Calculate the changes in biomass, inc. unobserved growth and mortality
    #must match MeasureID between plot and measure data; OrigPlotID2 not present in P

    m <- m[OrigPlotID1 == x$OrigPlotID1 & OrigPlotID2 == x$OrigPlotID2,] #subset data by plot
    p <- p[MeasureID %in% m$MeasureID]
    clim <- clim[OrigPlotID1 %in% x,]
    p <- setkey(p, MeasureYear)
    m <- setkey(m, TreeNumber)
    periods <- nrow(p) - 1

    #For each interval
    pSums <- lapply(1:periods, function(i, M = m, P = p, Clim = clim){

      #Calculate climate variables. Yong originally modeled CMD.
      #ACMD and ATA were added individually in separate model
      CMD <- mean(Clim$CMD[Clim$Year >= p$MeasureYear[i] &
                             Clim$Year <= p$MeasureYear[i+1]])
      ACMD <- mean(Clim$CMD[Clim$Year >= p$MeasureYear[i] &
                             Clim$Year <= p$MeasureYear[i+1]]) - p$mCMD[1]
      ATA <- mean(Clim$MAT[Clim$Year >= p$MeasureYear[i] &
                             Clim$Year <= p$MeasureYear[i+1]]) - p$mMAT[1]
      AT <- mean(Clim$MAT[Clim$Year >= p$MeasureYear[i] &
                             Clim$Year <= p$MeasureYear[i+1]])
      period <- paste0(P$MeasureYear[i], "-", P$MeasureYear[i+1])

      m1 <- M[MeasureYear == P$MeasureYear[i]]
      m2 <- M[MeasureYear == P$MeasureYear[i + 1]]
      censusLength <- P$MeasureYear[i + 1] - P$MeasureYear[i]
      year <-ceiling(sum(P$MeasureYear[i] + P$MeasureYear[i + 1])/2)
      living1 <- m1[m1$TreeNumber %in% m2$TreeNumber]
      living2 <- m2[m2$TreeNumber %in% m1$TreeNumber]
      dead <- m1[!m1$TreeNumber %in% m2$TreeNumber]
      newborn <- m2[!m2$TreeNumber %in% m1$TreeNumber]

      #Find observed annual changes in mortality and growth
      living2$origBiomass <- living1$biomass
      living <- living2[, .(newGrowth =  sum(biomass - origBiomass)/censusLength),
                        c("Species", "newSpeciesName")] %>%
        setkey(., Species, newSpeciesName)

      newborn <- newborn[, .(newGrowth = sum(biomass)/(censusLength/2)), c("Species", "newSpeciesName")] %>%
        setkey(., Species, newSpeciesName)
      #measure from census midpoint for new seedlings
      dead <- dead[, .(mortality = sum(biomass)/censusLength), by = c("Species", "newSpeciesName")] %>%
        setkey(., Species, newSpeciesName)

      #Find unobserved growth and mortality.
      #Not necessary when summing by species, b/c we can't assign species for unobserved trees
      #Unobserved growth and mortality = ~1% of observed, so climate influences on this are trivial.
      #Leaving this in nonetheless, in case we change methods
      #Unobserved recruits U = N * R * M * L
      #N = # of trees with DBH between 10 and 15
      # N <- nrow(m2[DBH <= 15]) #TODO ask Yong if this is m2, or total
      # #R = number of recruits between two successive censuses (trees in t2 not in t1)/census length
      # R <- nrow(newborn)/censusLength/N #I am not 100% sure if we divide by N or total stems in plot
      # #M = Mortality rate, n-trees with DBH 10 -15 that died between two census/interval length
      # M <- nrow(dead[DBH <= 15,])/censusLength/N
      # #L = census interval length
      # #Next calculate the median growth of the 10-15 DBH class, assume they grew to midpoint.
      # UnobservedR <- N * R * M * censusLength
      # UnobservedM <- UnobservedR * median(m2$biomass[m2$DBH <= 15])/censusLength/2
      # #assume unobserved trees died at midpoint. I think this overestimates growth and mortality
      # totalM <- UnobservedM + observedMortality
      # totalG <- UnobservedM + observedGrowth

      changes <- bind(newborn, living)

      #to prevent error if any table is empty
      changes$mortality <- 0
      dead$newGrowth <- 0

      changes <- bind(changes, dead)
      changes[is.na(changes)] <- 0
      changes <- changes[, .("netGrowth" = sum(newGrowth), "mortality" = sum(mortality)),
                         by = c("Species", "newSpeciesName")]
      changes <- changes[, .("species" = Species,
                             "sppLong" = newSpeciesName,
                             "netBiomass" = (netGrowth - mortality),
                             "growth" = netGrowth,
                             mortality)]

      changes$period <- period
      changes$CMD <- CMD
      changes$CMDA <- ACMD
      changes$ATA <- ATA
      changes$OrigPlotID1 <- p$OrigPlotID1[1]
      changes$year <- year
      changes$standAge <- p$baseSA[1] + P$baseSA[i+1] - P$baseSA[1]
      changes$logAge <- log(changes$standAge)
      changes$plotSize <- p$PlotSize[1]
      changes$periodLength <- censusLength
      changes$AT <- AT

      #stand age is constant through time in the data. hmm
      setcolorder(changes, c("OrigPlotID1", "period", "species", "sppLong", "growth", "mortality", "netBiomass",
                            "CMD", "CMDA", "AT", "ATA", "standAge", "logAge", "plotSize", "periodLength"))
      return(changes)
    })

    pSums <- data.table::rbindlist(pSums)
    return(pSums)
  })
  PSPmodelData <- rbindlist(pSppChange)
  PSPmodelData$species <- factor(PSPmodelData$species)
  PSPmodelData$sppLong <- factor(PSPmodelData$sppLong)

   #Standardize by plotSize and change units to Mg from kg. Now in Mg/ha
  PSPmodelData <- PSPmodelData[, growthMg := growth/plotSize/1000] %>%
    .[, mortalityMg := mortality/plotSize/1000] %>%
    .[, netBiomassMg := netBiomass/plotSize/1000]
  #26/02/2019 after discussion we decided not to include species in model.
  # Decided to parameterize inclusion of ATA or year. ATA better for projecting, year is canonical
  # Sum mortality, growth, and netBiomass by Plot and year
  PSPmodelData <- PSPmodelData[, .("growth" = sum(growthMg), "mortality" = sum(mortalityMg), "netBiomass" = sum(netBiomassMg),
                             'CMD' = mean(CMD), 'CMDA' = mean(CMDA), 'AT' = mean(AT), "ATA" = mean(ATA),
                             'standAge' = mean(standAge), 'logAge' = mean(logAge), "periodLength" = mean(periodLength),
                             'year' = mean(year), 'plotSize' = mean(plotSize)), by = c("OrigPlotID1", "period")]

  # gmcsMod <- nlme::lme(cbind(netBiomass, growth, mortality)  ~ logAge + CMD + ATA + logAge:CMD + CMD:ATA + ATA:logAge,
  #                      random = ~1 | OrigPlotID1, data = modelData, weights = varFunc(~plotSize^0.5 * periodLength))

  return(PSPmodelData)
}

resampleStacks <- function(stack, time, isATA = FALSE) {
  browser()
  currentRas <- grep(pattern = time, x = names(stack))
  if (length(currentRas) > 0) {
    yearRas <- stack[[currentRas]]
    if (isATA = TRUE) {
      yearRas[] <- yearRas[]/1000
    }
    yearRasResampled <- postProcess(yearRas,
                           rasterToMatch = sim$rasterToMatch,
                           studyArea = sim$studyArea,
                           filename2 = NULL)
  } else {
    stop("Climate dataset is limited to 2011-2100. Please limit sim times accordingly")
  }

  return(yearRasResampled)
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

  if (!suppliedElsewhere("studyArea", sim)) {
    message("studyArea not supplied. Using a random area in Alberta")
    sim$studyArea <- randomStudyArea(size = 1e6)
  }

  if (!suppliedElsewhere("studyAreaLarge", sim)) {
    message("studyAreaLarge not supplied. Using study area. You may want to supply studyAreaLarge if your studyArea is sub-provincial due to sample size issues with the multivariate model")
    sim$studyAreaLarge <- sim$studyArea
  }

  if (!suppliedElsewhere("PSPclimData", sim)) {
   sim$PSPclimData <- prepInputs(targetFile = file.path(dPath, "climateNA_PSPel_1920-2017YT.csv"),
                                 url = extractURL("PSPclimData"),
                                 destinationPath = dPath,
                                 fun = "data.table::fread")
  }

  if (!suppliedElsewhere("ATAstack", sim)) {
    #These should not be called using prepInputs -- each stack has 90 rasters and prepInputs isn't ready for stacks
    #they need to be subset, resampled, and reprojected every year
    sim$ATAstack <- prepInputs(targetFile = "CanATA_2011-2100.gri",
                               url = extractURL("ATAstack"),
                               destinationPath = dPath,
                               fun = "raster::stack",
                               overwrite = TRUE,
                               useCache = TRUE)
  }

  if (!suppliedElsewhere("CMDstack", sim)) {
    #These should not be called with RasterToMatch -- each stack has 90 rasters and prepInputs isn't ready for stacks
    #they need to be subset, resampled, and reprojected every year
    sim$CMDstack <- prepInputs(targetFile = "CanCMD_2011-2100.gri",
                               url = extractURL("CMDstack"),
                               destinationPath = dPath,
                               fun = "raster::stack",
                               overwrite = TRUE,
                               useCache = TRUE)
  }

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    message("rasterToMatch not supplied. Generating from LCC2005")
    sim$rasterToMatch <- LandR::prepInputsLCC(studyArea = studyAreaLarge, filename2 = NULL, destinationPath = dPath)
  }
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
