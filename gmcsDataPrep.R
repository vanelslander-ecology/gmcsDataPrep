defineModule(sim, list(
  name = "gmcsDataPrep",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = c(
    person(c("Ian", "MS"), "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person(c("Alex", "M"), "Chubaty", email = "achubaty@for-cast.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(gmcsDataPrep = "0.0.2"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "gmcsDataPrep.Rmd"),
  reqdPkgs = list("crayon", "data.table", "gamlss", "ggplot2", "glmm",
                  "PredictiveEcology/LandR@development (>= 1.1.0.9009)",
                  "ianmseddy/LandR.CS@development (>= 0.0.3.9000)",
                  "MASS", "nlme",
                  "PredictiveEcology/pemisc@development (>= 0.0.3.9002)",
                  "ianmseddy/PSPclean@development (>= 0.1.3.9002)",
                  "sf", "sp", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("biomassModel", "character", "Lambert2005", NA, NA,
                    desc =  paste("The model used to calculate biomass from DBH.",
                                  "Can be either 'Lambert2005' or 'Ung2008'.")),
    defineParameter("cacheClimateRas", "logical", TRUE, NA, NA,
                    desc = paste("should reprojection of climate rasters be cached every year?",
                                 "This will result in potentially > 100 rasters being cached")),
    defineParameter("doAssertion", "logical", getOption("LandR.assertions"), NA, NA,
                    desc = "assertions used to check climate data for NA values in valid pixels"),
    defineParameter("doPlotting", "logical", FALSE, NA, NA, desc = "if true, will plot and save models"),
    defineParameter("GCM", "character", "CCSM4_RCP4.5", NA, NA,
                    desc = paste("if using default climate data, the global climate model and rcp scenario to use.",
                                 "Defaults to CanESM2_RCP4.5. but other available options include CanESM2_RCP4.5 and CCSM4_RCP8.5.",
                                 "These were all generated using a 3 Arc-Minute DEM covering forested ecoregions of Canada.",
                                 "If ATA and CMI are supplied by the user, this parameter is ignored.")),
    defineParameter("growthModel", class = "call",
                    quote(glmmPQL(growth ~ logAge*(ATA + CMI) + ATA*CMI, random = ~1 | OrigPlotID1,
                                  weights = scale(PSPmodelData$plotSize^0.5 * PSPmodelData$periodLength, center = FALSE),
                                  data = PSPmodelData, family = "Gamma"(link = "log"))),
                    NA, NA,
                    desc = paste("Quoted model used to predict growth in PSP data as a function of",
                                 "logAge, CMI, ATA, and their interactions, with PlotID as a random effect")),
    defineParameter("minDBH", "numeric", 10, 0, NA,
                    desc = "The minimum DBH (cm) allowed. Each province uses different criteria for monitoring trees,
                    so absence of entries < min(DBH) does not equate to absence of trees. The following are approximations: ",
                    "Ontario = 2.5 cm (after 1991), Alberta = 7.3, SK = 9.7, BC = 4, and NFI = 9."),
    defineParameter("minTrees", "numeric", 30, 0, NA,
                    desc = paste("The minimum number of trees per initial plot. This is prior to filtering by minimum DBH, ",
                                 " This may not be suitable for every use case. The default is 30, based on the GCB paper ",
                                 "doi: 10.1111/gcb.12994")),
    defineParameter("minSize", "numeric", 0.02, 0, NA,
                    desc = paste("The minimum size (in hectares) of growth plot. All metrics are adjusted for area.",
                                 "The canonical methodlogy did not force a min. size but the minimum size was 0.04 ha.")),
    defineParameter("mortalityModel", class = "call",
                    quote(gamlss::gamlss(formula = mortality ~ logAge * (ATA + CMI) + ATA * CMI,
                                 LandR.CS::own(random = ~ 1|OrigPlotID1, weights = varFunc(~plotSize^0.5 * periodLength)),
                                 sigma.formula = ~logAge + ATA,  nu.formula = ~logAge, family = gamlss.dist::ZAIG,
                                 data = PSPmodelData)), NA, NA,
                    desc = paste("Quoted model used to predict mortality in PSP data as a function of logAge, CMI, ATA, and",
                                 "their interactions, with PlotID as a random effect. Defaults to zero-inflated inverse gaussian",
                                 "glm that requires custom LandR.CS predict function to predict...for now")),
    defineParameter("nullGrowthModel", class = "call",
                    quote(glmmPQL(growth ~ logAge, random = ~1 | OrigPlotID1,
                                  weights = scale(PSPmodelData$plotSize^0.5 * PSPmodelData$periodLength, center = FALSE),
                                  data = PSPmodelData, family = "Gamma"(link = "log"))), NA, NA,
                    desc = "a null model used only for comparative purposes"),
    defineParameter("nullMortalityModel", class = "call",
                    quote(nlme::lme(mortality ~ logAge, random = ~1 | OrigPlotID1,
                                    weights = varFunc(~plotSize^0.5 * periodLength), data = PSPmodelData)), NA, NA,
                    desc = "a null model used only for comparative purposes"),
    defineParameter("prepClimateLayers", "logical", TRUE,
                    desc = "schedule annual retrieval of climate layers when running simulation"),
    defineParameter("PSPab_damageColsToExclude", "numeric",3, NA, NA,
                    desc = paste("if sourcing Alberta PSP, which tree damage sources to exclude, if any.",
                                 "Defaults to Mountain Pine Beetle. Codes can be found in GOA PSP Manual.",
                                 "Any damage sources that are not excluded are modelled as endogenous climate impact")),
    defineParameter("PSPbc_damageColsToExclude", "character", "IBM", NA, NA,
                    desc = paste("for BC PSP, which tree damage sources to exclude, if any. Defaults to Mountain Pine Beetle.",
                                 "Any damage sources that are not excluded are modelled as endogenous climate impact")),
    defineParameter("PSPnfi_damageColsToExclude", "character", "IB", NA, NA,
                    desc = paste("if sourcing NFI PSP, which tree damage sources to exclude, if any. Defaults to bark beetles.",
                                 "Any damage sources that are not excluded are modelled as endogenous climate impact")),
    defineParameter("PSPdataTypes", "character", "all", NA, NA,
                    desc = paste("Which PSP datasets to source, defaulting to all. Other available options include",
                                 "'BC', 'AB', 'SK', 'NFI', and 'dummy'. 'dummy' should be used for unauthorized users.")),
    defineParameter("PSPperiod", "numeric", c(1958, 2011), NA, NA,
                    desc = paste("The years by which to compute climate normals and subset sampling plot data.",
                                 "Must be a vector of at least length 2.")),
    defineParameter("PSPvalidationPeriod", "numeric", NULL, NA, NA,
                    desc = paste("the period to build the validation dataset. Must be greater than PSPperiod",
                                 "e.g. c(1958-2018). Subsequent observations are used only if they are within this period,",
                                 "but outside the fitting period. E.g. Successive measurements in 2004 and 2017",
                                 "would be used even though the first measurement falls outside the 2011 fitting period",
                                 "as the 2011 cutoff would remove this paired obsevation from the fitting data.",
                                 "If NULL, then validation dataset will instead be randomly sampled from available measurements.")),
    defineParameter("stableClimateQuantile", "numeric", 0.9, 0, 1,
                    desc = paste("if the simulation time exceeds the projected climate years",
                                 "then missing years will be randomly sampled from a subset of the",
                                 "projected climate using this quantile as a threshold")),
    defineParameter("useHeight", "logical", TRUE, NA, NA,
                    desc = paste("Use height be used to calculate biomass (in addition to DBH). If height is NA for individual",
                                 "trees, then only DBH will be used for those measurements")),
    defineParameter("validationProportion", "numeric", 0.20, 0, 1,
                    desc = "proportion of data to use in validation set. Will be overridden by PSPvalidationPeriod"),
    defineParameter("yearOfFirstClimateImpact", 'numeric', 2011, NA, NA,
                    desc = paste("the first year for which to calculate climate impacts")),
    defineParameter(".useCache", "character", ".inputObjects", NA, NA,
                    desc = paste("Should this entire module be run with caching activated?",
                                 "This is generally intended for data-type modules,",
                                 "where stochasticity and time are not relevant."))
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "ATAstack", objectClass = "RasterStack",
                 desc = "annual projected mean annual temperature anomalies, units stored as tenth of a degree"),
    expectsInput(objectName = "CMIstack", objectClass = "RasterStack",
                 desc = "annual projected mean climate moisture deficit"),
    expectsInput(objectName = "CMInormal", objectClass = "RasterLayer",
                 desc = "Climate Moisture Index Normals from 1950-2010"),
    expectsInput(objectName = "PSPmeasure_gmcs", objectClass = "data.table", desc = "standardized tree measurements for PSPs",
                 sourceURL = "https://drive.google.com/file/d/1LmOaEtCZ6EBeIlAm6ttfLqBqQnQu4Ca7/"),
    expectsInput(objectName = "PSPplot_gmcs", objectClass = "data.table", desc = "standardized plot-level attributes for PSPs",
                 sourceURL = "https://drive.google.com/file/d/1LmOaEtCZ6EBeIlAm6ttfLqBqQnQu4Ca7/"),
    expectsInput(objectName = "PSPgis_gmcs", objectClass = "data.table", desc = "PSP plot data as sf object",
                 sourceURL = "https://drive.google.com/file/d/1LmOaEtCZ6EBeIlAm6ttfLqBqQnQu4Ca7/"),
    expectsInput(objectName = "PSPclimData", objectClass = "data.table",
                 desc = paste("climate data for each PSP from ClimateNA, in the native format returned by ClimateNA with csv",
                              "Temp is represented as degrees, not tenth of degrees as with the raster data"),
                 sourceURL = "https://drive.google.com/file/d/17RrqiepZaGp9bVPkwWEQa0Nu4aQxRL8s/view?usp=share_link"),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "template raster for ATA and CMI"),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame",
                 desc = "this area will be used to crop climate rasters", sourceURL = NA),
    expectsInput(objectName = "studyAreaPSP", objectClass = "SpatialPolygonsDataFrame",
                 desc = paste("this area will be used to subset PSP plots before building the statistical model.",
                              "Currently PSP datasets with repeat measures exist only for Saskatchewan,",
                              "Alberta, Boreal British Columbia, and Ontario"), sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "ATA", objectClass = "RasterLayer",
                  desc = "annual temperature anomaly, resampled using `rasterToMatch`"),
    createsOutput(objectName = "ATAstack", objectClass = "RasterStack",
                  desc = "annual projected mean annual temperature anomalies, units stored as tenth of a degree"),
    createsOutput(objectName = "CMI", objectClass = "RasterLayer",
                  desc = "climate moisture deficit at `time(sim)`, resampled using `rasterToMatch`"),
    createsOutput(objectName = "CMIstack", objectClass = "RasterStack",
                  desc = "annual projected mean climate moisture deficit"),
    createsOutput(objectName = "gcsModel", objectClass = "ModelObject?",
                  desc = "growth mixed effect model with normalized log(age), ATA, and CMI as predictors"),
    createsOutput(objectName = "mcsModel", objectClass = "ModelObject?",
                  desc = "mortality mixed effect model with normalized log(age), ATA, and CMI as predictors"),
    createsOutput(objectName = "PSPmodelData", objectClass = "data.table",
                  desc = "PSP growth mortality calculations"),
    createsOutput(objectName = "PSPvalidationData", objectClass = "data.table",
                  desc = "validation dataset for testing model")
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

      if (P(sim)$prepClimateLayers) {
        sim <- checkRasters(sim)
        sim <- scheduleEvent(sim, start(sim), eventType = "prepRasters", eventPriority = 1)
      }
      # sim <- scheduleEvent(sim, end(sim), eventType = "scrubGlobalEnv", eventPriority = 9)

    },
    prepRasters = {
      if (time(sim) < P(sim)$yearOfFirstClimateImpact) {
        sim$ATA <- sim$rasterToMatch #replace with a raster with no climate anomaly
        sim$ATA[!is.na(sim$ATA)] <- 0
        sim$CMI <- sim$CMInormal #replace with a raster with no climate anomaly
      } else {
        #check if current time is later than projected years
        if (time(sim) - start(sim) <= raster::nlayers(sim$ATAstack) - 1) {
          #-1 because start(sim) is a layer
          timeToUse <- time(sim)
        } else {
          #the simulation time must be later than the projected climate, e.g. 2102 - 2011 > 90 years of projected annual climate
          availableYears <- P(sim)$yearOfFirstClimateImpact + 1:raster::nlayers(sim$ATAstack) - 1
          cutoff <- quantile(availableYears, probs = P(sim)$stableClimateQuantile)
          timeToUse <- sample(availableYears[availableYears >= cutoff], size = 1)
          message("simulation time has surpassed climate layers - using ", timeToUse, " for year ", time(sim))
        }
        sim$ATA <- getCurrentClimate(climStack = sim$ATAstack, time = timeToUse, isATA = TRUE)
        sim$CMI <- getCurrentClimate(climStack = sim$CMIstack, time = timeToUse)
      }

      sim <- scheduleEvent(sim, time(sim) + 1, eventType = "prepRasters", eventPriority = 1)
    },

    scrubGlobalEnv = {
      on.exit(rm(PSPmodelData, envir = globalenv()), add = TRUE)
    },

    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {

  if (is.null(sim$mcsModel) | is.null(sim$gcsModel)) {
    message("building climate-sensitive growth and mortailty models")
    #stupid-catch
    if (length(P(sim)$PSPperiod) < 2) {
      stop("Please supply P(sim)$PSPperiod of length 2 or greater")
    }

    if (any(is.null(sim$PSPmeasure_gmcs), is.null(sim$PSPplot_gmcs), is.null(sim$PSPgis_gmcs))) {
      stop("The PSP objects are being supplied incorrectly. Please review loadOrder argument in simInit")
    }

    sim$PSPmodelData <- Cache(prepModelData,
                              studyAreaPSP = sim$studyAreaPSP,
                              PSPgis = sim$PSPgis_gmcs,
                              PSPmeasure = sim$PSPmeasure_gmcs,
                              PSPplot = sim$PSPplot_gmcs,
                              PSPclimData = sim$PSPclimData,
                              useHeight = P(sim)$useHeight,
                              biomassModel = P(sim)$biomassModel,
                              PSPperiod = P(sim)$PSPperiod,
                              minDBH = P(sim)$minDBH,
                              minSize = P(sim)$minSize,
                              minTrees = P(sim)$minTrees,
                              useCache = P(sim)$.useCache,
                              userTags = c("gmcsDataPrep", "prepModelData"))

    ####building validation data####
    message("Preparing validation dataset")
    if (!is.null(sim$PSPvalidationPeriod)) {
      #build validation data from observations in PSPvalidationPeriod that also aren't in model data
      sim$PSPvalidationData <- Cache(prepModelData,
                                     studyAreaPSP = sim$studyAreaPSP,
                                     PSPgis = sim$PSPgis_gmcs,
                                     PSPmeasure = sim$PSPmeasure_gmcs,
                                     PSPplot = sim$PSPplot_gmcs,
                                     PSPclimData = sim$PSPclimData,
                                     useHeight = P(sim)$useHeight,
                                     biomassModel = P(sim)$biomassModel,
                                     PSPperiod = P(sim)$PSPvalidationPeriod,
                                     minDBH = P(sim)$minDBH,
                                     minSize = P(sim)$minSize,
                                     minTrees = P(sim)$minTrees,
                                     useCache = P(sim)$.useCache,
                                     userTags = c("gmcsDataPrep", "prepValidationData"))

      #this removes all observations that are identical
      sim$PSPvalidationData <- setkey(sim$PSPvalidationData)[!sim$PSPmodelData]
      #this removes all observations for which there is no random effect in the fitting data
      sim$PSPvalidationData <- sim$PSPvalidationData[OrigPlotID1 %in% sim$PSPmodelData$OrigPlotID1, ]
    } else {
      # sample validation data from PSPmodelData
      outData <- Cache(FUN = prepValidationData,
                       PSPmodelData = sim$PSPmodelData,
                       validationProportion = P(sim)$validationProportion,
                       useCache = P(sim)$.useCache,
                       userTags = c("gmcsDataPrep", "prepValidationData"))
      sim$PSPmodelData <- outData$PSPmodelData #with some plots removed
      sim$PSPvalidationData <- outData$PSPvalidationData

    }

    ####model building####
    #only replace the models if NULL, so user can supply their own models
    if (is.null(sim$gcsModel)) {
      sim$gcsModel <- Cache(gmcsModelBuild,
                            PSPmodelData = sim$PSPmodelData,
                            model = P(sim)$growthModel,
                            userTags = c("gcsModel"))
    }
    if (is.null(sim$mcsModel)) {
      sim$mcsModel <- Cache(gmcsModelBuild,
                            PSPmodelData = sim$PSPmodelData,
                            model = P(sim)$mortalityModel,
                            userTags = c("mcsModel"))
    }
    if (is.null(sim$nullGrowthModel)) {
      sim$nullGrowthModel <- Cache(gmcsModelBuild,
                                   PSPmodelData = sim$PSPmodelData,
                                   model = P(sim)$nullMortalityModel,
                                   userTags = c("nullGrowthModel"))
    }
    if (is.null(sim$nullMortalityModel)) {
      sim$nullMortalityModel <- Cache(gmcsModelBuild,
                                      PSPmodelData = sim$PSPmodelData,
                                      model = P(sim)$nullMortalityModel,
                                      userTags = c("nullMortalityModel"))
    }
    #reporting NLL as comparison statistic - could do RME or MAE?
    if (nrow(sim$PSPvalidationData) > 0) {
      compareModels(nullGrowth = sim$nullGrowthModel,
                    nullMortality = sim$nullMortalityModel,
                    gcs = sim$gcsModel,
                    mcs = sim$mcsModel,
                    validationData = sim$PSPvalidationData,
                    doPlotting = P(sim)$doPlotting,
                    path = outputPath(sim))
    }
  }

  return(invisible(sim))
}

checkRasters <- function(sim){
  if (!compareRaster(sim$ATAstack, sim$rasterToMatch, stopiffalse = FALSE)) {
    #must postProcess the rasters as a stack with terra
    sim$ATAstack <- Cache(postProcess, sim$ATAstack, rasterToMatch = sim$rasterToMatch,
                          studyArea = sim$studyArea, userTags = c("postProcess", "ATAstack"))
  }
  if (!compareRaster(sim$CMIstack, sim$rasterToMatch, stopiffalse = FALSE)) {
    sim$CMIstack <- Cache(postProcess, sim$CMIstack, rasterToMatch = sim$rasterToMatch,
                          studyArea = sim$studyArea, userTags = c("postProcess", "CMIstack"))
  }
  return(sim)
}

prepModelData <- function(studyAreaPSP, PSPgis, PSPmeasure, PSPplot, PSPclimData, useHeight,
                          biomassModel, PSPperiod, minDBH, minSize, minTrees) {


  #Crop points to studyAreaPSP
  if (!is.null(studyAreaPSP)) {
    tempSA <- st_as_sf(studyAreaPSP) %>%
      st_transform(tempSA, crs = crs(PSPgis))

    message(yellow("Filtering PSPs to study Area..."))
    PSP_sa <- PSPgis[tempSA,]
    message(yellow(paste0("There are "), nrow(PSP_sa), " PSPs in your study area"))
  } else {
    PSP_sa <- PSPgis
  }
  #Restrict climate variables to only thosee of interest.. should be param
  #Calculate the derived variable CMI - previously calculated in input objects
  PSPclimData[, "CMI" := MAP - Eref]
  PSPclimData <- PSPclimData[, .(OrigPlotID1, Year, CMI, MAT)]

  #Filter other PSP datasets to those in study Area
  PSPmeasure <- PSPmeasure[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]
  PSPplot <- PSPplot[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]
  PSPclimData <- PSPclimData[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]

  #might as well drop species with no biomass match

  #length(PSPclimData)/length(PSP_sa) should always yield a whole number.
  #Filter data by study period
  message(yellow("Filtering by study period..."))
  PSPmeasure <- PSPmeasure[MeasureYear > min(PSPperiod) &
                             MeasureYear < max(PSPperiod),]
  PSPplot <- PSPplot[MeasureYear > min(PSPperiod) &
                       MeasureYear < max(PSPperiod),]
  PSPclimData[Year > min(PSPperiod) & Year < max(PSPperiod),]

  #Join data (should be small enough by now)
  PSPmeasure <- PSPmeasure[PSPplot, on = c('MeasureID', 'OrigPlotID1', 'MeasureYear')]

  #Restrict to trees > minDBH
  message(yellow("Filtering by min. DBH"))
  PSPmeasure <- PSPmeasure[DBH >= minDBH,]

  #Filter by > minTrees at first measurement (P) to ensure forest. Default 30
  message(yellow("Filtering by minimum trees in earliest measurement"))
  forestPlots <- PSPmeasure[MeasureYear == baseYear, .(measures = .N), OrigPlotID1] %>%
    .[measures >= minTrees,]
  PSPmeasure <- PSPmeasure[OrigPlotID1 %in% forestPlots$OrigPlotID1,]
  PSPplot <- PSPplot[OrigPlotID1 %in% PSPmeasure$OrigPlotID1,]
  repeats <- PSPplot[, .(measures = .N), by = OrigPlotID1]
  message(yellow(paste0("There are "), nrow(repeats), " PSPs with min.", minTrees, " trees at earliest measurement"))

  #Filter by min size
  message(yellow("Filtering by min. plot size"))
  bigEnough <- PSPplot[PlotSize > minSize, .N, .(OrigPlotID1)]
  PSPplot <- PSPplot[OrigPlotID1 %in% bigEnough$OrigPlotID1]
  message(yellow("There are ", nrow(bigEnough), " plots meeting the minimum plot size"))

  ## subset by biomass, because some plots have no species that can be estimated
  ## these will be counted in the min trees requirement, but may result in a plot of NA biomass if repeat measures = 2+
  if (useHeight) {
    PSPmeasureNoHeight <- PSPmeasure[is.na(Height)]
    PSPmeasureHeight <- PSPmeasure[!is.na(Height)]
    tempOut <- biomassCalculation(species = PSPmeasureHeight$newSpeciesName,
                                  DBH = PSPmeasureHeight$DBH,
                                  height = PSPmeasureHeight$Height,
                                  includeHeight = TRUE,
                                  equationSource = biomassModel)
    #check if height is missing, join if so -- function fails if data.table is empty
    if (nrow(PSPmeasureNoHeight) > 0) {
      tempOutNoHeight <- biomassCalculation(species = PSPmeasureNoHeight$newSpeciesName,
                                            DBH = PSPmeasureNoHeight$DBH,
                                            height = PSPmeasureNoHeight$Height,
                                            includeHeight = FALSE,
                                            equationSource = biomassModel)
      tempOut$biomass <- c(tempOut$biomass, tempOutNoHeight$biomass)
      tempOut$missedSpecies <- unique(c(tempOut$missedSpecies, tempOutNoHeight$missedSpecies))
      PSPmeasure <- rbind(PSPmeasureHeight, PSPmeasureNoHeight)
    }
    PSPmeasure[, biomass := tempOut$biomass]
    setkey(PSPmeasure, MeasureID, OrigPlotID1, TreeNumber)
  } else {
    tempOut <- biomassCalculation(species = PSPmeasure$newSpeciesName,
                                  DBH = PSPmeasure$DBH,
                                  height = PSPmeasure$Height,
                                  includeHeight = FALSE,
                                  equationSource = biomassModel)
    PSPmeasure$biomass <- tempOut$biomass
  }

  message(yellow("No biomass estimate possible for these species: "))
  print(tempOut$missedSpecies)

  #Filter by 3+ repeat measures - must be last filter criteria.
  #Some plots share ID but have different trees so simple count of plots insufficient to find repeat measures
  #Reduce PSPmeasure to MeasureID, PlotID1, PlotID2, MeasureYear, remove duplicates
  # then find repeat measures of MeasureYear, match back to MeasureID in both PSPplot and PSPmeasure.
  message(yellow("Filtering by at least 3 repeat measures per plot"))
  repeats <- PSPmeasure[, .(MeasureID, OrigPlotID1, MeasureYear)] %>%
    .[!duplicated(.)] %>%
    .[, .('repeatMeasures' = .N), by = .(OrigPlotID1)] %>%
    .[repeatMeasures > 2]
  setkey(repeats, OrigPlotID1)
  setkey(PSPmeasure, OrigPlotID1)
  PSPmeasure <- PSPmeasure[repeats]
  PSPplot <- PSPplot[MeasureID %in% PSPmeasure$MeasureID] #this ensures all plots have biomass/repeat measures

  message(yellow(paste0("There are "), nrow(repeats), " PSPs with min. 3 repeat measures"))

  climate <- PSPclimData[OrigPlotID1 %in% PSPmeasure$OrigPlotID1, .("CMI" = mean(CMI), "MAT" = mean(MAT)), OrigPlotID1]
  #not all PSP plots exist in PSPclimData - this must be fixed - July 2020 IE
  PSPmeasure <- PSPmeasure[OrigPlotID1 %in% climate$OrigPlotID1,]
  PSPplot <- PSPplot[climate, on = "OrigPlotID1"]

  if (any(nrow(PSPclimData) == 0, nrow(PSPmeasure) == 0, nrow(PSPgis) == 0)) {
    stop('all existing PSP data has been filtered.Try adjusting parameters')
  }

  TrueUniques <- PSPmeasure[, .N, .(OrigPlotID1)]

  pSppChange <- lapply(TrueUniques$OrigPlotID1,
                       FUN = sumPeriod, m = PSPmeasure, p = PSPplot, clim = PSPclimData)
  PSPmodelData <- rbindlist(pSppChange)
  PSPmodelData$species <- factor(PSPmodelData$species)
  PSPmodelData$sppLong <- factor(PSPmodelData$sppLong)

  #Standardize by plotSize and change units from kg/ha to g/m2. = *1000 g/kg / 10000 m2/ha
  PSPmodelData <- PSPmodelData[, growth_gm2 := growth/plotSize/10] %>%
    .[, mortality_gm2 := mortality/plotSize/10] %>%
    .[, netBiomass_gm2 := netBiomass/plotSize/10]
  #26/02/2019 after discussion we decided not to include species in model.
  # Decided to parameterize inclusion of ATA or year. ATA is better for projecting, but year is canonical
  # Sum species-specific mortality, growth, and net biomass by plot and year
  # growth is set to 0.1 if it would be 0 (to avoid model error-  0 growth is caused by measurement error)
  PSPmodelData <- PSPmodelData[, .("growth" = pmax(1, sum(growth_gm2)), "mortality" = sum(mortality_gm2),
                                   "netBiomass" = sum(netBiomass_gm2), "biomass" = sum(biomass),
                                   'CMI' = mean(CMI), 'CMIA' = mean(CMIA),
                                   'AT' = mean(AT), "ATA" = mean(ATA), 'standAge' = mean(standAge),
                                   'logAge' = mean(logAge), "periodLength" = mean(periodLength),
                                   'year' = mean(year), 'plotSize' = mean(plotSize)),
                               by = c("OrigPlotID1", "period")]

  return(PSPmodelData)
}

gmcsModelBuild <- function(PSPmodelData, model) {

  assign("PSPmodelData", PSPmodelData, globalenv())
  #this prevents cache envir arg from conflicting with eval envir
  gmcsModel <- eval(model, envir = environment())

  return(gmcsModel)
}

getCurrentClimate <- function(climStack, time, isATA = FALSE) {

  yearRas <- climStack[[grep(pattern = time, x = names(climStack))]]
  if (is.null(yearRas)) { stop("error with naming of climate raster stack")}
  yearRas <- raster::readAll(yearRas)
  #TODO: the CMI layer is being written to the temp drive... ATA is not because of value change

  if (isATA == TRUE) {
    #ATA was stored as an integer AND as tenth of a degree. Data uses degrees, so divide by ten
    #ClimateNA point data (used for historical PSP climate) is output as degrees, so this discrepancy must be corrected
    #better to explicitly do it here than implicitly in the input data
    yearRas[] <- yearRas[] / 10 #scale to degrees to make comparable with model data
  }
  return(yearRas)
}

pspIntervals <- function(i, M, P, Clim) {
  #Calculate climate variables
  #TODO: this should take the mean of whatever variable over the study period
  #the anomaly can be calculated outside the function - this would support other climate variables
  #ACMI and ATA were added individually in separate model
  CMI <- mean(Clim$CMI[Clim$Year >= P$MeasureYear[i] & Clim$Year <= P$MeasureYear[i + 1]])
  ACMI <- mean(Clim$CMI[Clim$Year >= P$MeasureYear[i] & Clim$Year <= P$MeasureYear[i + 1]]) - P$CMI[1]
  ATA <- mean(Clim$MAT[Clim$Year >= P$MeasureYear[i] & Clim$Year <= P$MeasureYear[i + 1]]) - P$MAT[1]
  AT <- mean(Clim$MAT[Clim$Year >= P$MeasureYear[i] & Clim$Year <= P$MeasureYear[i + 1]])
  period <- paste0(P$MeasureYear[i], "-", P$MeasureYear[i + 1])

  m1 <- M[MeasureYear == P$MeasureYear[i]]
  m2 <- M[MeasureYear == P$MeasureYear[i + 1]]
  censusLength <- P$MeasureYear[i + 1] - P$MeasureYear[i]
  year <- ceiling(sum(P$MeasureYear[i] + P$MeasureYear[i + 1]) / 2)
  living1 <- m1[m1$TreeNumber %in% m2$TreeNumber]
  living2 <- m2[m2$TreeNumber %in% m1$TreeNumber]
  dead <- m1[!m1$TreeNumber %in% m2$TreeNumber]
  newborn <- m2[!m2$TreeNumber %in% m1$TreeNumber]

  if (nrow(living1) != nrow(living2) | nrow(living1) == 0) {
    warning("there is a problem in the PSP data with the plots: ", unique(m1$MeasureID), " ", unique(m2$MeasureID))
    #nrow(living1) == 0 will happen if tree numbers change between measurements
  }
  #Find observed annual changes in mortality and growth
  living2$origBiomass <- living1$biomass
  #growth cannot be negative, by definition
  living2[biomass < origBiomass, biomass := origBiomass]

  living <- living2[, .(newGrowth =  sum(biomass - origBiomass)/censusLength,
                        biomass = sum(biomass)),,
                    c("Species", "newSpeciesName")] %>%
    setkey(., Species, newSpeciesName)

  newborn <- newborn[, .(newGrowth = sum(biomass) / (censusLength / 2),
                         biomass = sum(biomass) / (censusLength / 2)),
                     c("Species", "newSpeciesName")] %>%
    setkey(., Species, newSpeciesName)
  #measure from census midpoint for new seedlings
  dead <- dead[, .(mortality = sum(biomass) / censusLength), by = c("Species", "newSpeciesName")] %>%
    setkey(., Species, newSpeciesName)

  #Find unobserved growth and mortality.
  #Not necessary when summing by species, b/c we can't assign species for unobserved trees
  #Unobserved growth and mortality = ~1% of observed, so climate influences on this are trivial.
  #Leaving this in nonetheless, in case we change methods
  #Unobserved recruits U = N * R * M * L
  #N = # of trees with DBH between 10 and 15
  # N <- nrow(t2[DBH <= 15]) # where t2 = the second measurement
  # (I changed t from it's original 'm', to avoid confusion with metres squared and M mortality)
  # #R = number of recruits between two successive censuses (trees in t2 not in t1)/census length
  # R <- nrow(newborn)/censusLength/N #I am not 100% sure if we divide by N or total stems in plot
  # #M = Mortality rate, n-trees with DBH 10 -15 that died between two census/interval length
  # M <- nrow(dead[DBH <= 15,])/censusLength/N
  # #L = census interval length
  # #Next calculate the median growth of the 10-15 DBH class, assume they grew to midpoint.
  # UnobservedR <- N * R * M * censusLength
  # UnobservedM <- UnobservedR * median(t2$biomass[t2$DBH <= 15])/censusLength/2
  # #assume unobserved trees died at midpoint. I think this overestimates growth and mortality
  # totalM <- UnobservedM + observedMortality
  # totalG <- UnobservedM + observedGrowth

  changes <- bind(newborn, living)


  changes$mortality <- 0
  dead$newGrowth <- 0
  changes <- bind(changes, dead)
  changes[is.na(changes)] <- 0
  changes <- changes[, .("netGrowth" = sum(newGrowth), "mortality" = sum(mortality),
                         biomass = sum(biomass, na.rm = TRUE)),
                     by = c("Species", "newSpeciesName")]
  changes <- changes[, .("species" = Species,
                         "sppLong" = newSpeciesName,
                         "netBiomass" = (netGrowth - mortality),
                         "biomass" = biomass,
                         "growth" = netGrowth,
                         mortality)]

  changes$period <- period
  changes$CMI <- CMI
  changes$CMIA <- ACMI
  changes$ATA <- ATA
  changes$OrigPlotID1 <- P$OrigPlotID1[1]
  changes$year <- year
  changes$standAge <- P$baseSA[1] + P$MeasureYear[i + 1] - P$MeasureYear[1]
  changes$logAge <- log(changes$standAge)
  changes$plotSize <- P$PlotSize[1]
  changes$periodLength <- censusLength
  changes$AT <- AT

  setcolorder(changes, c("OrigPlotID1", "period", "species", "sppLong", "growth", "mortality", "netBiomass",
                         "CMI", "CMIA", "AT", "ATA", "standAge", "logAge", "plotSize", "periodLength"))
  return(changes)
}

sumPeriod <- function(x, m, p, clim) {
  # Duplicate plots arise from variable 'stand' (OrigPlotID2) that varied within the same plot.
  # this has been corrected by treating these as new plot ids.
  # note OrigPlotID2 has been removed in latest edition of PSPs, as stand/plot fields were concatenated
  # TODO: review this code and confirm if it is still necessary
  # Tree No. is not unique between stands, which means the same plot can have duplicate trees.
  # sort by year. Calculate the changes in biomass, inc. unobserved growth and mortality
  # must match MeasureID between plot and measure data;
  m <- m[OrigPlotID1 == x,] #subset data by plot
  p <- p[MeasureID %in% m$MeasureID]
  clim <- clim[OrigPlotID1 %in% x,]
  p <- setkey(p, MeasureYear)
  m <- setkey(m, TreeNumber)
  periods <- nrow(p) - 1

  #For each interval
  pSums <- lapply(1:periods, FUN = pspIntervals, M = m, P = p, Clim = clim)

  pSums <- rbindlist(pSums)
  return(pSums)
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")

  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("PSPmeasure_gmcs", sim) |
      !suppliedElsewhere("PSPplot_gmcs", sim) |
      !suppliedElsewhere("PSPgis_gmcs", sim) ) {
    message("sourcing PSP data for gmcsDataPrep...")

    if ("dummy" %in% P(sim)$PSPdataTypes) {
      message("generating randomized PSP data")
      sim$PSPmeasure_gmcs <- prepInputs(targetFile = "randomizedPSPmeasure_sppParams.rds",
                                        archive = "randomized_LandR_speciesParameters_Inputs.zip",
                                        url =  extractURL('PSPmeasure_gmcs', sim),
                                        destinationPath = dPath,
                                        fun = "readRDS")

      sim$PSPplot_gmcs <- prepInputs(targetFile = "randomizedPSPplot_sppParams.rds",
                                     archive = "randomized_LandR_speciesParameters_Inputs.zip",
                                     url = extractURL('PSPplot_gmcs', sim),
                                     destinationPath = dPath,
                                     fun = "readRDS")

      sim$PSPgis_gmcs <- prepInputs(targetFile = "randomizedPSPgis_sppParams.rds",
                                    archive = "randomized_LandR_speciesParameters_Inputs.zip",
                                    url = extractURL('PSPgis_gmcs', sim),
                                    overwrite = TRUE,
                                    destinationPath = dPath,
                                    fun = "readRDS")
    } else {
      if (!any(c("BC", "AB", "SK", "ON", "NFI", "all") %in% P(sim)$PSPdataTypes)) {
        stop("Please review P(sim)$dataTypes - incorrect value specified")
      }

      PSPmeasure_gmcs <- list()
      PSPplot_gmcs <- list()

      if ("BC" %in% P(sim)$PSPdataTypes | "all" %in% P(sim)$PSPdataTypes) {
        PSPbc <- prepInputsBCPSP(dPath = dPath)
        PSPbc <- PSPclean::dataPurification_BCPSP(treeDataRaw = PSPbc$treeDataRaw,
                                                  plotHeaderDataRaw = PSPbc$plotHeaderDataRaw,
                                                  damageAgentCodes = PSPbc$pspBCdamageAgentCodes,
                                                  codesToExclude = P(sim)$PSPbc_damageColsToExclude)
        PSPmeasure_gmcs[["BC"]] <- PSPbc$treeData
        PSPplot_gmcs[["BC"]] <- PSPbc$plotHeaderData
      }
      if ("AB" %in% P(sim)$PSPdataTypes | "all" %in% P(sim)$PSPdataTypes) {
        PSPab <- prepInputsAlbertaPSP(dPath = dPath)
        PSPab <- PSPclean::dataPurification_ABPSP(treeMeasure = PSPab$pspABtreeMeasure,
                                                  plotMeasure = PSPab$pspABplotMeasure,
                                                  tree = PSPab$pspABtree,
                                                  plot = PSPab$pspABplot,
                                                    codesToExclude = P(sim)$PSPab_damageColsToExclude)
        PSPmeasure_gmcs[["AB"]] <- PSPab$treeData
        PSPplot_gmcs[["AB"]] <- PSPab$plotHeaderData
      }

      if ("SK" %in% P(sim)$PSPdataTypes | "all" %in% P(sim)$PSPdataTypes) {
        PSPsk <- prepInputsSaskatchwanPSP(dPath = dPath)
        PSPsk <- PSPclean::dataPurification_SKPSP(SADataRaw = PSPsk$SADataRaw,
                                                  plotHeaderRaw = PSPsk$plotHeaderRaw,
                                                  measureHeaderRaw = PSPsk$measureHeaderRaw,
                                                  treeDataRaw = PSPsk$treeDataRaw)
        PSPmeasure_gmcs[["SK"]] <- PSPsk$treeData
        PSPplot_gmcs[["SK"]] <- PSPsk$plotHeaderData

        TSPsk <- prepInputsSaskatchwanTSP(dPath = dPath)
        TSPsk <- PSPclean::dataPurification_SKTSP_Mistik(compiledPlotData = TSPsk$compiledPlotData,
                                                         compiledTreeData = TSPsk$compiledTreeData)
        PSPmeasure_gmcs[["SKtsp"]] <- TSPsk$treeData
        PSPplot_gmcs[["SKtsp"]] <- TSPsk$plotHeaderData
      }

      if ("ON" %in% P(sim)$PSPdataTypes | "all" %in% P(sim)$PSPdataTypes) {
        PSPon <- prepInputsOntarioPSP(dPath = dPath)
        #sppEquiv should not be subset to species of interest the way LandR requires
        #the latin is used to translate species into common names for the biomass equations
        sppEquivForON <- LandR::sppEquivalencies_CA
        #removing LandR:: may cause a local copy to be used - avoid.
        PSPon <- PSPclean::dataPurification_ONPSP(PSPon, sppEquiv = sppEquivForON)
        PSPmeasure_gmcs[["ON"]] <- PSPon$treeData
        PSPplot_gmcs[["ON"]] <- PSPon$plotHeaderData
      }

      if ("NFI" %in% P(sim)$PSPdataTypes | "all" %in% P(sim)$PSPdataTypes) {
        PSPnfi <- prepInputsNFIPSP(dPath = dPath)
        PSPnfi <- PSPclean::dataPurification_NFIPSP(lgptreeRaw = PSPnfi$pspTreeMeasure,
                                                    lgpHeaderRaw = PSPnfi$pspHeader,
                                                    approxLocation = PSPnfi$pspLocation,
                                                    treeDamage = PSPnfi$pspTreeDamage,
                                                    codesToExclude = P(sim)$PSPnfi_damageColsToExclude)
        PSPmeasure_gmcs[["NFI"]] <- PSPnfi$treeData
        PSPplot_gmcs[["NFI"]] <- PSPnfi$plotHeaderData
      }

      PSPmeasure_gmcs <- rbindlist(PSPmeasure_gmcs, fill = TRUE)
      PSPplot_gmcs <- rbindlist(PSPplot_gmcs, fill = TRUE)
      PSPgis_gmcs <- geoCleanPSP(Locations = PSPplot_gmcs)
      locCols <- c("Zone", "Datum", "Easting", "Northing", "Latitude", "Longitude")
      locCols <- locCols[locCols %in% names(PSPplot_gmcs)]
      set(PSPplot_gmcs, NULL, locCols, NULL)
      #keep only plots with valid coordinates
      PSPmeasure_gmcs <- PSPmeasure_gmcs[OrigPlotID1 %in% PSPgis_gmcs$OrigPlotID1,]
      PSPplot_gmcs <- PSPplot_gmcs[OrigPlotID1 %in% PSPgis_gmcs$OrigPlotID1,]
      sim$PSPmeasure_gmcs <- PSPmeasure_gmcs
      sim$PSPplot_gmcs <- PSPplot_gmcs
      sim$PSPgis_gmcs <- PSPgis_gmcs
    }
  }

  if (!suppliedElsewhere("studyArea", sim)) {
    message("studyArea not supplied. Using a random area in Alberta")
    sim$studyArea <- randomStudyArea(size = 1e6*50)
  }

  if (!suppliedElsewhere("PSPclimData", sim)) {
    sim$PSPclimData <- prepInputs(url = extractURL("PSPclimData"),
                                  destinationPath = dPath,
                                  targetFile = "PSPforClimateNA_BC_AB_SK_ON_NFI_1920-2021SY.csv",
                                  fun = "data.table::fread")
    setnames(sim$PSPclimData, old = c("id1", "id2"), new = c("OrigPlotID1", "OrigPlotID2"))

    sim$PSPclimData <- sim$PSPclimData[MAT != -9999] #missing plots get -9999 as variable
  }

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    message("rasterToMatch not supplied. Generating from LCC2005")
    sim$rasterToMatch <- prepInputsLCC(studyArea = sim$studyArea, filename2 = NULL, destinationPath = dPath)
  }
  if (P(sim)$prepClimateLayers) {
    if (!suppliedElsewhere("ATAstack", sim)) {

      #These should not be called using rasterToMatch (stack, memory)
      if (P(sim)$GCM == "CCSM4_RCP4.5") {
        ata.url <- "https://drive.google.com/open?id=1sGRp0zNjlQUg6LXpEgG4anT2wx1jvuUQ"
        ata.tf <- "Can3ArcMinute_CCSM4_RCP45_ATA2011-2100.grd"
        ata.arc <- "Canada3ArcMinute_CCSM4_45_ATA2011-2100.zip"
      } else if (P(sim)$GCM == "CanESM2_RCP4.5") {
        ata.url <- "https://drive.google.com/open?id=1d8wy70gxDcO2MKsQt7tYBpryKudE-99h"
        ata.tf <- "Can3ArcMinute_CanESM2_RCP45_ATA2011-2100.grd"
        ata.arc <- "Canada3ArcMinute_ATA2011-2100.zip"
      } else if (P(sim)$GCM == "CCSM4_RCP8.5") {
        ata.url <- "https://drive.google.com/open?id=1_LXyPRdWbUj_Kk3ab-bgjqDXcowg_lpM"
        ata.tf <- "Can3ArcMinute_CCSM4_RCP85_ATA2011-2100.grd"
        ata.arc <- "Canada3ArcMinute_CCSM4_85_ATA2011-2100.zip"
      } else {
        stop("unrecognized GCM in P(sim)$GCM")
      }

      sim$ATAstack <- prepInputs(targetFile = ata.tf,
                                 archive = ata.arc,
                                 alsoExtract = "similar",
                                 url = ata.url,
                                 destinationPath = dPath,
                                 studyArea = sim$studyArea,
                                 rasterToMatch = sim$rasterToMatch,
                                 fun = "raster::stack",
                                 useCache = TRUE,
                                 userTags = c(currentModule(sim), "ATAstack"))
      #if a pixel is 10 degrees above average, needs 4S
    }


    if (!suppliedElsewhere("CMIstack", sim)) {

      if (P(sim)$GCM == "CCSM4_RCP4.5") {
        cmi.url <- "https://drive.google.com/open?id=1ERoQmCuQp3_iffQ0kXN7SCQr07M7dawv"
        cmi.tf <- "Canada3ArcMinute_CCSM4_45_CMI2011-2100.grd"
        cmi.arc <- "Canada3ArcMinute_CCSM4_45_CMI2011-2100.zip"
      } else if (P(sim)$GCM == "CanESM2_RCP4.5") {
        cmi.url <- "https://drive.google.com/open?id=1MwhK3eD1W6u0AgFbRgVg7j-qqyk0-3yA"
        cmi.tf <- "Canada3ArcMinute_CMI2011-2100.grd"
        cmi.arc <- "Canada3ArcMinute_CMI2011-2100.zip"
      } else if (P(sim)$GCM == "CCSM4_RCP8.5") {
        cmi.url <- "https://drive.google.com/open?id=1OcVsAQXKO4N4ZIESNmIZAI9IZcutctHX"
        cmi.tf <- "Canada3ArcMinute_CCSM4_85_CMI2011-2100.grd"
        cmi.arc <- "Canada3ArcMinute_CCSM4_85_CMI2011-2100.zip"
      } else {
        stop("unrecognized GCM in P(sim)$GCM")
      }

      sim$CMIstack <- prepInputs(targetFile = cmi.tf,
                                 archive = cmi.arc,
                                 alsoExtract = "similar",
                                 url = cmi.url,
                                 destinationPath = dPath,
                                 rasterTomatch = sim$rasterToMatch,
                                 studyArea = sim$studyArea,
                                 fun = "raster::stack",
                                 useCache = TRUE,
                                 userTags = c(currentModule(sim), "CMIstack"))

    }

    if (!suppliedElsewhere("CMInormal", sim)) {
      sim$CMInormal <- prepInputs(targetFile = "Canada3ArcMinute_normalCMI.grd",
                                  archive = "Canada3ArcMinute_normalCMI.zip",
                                  url = "https://drive.google.com/open?id=16YMgx9t2eW8-fT5YyW0xEbjKODYNCiys",
                                  destinationPath = dPath,
                                  fun = "raster::raster",
                                  studyArea = sim$studyArea,
                                  rasterToMatch = sim$rasterToMatch,
                                  useCache = TRUE,
                                  overwrite = TRUE,
                                  userTags = c(currentModule(sim), "CMInormal"),
                                  method = "bilinear",
                                  alsoExtract = "Canada3ArcMinute_normalCMI.gri")
    }
  }

  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
