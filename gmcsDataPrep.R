defineModule(sim, list(
  name = "gmcsDataPrep",
  description = "Prepare data to provide grow and mortality (climate-sensitve) adjustments for LandR.CS",
  keywords = c("LandR", "LandR.CS"),
  authors = c(
    person(c("Ian", "MS"), "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person(c("Alex", "M"), "Chubaty", email = "achubaty@for-cast.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(gmcsDataPrep = "0.0.2.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "gmcsDataPrep.Rmd"),
  reqdPkgs = list("crayon", "data.table", "gamlss", "ggplot2", "glmm",
                  "PredictiveEcology/LandR@development (>= 1.1.0.9009)",
                  "ianmseddy/LandR.CS@development (>= 0.0.3.9000)",
                  "MASS", "nlme",
                  "PredictiveEcology/pemisc@development (>= 0.0.3.9002)",
                  "ianmseddy/PSPclean@development (>= 0.1.4.9003)", "sf"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("biomassModel", "character", "Lambert2005", NA, NA,
                    desc =  paste("The model used to calculate biomass from DBH.",
                                  "Can be either 'Lambert2005' or 'Ung2008'.")),
    defineParameter("climateVariables", "character", c("ATA" = "MAT", "CMI"), NA, NA,
                    desc = paste("character vector of climate variables from ClimateNA used in the growth/mortality models.",
                                 "If a model uses a variable formula that represents a deviation from a climate normal,",
                                 "it should be indicated with a name, where the name represents the variable in the formula.",
                                 "For example, the default climate variable and model use the anomaly of `MAT`: `ATA`.")),
    defineParameter("doAssertion", "logical", getOption("LandR.assertions"), NA, NA,
                    desc = "assertions used to check climate data for NA values in valid pixels"),
    defineParameter("doPlotting", "logical", FALSE, NA, NA, desc = "if true, will plot and save models"),
    defineParameter("GCM", "character", "CanESM5_ssp370", NA, NA,
                    desc = paste("See `canClimateData` module for other supported climate models.",
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
                    desc = paste("The minimum number of trees per initial plot.",
                                 "This is prior to filtering by minimum DBH.",
                                 "This may not be suitable for every use case.
                                 The default is 30, based on the GCB paper <doi: 10.1111/gcb.12994>.")),
    defineParameter("minSize", "numeric", 0.02, 0, NA,
                    desc = paste("The minimum size (in hectares) of growth plot. All metrics are adjusted for area.",
                                 "The canonical methodology did not force a minimum size but the minimum size was 0.04 ha.")),
    defineParameter("mortalityModel", class = "call",
                    quote(gamlss::gamlss(formula = mortality ~ logAge * (ATA + CMI) + ATA * CMI,
                                 LandR.CS::own(random = ~ 1|OrigPlotID1, weights = varFunc(~plotSize^0.5 * periodLength)),
                                 sigma.formula = ~logAge + ATA,  nu.formula = ~logAge, family = gamlss.dist::ZAIG,
                                 data = PSPmodelData)), NA, NA,
                    desc = paste("Quoted model used to predict mortality as a function of `logAge`, `CMI`, `ATA`, and",
                                 "their interactions, with `PlotID` as random effect. Defaults to zero-inflated inverse gaussian",
                                 "glm that requires custom `LandR.CS` predict function to predict.")),
    defineParameter("nullGrowthModel", class = "call",
                    quote(glmmPQL(growth ~ logAge, random = ~1 | OrigPlotID1,
                                  weights = scale(PSPmodelData$plotSize^0.5 * PSPmodelData$periodLength, center = FALSE),
                                  data = PSPmodelData, family = "Gamma"(link = "log"))), NA, NA,
                    desc = "a null model used only for comparative purposes - can be accessed through 'mod'"),
    defineParameter("nullMortalityModel", class = "call",
                    quote(nlme::lme(mortality ~ logAge, random = ~1 | OrigPlotID1,
                                    weights = varFunc(~plotSize^0.5 * periodLength), data = PSPmodelData)), NA, NA,
                    desc = "a null model used only for comparative purposes - can be accessed through 'mod'"),
    defineParameter("PSPdataTypes", "character", "all", NA, NA,
                    desc = paste("Which PSP datasets to source, defaulting to all. Other available options include",
                                 "'BC', 'AB', 'SK', 'NFI', 'ON', 'NB', and 'dummy'. 'dummy' is for unauthorized users.")),
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
                    desc = "proportion of data to use in validation set. Will be overridden by `PSPvalidationPeriod`."),
    defineParameter(".useCache", "character", ".inputObjects", NA, NA,
                    desc = paste("Should this entire module be run with caching activated?",
                                 "This is generally intended for data-type modules,",
                                 "where stochasticity and time are not relevant."))
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "PSPmeasure_gmcs", objectClass = "data.table", desc = "standardized tree measurements for PSPs",
                 sourceURL = "https://drive.google.com/file/d/1LmOaEtCZ6EBeIlAm6ttfLqBqQnQu4Ca7/"),
    expectsInput(objectName = "PSPplot_gmcs", objectClass = "data.table", desc = "standardized plot-level attributes for PSPs",
                 sourceURL = "https://drive.google.com/file/d/1LmOaEtCZ6EBeIlAm6ttfLqBqQnQu4Ca7/"),
    expectsInput(objectName = "PSPgis_gmcs", objectClass = "data.table", desc = "PSP plot data as sf object",
                 sourceURL = "https://drive.google.com/file/d/1LmOaEtCZ6EBeIlAm6ttfLqBqQnQu4Ca7/"),
    expectsInput(objectName = "PSPclimData", objectClass = "data.table",
                 desc = paste("climate data for each PSP from ClimateNA. Temp is represented in degrees Celsius.",
                              "see https://climatena.ca/Help2 for details."),
                 sourceURL = "https://drive.google.com/file/d/1kgQn27-pC0oSTfUqEbj7LWi1W4PpHxuV/view?usp=drive_link"),
    expectsInput(objectName = "studyAreaPSP", objectClass = "SpatialPolygonsDataFrame",
                 desc = paste("this area will be used to subset PSP plots before building the statistical model.",
                              "Currently PSP datasets with repeat measures exist only for Saskatchewan,",
                              "Alberta, Boreal British Columbia, and Ontario"), sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "gcsModel", objectClass = "ModelObject?",
                  desc = "growth model with covariates indicated by P(sim)$climateVariables and log(age)"),
    createsOutput(objectName = "mcsModel", objectClass = "ModelObject?",
                  desc = "mortality model with covariates indicated by P(sim)$climateVariables and log(age)"),
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

      sim <- scheduleEvent(sim, start(sim) + 1L, eventType = "scheduleScrubGlobalEnv", eventPriority = .last())
    },
    scheduleScrubGlobalEnv = {
      ## 2024-05: this event provides a temporary workaround to a SpaDES.core scheduling bug.
      ## When `spades.allowInitDuringSimInit` is TRUE, SpaDES.core executes init events
      ## in a new simulation, with a modified end time, meaning any events using `end(sim)`
      ## will have those events scheduled for this modified time, and not the actual end time.
      ## We work around this by scheduling the scheduling for later.
      ## TODO: remove this workaround when either: gamlss no longer used, or scheduling bug fixed.
      sim <- scheduleEvent(sim, end(sim), eventType = "scrubGlobalEnv", eventPriority = .last())
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
    message("building climate-sensitive growth and mortality models")
    #stupid-catch
    if (length(P(sim)$PSPperiod) < 2) {
      stop("Please supply P(sim)$PSPperiod of length 2 or greater")
    }

    if (any(is.null(sim$PSPmeasure_gmcs), is.null(sim$PSPplot_gmcs), is.null(sim$PSPgis_gmcs))) {
      stop("The PSP objects are being supplied incorrectly. Please review loadOrder argument in simInit")
    }

    sim$PSPmodelData <- Cache(prepModelData,
                              climateVariables = P(sim)$climateVariables,
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

    ## building validation data
    message("Preparing validation dataset")
    if (!is.null(sim$PSPvalidationPeriod)) {
      ## build validation data from observations in PSPvalidationPeriod that also aren't in model data
      sim$PSPvalidationData <- Cache(prepModelData,
                                     climateVariables = P(sim)$climateVariables,
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

      ## remove all observations that are identical
      sim$PSPvalidationData <- setkey(sim$PSPvalidationData)[!sim$PSPmodelData]
      ## remove all observations for which there is no random effect in the fitting data
      sim$PSPvalidationData <- sim$PSPvalidationData[OrigPlotID1 %in% sim$PSPmodelData$OrigPlotID1, ]
    } else {
      ## sample validation data from PSPmodelData
      outData <- Cache(FUN = prepValidationData,
                       PSPmodelData = sim$PSPmodelData,
                       validationProportion = P(sim)$validationProportion,
                       useCache = P(sim)$.useCache,
                       userTags = c("gmcsDataPrep", "prepValidationData"))
      sim$PSPmodelData <- outData$PSPmodelData ## with some plots removed
      sim$PSPvalidationData <- outData$PSPvalidationData
    }

    ## model building
    ## only replace the models if NULL, so user can supply their own models
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

    nullGrowthModel <- Cache(gmcsModelBuild,
                             PSPmodelData = sim$PSPmodelData,
                             model = P(sim)$nullMortalityModel,
                             userTags = c("nullGrowthModel"))

    nullMortalityModel <- Cache(gmcsModelBuild,
                                PSPmodelData = sim$PSPmodelData,
                                model = P(sim)$nullMortalityModel,
                                userTags = c("nullMortalityModel"))

    ## reporting NLL as comparison statistic - could do RME or MAE?
    if (nrow(sim$PSPvalidationData) > 0) {
      assign("PSPmodelData", sim$PSPmodelData, .GlobalEnv) ## needed until end of sim
      ## TODO: use more specific name to avoid clobbering user's global env objs
      ## E.g., `._tmp_gmcsDataPrep_PSPmodelData_.`
      compareModels(nullGrowth = nullGrowthModel,
                    nullMortality = nullMortalityModel,
                    gcs = sim$gcsModel,
                    mcs = sim$mcsModel,
                    validationData = sim$PSPvalidationData,
                    doPlotting = P(sim)$doPlotting,
                    path = outputPath(sim),
                    studyAreaName = P(sim)$.studyAreaName)
      # rm(PSPmodelData, envir = .GlobalEnv) ## TODO
    }
  }

  return(invisible(sim))
}



prepModelData <- function(climateVariables, studyAreaPSP, PSPgis, PSPmeasure, PSPplot, PSPclimData, useHeight,
                          biomassModel, PSPperiod, minDBH, minSize, minTrees) {

  message(yellow("There are", nrow(PSPgis), "initial PSPs"))
  ## crop points to studyAreaPSP
  if (!is.null(studyAreaPSP)) {
    message(yellow("Filtering PSPs to study Area..."))
    #TODO: confirm this postProcess works
    PSP_sa <- postProcess(PSPgis, maskTo = studyAreaPSP)
    message(yellow(paste0("There are "), nrow(PSP_sa), " PSPs in your study area"))
  } else {
    PSP_sa <- PSPgis
  }
  #Restrict climate variables to only those of interest.. should be param

  #Filter other PSP datasets to those in study Area
  PSPmeasure <- PSPmeasure[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]
  PSPplot <- PSPplot[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]
  PSPclimData <- PSPclimData[OrigPlotID1 %in% PSP_sa$OrigPlotID1,]

  ## might as well drop species with no biomass match

  ## `length(PSPclimData)/length(PSP_sa)` should always yield a whole number.
  ## Filter data by study period
  message(yellow("Filtering by study period..."))
  PSPmeasure <- PSPmeasure[MeasureYear > min(PSPperiod) &
                             MeasureYear < max(PSPperiod),]
  PSPplot <- PSPplot[MeasureYear > min(PSPperiod) &
                       MeasureYear < max(PSPperiod),]
  PSPclimData[Year > min(PSPperiod) & Year < max(PSPperiod),]
  message(yellow(paste0("There are "), length(unique(PSPplot$OrigPlotID1)), " PSPs in the study period"))
  ## Join data (should be small enough by now)
  PSPmeasure <- PSPmeasure[PSPplot, on = c('MeasureID', 'OrigPlotID1', 'MeasureYear')]

  ## Restrict to trees > minDBH
  message(yellow("Filtering by min. DBH"))
  PSPmeasure <- PSPmeasure[DBH >= minDBH,]

  ## Filter by > minTrees at first measurement (P) to ensure forest. Default 30
  message(yellow("Filtering by minimum trees in earliest measurement"))
  forestPlots <- PSPmeasure[MeasureYear == baseYear, .(measures = .N), OrigPlotID1] %>%
    .[measures >= minTrees,]
  PSPmeasure <- PSPmeasure[OrigPlotID1 %in% forestPlots$OrigPlotID1,]
  PSPplot <- PSPplot[OrigPlotID1 %in% PSPmeasure$OrigPlotID1,]
  repeats <- PSPplot[, .(measures = .N), by = OrigPlotID1]
  message(yellow(paste0("There are "), nrow(repeats), " PSPs with min.", minTrees, " trees at earliest measurement"))

  ## Filter by min size
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
    ## check if height is missing, join if so -- function fails if data.table is empty
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

  ## Filter by 3+ repeat measures - must be last filter criteria.
  ## Some plots share ID but have different trees so simple count of plots insufficient to find repeat measures
  ## Reduce PSPmeasure to MeasureID, PlotID1, PlotID2, MeasureYear, remove duplicates
  ## then find repeat measures of MeasureYear, match back to MeasureID in both PSPplot and PSPmeasure.
  message(yellow("Filtering by at least 3 repeat measures per plot"))
  repeats <- PSPmeasure[, .(MeasureID, OrigPlotID1, MeasureYear)] %>%
    .[!duplicated(.)] %>%
    .[, .('repeatMeasures' = .N), by = .(OrigPlotID1)] %>%
    .[repeatMeasures > 2]
  setkey(repeats, OrigPlotID1)
  setkey(PSPmeasure, OrigPlotID1)
  PSPmeasure <- PSPmeasure[repeats]
  PSPplot <- PSPplot[MeasureID %in% PSPmeasure$MeasureID] ## ensures all plots have biomass/repeat measures

  message(yellow(paste0("There are "), nrow(repeats), " PSPs with min. 3 repeat measures"))

  tempVariableNames <- unname(climateVariables)
  #data.table will assign the subset columns to the variable name, which is problematic if some are NULL
  PSPclimData <- PSPclimData[OrigPlotID1 %in% PSPmeasure$OrigPlotID1, .SD,
                             .SDcols = tempVariableNames, .(OrigPlotID1, Year)]
  PSPmeasure <- PSPmeasure[OrigPlotID1 %in% PSPclimData$OrigPlotID1,]

  if (any(nrow(PSPclimData) == 0, nrow(PSPmeasure) == 0, nrow(PSPgis) == 0)) {
    stop('all existing PSP data has been filtered.Try adjusting parameters')
  }

  pSppChange <- lapply(unique(PSPplot$OrigPlotID1),
                       FUN = sumPeriod, m = PSPmeasure, p = PSPplot,
                       clim = PSPclimData, climVar = tempVariableNames)
  PSPmodelData <- rbindlist(pSppChange)

  #if c.m. has partial names, the empty names become "", with no names they are NULL
  anomalies <- climateVariables[!names(climateVariables) %in% c("")]
  if (length(anomalies) > 0) {
    #check if year is already subset - if not, subset to PSP period
    anomalyData <- PSPclimData[Year >= min(PSPperiod) & Year <= max(PSPperiod),
                               lapply(.SD, mean), .SDcol = anomalies, .(OrigPlotID1)]
    setnames(anomalyData, old = anomalies, new = names(anomalies))
    PSPmodelData <- anomalyData[PSPmodelData, on = c("OrigPlotID1")]

    #recalculate the anomaly(s) via subtraction
    for (i in length(anomalies)) {
      #index because the name isn't preserved if you take the object itself
      temp <- anomalies[i]
      setnames(PSPmodelData, c(temp, names(temp)), c("var", "anom"))
      PSPmodelData[, anom := var - anom]
      newOrderCols <- names(PSPmodelData)[!names(PSPmodelData) %in% c("var", "anom") ]
      setcolorder(PSPmodelData, newOrderCols)
      setnames(PSPmodelData, c("var", "anom"), c(temp, names(temp)))
    }
  }
  PSPmodelData$species <- factor(PSPmodelData$species)
  PSPmodelData$sppLong <- factor(PSPmodelData$sppLong)

  ## Standardize by plotSize and change units from kg/ha to g/m2. = *1000 g/kg / 10000 m2/ha
  PSPmodelData <- PSPmodelData[, growth_gm2 := growth/plotSize/10] %>%
    .[, mortality_gm2 := mortality/plotSize/10] %>%
    .[, netBiomass_gm2 := netBiomass/plotSize/10]
  #26/02/2019 after discussion we decided not to include species in model.
  # Decided to parameterize inclusion of ATA or year. ATA is better for projecting, but year is canonical
  # Sum species-specific mortality, growth, and net biomass by plot and year
  # growth is set to 0.1 if it would be 0 (to avoid model error - anyway  0 growth is measurement error)

  #this step wouldn't be necessary if we aggregated species biomass in the interval function
  PSPmodelSum <- PSPmodelData[, .("growth" = pmax(1, sum(growth_gm2)), "mortality" = sum(mortality_gm2),
                                  "netBiomass" = sum(netBiomass_gm2), "biomass" = sum(biomass)),
                              by = c("OrigPlotID1", "period")]
  PSPmodelData[, c("species", "sppLong", "mortality_gm2", "growth_gm2", "netBiomass_gm2") := NULL]
  subCols <- names(PSPmodelData)[!names(PSPmodelData) %in% c(names(PSPmodelSum))]

  PSPmodelMean <- unique(PSPmodelData[, .SD, .SDcols = c(subCols, "OrigPlotID1", "period")])

  PSPmodelData <- PSPmodelSum[PSPmodelMean, on = c("period", "OrigPlotID1")]


  setcolorder(PSPmodelData, c("OrigPlotID1", "plotSize", "year", "period", "periodLength",
                              "standAge", "logAge", "growth", "mortality", "biomass", "netBiomass"))
  return(PSPmodelData)
}

gmcsModelBuild <- function(PSPmodelData, model) {
  assign("PSPmodelData", PSPmodelData, .GlobalEnv)
  ## this prevents cache envir arg from conflicting with eval envir
  gmcsModel <- eval(model, envir = environment())

  return(gmcsModel)
}

pspIntervals <- function(i, M, P, Clim, ClimVar) {
  #Calculate climate variables
  meanClim <- Clim[Year >= P$MeasureYear[i] & Clim$Year <= P$MeasureYear[i + 1],
                   lapply(.SD, mean), .SDcol = ClimVar, .(OrigPlotID1)]

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
    ## `nrow(living1) == 0` will happen if tree numbers change between measurements
  }
  ## Find observed annual changes in mortality and growth
  living2$origBiomass <- living1$biomass
  ## growth cannot be negative, by definition
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
  changes <- rbind(newborn, living)

  changes$mortality <- 0
  dead$newGrowth <- 0
  changes <- rbind(changes, dead, fill = TRUE)
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
  #join up with the climate means
  changes$period <- period


  changes$OrigPlotID1 <- P$OrigPlotID1[1]
  changes$year <- year
  changes$standAge <- P$baseSA[1] + P$MeasureYear[i + 1] - P$MeasureYear[1]
  changes$logAge <- log(changes$standAge)
  changes$plotSize <- P$PlotSize[1]
  changes$periodLength <- censusLength

  changes <- meanClim[changes, on = "OrigPlotID1"]
  setcolorder(changes, c("OrigPlotID1", "period", "species", "sppLong", "growth", "mortality", "netBiomass",
                         "standAge", "logAge", "plotSize", "periodLength", ClimVar))
  return(changes)
}

sumPeriod <- function(x, m, p, clim, climVar) {
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
  pSums <- lapply(1:periods, FUN = pspIntervals,
                  M = m, P = p, Clim = clim, ClimVar = climVar)

  pSums <- rbindlist(pSums)
  return(pSums)
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(inputPath(sim), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("PSPmeasure_gmcs", sim) |
      !suppliedElsewhere("PSPplot_gmcs", sim) |
      !suppliedElsewhere("PSPgis_gmcs", sim)) {
    message("sourcing PSP data for gmcsDataPrep...")

    PSPdata <- Cache(getPSP,
                     PSPdataTypes = P(sim)$PSPdataTypes,
                     destinationPath = dPath,
                     forGMCS = TRUE,
                     userTags = c(cacheTags, P(sim)$PSPdataTypes, "getPSP"))

    sim$PSPmeasure_gmcs <- PSPdata$PSPmeasure
    sim$PSPplot_gmcs <- PSPdata$PSPplot
    sim$PSPgis_gmcs <- PSPdata$PSPgis
  }

  if (!suppliedElsewhere("studyArea", sim)) {
    message("studyArea not supplied. Using a random area in Alberta")
    sim$studyArea <- randomStudyArea(size = 1e6*50)
  }

  if (!suppliedElsewhere("PSPclimData", sim)) {
    sim$PSPclimData <- prepInputs(url = extractURL("PSPclimData"),
                                  targetFile = "PSPforClimateNA_BC_AB_SK_ON_NB_NFI_1921-2021MSY.csv",
                                  destinationPath = dPath,
                                  fun = "data.table::fread")
    setnames(sim$PSPclimData, old = c("id1", "id2"), new = c("OrigPlotID1", "OrigPlotID2"))

    sim$PSPclimData <- sim$PSPclimData[MAT != -9999] #missing plots get -9999 as variable
  }

  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
