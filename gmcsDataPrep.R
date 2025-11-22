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
  reqdPkgs = list("caret (>= 7.0.2.9001)", #clone ceresbarros/caret/pkg/caret or load from the submodule
                  "crayon", "data.table", "ggplot2", 
                  "purrr", "pROC", "sf", "xgboost (>= 3.0.5.1)",
                  #maybe install.packages('xgboost', repos = c('https://dmlc.r-universe.dev', 'https://cloud.r-project.org'))
                  "PredictiveEcology/LandR@development (>= 1.1.4)",
                  "ianmseddy/LandR.CS@development (>= 0.0.3.9000)",
                  "PredictiveEcology/reproducible (>= 2.1.0)",
                  "PredictiveEcology/pemisc@development (>= 0.0.3.9002)",
                  "ianmseddy/PSPclean@development (>= 0.1.5.9002)", 
                  "PredictiveEcology/SHAPforxgboost (>= 0.1.3.9001)"),
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
    defineParameter("minMeasures", "numeric", 2, Inf, 3, 
                    desc = paste0("the minimum number of measurements per plot. Each pair of measurements",
                                  "generates one observation of growth and mortality")),
    defineParameter("minTrees", "numeric", 30, 0, NA,
                    desc = paste("The minimum number of trees per initial plot.",
                                 "This is prior to filtering by minimum DBH.",
                                 "This may not be suitable for every use case.
                                 The default is 30, based on the GCB paper <doi: 10.1111/gcb.12994>.")),
    defineParameter("minSampleForSpecies", "numeric", 1000, 0, NA, 
                    desc = paste("the minimum number of observations of tree species within stands below which", 
                                 "they are combined as a single category of 'other spp' in the climate-sensitive models")),
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
    defineParameter("PSPperiod", "numeric", c(1900, 2025), NA, NA,
                    desc = paste("The measurement years by which to subset sampling plot data, if any",
                                 "Must be a vector of at least length 2.")),
    defineParameter("PSPvalidationPeriod", "numeric", NULL, NA, NA,
                    desc = paste("the period to build the validation dataset. Must be greater than PSPperiod",
                                 "e.g. c(1958-2018). Subsequent observations are used only if they are within this period,",
                                 "but outside the fitting period. E.g. Successive measurements in 2004 and 2017",
                                 "would be used even though the first measurement falls outside the 2011 fitting period",
                                 "as the 2011 cutoff would remove this paired obsevation from the fitting data.",
                                 "If NULL, then validation dataset will instead be randomly sampled from available measurements.")),
    defineParameter("sppEquivCol", "character", "LandR", NA, NA, 
                    desc = paste("the column in `LandR::sppEquivalencies_CA` to use for standardizing PSP names.",
                                 "Note that biomass is estimated from tree plot data using the column `PSP`.", 
                                 "Combining the models for species with separate biomass equations (e.g. Populus balsamea, Populus treumuloides)",
                                 "is possible by passing a sppEquivCol that has a single value for these entries.")),
    defineParameter("useHeight", "logical", TRUE, NA, NA,
                    desc = paste("Use height be used to calculate biomass (in addition to DBH). If height is NA for individual",
                                 "trees, then only DBH will be used for those measurements")),
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
                 sourceURL = "https://drive.google.com/file/d/1KFkX6bVCzwEA6V9MQEWqXxIEnON6YerN/view?usp=drive_link"),
    expectsInput(objectName = "studyAreaPSP", objectClass = "SpatVector",
                 desc = paste("Optional area used to subset PSP plots before building the statistical models.",
                              "Any class of spatial object is acceptable."), sourceURL = NA), 
    expectsInput(objectName = "sppEquiv", objectClass = "data.table", 
                 desc = "Table of species equivalencies. See `LandR::sppEquivalencies_CA`")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "gcsModel", objectClass = "ModelObject?",
                  desc = "growth model with covariates indicated by P(sim)$climateVariables and log(age)"),
    createsOutput(objectName = "mcsModel", objectClass = "ModelObject?",
                  desc = "mortality model with covariates indicated by P(sim)$climateVariables and log(age)"),
    createsOutput(objectName = "PSPmodelData", objectClass = "data.table",
                  desc = "PSP growth mortality calculations")
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
    
    #checks
    if (length(P(sim)$PSPperiod) < 2) {
      stop("Please supply P(sim)$PSPperiod of length 2 or greater")
    }
    
    if (any(is.null(sim$PSPmeasure_gmcs), is.null(sim$PSPplot_gmcs), is.null(sim$PSPgis_gmcs))) {
      stop("The PSP objects are being supplied incorrectly. Please review loadOrder argument in simInit")
    }
    
    #numeric from plotID
    #this should be done before creating modelData so the factors aren't duplicated in the validation set
    sim$PSPplot_gmcs[, plotNumeric := as.numeric(as.factor(OrigPlotID1))]
    
    
    sim$PSPmodelData <- prepModelData(
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
      minMeasures = P(sim)$minMeasures,
      minSize = P(sim)$minSize,
      minTrees = P(sim)$minTrees) |>
      Cache(userTags = c("gmcsDataPrep", "prepModelData"))
    
    PSPmodelData <- sim$PSPmodelData
    
    #TODO: decide if this exclusion should occur in object saved by sim
    #take only species of interst (ie in sim$sppEquiv)
    PSPmodelData[, N := .N, .(spp)]
    PSPmodelData <- PSPmodelData[N > P(sim)$minSampleForSpecies,]
    #TODO: set aside some for validation - unclear if necessary
    
    #Prepare Data for XGBoost
    anomalyVariables <- setdiff(names(P(sim)$climateVariables), "")
    allClimVar <- c(P(sim)$climateVariables, anomalyVariables)
    
    #need to remove non-useful columns due to use of categorical data
    #don't add mortality or it will be treated as a covariate 
    PSPmodelData <- PSPmodelData[, .SD, 
                                 .SDcols = c("logGrowth", "logMortality", allClimVar,
                                             "biomass", "logAge", "standBiomass", "spp")]
    
    # Add dummy variables for factor columns -- i.e., the random effects
    if (all(sapply(PSPmodelData, is.numeric)) %in% FALSE)
      PSPmodelData <- model.matrix(~ . + 0, data = PSPmodelData)
    PSPmodelData <- as.data.table(PSPmodelData)
    
    colnamesPred <- setdiff(colnames(PSPmodelData), "logGrowth") ## after model.matrix bcs colnames change
    ## model building
    ## only replace the models if NULL, so user can supply their own models
    if (is.null(sim$gcsModel)) {
      #drop mortality (hence copy)
      xgbTrainData_g <- copy(PSPmodelData)
      xgbTrainData_g[, logMortality := NULL]
      
      #hyperparameter tuning and kfold cross validation
      sim$gcsModel <- runXGBOOST(dat = xgbTrainData_g, dig = NULL,
                                 nFolds = 5,
                                 eval_metric = c("rmse"),
                                 colnamesResp = "logGrowth", 
                                 figDir = "outputs/figures/gmcsDataPrep", 
                                 cachePath = cachePath(sim)) |>
        Cache()
      
      r2 <- sapply(sim$gcsModel, r2Fun)
      r2 <- mean(r2)
      message("r-squared for climate-sensitive growth model is: ", r2)
      
      rm(xgbTrainData_g)
    }
    
    if (is.null(sim$mcsModel)) {
      
      #drop growth (hence copy)
      xgbTrainData_m <- copy(PSPmodelData)
      xgbTrainData_m[, logGrowth := NULL]
      
      #hyperparameter tuning and kfold cross validation
      sim$mcsModel <- runXGBOOST(dat = xgbTrainData_m, dig = NULL,
                                 nFolds = 5,
                                 eval_metric = c("rmse"),
                                 colnamesResp = "logMortality", 
                                 figDir = "outputs/figures/gmcsDataPrep", 
                                 cachePath = cachePath(sim)) |>
        Cache()

      r2 <- sapply(sim$mcsModel, r2Fun)
      r2 <- mean(r2)
      message("r-squared for climate-sensitive mortality model is: ", r2)
    }
  }
  
  
  return(invisible(sim))
}



prepModelData <- function(climateVariables, studyAreaPSP, PSPgis, PSPmeasure, PSPplot, 
                          PSPclimData, useHeight, biomassModel, PSPperiod, minDBH, 
                          minMeasures, minSize, minTrees) {

  message(yellow("There are", nrow(PSPgis), "initial PSPs"))
  ## crop points to studyAreaPSP
  if (!is.null(studyAreaPSP)) {
    message(yellow("Filtering PSPs to study Area..."))
    #TODO: confirm this postProcess works
    PSP_sa <- reproducible::postProcess(PSPgis, maskTo = studyAreaPSP)
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
  ## these will be counted in the min trees requirement, 
  #but may result in a plot of NA biomass if repeat measures = 2+
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
  #remove these measures as their inclusion only complicates code for no benefit
  PSPmeasure <- PSPmeasure[biomass > 0]
  message(yellow("No biomass estimate possible for these species: "))
  print(tempOut$missedSpecies)

  ## Filter by 3+ repeat measures - must be last filter criteria.
  ## Some plots share ID but have different trees so simple count of plots insufficient to find repeat measures
  ## Reduce PSPmeasure to MeasureID, PlotID1, PlotID2, MeasureYear, remove duplicates
  ## then find repeat measures of MeasureYear, match back to MeasureID in both PSPplot and PSPmeasure.
  message(yellow("Filtering by at least ",  " measures per plot"))

  repeats <- PSPmeasure[, .(MeasureID, OrigPlotID1, MeasureYear)] %>%
    .[!duplicated(.)] %>%
    .[, .('repeatMeasures' = .N), by = .(OrigPlotID1)] %>%
    .[repeatMeasures >= minMeasures]
  setkey(repeats, OrigPlotID1)
  setkey(PSPmeasure, OrigPlotID1)
  PSPmeasure <- PSPmeasure[repeats]
  PSPplot <- PSPplot[MeasureID %in% PSPmeasure$MeasureID] ## ensures all plots have biomass/repeat measures

  message(yellow(paste0("There are "), nrow(repeats), 
                 " PSPs with min. ", minMeasures, " repeat measures"))

  tempVariableNames <- unname(climateVariables)
  #data.table will assign the subset columns to the variable name, which is problematic if some are NULL
  PSPclimData <- PSPclimData[OrigPlotID1 %in% PSPmeasure$OrigPlotID1, .SD,
                             .SDcols = tempVariableNames, .(OrigPlotID1, Year)]
  PSPmeasure <- PSPmeasure[OrigPlotID1 %in% PSPclimData$OrigPlotID1,]

  if (any(nrow(PSPclimData) == 0, nrow(PSPmeasure) == 0, nrow(PSPgis) == 0)) {
    stop('all existing PSP data has been filtered.Try adjusting parameters')
  }

  #Calculate mean of climate variables
  pSppChange <- lapply(unique(PSPplot$OrigPlotID1),
                       FUN = sumPeriod, m = PSPmeasure, p = PSPplot, dbh = minDBH,
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
    for (i in 1:length(anomalies)) {
      #index because the name isn't preserved if you take the object itself
      temp <- anomalies[i]
      setnames(PSPmodelData, c(temp, names(temp)), c("var", "anom"))
      PSPmodelData[, anom := var - anom]
      newOrderCols <- names(PSPmodelData)[!names(PSPmodelData) %in% c("var", "anom") ]
      setcolorder(PSPmodelData, newOrderCols)
      setnames(PSPmodelData, c("var", "anom"), c(temp, names(temp)))
    }
  }

  
  ## Standardize by plotSize and change units from kg/ha to g/m2. = *1000 g/kg / 10000 m2/ha
  PSPmodelData <- PSPmodelData[, growth_gm2 := growth/plotSize/10] %>%
    .[, mortality_gm2 := mortality/plotSize/10] %>%
    .[, netBiomassChng_gm2 := netBiomassChng/plotSize/10]
  
  # Sum species-specific mortality, growth, and net biomass by plot and year
  # growth is set to 1 if it would be 0 (to avoid model error - anyway  0 growth is measurement error)
  PSPmodelSum <- PSPmodelData[, .("growth" = pmax(1, sum(growth_gm2)), "mortality" = sum(mortality_gm2),
                                  "netBiomass" = sum(netBiomassChng_gm2), biomass = sum(biomass)),
                              by = c("OrigPlotID1", "period", "Species")]
  
  PSPmodelData[, c("mortality_gm2", "growth_gm2", "netBiomassChng_gm2", "growth", "mortality") := NULL]
  PSPmodelData <- unique(PSPmodelData)
  subCols <- names(PSPmodelData)[!names(PSPmodelData) %in% c(names(PSPmodelSum))]
  joinCols <- setdiff(names(PSPmodelData), subCols)
  
  #join back to get the climate and other relevant information
  # is this true?
  PSPmodelMean <- unique(PSPmodelData[, .SD, .SDcols = c(subCols, joinCols)])
  PSPmodelMean[, N := .N, .(OrigPlotID1, period, Species)]
  if (nrow(PSPmodelMean[N > 1,]) > 0) {
    stop("an issue has occured with PSP data model building")
  }
  PSPmodelData <- PSPmodelSum[PSPmodelMean, on = joinCols]
  
  PSPmodelData <- unique(PSPplot[, .(OrigPlotID1, plotNumeric)])[PSPmodelData, on = c("OrigPlotID1")]
  PSPmodelData[, logGrowth := log(growth)]
  PSPmodelData[, logMortality := log(mortality)]

  setcolorder(PSPmodelData, c("OrigPlotID1", "plotNumeric", "plotSize", "year", "period", "periodLength",
                              "standAge", "logAge", "Species", "growth", "logGrowth", 
                              "mortality", "logMortality", "biomass", "netBiomassChng"))
  
  #calculate biomass as the sum of biomass by species within a plot, 
  # and scale growth by biomass 

  PSPmodelData[, standBiomass := sum(biomass), .(OrigPlotID1, period)]
  PSPmodelData[, growth_over_B := growth/biomass]
  PSPmodelData[, mortality_over_B := mortality/biomass]
  
  #TODO: spp should probably join with sppEquiv at some point - maybe here?

  PSPmodelData[, spp := Species]

  return(PSPmodelData)
}

gmcsModelBuild <- function(PSPmodelData, model) {
  assign("PSPmodelData", PSPmodelData, .GlobalEnv)
  ## this prevents cache envir arg from conflicting with eval envir
  gmcsModel <- eval(model, envir = environment())

  return(gmcsModel)
}

pspIntervals <- function(i, M, P, Clim, ClimVar, dbh) {
  
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
  
  if (nrow(newborn) > 0) {
    #assume that they were 1nth away from minDBH, where n = measurement interval over stand age
    #it should be the censusLength + baseSA, 
    newborn_interpolated <- copy(newborn)
    currentAge <- P$MeasureYear[i + 1] - P$baseYear[i + 1] + P$baseSA[i + 1]
    increment <- (1 - censusLength/currentAge) 
    #must use current age to ensure censusLength is always smaller, else multiplier is negative!
    #note: dbh in this equation is the minimum dbh threshold
    newborn_interpolated[, DBH := dbh * increment]
    if (any(is.na(newborn_interpolated$Height))) {
      useHeight = FALSE
    } else {
      useHeight = TRUE
    }
    newborn_interpolated[, biomass := biomassCalculation(newSpeciesName, DBH, height = Height, 
                                                         includeHeight = useHeight)$biomass, ]
    newborn[, origBiomass := newborn_interpolated$biomass]
  } else {
    newborn[, origBiomass := 0]
  }
  
  if (nrow(living1) != nrow(living2) | nrow(living1) == 0) {
    warning("there is a problem in the PSP data with the plots: ", unique(m1$MeasureID), " ", unique(m2$MeasureID))
    return(NULL)
    ## `nrow(living1) == 0` will happen if tree numbers change between measurements
  }
  ## Find observed annual changes in mortality and growth
  living2$origBiomass <- living1$biomass
  ## growth cannot be negative, by definition
  living2[biomass < origBiomass, biomass := origBiomass]
  
  # TODO: there are some plots that have black spruce with different codings (e.g. BL, Bl)
  # (this is likely true for others - but only tested in BC atm)
  # These SHOULD be combined, but the way to do that is to only return the single standardized column name
  # unfortunately this means if any species share a biomass equation, we lose knowledge of their existence
  # for example Engelmann and Hybrid white spruce. Consider the implications 
  living <- living2[, .(newGrowth =  sum(biomass - origBiomass)/censusLength,
                        biomass = sum(origBiomass)), .(Species)] |>
    setkey(Species)
  
  newborn <- newborn[, .(newGrowth = sum(biomass - origBiomass) ,
                         biomass = sum(origBiomass)), .(Species)] |>
    setkey(Species)
  #measure from census midpoint for new seedlings
  dead <- dead[, .(mortality = sum(biomass) / censusLength, biomass = sum(biomass)), .(Species)] |>
    setkey(Species)
  
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
  
  # changes <- rbind(newborn, living)
  changes <- living
  
  changes$mortality <- 0
  dead$newGrowth <- 0
  changes <- rbind(changes, dead, newborn, fill = TRUE)
  
  #fill NA as zero - regen has no mortality, dead has no growth 
  changes[, c("newGrowth", "biomass", "mortality") := lapply(.SD, FUN = nafill, fill = 0),
          .SDcols = c("newGrowth", "biomass", "mortality")]
  
  #sum growth mortality and biomass by species
  changes <- changes[, .("growth" = sum(newGrowth), "mortality" = sum(mortality),
                         biomass = sum(biomass, na.rm = TRUE)),
                     .(Species)]
  changes[, netBiomassChng := growth - mortality]
  
  changes$period <- period
  changes$OrigPlotID1 <- P$OrigPlotID1[1]
  changes$year <- year
  changes$standAge <- P$baseSA[1] + P$MeasureYear[i + 1] - P$MeasureYear[1]
  changes$logAge <- log(changes$standAge)
  changes$plotSize <- P$PlotSize[1]
  changes$periodLength <- censusLength
  
  changes <- meanClim[changes, on = "OrigPlotID1"]
  setcolorder(changes, c("OrigPlotID1", "period", "Species", "growth", "mortality", "netBiomassChng",
                         "standAge", "logAge", "plotSize", "periodLength", ClimVar))
  return(changes)
}

sumPeriod <- function(x, m, p, clim, climVar, dbh) {
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
                  M = m, P = p, Clim = clim, ClimVar = climVar, dbh = dbh)

  pSums <- rbindlist(pSums)
  return(pSums)
}

r2Fun <- function(x) { 
  R2 <- 1 - sum(x$valData$resid^2) / sum((x$valData$obs - mean(x$valData$obs))^2)
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
                                  targetFile = "PSPforClimateNA_BC_AB_SK_ON_QC_NB_NFI.csv",
                                  destinationPath = dPath,
                                  fun = "data.table::fread")
    sim$PSPclimData <- sim$PSPclimData[MAT != -9999] #missing plots get -9999 as variable
    #this must be done first as some data is "malformed" exiting ClimateNA
    setnames(sim$PSPclimData, old = c("id1"), new = c("OrigPlotID1"))
    if (!is.null(sim$PSPclimData$id2)) {
      sim$PSPclimData[, id2 := NULL]
    }
  }
  
  #TODO: do this manually for now until Parvin's changes materialize
  temp <- LandR::sppEquivalencies_CA[, .(PSP, LandR)][grep("_spp", LandR, invert = TRUE)]
  temp <- unique(temp[LandR != "" & PSP != "" & LandR != "Quer_bic",])
  sim$PSPmeasure_gmcs <- temp[sim$PSPmeasure_gmcs, on = c("PSP" = "newSpeciesName")]
  sim$PSPmeasure_gmcs[, Species := NULL]
  setnames(sim$PSPmeasure_gmcs, old = c(P(sim)$sppEquivCol, "PSP"), new = c("Species", "newSpeciesName"))

  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
