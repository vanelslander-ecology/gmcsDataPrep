---
title: "gmcsDataPrep"
author: "Ian MS Eddy"
date: "21 January 2019; updated April 2024"
output:
  html_document:
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

This module uses climate data and permanent sampling plot data to prepare a multivariate mixed effect model of growth, mortality, and net biomass change.
The data includes BC, Alberta, Saskatchewan, Ontario, New Brunswick, and the first NFI PSP measurement, which is mainly western Canada.
However, because the module requires repeat measurements, this module should only be used with a `studyAreaLarge` that covers at a minimum part of these jurisdictions. 

The module does not estimate unobserved growth and mortality (trees that grew and died between census measurements).
The formulas are included as comments.
It is not advisable to estimate the unobserved mortality and growth of species, and although the multivariate model does not have a species term, we intended for the model to one day include species.
Consequently, we exclude unobserved mortality and growth (estimates are usually 0.5-1.5% of total growth and mortality, so no biggie). 

# Usage


```r
library(SpaDES)
library(LandR)
library(sf)
library(fasterize)
library(terra)

setPaths(modulePath = file.path("../"))
getPaths() # shows where the 4 relevant paths are

times <- list(start = 2011, end = 2012)

## the study area isn't necessary to test models;
## it is only used to satisfy climate data prep. Smaller is better.
studyArea <- randomStudyArea(size = 10000*6.25*400)
templateRaster <- rast(ext(studyArea), crs = crs(studyArea), res = c(500,500))
templateRaster <- setValues(templateRaster, 1)

#this is the contiguous ecoregions of the RIA. It is suggested that users pass a studyAreaPSP 
#it should represent a geographic area where trees would be expected to have a similar climate response
studyAreaPSP <- prepInputs(url = "https://drive.google.com/open?id=10yhleaumhwa3hAv_8o7TE15lyesgkDV_",
                           destinationPath = tempdir())

parameters <- list(
  gmcsDataPrep = list(
    "minDBH" = 9, #the minimum DBH varies over time and space, so 9+ is recommended
    "useHeight" = TRUE,
    ".useCache" = FALSE,
    "PSPdataTypes" = "all",
    "PSPab_damageColsToExclude" = NULL,
    "PSPbc_damageColsToExclude" = NULL,
    "PSPnfi_damageColsToExclude" = NULL,
    "PSPperiod" = c(1958,2019),
    #an alternative mortality model - gamma not possible due to 0 
    "mortalityModel" = quote(glmmPQL(mortality ~ logAge*(ATA + CMI) + logAge^2 *(ATA + CMI) + ATA*CMI, random = ~1 | OrigPlotID1,
                                    weights = scale(PSPmodelData$plotSize^0.5 * PSPmodelData$periodLength, center = FALSE),
                                    data = PSPmodelData, family = "gaussian"(link = 'identity'))),
    #the 'new' growth model with quadratic. 
    "growthModel" = quote(glmmPQL(growth ~ logAge*(ATA + CMI) + logAge^2 * (CMI + ATA) + ATA:CMI, random = ~1 | OrigPlotID1,
                                    weights = scale(PSPmodelData$plotSize^0.5 * PSPmodelData$periodLength, center = FALSE),
                                    data = PSPmodelData, family = 'Gamma'(link = 'log'))),
    #the original model with quadratic term
    # "growthModel" = quote(nlme::lme(growth ~ logAge*(ATA + CMI) + logAge^2 *(ATA + CMI) + ATA*CMI, random = ~1 | OrigPlotID1,
    #                                 weights = varFunc(~plotSize^0.5 * periodLength), data = PSPmodelData)),
    #Yong's original growth model
    # "growthModel" = quote(nlme::lme(growth ~ logAge*(ATA + CMI) + ATA*CMI, random = ~1 | OrigPlotID1,
    #                                  weights = varFunc(~plotSize^0.5 * periodLength), data = PSPmodelData)),
    "PSPvalidationPeriod" = NULL #pass NULL to instead randomly sample the validation data
  )
)


modules <- list("gmcsDataPrep")
objects <- list(studyArea = studyArea, 
                rasterToMatch = templateRaster,
                studyAreaPSP = studyAreaPSP)
inputs <- list()
outputs <- list()

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects)

mySimOut <- spades(mySim)
```

# Parameters

Provide a summary of user-visible parameters.


|paramName                  |paramClass |default      |min |max |paramDesc                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|:--------------------------|:----------|:------------|:---|:---|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|biomassModel               |character  |Lambert2005  |NA  |NA  |The model used to calculate biomass from DBH. Can be either 'Lambert2005' or 'Ung2008'.                                                                                                                                                                                                                                                                                                                                                                                                                              |
|climateVariables           |character  |MAT, CMI     |NA  |NA  |character vector of climate variables from ClimateNA used in the growth/mortality models. If a model uses a variable formula that represents a deviation from a climate normal, it should be indicated with a name, where the name represents the variable in the formula. For example, the default climate variable and model use the anomaly of `MAT`: `ATA`.                                                                                                                                                      |
|doAssertion                |logical    |             |NA  |NA  |assertions used to check climate data for NA values in valid pixels                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|doPlotting                 |logical    |FALSE        |NA  |NA  |if true, will plot and save models                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|GCM                        |character  |CanESM5_.... |NA  |NA  |See `canClimateData` module for other supported climate models. If ATA and CMI are supplied by the user, this parameter is ignored.                                                                                                                                                                                                                                                                                                                                                                                  |
|growthModel                |call       |glmmPQL,.... |NA  |NA  |Quoted model used to predict growth in PSP data as a function of logAge, CMI, ATA, and their interactions, with PlotID as a random effect                                                                                                                                                                                                                                                                                                                                                                            |
|minDBH                     |numeric    |10           |0   |NA  |The minimum DBH (cm) allowed. Each province uses different criteria for monitoring trees, so absence of entries < min(DBH) does not equate to absence of trees. The following are approximations: Ontario = 2.5 cm (after 1991), Alberta = 7.3, SK = 9.7, BC = 4, and NFI = 9.                                                                                                                                                                                                                                       |
|minTrees                   |numeric    |30           |0   |NA  |The minimum number of trees per initial plot. This is prior to filtering by minimum DBH. This may not be suitable for every use case. The default is 30, based on the GCB paper <doi: 10.1111/gcb.12994>.                                                                                                                                                                                                                                                                                                            |
|minSize                    |numeric    |0.02         |0   |NA  |The minimum size (in hectares) of growth plot. All metrics are adjusted for area. The canonical methodology did not force a minimum size but the minimum size was 0.04 ha.                                                                                                                                                                                                                                                                                                                                           |
|mortalityModel             |call       |gamlss::.... |NA  |NA  |Quoted model used to predict mortality in PSP data as a function of `logAge`, `CMI`, `ATA`, and their interactions, with `PlotID` as a random effect. Defaults to zero-inflated inverse gaussian glm that requires custom `LandR.CS` predict function to predict.                                                                                                                                                                                                                                                    |
|nullGrowthModel            |call       |glmmPQL,.... |NA  |NA  |a null model used only for comparative purposes - can be accessed through 'mod'                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|nullMortalityModel         |call       |nlme::lm.... |NA  |NA  |a null model used only for comparative purposes - can be accessed through 'mod'                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|PSPab_damageColsToExclude  |numeric    |3            |NA  |NA  |if sourcing Alberta PSP, which tree damage sources to exclude, if any. Defaults to Mountain Pine Beetle. Codes can be found in GOA PSP Manual. Any damage sources that are not excluded are modelled as endogenous climate impact                                                                                                                                                                                                                                                                                    |
|PSPbc_damageColsToExclude  |character  |IBM          |NA  |NA  |for BC PSP, which tree damage sources to exclude, if any. Defaults to Mountain Pine Beetle. Any damage sources that are not excluded are modelled as endogenous climate impact                                                                                                                                                                                                                                                                                                                                       |
|PSPnfi_damageColsToExclude |character  |IB           |NA  |NA  |if sourcing NFI PSP, which tree damage sources to exclude, if any. Defaults to bark beetles. Any damage sources that are not excluded are modelled as endogenous climate impact                                                                                                                                                                                                                                                                                                                                      |
|PSPdataTypes               |character  |all          |NA  |NA  |Which PSP datasets to source, defaulting to all. Other available options include 'BC', 'AB', 'SK', 'NFI', and 'dummy'. 'dummy' should be used for unauthorized users.                                                                                                                                                                                                                                                                                                                                                |
|PSPperiod                  |numeric    |1958, 2011   |NA  |NA  |The years by which to compute climate normals and subset sampling plot data. Must be a vector of at least length 2.                                                                                                                                                                                                                                                                                                                                                                                                  |
|PSPvalidationPeriod        |numeric    |             |NA  |NA  |the period to build the validation dataset. Must be greater than PSPperiod e.g. c(1958-2018). Subsequent observations are used only if they are within this period, but outside the fitting period. E.g. Successive measurements in 2004 and 2017 would be used even though the first measurement falls outside the 2011 fitting period as the 2011 cutoff would remove this paired obsevation from the fitting data. If NULL, then validation dataset will instead be randomly sampled from available measurements. |
|stableClimateQuantile      |numeric    |0.9          |0   |1   |if the simulation time exceeds the projected climate years then missing years will be randomly sampled from a subset of the projected climate using this quantile as a threshold                                                                                                                                                                                                                                                                                                                                     |
|useHeight                  |logical    |TRUE         |NA  |NA  |Use height be used to calculate biomass (in addition to DBH). If height is NA for individual trees, then only DBH will be used for those measurements                                                                                                                                                                                                                                                                                                                                                                |
|validationProportion       |numeric    |0.2          |0   |1   |proportion of data to use in validation set. Will be overridden by `PSPvalidationPeriod`.                                                                                                                                                                                                                                                                                                                                                                                                                            |
|.useCache                  |character  |.inputOb.... |NA  |NA  |Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant.                                                                                                                                                                                                                                                                                                                                                            |

# Events

- TODO

## Plotting

- TODO

## Saving

- TODO

# Data dependencies

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData("gmcsDataPrep", "path/to/modules/dir")` may be sufficient.

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData("canClimateData", "..")` may be sufficient.


|objectName             |objectClass |desc                                                               |sourceURL |
|:----------------------|:-----------|:------------------------------------------------------------------|:---------|
|rasterToMatch          |SpatRaster  |template raster corresponding to `studyArea`.                      |NA        |
|rasterToMatchReporting |SpatRaster  |template raster corresponding to `studyAreaReporting`.             |NA        |
|studyArea              |sf          |study area used for simulation (buffered to mitigate edge effects) |NA        |
|studyAreaReporting     |sf          |study area used for reporting/post-processing                      |NA        |

## Output data

Description of the module outputs.


|objectName               |objectClass |desc                                                                          |
|:------------------------|:-----------|:-----------------------------------------------------------------------------|
|ATAstack                 |SpatRaster  |annual projected mean annual temperature anomalies                            |
|CMIstack                 |SpatRaster  |annual projected mean climate moisture deficit                                |
|CMInormal                |SpatRaster  |Climate Moisture Index Normals from 1950-2010                                 |
|historicalClimateRasters |list        |list of a single raster stack - historical MDC calculated from ClimateNA data |
|projectedClimateRasters  |list        |list of a single raster stack - projected MDC calculated from ClimateNA data  |

# Links to other modules

- [Biomass_core](https://github.com/PredictiveEcology/Biomass_core)
