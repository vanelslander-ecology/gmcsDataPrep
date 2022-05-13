compareModels <- function(nullGrowth, nullMortality, gcs, mcs, validationData, doPlotting, path) {

  gcsNLL <- assessNLL(validationData = validationData, gmModel = gcs,
                      observedCol = "growth", plotCol = "climate-sensitive",
                      doPlotting = doPlotting, path = path)
  nullGrowthNLL <- assessNLL(validationData = validationData, gmModel = nullGrowth,
                             observedCol = "growth", plotCol = "null", doPlotting, path)
  mcsNLL <- assessNLL(validationData = validationData, gmModel = mcs,
                      observedCol = "mortality", plotCol = "climate-sensitive", doPlotting, path)
  nullMortalityNLL <- assessNLL(validationData = validationData, gmModel = nullMortality,
                                observedCol = "mortality", plotCol = "null", doPlotting, path)

  message("negative log likelihood of climate-sensitive growth model: ", gcsNLL)
  message("negative log likelihood of null growth model: ", nullGrowthNLL)
  if (gcsNLL > nullGrowthNLL) {
    warning("consider revising the climate-sensitive growth model; it underperforms in comparison with the null model")
  }
  message("negative log likelihood of climate-sensitive mortality model: ", mcsNLL)
  message("negative log likelihood of null mortality model: ", nullMortalityNLL)
  if (mcsNLL > nullMortalityNLL) {
    warning("consider revising the climate-sensitive mortality model; it underperforms in comparison with the null model")
  }
}

assessNLL <- function(validationData, observedCol, gmModel, plotCol, doPlotting, path) {
  #not sure if we should keep the prediction col in the dataset...
  validationData <- copy(validationData)
  validationData[, predictionVar := predict(gmModel, newdata = validationData, type = "response")]
  validationData[, observed := validationData[, .SD, .SDcol = observedCol]]
  if (doPlotting) {
    a <- ggplot(data = validationData, aes(y = predictionVar, x = observed, col = standAge)) +
      geom_point() +
      labs(x = "observed", y = "predicted", title = paste(plotCol, observedCol, sep = " ")) +
      scale_colour_continuous(type = "viridis") +
      geom_abline(slope = 1)

    ggsave(a, filename = file.path(path, paste(plotCol, observedCol, "ggplot.png", sep = "_")))

  }
  # stdev <- sd(validationData$predictionVar - validationData$observed)
  stdev <- sd(validationData$observed)
  validationData[, ll := dnorm(x = predictionVar, mean = observed, sd = stdev, log = TRUE)]
  sumNLL <- sum(validationData$ll) * -1

  return(sumNLL)
}

prepValidationData <- function(PSPmodelData, validationProportion) {
  #need at least 3 plots for validation - 2 to estimate model w/ random plot effect, 1+ to validate
  possiblePlots <- PSPmodelData[, .N, .(OrigPlotID1)][N > 2,]
  #calculate the targetN for the validation dataset.
  targetN <- round(nrow(PSPmodelData) * validationProportion)
  #set aside min. 2 observations per plot to estimate random component
  availableN <- sum(possiblePlots$N - 2)

  #randomly sample, stratified by OrigPlotID1
  validationPlots <- PSPmodelData[OrigPlotID1 %in% possiblePlots$OrigPlotID1]
  validationPlots <- validationPlots[, .SD[sample(.N, size = c(.N - 2), replace = FALSE)],  .(OrigPlotID1)]

  if (targetN > availableN) {
    warning("not enough plots for current validation target")
  } else {
    #reserve these extra plots for fitting
    validationPlots[, foo := sample(.N, replace = FALSE)]
    validationPlots <- validationPlots[foo > availableN - targetN,]
    validationPlots[, foo := NULL]
  }
  #MeasureID is no longer in data because each observation is two meausurements
  #Use period or year to distinguish repeat observations with anti-join
  PSPmodelData <- PSPmodelData[!validationPlots, on = c("OrigPlotID1", "period")]

  return(list("PSPmodelData" = PSPmodelData,
              "PSPvalidationData" = validationPlots))
}
