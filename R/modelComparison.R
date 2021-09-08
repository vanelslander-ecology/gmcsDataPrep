compareModels <- function(nullGrowth, nullMortality, gcs, mcs, validationData) {

  gcsNLL <- assessNLL(validationData = validationData, gmModel = gcs, observedCol = "growth")
  nullGrowthNLL <- assessNLL(validationData = validationData, gmModel = nullGrowth, observedCol = "growth")
  mcsNLL <- assessNLL(validationData = validationData, gmModel = mcs, observedCol = "mortality")
  nullMortalityNLL <- assessNLL(validationData = validationData, gmModel = nullMortality, observedCol = "mortality")

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


assessNLL <- function(validationData, observedCol, gmModel) {

  #not sure if we should keep the prediction col in the dataset...
  validationData <- copy(validationData)
  validationData[, predictionVar := predict(gmModel, newdata = validationData, type = 'response')]
  validationData[, observed := validationData[, .SD, .SDcol = observedCol]]
  # stdev <- sd(validationData$predictionVar - validationData$observed)
  stdev <- sd(validationData$observed)
  validationData[, ll := dnorm(x = predictionVar, mean = observed, sd = stdev, log = TRUE)]
  sumNLL <- sum(validationData$ll) * -1

  return(sumNLL)
}