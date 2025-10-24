#changes
#anomaly variables

climateVariables <- P(sim)$climateVariables
PSPmodelData <- sim$PSPmodelData
PSPvalidationData <- sim$PSPvalidationData

anoms <- setdiff(names(climateVariables), "")


gpBoost_PSP <- function(trainingData = PSPmodelData, 
                        validationData = PSPvalidationData, 
                        stat, climVar = c(climateVariables, anoms)) {
  #------------------------------------------------------------
  # 1. Prepare data 
  
  gpboostXCols <- c("plotNumeric", "growth", "mortality", "logAge", "biomass", climVar)
  gpBoostX <- as.matrix(trainingData[, .SD, .SDcols = gpboostXCols])

  myY <- gpBoostX[, eval(stat)]
  myX <- gpBoostX[, c(climateVariables, anoms, "biomass", "logAge")]
  
  # Validation dataset (same columns)
  validX <- as.matrix(validationData[, c(climateVariables, anoms, "biomass", "logAge"), with = FALSE])
  validY <- validationData[[eval(stat)]]
  
  #------------------------------------------------------------
  # 2. Define GP model (random effects)
  
  
  gp_model <- GPModel(group_data = gpBoostX[, "plotNumeric"])
  
  #------------------------------------------------------------
  # 3. Create datasets for gpboost
  
  dtrain <- gpb.Dataset(data = myX, label = myY)
  dvalid <- gpb.Dataset(data = validX, label = validY)
  
  #------------------------------------------------------------
  # 4. Specify parameters and validation tracking
 
  
  params <- list(
    objective = "regression_l2",
    learning_rate = 0.03,
    max_depth = 8,
    min_data_in_leaf = 30,
    metric = "l2"  # can also use "loglikelihood" for probabilistic fits
  )
  
  #------------------------------------------------------------
  # 5. Fit model with validation and early stopping
 
  
  bst <- gpboost(
    data = dtrain,
    gp_model = gp_model,
    valids = list(validation = dvalid),
    use_gp_model_for_validation = FALSE,
    params = params,
    nrounds = 10000,
    early_stopping_rounds = 50,
    verbose = 1
  )
  
  # The best iteration is stored here:
  best_iter <- bst$best_iter
  message("Best iteration:", best_iter, "\n")
  
  #------------------------------------------------------------
  # 6. Extract log-likelihood (or loss) curves
  
  
  # gpboost stores evaluation results inside bst$record_evals
  evals <- bst$record_evals
  
  # Convert to a tidy data frame for plotting
  eval_df <- data.table(
    iteration = seq_along(evals$train$l2$eval),
    train = unlist(evals$train$l2$eval),
    valid = unlist(evals$validation$l2$eval)
  )
  
  eval_df_long <- melt(eval_df, id.vars = "iteration", variable.name = "dataset", value.name = "loss")
  
  #------------------------------------------------------------
  # 7. Plot training vs validation loss over iterations
  
  
  
  imp <- gpb.importance(bst)
  
  summary(gp_model)
  return(list(imp = imp, eval_df_long = eval_df_long, bst = bst))
}

growthMod <- gpBoost_PSP(stat = "growth")

ggplot(growthMod$eval_df_long, aes(x = iteration, y = loss, color = dataset)) +
  geom_line(size = 1) +
  geom_vline(xintercept = best_iter, linetype = "dashed", color = "black") +
  labs(
    title = "GPBoost Model Training and Validation Loss",
    subtitle = paste("Early stopping at iteration", best_iter),
    y = "L2 Loss (lower is better)",
    x = "Iteration"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 12)) + 
  facet_wrap(~dataset, ncol = 1, scales = "free")

ggplot(growthMod$imp[order(-growthMod$imp$Gain)], aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col() +
  coord_flip() +
  labs(title = "GPBoost Feature Importance for growth", y = "Gain", x = NULL)

mortMod <- gpBoost_PSP(stat = "mortality")

ggplot(mortMod$eval_df_long, aes(x = iteration, y = loss, color = dataset)) +
  geom_line(size = 1) +
  geom_vline(xintercept = best_iter, linetype = "dashed", color = "black") +
  labs(
    title = "GPBoost Model Training and Validation Loss",
    subtitle = paste("Early stopping at iteration", best_iter),
    y = "L2 Loss (lower is better)",
    x = "Iteration"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 12)) + 
  facet_wrap(~dataset, ncol = 1, scales = "free")

ggplot(mortMod$imp[order(-mortMod$imp$Gain)], aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col() +
  coord_flip() +
  labs(title = "GPBoost Feature Importance for mortality", y = "Gain", x = NULL)
