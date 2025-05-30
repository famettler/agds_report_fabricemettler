# Function for stepwise forward regression

stepwise_forward <- function(data, response, predictors) {
  # starting with an empty model
  null_formula <- as.formula(paste(response, "~ 1"))
  empty_model <- lm(null_formula, data = data)
  
  best_models <- list(empty_model)
  best_AICs <- c(empty_model)
  current_predictors <-c()
  remaining_predictors <- predictors
  best_AIC <- AIC(empty_model)
  function_stop <- FALSE
  
  while (!function_stop && length(remaining_predictors) > 0) {
    all_models <- map(remaining_predictors, function(pred) {
      new_predictors <- c(current_predictors, pred)
      formula_p <- as.formula(paste(response, "~", paste(new_predictors, collapse = "+")))
      lm(formula_p, data = data)
    })
    model_stats <- tibble(
      predictor = remaining_predictors,
      r_square = map_dbl(all_models, ~ summary(.x)$r.squared), 
      AIC = map_dbl(all_models, AIC), 
      model = all_models
    )
    # selection of best model based on R^2
    best_row <- model_stats |>
      filter(r_square == max(r_square)) |>
      slice(1)
    
    # compare the AIC to previous model
    if (best_row$AIC < best_AIC) {
      best_AIC <- best_row$AIC
      current_predictors <- c(current_predictors, best_row$predictor)
      remaining_predictors <- setdiff(remaining_predictors, best_row$predictor)
      best_models <- append(best_models, list(best_row$model[[1]]))
      best_AICs <- c(best_AICs, best_AIC)
    } else {
      function_stop <- TRUE
    }
  }
  # return the best model
  return(list(
    final_model = best_models,
    steps = length(best_models),
    predictors = current_predictors,
    AICs = best_AICs
  ))
}
