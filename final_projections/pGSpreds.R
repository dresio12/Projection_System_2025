#Building the REZ projection model
library(baseballr)
library(lubridate)
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(tidyr)
library(glmnet)
library(randomForest)
library(caret)
library(xgboost)
library(Matrix)
library(stringi)
library(parallel)
library(rBayesianOptimization)


#load in batstest from GitHub
pitchtrain <- read.csv("pitch_train.csv", stringsAsFactors = FALSE, check.names = FALSE)

pitchpredict <- read.csv("pitch_predict.csv", stringsAsFactors = FALSE, check.names = FALSE)


#
#
#testing on GS

base_stat <- "GS"

# Get the column names that end with _Y1, _Y2, _Y3, _Y4
predictors <- grep("_Y[1-4]$", colnames(pitchtrain), value = TRUE)

options(expressions = 10000)  # Default is usually 500

# Create the formula dynamically
formula <- as.formula(paste("GS ~", paste(paste0("`", predictors, "`"), collapse = " + ")))

formula_pred <- as.formula(paste("~", paste(paste0("`", predictors, "`"), collapse = " + ")))


#XGBoost

# Convert to matrix format (excluding target variable)
train_matrix <- as.matrix(pitchtrain[predictors])
test_matrix <- as.matrix(pitchpredict[predictors])

train_matrix[is.infinite(train_matrix)] <- NA
test_matrix[is.infinite(test_matrix)] <- NA
train_matrix[is.nan(train_matrix)] <- NA
test_matrix[is.nan(test_matrix)] <- NA

# Extract target variable
train_labels <- pitchtrain$GS
test_labels  <- pitchpredict$GS

# Define the evaluation function for Bayesian optimization
xgb_cv_bayes <- function(eta, max_depth, subsample, colsample_bytree) {
  # Convert max_depth to integer
  max_depth <- as.integer(max_depth)
  
  params <- list(
    objective = "reg:squarederror",
    eta = eta,
    max_depth = max_depth,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    missing = NA
  )
  
  result <- tryCatch({
    cv_results <- xgb.cv(
      params = params,
      data = train_matrix,
      label = train_labels,
      nfold = 5,
      nrounds = 1200,
      early_stopping_rounds = 10,
      verbose = 0,
      nthread = parallel::detectCores() - 1
    )
    
    # Get the best RMSE
    best_rmse <- min(cv_results$evaluation_log$test_rmse_mean)
    best_iter <- which.min(cv_results$evaluation_log$test_rmse_mean)
    
    list(Score = -best_rmse, Pred = best_iter)
  }, error = function(e) {
    # Return a very poor score if an error occurs
    list(Score = -999999, Pred = 0)
  })
  
  return(result)
}

# Define the search bounds for each parameter
bounds <- list(
  eta = c(0.01, 0.3),
  max_depth = c(2, 15),
  subsample = c(0.6, 1.0),
  colsample_bytree = c(0.6, 1.0)
)

# Run Bayesian optimization
set.seed(123) # for reproducibility
bayes_results <- BayesianOptimization(
  FUN = xgb_cv_bayes,
  bounds = bounds,
  init_points = 10,     # Number of random points to start with
  n_iter = 15,         # Number of iterations of Bayesian optimization
  acq = "ei",
  verbose = TRUE
)

# Get the best parameters
best_params <- bayes_results$Best_Par
best_rmse <- -bayes_results$Best_Value 

# Output the best parameters and RMSE
print(paste("Best RMSE:", round(best_rmse, 4)))
print(best_params)

# Train the final model with the best parameters
final_params <- list(
  objective = "reg:squarederror",  
  eta = best_params["eta"],           
  max_depth = as.integer(best_params["max_depth"]),  
  subsample = best_params["subsample"],  
  colsample_bytree = best_params["colsample_bytree"]     
)

xgb_model <- xgboost(
  data = train_matrix,
  label = train_labels,
  nrounds = 6000,       # Maximum boosting rounds
  params = final_params,
  verbose = TRUE
)

# Make predictions on the test set
xgb_predictions <- predict(xgb_model, test_matrix)

# Calculate RMSE and R-squared
rmse_xgb <- sqrt(mean((xgb_predictions - test_labels)^2))
r2_xgb <- cor(xgb_predictions, test_labels)^2

print(paste("XGBoost RMSE:", round(rmse_xgb, 4)))
print(paste("XGBoost R²:", round(r2_xgb, 4)))

# Plot feature importance
features <- xgb.importance(model = xgb_model)  # Displays feature importance
xgb.plot.importance(xgb.importance(model = xgb_model))  # Plots feature importance

# Add predictions to the test_data and round them
pitchpredict <- pitchpredict |> mutate(predicted_GS = round(xgb_predictions, 3))

pitchpredict$playerid <- as.character(pitchpredict$playerid)

pred_GS <- pitchpredict 

only_preds <- pred_GS |>
  select(PlayerName, playerid, Throws, predicted_GS)

#plotting

# Select relevant columns for output
boostedGS5a <- pred_GS |>
  select(PlayerName, predicted_GS, GS_Y1, GS_Y2, GS_Y3)

boostedGS5a$meanGS <- rowMeans(boostedGS5a[, c("GS_Y1", "GS_Y2", "GS_Y3")], na.rm = TRUE)


#plot
ggplot(boostedGS5a, aes(x = predicted_GS, y = GS_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted GS vs 2024 GS",
    x = "2025 Predicted GS",
    y = "2024 GS"
  ) +
  theme_minimal()


ggplot(boostedGS5a, aes(x = predicted_GS, y = meanGS)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted GS vs 3-Year Mean GS",
    x = "2025 Predicted GS",
    y = "Mean GS"
  ) +
  theme_minimal()


#plot again, this time with players with meaningful 2024 GS

boostedGS5b <- pred_GS |>
  filter(IP_Y1 >= 20) |>
  select(PlayerName, predicted_GS, GS_Y1, GS_Y2, GS_Y3)

boostedGS5b$meanGS <- rowMeans(boostedGS5b[, c("GS_Y1", "GS_Y2", "GS_Y3")], na.rm = TRUE)


ggplot(boostedGS5b, aes(x = predicted_GS, y = GS_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted GS vs 2024 GS",
    x = "2025 Predicted GS",
    y = "2024 GS"
  ) +
  theme_minimal()


ggplot(boostedGS5b, aes(x = predicted_GS, y = meanGS)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted GS vs 3-Year Mean GS",
    x = "2025 Predicted GS",
    y = "Mean GS"
  ) +
  theme_minimal()


#plot again, increase GS

boostedGS5c <- pred_GS |>
  filter(IP_Y1 >= 50) |>
  select(PlayerName, predicted_GS, GS_Y1, GS_Y2, GS_Y3) 

boostedGS5c$meanGS <- rowMeans(boostedGS5c[, c("GS_Y1", "GS_Y2", "GS_Y3")], na.rm = TRUE)


ggplot(boostedGS5c, aes(x = predicted_GS, y = GS_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted GS vs 2024 GS",
    x = "2025 Predicted GS",
    y = "2024 GS"
  ) +
  theme_minimal()

ggplot(boostedGS5c, aes(x = predicted_GS, y = meanGS)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted GS vs 3-Year Mean GS",
    x = "2025 Predicted GS",
    y = "Mean GS"
  ) +
  theme_minimal()

write.csv(only_preds, "pred_pGS.csv", row.names = FALSE)
