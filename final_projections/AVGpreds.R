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

batstrain <- read.csv("batstrain.csv", stringsAsFactors = FALSE)

batspredict <- read.csv("batspredict.csv", stringsAsFactors = FALSE)


#XGBoost for AVG

base_stat <- "AVG"

# Get the column names that end with _Y1, _Y2, _Y3, _Y4
predictors <- grep("_Y[1-4]$", colnames(batstrain), value = TRUE)

# Create the formula dynamically
formula <- as.formula(paste("AVG ~", paste(predictors, collapse = " + ")))
formula_pred <- as.formula(paste("~", paste(predictors, collapse = " + ")))

# Convert to matrix format (excluding target variable)
train_matrix <- as.matrix(batstrain[predictors])
test_matrix <- as.matrix(batspredict[predictors])
train_matrix[is.infinite(train_matrix)] <- NA
test_matrix[is.infinite(test_matrix)] <- NA
train_matrix[is.nan(train_matrix)] <- NA
test_matrix[is.nan(test_matrix)] <- NA

# Extract target variable
train_labels <- batstrain$AVG
test_labels  <- batspredict$AVG


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
  eta = c(0.001, 0.4),
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
  nrounds = 4000,       # Maximum boosting rounds
  params = final_params,
  verbose = TRUE
)

# Make predictions on the test set
xgb_predictions <- predict(xgb_model, test_matrix)

# Calculate RMSE and R-squared
rmse_xgb <- sqrt(mean((xgb_predictions - test_labels)^2))
r2_xgb <- cor(xgb_predictions, test_labels)^2

# Plot feature importance
features <- xgb.importance(model = xgb_model)  # Displays feature importance
xgb.plot.importance(xgb.importance(model = xgb_model))  # Plots feature importance

# Add predictions to the test_data and round them
batspredict <- batspredict |> mutate(predicted_AVG = round(xgb_predictions, 3))

#reintroduce park factor adjustment
teams <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/source_projections.csv", stringsAsFactors = FALSE)

teams$name <- gsub("á", "a" , teams$name)
teams$name <- gsub("é", "e" , teams$name)
teams$name <- gsub("í", "i" , teams$name)
teams$name <- gsub("ó", "o" , teams$name)
teams$name <- gsub("ú", "u" , teams$name)
teams$name <- gsub("ñ", "n" , teams$name)
teams$name <- gsub("-", "" , teams$name)
teams$name <- gsub("\\.", "", teams$name)

teams <- teams |> select(name, playerid, team) |>
  unique()

teams <- teams |>
  mutate(team = if_else(team == "OAK", "ATH", team))


batspredict$playerid <- as.character(batspredict$playerid)

pred_AVG <- left_join(batspredict, teams)

rolling_lhb_pf <- read_excel("rolling_lhb_pf.xlsx")
rolling_rhb_pf <- read_excel("rolling_rhb_pf.xlsx")

pfs_combined <- full_join(rolling_lhb_pf, rolling_rhb_pf, 
                          by = c("Season", "team"), 
                          suffix = c("_LHB", "_RHB"))

# Apply weighted park factor calculation
pfs_combined <- pfs_combined %>%
  mutate(across(ends_with("pf_LHB"), 
                ~ (.x * 0.67) + (get(sub("LHB", "RHB", cur_column())) * 0.33),
                .names = "{sub('_LHB', '', .col)}"))

pfs_combined <- pfs_combined |>
  select(1:2, 27:36) |>
  mutate(Bats = "B")

pfs <- bind_rows(rolling_lhb_pf, rolling_rhb_pf, pfs_combined) |>
  select(-Season)

pfs <- pfs |>
  mutate(team = if_else(team == "OAK", "ATH", team))

pred_AVG <- left_join(pred_AVG, pfs) |>
  select(name, playerid, Age, Bats, 327:339) |>
  select(1:6)

write.csv(pred_AVG, "AVG_preds.csv", row.names = FALSE)

