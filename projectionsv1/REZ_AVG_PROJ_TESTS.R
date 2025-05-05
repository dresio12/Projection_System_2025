#Building the REZ projection model
library(baseballr)
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


#begin making training and predicting dfs

batstest <- batstest |>
  group_by(name) |>  # Group by player name
  summarize(across(everything(), ~ first(na.omit(.))), .groups = "drop")

# Extract and reorder columns by year in descending order
batstest <- batstest[, 
                     order(
                       sapply(colnames(batstest), function(x) {
                         year <- stringr::str_extract(x, "\\d{4}")
                         as.numeric(year)  # Convert extracted year to numeric for sorting
                       }),
                       decreasing = TRUE,
                       na.last = TRUE
                     )
]

#removing unneeded columns

batstest <- batstest |>
  select(559, 3:92, 96:185, 189:278, 282:371, 375:464, 468:557)

batstest$name <- gsub("\\.", "", batstest$name)

batstest <- batstest %>%
  mutate(name = case_when(
    name == "Alfonso Rivas" ~ "Alfonso Rivas III",
    TRUE ~ name
  ),
  name = case_when(
    name == "Victor Scott" ~ "Victor Scott II",
    TRUE ~ name
  ),
  name = case_when(
    name == "Calvin Mitchell" ~ "Cal Mitchell",
    TRUE ~ name
  ),
  name = case_when(
    name == "Leonardo Rivas" ~ "Leo Rivas",
    TRUE ~ name
  ),
  name = case_when(
    name == "Diego Castillo" ~ "Diego A Castillo",
    TRUE ~ name
  ),
  name = case_when(
    name == "Ha-seong Kim" ~ "HaSeong Kim",
    TRUE ~ name
  ),
  name = case_when(
    name == "Jackson Frazier" ~ "Clint Frazier",
    TRUE ~ name
  ),
  name = case_when(
    name == "Ji-Man Choi" ~ "Ji Man Choi",
    TRUE ~ name
  ),
  name = case_when(
    name == "Jonny Deluca" ~ "Jonny DeLuca",
    TRUE ~ name
  )
  )


#renaming columns to remove current year suffix, 
#assign Y1 as most recent, Y3 most distant

#2024:2021 interval 1
int1 <- batstest |>
  select(1:361) |>
  rename_with(~ str_replace(., "_2024$", ""), ends_with("_2024")) |>
  rename_with(~ str_replace(., "_2023$", "_Y1"), ends_with("_2023")) |>
  rename_with(~ str_replace(., "_2022$", "_Y2"), ends_with("_2022")) |>
  rename_with(~ str_replace(., "_2021$", "_Y3"), ends_with("_2021"))


#2023:2020
int2 <- batstest |>
  select(1, 92:451) |>
  rename_with(~ str_replace(., "_2023$", ""), ends_with("_2023")) |>
  rename_with(~ str_replace(., "_2022$", "_Y1"), ends_with("_2022")) |>
  rename_with(~ str_replace(., "_2021$", "_Y2"), ends_with("_2021")) |>
  rename_with(~ str_replace(., "_2020$", "_Y3"), ends_with("_2020"))


#2022:2019
int3 <- batstest |>
  select(1, 182:541) |>
  rename_with(~ str_replace(., "_2022$", ""), ends_with("_2022")) |>
  rename_with(~ str_replace(., "_2021$", "_Y1"), ends_with("_2021")) |>
  rename_with(~ str_replace(., "_2020$", "_Y2"), ends_with("_2020")) |>
  rename_with(~ str_replace(., "_2019$", "_Y3"), ends_with("_2019"))


#2025:2022 1-91
int4 <- batstest |>
  select(1:271) |>
  rename_with(~ str_replace(., "_2024$", "_Y1"), ends_with("_2024")) |>
  rename_with(~ str_replace(., "_2023$", "_Y2"), ends_with("_2023")) |>
  rename_with(~ str_replace(., "_2022$", "_Y3"), ends_with("_2022"))


empty <- batstest |>
  select(1:91) |>
  rename_with(~ str_replace(., "_2024$", ""), ends_with("_2024")) |>
  mutate(Age = ifelse(!is.na(Age), Age + 1, Age))

empty[ ,3:91] <- NA


#create training and predicting dfs
batstrain <- bind_rows(int1, int2, int3)

batspredict <- left_join(empty, int4)


#remove NA values from batstrain, 
#include only rows with meaningful number of PAs to build meaningful associations

#im leaving in 3 year spans where one season may not have 50 PA because
#xgboost may be able to find connections between those seasons and future performance

batstrain <- batstrain |>
  drop_na() |>
  filter(PA >=50)

#remove some of the inactive players (will remove all later)
#(for future: fill in NAs with league averages or imputations for
#active players who didn't play 3 consecutive years)

players <- baseballr::mlb_sports_players(sport_id = 1, season = 2024)

# Remove including special characters
players$full_name <- gsub("ÃÂ©", "e" , players$full_name)
players$full_name <- gsub("ÃÂ±", "n" , players$full_name)
players$full_name <- gsub("ÃÂ¡", "a" , players$full_name)
players$full_name <- gsub("ÃÂ¡", "i" , players$full_name)
players$full_name <- gsub("i³", "i" , players$full_name)
players$full_name <- gsub("Ã³", "o" , players$full_name)
players$full_name <- gsub("ÃÂº", "u" , players$full_name)

players <- players |>
  select(full_name, active)

batspredict <- left_join(batspredict, players, by = c('name' = 'full_name'))
batspredict <- batspredict |> select(362, 1:361)
  
batspredict <- batspredict |>
  mutate(active = ifelse(!is.na(Age), "TRUE", active))

batspredict <- batspredict |>
  filter(!(is.na(Age) & is.na(Age_Y1) & is.na(Age_Y2) & is.na(Age_Y3))) |>
  filter(!(is.na(Age) & is.na(Age_Y1) & is.na(Age_Y2))) |>
  filter(!(is.na(Age_Y1) & is.na(Age_Y2))) |>
  mutate(Age = ifelse(is.na(Age) & !is.na(Age_Y2), Age_Y2 + 2, Age))


#
#
#testing on batting average

base_stat <- "AVG"

# Get the column names that end with _Y1, _Y2, or _Y3
predictors <- grep("_Y[1-3]$", colnames(batstrain), value = TRUE)

# Create the formula dynamically
formula <- as.formula(paste("AVG ~", paste(predictors, collapse = " + ")))

formula_pred <- as.formula(paste("~", paste(predictors, collapse = " + ")))


#XGBoost

# Convert to matrix format (excluding target variable)
train_matrix <- model.matrix(formula, data = batstrain)[, -1]
test_matrix <- as.matrix(batspredict[predictors])

# Extract target variable
train_labels <- batstrain$AVG
test_labels  <- batspredict$AVG

# Define hyperparameter grid
param_grid <- expand.grid(
  eta = c(0.01, 0.1, 0.3),         # Learning rate
  max_depth = c(3, 6, 9),          # Depth of trees
  subsample = c(0.7, 0.8, 1),      # Fraction of data used for each tree
  colsample_bytree = c(0.7, 0.8, 1) # Fraction of features used for each tree
)

# Initialize variables to store best result
best_rmse <- Inf
best_params <- NULL

# Loop through the hyperparameter grid
for (i in 1:nrow(param_grid)) {
  # Extract current set of parameters
  params <- list(
    objective = "reg:squarederror",  # Regression problem
    eta = param_grid$eta[i],            # Learning rate
    max_depth = param_grid$max_depth[i],  # Depth of trees
    subsample = param_grid$subsample[i],  # Fraction of training data used per tree
    colsample_bytree = param_grid$colsample_bytree[i] # Fraction of features per tree
  )
  
  # Perform cross-validation with current hyperparameters
  cv_results <- xgb.cv(
    params = params,
    data = train_matrix,
    label = train_labels,
    nfold = 5,  # Number of folds for cross-validation
    nrounds = 1000,  # Maximum number of boosting rounds
    early_stopping_rounds = 10,  # Stop early if performance doesn't improve
    verbose = 0  # Suppress output
  )
  
  # Get the best RMSE for this combination of parameters
  best_iter_rmse <- min(cv_results$evaluation_log$test_rmse_mean)
  
  # If this set of parameters gives better performance, update best result
  if (best_iter_rmse < best_rmse) {
    best_rmse <- best_iter_rmse
    best_params <- param_grid[i, , drop = FALSE]
  }
}

# Output the best parameters and RMSE
print(paste("Best RMSE:", round(best_rmse, 4)))
print("Best Parameters:")
(best_params)

# Now, train the final model with the best parameters
final_params <- list(
  objective = "reg:squarederror",  # Regression problem
  eta = best_params$eta,           # Best learning rate
  max_depth = best_params$max_depth,  # Best max depth
  subsample = best_params$subsample,  # Best subsample value
  colsample_bytree = best_params$colsample_bytree  # Best colsample value
)

xgb_model <- xgboost(
  data = train_matrix,
  label = train_labels,
  nrounds = 5000,         # Maximum boosting rounds (set a reasonable limit)
  params = final_params   # Best hyperparameters from grid search
)

# Make predictions on the test set
xgb_predictions <- predict(xgb_model, test_matrix)

# Calculate RMSE and R-squared
rmse_xgb <- sqrt(mean((xgb_predictions - test_labels)^2))
r2_xgb <- cor(xgb_predictions, test_labels)^2

print(paste("XGBoost RMSE:", round(rmse_xgb, 4)))
print(paste("XGBoost R²:", round(r2_xgb, 4)))

# Plot feature importance
xgb.importance(model = xgb_model)  # Displays feature importance
xgb.plot.importance(xgb.importance(model = xgb_model))  # Plots feature importance

# Add predictions to the test_data and round them
batspredict <- batspredict |> mutate(predicted_avg = round(xgb_predictions, 3))

# Select relevant columns for output
boostedAVG1a <- batspredict |>
  select(name, predicted_avg, AVG_Y1, AVG_Y2, AVG_Y3)

boostedAVG1a$meanAVG <- rowMeans(boostedAVG[, c("AVG_Y1", "AVG_Y2", "AVG_Y3")], na.rm = TRUE)


#plot
ggplot(boostedAVG1a, aes(x = predicted_avg, y = AVG_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 2024 AVG",
    x = "2025 Predicted AVG",
    y = "2024 AVG"
  ) +
  theme_minimal()


ggplot(boostedAVG1a, aes(x = predicted_avg, y = meanAVG)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 3-Year Mean AVG",
    x = "2025 Predicted AVG",
    y = "Mean AVG"
  ) +
  theme_minimal()


#plot again, this time with players with meaningful 2024 PA

boostedAVG1b <- batspredict |>
  filter(PA_Y1 >= 50) |>
  select(name, predicted_avg, AVG_Y1, AVG_Y2, AVG_Y3)

boostedAVG1b$meanAVG <- rowMeans(boostedAVG1b[, c("AVG_Y1", "AVG_Y2", "AVG_Y3")], na.rm = TRUE)


ggplot(boostedAVG1b, aes(x = predicted_avg, y = AVG_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 2024 AVG",
    x = "2025 Predicted AVG",
    y = "2024 AVG"
  ) +
  theme_minimal()


ggplot(boostedAVG1b, aes(x = predicted_avg, y = meanAVG)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 3-Year Mean AVG",
    x = "2025 Predicted AVG",
    y = "Mean AVG"
  ) +
  theme_minimal()


#plot again, increase PA

boostedAVG1c <- batspredict |>
  filter(PA_Y1 >= 250) |>
  select(name, predicted_avg, AVG_Y1, AVG_Y2, AVG_Y3) 

boostedAVG1c$meanAVG <- rowMeans(boostedAVG1c[, c("AVG_Y1", "AVG_Y2", "AVG_Y3")], na.rm = TRUE)


ggplot(boostedAVG1c, aes(x = predicted_avg, y = AVG_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 2024 AVG",
    x = "2025 Predicted AVG",
    y = "2024 AVG"
  ) +
  theme_minimal()

ggplot(boostedAVG1c, aes(x = predicted_avg, y = meanAVG)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 3-Year Mean AVG",
    x = "2025 Predicted AVG",
    y = "Mean AVG"
  ) +
  theme_minimal()


write.csv(batspredict, "clean_unclean.csv", row.names = FALSE)
