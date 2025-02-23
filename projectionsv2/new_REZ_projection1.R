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


#load in batstest from GitHub
batstest <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/projectionsv2/bats2.csv", stringsAsFactors = FALSE)

batstest$name <- gsub("á", "a" , batstest$name)
batstest$name <- gsub("é", "e" , batstest$name)
batstest$name <- gsub("í", "i" , batstest$name)
batstest$name <- gsub("ó", "o" , batstest$name)
batstest$name <- gsub("ú", "u" , batstest$name)
batstest$name <- gsub("ñ", "n" , batstest$name)
batstest$name <- gsub("-", "" , batstest$name)
batstest$name <- gsub("\\.", "", batstest$name)
  
#renaming columns to remove current year suffix, 
#assign Y1 as most recent, Y3 most distant

#2024:2020 interval 1
int1 <- batstest |>
  select(1,2, 4:318) |>
  rename_with(~ str_replace(., "_2024$", ""), ends_with("_2024")) |>
  rename_with(~ str_replace(., "_2023$", "_Y1"), ends_with("_2023")) |>
  rename_with(~ str_replace(., "_2022$", "_Y2"), ends_with("_2022")) |>
  rename_with(~ str_replace(., "_2021$", "_Y3"), ends_with("_2021")) |>
  rename_with(~ str_replace(., "_2020$", "_Y4"), ends_with("_2020")) |>
  mutate(Year = 2024)


#2023:2019
int2 <- batstest |>
  select(1,2, 67:381) |>
  rename_with(~ str_replace(., "_2023$", ""), ends_with("_2023")) |>
  rename_with(~ str_replace(., "_2022$", "_Y1"), ends_with("_2022")) |>
  rename_with(~ str_replace(., "_2021$", "_Y2"), ends_with("_2021")) |>
  rename_with(~ str_replace(., "_2020$", "_Y3"), ends_with("_2020")) |>
  rename_with(~ str_replace(., "_2019$", "_Y4"), ends_with("_2019")) |>
  mutate(Year = 2023)


#2022:2018
int3 <- batstest |>
  select(1,2, 130:444) |>
  rename_with(~ str_replace(., "_2022$", ""), ends_with("_2022")) |>
  rename_with(~ str_replace(., "_2021$", "_Y1"), ends_with("_2021")) |>
  rename_with(~ str_replace(., "_2020$", "_Y2"), ends_with("_2020")) |>
  rename_with(~ str_replace(., "_2019$", "_Y3"), ends_with("_2019")) |>
  rename_with(~ str_replace(., "_2018$", "_Y4"), ends_with("_2018")) |>
  mutate(Year = 2022)


#2025:2021 1-91
int4 <- batstest |>
  select(1,2, 4:255) |>
  rename_with(~ str_replace(., "_2024$", "_Y1"), ends_with("_2024")) |>
  rename_with(~ str_replace(., "_2023$", "_Y2"), ends_with("_2023")) |>
  rename_with(~ str_replace(., "_2022$", "_Y3"), ends_with("_2022")) |>
  rename_with(~ str_replace(., "_2021$", "_Y4"), ends_with("_2021")) 


empty <- batstest |>
  select(1,2, 4:66) |>
  rename_with(~ str_replace(., "_2024$", ""), ends_with("_2024")) |>
  mutate(Age = ifelse(!is.na(Age), Age + 1, Age))

empty[ ,4:65] <- NA


#create training and predicting dfs
batstrain <- bind_rows(int1, int2, int3)

batspredict <- left_join(empty, int4)

#include only rows with meaningful number of PAs to build meaningful associations

#im leaving in 3 year spans where one season may not have 50 PA because
#xgboost may be able to find connections between those seasons and future performance

batstrain <- batstrain |>
  filter(PA >=50)

#adding in approximate major league experience

#obtaining debuts
player_keys <- baseballr::chadwick_player_lu()
player_keys <- player_keys |> select(3,7)

#int4
players <- baseballr::mlb_sports_players(sport_id = 1, season = 2024)

#int1
players2 <- baseballr::mlb_sports_players(sport_id = 1, season = 2023)

#int2
players3 <- baseballr::mlb_sports_players(sport_id = 1, season = 2022)

#int3
players4 <- baseballr::mlb_sports_players(sport_id = 1, season = 2021)

players <- players %>%
  filter(primary_position_name != "Pitcher")

players2 <- players2 %>%
  filter(primary_position_name != "Pitcher")

players3 <- players3 %>%
  filter(primary_position_name != "Pitcher")

players4 <- players4 %>%
  filter(primary_position_name != "Pitcher")

# Remove special characters
players$full_name <- gsub("Ã©", "e" , players$full_name)
players$full_name <- gsub("Ã±", "n" , players$full_name)
players$full_name <- gsub("Ã¡", "a" , players$full_name)
players$full_name <- gsub("i³", "i" , players$full_name)
players$full_name <- gsub("Ã³", "o" , players$full_name)
players$full_name <- gsub("Ãº", "u" , players$full_name)
players$full_name <- gsub("Ã", "i" , players$full_name)
players$full_name <- gsub("\\.", "", players$full_name)

players <- left_join(players, player_keys, by = c('player_id' = 'key_mlbam'))

players <- players |>
  select(player_id, key_fangraphs, full_name, current_age, active, mlb_debut_date) 

# Remove special characters
players2$full_name <- gsub("Ã©", "e" , players2$full_name)
players2$full_name <- gsub("Ã±", "n" , players2$full_name)
players2$full_name <- gsub("Ã¡", "a" , players2$full_name)
players2$full_name <- gsub("i³", "i" , players2$full_name)
players2$full_name <- gsub("Ã³", "o" , players2$full_name)
players2$full_name <- gsub("Ãº", "u" , players2$full_name)
players2$full_name <- gsub("Ã", "i" , players2$full_name)
players2$full_name <- gsub("\\.", "", players2$full_name)

players2 <- left_join(players2, player_keys, by = c('player_id' = 'key_mlbam'))

players2 <- players2 |>
  select(player_id, key_fangraphs, full_name, current_age, active, mlb_debut_date) 

# Remove special characters
players3$full_name <- gsub("Ã©", "e" , players3$full_name)
players3$full_name <- gsub("Ã±", "n" , players3$full_name)
players3$full_name <- gsub("Ã¡", "a" , players3$full_name)
players3$full_name <- gsub("i³", "i" , players3$full_name)
players3$full_name <- gsub("Ã³", "o" , players3$full_name)
players3$full_name <- gsub("Ãº", "u" , players3$full_name)
players3$full_name <- gsub("Ã", "i" , players3$full_name)
players3$full_name <- gsub("\\.", "", players3$full_name)

players3 <- left_join(players3, player_keys, by = c('player_id' = 'key_mlbam'))

players3 <- players3 |>
  select(player_id, key_fangraphs, full_name, current_age, active, mlb_debut_date) 


# Remove special characters
players4$full_name <- gsub("Ã©", "e" , players4$full_name)
players4$full_name <- gsub("Ã±", "n" , players4$full_name)
players4$full_name <- gsub("Ã¡", "a" , players4$full_name)
players4$full_name <- gsub("i³", "i" , players4$full_name)
players4$full_name <- gsub("Ã³", "o" , players4$full_name)
players4$full_name <- gsub("Ãº", "u" , players4$full_name)
players4$full_name <- gsub("Ã", "i" , players4$full_name)
players4$full_name <- gsub("\\.", "", players4$full_name)

players4 <- left_join(players4, player_keys, by = c('player_id' = 'key_mlbam'))

players4 <- players4 |>
  select(player_id, key_fangraphs, full_name, current_age, active, mlb_debut_date) 

# Normalize names in all datasets
batspredict <- batspredict %>%
  mutate(name = stri_trans_general(name, "Latin-ASCII"))

players <- players %>%
  mutate(full_name = stri_trans_general(full_name, "Latin-ASCII"))

players$full_name <- gsub("-", "", players$full_name)

players2 <- players2 %>%
  mutate(full_name = stri_trans_general(full_name, "Latin-ASCII"))

players2$full_name <- gsub("-", "", players2$full_name)

players3 <- players3 %>%
  mutate(full_name = stri_trans_general(full_name, "Latin-ASCII"))

players3$full_name <- gsub("-", "", players3$full_name)

players4 <- players4 %>%
  mutate(full_name = stri_trans_general(full_name, "Latin-ASCII"))

players4$full_name <- gsub("-", "", players4$full_name)

batstrain <- batstrain %>%
  mutate(name = stri_trans_general(name, "Latin-ASCII"))



#other df changes
players2$full_name[380] <- "Oscar Mercado"
players3$full_name[410] <- "Oscar Mercado"
players4$full_name[389] <- "Oscar Mercado"

#join debut+age _Y1, _Y2, _Y3 to respective DFS
batspredict <- batspredict |>
  mutate(Year = 2025)

#combine all players into one df
playerdebuts <- bind_rows(players, players2, players3, players4) |>
  select(full_name, mlb_debut_date, active, player_id, key_fangraphs) |>
  unique()

playerdebuts <- playerdebuts |>
  rename(debut = mlb_debut_date)

nofg <- playerdebuts |>
  filter(is.na(key_fangraphs)) |>
  select(1:3)

batspredict <- left_join(batspredict, playerdebuts, by = c('name' = 'full_name',
                                                    'playerid' = 'key_fangraphs'))

batspredict <- left_join(batspredict, nofg, by = c('name' = 'full_name') )

batspredict <- batspredict |>
  mutate(
    debut.x = ifelse(is.na(debut.x), debut.y, debut.x),
    active.x = ifelse(is.na(active.x), active.y, active.x)
    ) |>
  rename(
    debut = debut.x,
    active = active.x
  ) |>
  select(1:320)

#add MLE columns
batspredict <- batspredict |>
  mutate(debut = as.Date(debut),
         debut_year = year(debut),
         debut_month = month(debut),
         debut_day = day(debut),
         current_year = 2025,  # Update as needed
         
         # Base Years Since Debut calculation
         MLE_Y1 = case_when(
           debut_year == 2024 & (debut_month > 8 | (debut_month == 8 & debut_day >= 20)) ~ 0, # Late 2020 = 0
           debut_year == 2024 & debut_month < 8 ~ 1, # Before Aug 2020 = 1
           debut_month == 8 & debut_day >= 1 & debut_day <= 19 ~ (current_year - debut_year - 1) + 0.5, # Any year Aug 1-19 gets 0.5
           (debut_month > 8 | (debut_month == 8 & debut_day >= 20)) ~ (current_year - debut_year - 1), # Any year after Aug 20 gets -1
           TRUE ~ current_year - debut_year
         ),
         
         # Adjust Y2, Y3, Y4
         MLE_Y2 = ifelse(MLE_Y1 > 0, MLE_Y1 - 1, NA),
         MLE_Y3 = ifelse(MLE_Y1 > 1, MLE_Y1 - 2, NA),
         MLE_Y4 = ifelse(MLE_Y1 > 2, MLE_Y1 - 3, NA)
  )


batspredict <- batspredict |> select(318, 1:317, 319, 325:328)

batspredict <- batspredict |>
  filter(!(is.na(Age) & is.na(Age_Y1) & is.na(Age_Y2) & is.na(Age_Y3))) |>
  filter(!(is.na(Age) & is.na(Age_Y1) & is.na(Age_Y2))) |>
  filter(!(is.na(Age_Y1) & is.na(Age_Y2))) |>
  mutate(Age = ifelse(is.na(Age) & !is.na(Age_Y2), Age_Y2 + 2, Age))


#add debut years to training dataset
batstrain <- left_join(batstrain, playerdebuts, by = c('name' = 'full_name',
                                                           'playerid' = 'key_fangraphs'))

batstrain <- left_join(batstrain, nofg, by = c('name' = 'full_name') )

batstrain <- batstrain |>
  mutate(
    debut.x = ifelse(is.na(debut.x), debut.y, debut.x),
    active.x = ifelse(is.na(active.x), active.y, active.x)
  ) |>
  rename(
    debut = debut.x,
    active = active.x
  ) |>
  select(1:320)


#add MLE columns
batstrain <- batstrain |>
  mutate(debut = as.Date(debut),
         debut_year = year(debut),
         debut_month = month(debut),
         debut_day = day(debut),
         current_year = 2024,  # Update as needed
         
         # Base Years Since Debut calculation
         MLE_Y1 = case_when(
           debut_year == 2023 & (debut_month > 8 | (debut_month == 8 & debut_day >= 20)) ~ 0, # Late 2023= 0
           debut_year == 2023 & debut_month < 8 ~ 1, # Before Aug 2020 = 1
           debut_month == 8 & debut_day >= 1 & debut_day <= 19 ~ (current_year - debut_year - 1) + 0.5, # Any year Aug 1-19 gets +0.5
           (debut_month > 8 | (debut_month == 8 & debut_day >= 20)) ~ (current_year - debut_year - 1), # Any year after Aug20 gets -1
           TRUE ~ current_year - debut_year
         ),
         
         # Adjust Y2 and Y3
         MLE_Y2 = ifelse(MLE_Y1 > 0, MLE_Y1 - 1, NA),
         MLE_Y3 = ifelse(MLE_Y1 > 1, MLE_Y1 - 2, NA),
         MLE_Y4 = ifelse(MLE_Y1 > 2, MLE_Y1 - 3, NA)
  )

batstrain <- batstrain |>
  mutate(MLE_Y1 = ifelse(Year == 2023, MLE_Y1 - 1, MLE_Y1),
         MLE_Y1 = ifelse(Year == 2022, MLE_Y1 - 2, MLE_Y1),
         MLE_Y2 = ifelse(Year == 2023, MLE_Y2 - 1, MLE_Y2),
         MLE_Y2 = ifelse(Year == 2022, MLE_Y2 - 2, MLE_Y2),
         MLE_Y3 = ifelse(Year == 2023, MLE_Y3 - 1, MLE_Y3),
         MLE_Y3 = ifelse(Year == 2022, MLE_Y3 - 2, MLE_Y3),
         MLE_Y4 = ifelse(Year == 2023, MLE_Y4 - 1, MLE_Y4),
         MLE_Y4 = ifelse(Year == 2022, MLE_Y4 - 2, MLE_Y4),
         MLE_Y1 = ifelse(MLE_Y1 < 0, NA, MLE_Y1),
         MLE_Y2 = ifelse(MLE_Y2 < 0, NA, MLE_Y2),
         MLE_Y3 = ifelse(MLE_Y3 < 0, NA, MLE_Y3),
         MLE_Y4 = ifelse(MLE_Y4 < 0, NA, MLE_Y4)
  )

batstrain <- batstrain |> select(318, 1:317, 319, 325:328)


#final cleaning
batspredict <- batspredict |>
  filter(!is.na(debut))

#
#
#testing on batting average

base_stat <- "AVG"

# Get the column names that end with _Y1, _Y2, _Y3, _Y4
predictors <- grep("_Y[1-4]$", colnames(batstrain), value = TRUE)

# Create the formula dynamically
formula <- as.formula(paste("AVG ~", paste(predictors, collapse = " + ")))

formula_pred <- as.formula(paste("~", paste(predictors, collapse = " + ")))


#XGBoost

# Convert to matrix format (excluding target variable)
train_matrix <- as.matrix(batstrain[predictors])
test_matrix <- as.matrix(batspredict[predictors])

train_matrix[is.infinite(train_matrix)] <- NA
test_matrix[is.infinite(test_matrix)] <- NA

# Extract target variable
train_labels <- batstrain$AVG
test_labels  <- batspredict$AVG

# Define hyperparameter grid
param_grid <- expand.grid(
  eta = c(0.01, 0.05, 0.1),         # Learning rate
  max_depth = c(3, 4, 6),          # Depth of trees
  subsample = c(0.8, 0.9, 1),      # Fraction of data used for each tree
  colsample_bytree = c(0.7, 0.8, 1) # Fraction of features used for each tree
)

# Initialize variables to store best result
best_rmse <- Inf
best_params <- NULL

#provide mean league average
mean_AVG <- .245

# Loop through the hyperparameter grid
for (i in 1:nrow(param_grid)) {
  # Extract current set of parameters
  params <- list(
    objective = "reg:squarederror",  
    eta = param_grid$eta[i],            
    max_depth = param_grid$max_depth[i],  
    subsample = param_grid$subsample[i],  
    colsample_bytree = param_grid$colsample_bytree[i],
    base_score = mean_AVG               
  )
  
  # Perform cross-validation with current hyperparameters
  cv_results <- xgb.cv(
    params = params,
    data = train_matrix,
    label = train_labels,
    nfold = 5,  # Number of folds for cross-validation
    nrounds = 1000,  # Maximum number of boosting rounds
    early_stopping_rounds = 10,  # Stop early if performance doesn't improve
    verbose = 0,  # Suppress output
    nthread = parallel::detectCores() - 1
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

# Train the final model with the best parameters
final_params <- list(
  objective = "reg:squarederror",  
  eta = best_params$eta,           
  max_depth = best_params$max_depth,  
  subsample = best_params$subsample,  
  colsample_bytree = best_params$colsample_bytree,
  base_score = mean_AVG            
)

xgb_model <- xgboost(
  data = train_matrix,
  label = train_labels,
  nrounds = 1500,         # Maximum boosting rounds (set a reasonable limit)
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
boostedAVG5a <- batspredict |>
  select(name, predicted_avg, AVG_Y1, AVG_Y2, AVG_Y3)

boostedAVG5a$meanAVG <- rowMeans(boostedAVG5a[, c("AVG_Y1", "AVG_Y2", "AVG_Y3")], na.rm = TRUE)


#plot
ggplot(boostedAVG5a, aes(x = predicted_avg, y = AVG_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 2024 AVG",
    x = "2025 Predicted AVG",
    y = "2024 AVG"
  ) +
  theme_minimal()


ggplot(boostedAVG5a, aes(x = predicted_avg, y = meanAVG)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 3-Year Mean AVG",
    x = "2025 Predicted AVG",
    y = "Mean AVG"
  ) +
  theme_minimal()


#plot again, this time with players with meaningful 2024 PA

boostedAVG5b <- batspredict |>
  filter(PA_Y1 >= 50) |>
  select(name, predicted_avg, AVG_Y1, AVG_Y2, AVG_Y3)

boostedAVG5b$meanAVG <- rowMeans(boostedAVG5b[, c("AVG_Y1", "AVG_Y2", "AVG_Y3")], na.rm = TRUE)


ggplot(boostedAVG5b, aes(x = predicted_avg, y = AVG_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 2024 AVG",
    x = "2025 Predicted AVG",
    y = "2024 AVG"
  ) +
  theme_minimal()


ggplot(boostedAVG5b, aes(x = predicted_avg, y = meanAVG)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 3-Year Mean AVG",
    x = "2025 Predicted AVG",
    y = "Mean AVG"
  ) +
  theme_minimal()


#plot again, increase PA

boostedAVG5c <- batspredict |>
  filter(PA_Y1 >= 250) |>
  select(name, predicted_avg, AVG_Y1, AVG_Y2, AVG_Y3) 

boostedAVG5c$meanAVG <- rowMeans(boostedAVG5c[, c("AVG_Y1", "AVG_Y2", "AVG_Y3")], na.rm = TRUE)


ggplot(boostedAVG5c, aes(x = predicted_avg, y = AVG_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 2024 AVG",
    x = "2025 Predicted AVG",
    y = "2024 AVG"
  ) +
  theme_minimal()

ggplot(boostedAVG5c, aes(x = predicted_avg, y = meanAVG)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 3-Year Mean AVG",
    x = "2025 Predicted AVG",
    y = "Mean AVG"
  ) +
  theme_minimal()

write.csv(batspredict, "pf_adj_avg.csv", row.names = FALSE)
