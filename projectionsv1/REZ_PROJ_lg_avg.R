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


#load in batstest from GitHub
batstest <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/batstest.csv", stringsAsFactors = FALSE)

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


#creating lg_stat_year columns
# Identify the unique years in the dataset based on column names
years <- unique(sub(".*_(\\d{4})$", "\\1", colnames(batstest)))

# Loop through each year and calculate the league average for each stat
for (year in years) {
  # Select columns that match the current year
  stat_cols <- grep(paste0("_", year, "$"), colnames(batstest), value = TRUE)
  
  # Compute the mean for each stat column and create new league average columns
  for (col in stat_cols) {
    lg_col_name <- paste0("lg_", col)  # Create the new column name
    batstest[[lg_col_name]] <- mean(batstest[[col]], na.rm = TRUE)  # Calculate and assign the mean
  }
}

batstest <- batstest |> select(-542, -632, -722, -812, -902, - 992)

#renaming columns to remove current year suffix, 
#assign Y1 as most recent, Y3 most distant

#2024:2021 interval 1
int1 <- batstest |>
  select(1:361) |>
  rename_with(~ str_replace(., "_2024$", ""), ends_with("_2024")) |>
  rename_with(~ str_replace(., "_2023$", "_Y1"), ends_with("_2023")) |>
  rename_with(~ str_replace(., "_2022$", "_Y2"), ends_with("_2022")) |>
  rename_with(~ str_replace(., "_2021$", "_Y3"), ends_with("_2021")) |>
  mutate(Year = 2024)


#2023:2020
int2 <- batstest |>
  select(1, 92:451) |>
  rename_with(~ str_replace(., "_2023$", ""), ends_with("_2023")) |>
  rename_with(~ str_replace(., "_2022$", "_Y1"), ends_with("_2022")) |>
  rename_with(~ str_replace(., "_2021$", "_Y2"), ends_with("_2021")) |>
  rename_with(~ str_replace(., "_2020$", "_Y3"), ends_with("_2020")) |>
  mutate(Year = 2023)


#2022:2019
int3 <- batstest |>
  select(1, 182:541) |>
  rename_with(~ str_replace(., "_2022$", ""), ends_with("_2022")) |>
  rename_with(~ str_replace(., "_2021$", "_Y1"), ends_with("_2021")) |>
  rename_with(~ str_replace(., "_2020$", "_Y2"), ends_with("_2020")) |>
  rename_with(~ str_replace(., "_2019$", "_Y3"), ends_with("_2019")) |>
  mutate(Year = 2022)


#2025:2022 1-91
int4 <- batstest |>
  select(1:271, 542:808) |>
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

batspredict$name <- gsub("-", "", batspredict$name)

batstrain$name <- gsub("-", "", batstrain$name)

#include only rows with meaningful number of PAs to build meaningful associations

#im leaving in 3 year spans where one season may not have 50 PA because
#xgboost may be able to find connections between those seasons and future performance

batstrain <- batstrain |>
  drop_na() |>
  filter(PA >=50)

#filling in NAs with lg_avgs

# Loop through each column in batspredict
for (col in colnames(batspredict)) {
  # Skip columns that are already league averages (to avoid lg_lg_ duplication)
  if (grepl("^lg_", col)) next
  
  # Construct the corresponding league average column name
  lg_col <- paste0("lg_", col)
  
  # Check if the league average column exists
  if (lg_col %in% colnames(batspredict)) {
    # Replace NA values in batspredict with the corresponding league average
    batspredict[[col]][is.na(batspredict[[col]])] <- batspredict[[lg_col]][is.na(batspredict[[col]])]
  }
}

#remove lg_avgs since we no longer need them
batspredict <- batspredict |> select(1:361)

#Adding in Ages
#obtaining debuts

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

players <- players |>
  select(full_name, active) 

# Remove special characters
players2$full_name <- gsub("Ã©", "e" , players2$full_name)
players2$full_name <- gsub("Ã±", "n" , players2$full_name)
players2$full_name <- gsub("Ã¡", "a" , players2$full_name)
players2$full_name <- gsub("i³", "i" , players2$full_name)
players2$full_name <- gsub("Ã³", "o" , players2$full_name)
players2$full_name <- gsub("Ãº", "u" , players2$full_name)
players2$full_name <- gsub("Ã", "i" , players2$full_name)
players2$full_name <- gsub("\\.", "", players2$full_name)

players2 <- players2 |>
  select(full_name, active)

# Remove special characters
players3$full_name <- gsub("Ã©", "e" , players3$full_name)
players3$full_name <- gsub("Ã±", "n" , players3$full_name)
players3$full_name <- gsub("Ã¡", "a" , players3$full_name)
players3$full_name <- gsub("i³", "i" , players3$full_name)
players3$full_name <- gsub("Ã³", "o" , players3$full_name)
players3$full_name <- gsub("Ãº", "u" , players3$full_name)
players3$full_name <- gsub("Ã", "i" , players3$full_name)
players3$full_name <- gsub("\\.", "", players3$full_name)

players3 <- players3 |>
  select(full_name, active)


# Remove special characters
players4$full_name <- gsub("Ã©", "e" , players4$full_name)
players4$full_name <- gsub("Ã±", "n" , players4$full_name)
players4$full_name <- gsub("Ã¡", "a" , players4$full_name)
players4$full_name <- gsub("i³", "i" , players4$full_name)
players4$full_name <- gsub("Ã³", "o" , players4$full_name)
players4$full_name <- gsub("Ãº", "u" , players4$full_name)
players4$full_name <- gsub("Ã", "i" , players4$full_name)
players4$full_name <- gsub("\\.", "", players4$full_name)

players4 <- players4 |>
  select(full_name, active)

#combine all players into one df
playerdebuts <- bind_rows(players, players2, players3, players4) |>
  select(full_name, active) |>
  unique()


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
players2$full_name[449] <- "Carlos PerezOAK"
players3$full_name[480] <- "Carlos PerezOAK"
players2$full_name[380] <- "Oscar Mercado"
players3$full_name[410] <- "Oscar Mercado"
players4$full_name[389] <- "Oscar Mercado"


batspredict <- left_join(batspredict, playerdebuts, by = c('name' = 'full_name'))


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
train_matrix <- as.matrix(batstrain[predictors])
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

# Train the final model with the best parameters
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
  nrounds = 4000,         # Maximum boosting rounds (set a reasonable limit)
  params = final_params   # Best hyperparameters from grid search
)

# Make predictions on the test set
xgb_predictions <- predict(xgb_model, test_matrix)


# Plot feature importance
xgb.importance(model = xgb_model)  # Displays feature importance
xgb.plot.importance(xgb.importance(model = xgb_model))  # Plots feature importance

# Add predictions to the test_data and round them
batspredict <- batspredict |> mutate(predicted_avg = round(xgb_predictions, 3))


# Select relevant columns for output
boostedAVG4a <- batspredict |>
  select(name, predicted_avg, AVG_Y1, AVG_Y2, AVG_Y3)

boostedAVG4a$meanAVG <- rowMeans(boostedAVG4a[, c("AVG_Y1", "AVG_Y2", "AVG_Y3")], na.rm = TRUE)


#plot
ggplot(boostedAVG4a, aes(x = predicted_avg, y = AVG_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 2024 AVG",
    x = "2025 Predicted AVG",
    y = "2024 AVG"
  ) +
  theme_minimal()


ggplot(boostedAVG4a, aes(x = predicted_avg, y = meanAVG)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 3-Year Mean AVG",
    x = "2025 Predicted AVG",
    y = "Mean AVG"
  ) +
  theme_minimal()


#plot again, this time with players with meaningful 2024 PA

boostedAVG4b <- batspredict |>
  filter(PA_Y1 >= 50) |>
  select(name, predicted_avg, AVG_Y1, AVG_Y2, AVG_Y3)

boostedAVG4b$meanAVG <- rowMeans(boostedAVG4b[, c("AVG_Y1", "AVG_Y2", "AVG_Y3")], na.rm = TRUE)


ggplot(boostedAVG4b, aes(x = predicted_avg, y = AVG_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 2024 AVG",
    x = "2025 Predicted AVG",
    y = "2024 AVG"
  ) +
  theme_minimal()


ggplot(boostedAVG4b, aes(x = predicted_avg, y = meanAVG)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 3-Year Mean AVG",
    x = "2025 Predicted AVG",
    y = "Mean AVG"
  ) +
  theme_minimal()


#plot again, increase PA

boostedAVG4c <- batspredict |>
  filter(PA_Y1 >= 250) |>
  select(name, predicted_avg, AVG_Y1, AVG_Y2, AVG_Y3) 

boostedAVG4c$meanAVG <- rowMeans(boostedAVG4c[, c("AVG_Y1", "AVG_Y2", "AVG_Y3")], na.rm = TRUE)


ggplot(boostedAVG4c, aes(x = predicted_avg, y = AVG_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 2024 AVG",
    x = "2025 Predicted AVG",
    y = "2024 AVG"
  ) +
  theme_minimal()

ggplot(boostedAVG4c, aes(x = predicted_avg, y = meanAVG)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 3-Year Mean AVG",
    x = "2025 Predicted AVG",
    y = "Mean AVG"
  ) +
  theme_minimal()


write.csv(batspredict, "clean_lgavg.csv", row.names = FALSE)
