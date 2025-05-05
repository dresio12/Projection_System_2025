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

batspredict$name <- gsub("-", "", batspredict$name)

batstrain$name <- gsub("-", "", batstrain$name)

#include only rows with meaningful number of PAs to build meaningful associations

#im leaving in 3 year spans where one season may not have 50 PA because
#xgboost may be able to find connections between those seasons and future performance

batstrain <- batstrain |>
  filter(PA >=50)

#adding in approximate major league experience

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
  select(full_name, current_age, active, mlb_debut_date) 

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
  select(full_name, current_age, active, mlb_debut_date)

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
  select(full_name, current_age, active, mlb_debut_date)


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
  select(full_name, current_age, active, mlb_debut_date)

#combine all players into one df
playerdebuts <- bind_rows(players, players2, players3, players4) |>
  select(full_name, mlb_debut_date, active) |>
  unique()

playerdebuts <- playerdebuts |>
  rename(debut = mlb_debut_date)


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

#join debut+age _Y1, _Y2, _Y3 to respective DFS
batspredict <- batspredict |>
  mutate(Year = 2025)

batspredict <- left_join(batspredict, playerdebuts, by = c('name' = 'full_name'))


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
         
         # Adjust Y2 and Y3
         MLE_Y2 = ifelse(MLE_Y1 > 0, MLE_Y1 - 1, NA),
         MLE_Y3 = ifelse(MLE_Y1 > 1, MLE_Y1 - 2, NA)
  )


batspredict <- batspredict |> select(364, 1:361, 363, 369:371)

batspredict <- batspredict |>
  mutate(active = ifelse(!is.na(Age), "TRUE", active))

batspredict <- batspredict |>
  filter(!(is.na(Age) & is.na(Age_Y1) & is.na(Age_Y2) & is.na(Age_Y3))) |>
  filter(!(is.na(Age) & is.na(Age_Y1) & is.na(Age_Y2))) |>
  filter(!(is.na(Age_Y1) & is.na(Age_Y2))) |>
  mutate(Age = ifelse(is.na(Age) & !is.na(Age_Y2), Age_Y2 + 2, Age))

#remove brett phillips
batspredict <- batspredict[-100, ]

#add debut years to training dataset
playerdebuts <- bind_rows(players2, players3, players4) |>
  select(full_name, mlb_debut_date) |>
  unique()

playerdebuts <- playerdebuts |>
  rename(debut = mlb_debut_date)

batstrain <- left_join(batstrain, playerdebuts, by = c('name' = 'full_name'))

#remove brett phillips
batstrain <- batstrain[-591, ]
batstrain <- batstrain[-1136, ]

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
         MLE_Y3 = ifelse(MLE_Y1 > 1, MLE_Y1 - 2, NA)
  )

batstrain <- batstrain |>
  mutate(MLE_Y1 = ifelse(Year == 2023, MLE_Y1 - 1, MLE_Y1),
         MLE_Y1 = ifelse(Year == 2022, MLE_Y1 - 2, MLE_Y1),
         MLE_Y2 = ifelse(Year == 2023, MLE_Y2 - 1, MLE_Y2),
         MLE_Y2 = ifelse(Year == 2022, MLE_Y2 - 2, MLE_Y2),
         MLE_Y3 = ifelse(Year == 2023, MLE_Y3 - 1, MLE_Y3),
         MLE_Y3 = ifelse(Year == 2022, MLE_Y3 - 2, MLE_Y3),
         MLE_Y1 = ifelse(MLE_Y1 < 0, NA, MLE_Y1),
         MLE_Y2 = ifelse(MLE_Y2 < 0, NA, MLE_Y2),
         MLE_Y3 = ifelse(MLE_Y3 < 0, NA, MLE_Y3)
         )

batstrain <- batstrain |> select(1:361, 363, 368:370)

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
  nrounds = 2000,         # Maximum boosting rounds (set a reasonable limit)
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
boostedAVG3a <- batspredict |>
  select(name, predicted_avg, AVG_Y1, AVG_Y2, AVG_Y3)

boostedAVG3a$meanAVG <- rowMeans(boostedAVG3a[, c("AVG_Y1", "AVG_Y2", "AVG_Y3")], na.rm = TRUE)


#plot
ggplot(boostedAVG3a, aes(x = predicted_avg, y = AVG_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 2024 AVG",
    x = "2025 Predicted AVG",
    y = "2024 AVG"
  ) +
  theme_minimal()


ggplot(boostedAVG3a, aes(x = predicted_avg, y = meanAVG)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 3-Year Mean AVG",
    x = "2025 Predicted AVG",
    y = "Mean AVG"
  ) +
  theme_minimal()


#plot again, this time with players with meaningful 2024 PA

boostedAVG3b <- batspredict |>
  filter(PA_Y1 >= 50) |>
  select(name, predicted_avg, AVG_Y1, AVG_Y2, AVG_Y3)

boostedAVG3b$meanAVG <- rowMeans(boostedAVG3b[, c("AVG_Y1", "AVG_Y2", "AVG_Y3")], na.rm = TRUE)


ggplot(boostedAVG3b, aes(x = predicted_avg, y = AVG_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 2024 AVG",
    x = "2025 Predicted AVG",
    y = "2024 AVG"
  ) +
  theme_minimal()


ggplot(boostedAVG3b, aes(x = predicted_avg, y = meanAVG)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 3-Year Mean AVG",
    x = "2025 Predicted AVG",
    y = "Mean AVG"
  ) +
  theme_minimal()


#plot again, increase PA

boostedAVG3c <- batspredict |>
  filter(PA_Y1 >= 250) |>
  select(name, predicted_avg, AVG_Y1, AVG_Y2, AVG_Y3) 

boostedAVG3c$meanAVG <- rowMeans(boostedAVG3c[, c("AVG_Y1", "AVG_Y2", "AVG_Y3")], na.rm = TRUE)


ggplot(boostedAVG3c, aes(x = predicted_avg, y = AVG_Y1)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 2024 AVG",
    x = "2025 Predicted AVG",
    y = "2024 AVG"
  ) +
  theme_minimal()

ggplot(boostedAVG3c, aes(x = predicted_avg, y = meanAVG)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add a reference line (ideal prediction)
  labs(
    title = "2025 Predicted AVG vs 3-Year Mean AVG",
    x = "2025 Predicted AVG",
    y = "Mean AVG"
  ) +
  theme_minimal()


write.csv(batspredict, "unclean_unclean_mle.csv", row.names = FALSE)
