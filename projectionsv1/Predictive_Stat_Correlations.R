library(baseballr)
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(tidyr)
library(stringr)
library(glmnet)

pitcherstatsmain <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/pitcherstats.csv", stringsAsFactors = FALSE)

batterstatsmain <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/batterstats.csv", stringsAsFactors = FALSE)

batterstats <- batterstatsmain |>
  select(1, 5, 2:3, 8, 12:61, 70, 73:77, 85, 111:121, 138:147, 154:157, 281:287,
         289:291)

batterstats <- batterstats |>
  rename('name' = 'PlayerNameRoute.x',
         'BB_pct_plus' = 'BB_pct.',
         'K_pct_plus' = 'K_pct.',
         'OBP_plus' = 'OBP.',
         'SLG_plus' = 'SLG.',
         'ISO_plus' = 'ISO.',
         'BABIP_plus' = 'BABIP.',
         'LD_pct_plus' = 'LD_pct.',
         'FB_pct_plus' = 'FB_pct.',
         'GB_pct_plus' = 'GB_pct.',
         'HRFB_pct_plus' = 'HRFB_pct.'
         )

batterstats$name[c(3038)] <- "Carlos PerezOAK"


#creating minimum PA for correlation matrices
batterstats <- batterstats |>
  filter(PA >=50)

#create rowbind dfs for the adjacent seasons
#selects necessary columns
#widens df to assign season to each variable of interest
#


#2019-20

b1920 <- batterstats |>
  filter(Season == '2019' | Season == "2020") |>
  select(name, Season, 3:94, 97) |>
  pivot_wider(names_from = Season, values_from = 3:95)


#2020-21 

b2021 <- batterstats |>
  filter(Season == '2020' | Season == "2021") |>
  select(name, Season, 3:94, 97) |>
  pivot_wider(names_from = Season, values_from = 3:95)


#2021-22

b2122 <- batterstats |>
  filter(Season == '2021' | Season == "2022") |>
  select(name, Season, 3:94, 97) |> 
  pivot_wider(names_from = Season, values_from = 3:95)


#2022-23

b2223 <- batterstats |>
  filter(Season == '2022' | Season == "2023") |>
  select(name, Season, 3:94, 97) |> 
  pivot_wider(names_from = Season, values_from = 3:95)


#2023-24

b2324 <- batterstats |>
  filter(Season == '2023' | Season == "2024") |>
  select(name, Season, 3:94, 97) |> 
  pivot_wider(names_from = Season, values_from = 3:95)


# Subset data to remove non-numeric columns
#named ds for data subset, followed by corresponding tag

#2019-20
ds1920 <- b1920 |>
  select(8:185) |>
  na.omit()


#2020-21 
ds2021 <- b2021 |>
  select(8:185) |>
  na.omit()


#2021-22
ds2122 <- b2122 |>
  select(8:185) |>
  na.omit()


#2022-23
ds2223 <- b2223 |>
  select(8:185) |>
  na.omit()


#2023-24 dfs
ds2324 <- b2324 |>
  select(8:185) |>
  na.omit()


# Identify yearly columns separately
cols_2019 <- grep("_2019$", names(ds1920), value = TRUE)
cols_2020 <- grep("_2020$", names(ds1920), value = TRUE)
cols_2021 <- grep("_2021$", names(ds2122), value = TRUE)
cols_2022 <- grep("_2022$", names(ds2122), value = TRUE)
cols_2023 <- grep("_2023$", names(ds2324), value = TRUE)
cols_2024 <- grep("_2024$", names(ds2324), value = TRUE)

# Compute correlation matrix (2019 stats vs. 2020 stats)
cm1920 <- cor(ds1920[, cols_2019], 
              ds1920[, cols_2020], 
              use = "complete.obs")


# Compute correlation matrix (2020 stats vs. 2021 stats)
cm2021 <- cor(ds2021[, cols_2020], 
              ds2021[, cols_2021], 
              use = "complete.obs")


# Compute correlation matrix (2021 stats vs. 2022 stats)
cm2122 <- cor(ds2122[, cols_2021], 
              ds2122[, cols_2022], 
              use = "complete.obs")


# Compute correlation matrix (2022 stats vs. 2023 stats)
cm2223 <- cor(ds2223[, cols_2022], 
              ds2223[, cols_2023], 
              use = "complete.obs")


# Compute correlation matrix (2023 stats vs. 2024 stats)
cm2324 <- cor(ds2324[, cols_2023], 
              ds2324[, cols_2024], 
              use = "complete.obs")

#
#
#

#Standardizing data
dfs <- list(ds1920, ds2021, ds2122, ds2223, ds2324)

# Function to standardize a dataframe
standardize_df <- function(df) {
  # Apply scale function to all numeric columns in the dataframe
  df_standardized <- df
  numeric_cols <- sapply(df, is.numeric)  # Identify numeric columns
  df_standardized[numeric_cols] <- scale(df[numeric_cols])
  
  return(df_standardized)
}

# Apply the standardization function to each dataframe
sds1920 <- standardize_df(ds1920)
sds2021 <- standardize_df(ds2021)
sds2122 <- standardize_df(ds2122)
sds2223 <- standardize_df(ds2223)
sds2324 <- standardize_df(ds2324)

# Compute correlation matrix (2019 stats vs. 2020 stats)
scm1920 <- cor(sds1920[, cols_2019], 
              sds1920[, cols_2020], 
              use = "complete.obs")


# Compute correlation matrix (2020 stats vs. 2021 stats)
scm2021 <- cor(sds2021[, cols_2020], 
              sds2021[, cols_2021], 
              use = "complete.obs")


# Compute correlation matrix (2021 stats vs. 2022 stats)
scm2122 <- cor(sds2122[, cols_2021], 
              sds2122[, cols_2022], 
              use = "complete.obs")


# Compute correlation matrix (2022 stats vs. 2023 stats)
scm2223 <- cor(sds2223[, cols_2022], 
              sds2223[, cols_2023], 
              use = "complete.obs")


# Compute standardized correlation matrix (2023 stats vs. 2024 stats)
scm2324 <- cor(sds2324[, cols_2023], 
              sds2324[, cols_2024], 
              use = "complete.obs")

#
#
# Identifying Top 10 Predictors for Each Stat in Seasons 2021-2024
s_cor_matrices <- list(
  "2021" = scm2021,  
  "2022" = scm2122,  
  "2023" = scm2223,
  "2024" = scm2324 
)

# Manually define the base stats
base_stat_list <- c("Age", "G", "AB", "PA", "H", "X1B", "X2B", "X3B", "HR", "R", 
                    "RBI", "BB", "IBB", "SO", "HBP", "SF", "SH", "GDP", "SB", 
                    "CS", "AVG", "GB", "FB", "LD", "IFFB", "Pitches", "Strikes", 
                    "Balls", "IFH", "BU", "BUH", "BB_pct", "K_pct", "BB_K", 
                    "OBP", "SLG", "OPS", "ISO", "BABIP", "GB_FB", "LD_pct", 
                    "GB_pct", "FB_pct", "IFFB_pct", "HR_FB", "IFH_pct", "BUH_pct",
                    "TTO_pct", "wOBA", "wRAA", "wRC", "WAR", "BaseRunning",
                    "Spd", "wRC_plus", "wBsR", "WPA", "Clutch", "O.Swing_pct",
                    "Z.Swing_pct", "Swing_pct", "O.Contact_pct", "Z.Contact_pct",
                    "Contact_pct", "Zone_pct", "F.Strike_pct", "SwStr_pct",
                    "CStr_pct", "C.SwStr_pct", "BB_pct_plus", "K_pct_plus", 
                    "OBP_plus", "SLG_plus", "ISO_plus", "BABIP_plus", "LD_pct_plus",
                    "GB_pct_plus", "FB_pct_plus", "HRFB_pct_plus", "xwOBA", 
                    "xAVG", "xSLG", "XBR", "EV", "LA", "Barrels", "Barrel_pct",
                    "maxEV", "HardHit", "HardHit_pct") 

## Function to clean column names by removing year suffix (handles multiple underscores)
clean_column_name <- function(col_name) {
  # Remove the last part if it matches a year suffix (_####)
  sub("_(\\d{4})$", "", col_name)
}

# Function to extract top predictors and their correlation values for each base stat in a given season
s_get_top_predictors_with_values <- function(cor_matrix, base_stat, top_n = 10) {
  predictors_with_values <- list()
  
  # Clean column names to remove year suffix
  clean_colnames <- sapply(colnames(cor_matrix), clean_column_name)
  
  for (col in colnames(cor_matrix)) {
    # Exclude the column itself to avoid self-correlation
    correlations <- cor_matrix[, col]
    correlations <- correlations[!names(correlations) %in% col]
    
    # Match the cleaned column name with the base stat
    if (clean_column_name(col) == base_stat) {
      # Sort the correlations for this predictor in descending order
      sorted_correlations <- sort(abs(correlations), decreasing = TRUE)[1:top_n]
      
      # Modify predictor names to clean them
      predictors <- sapply(names(sorted_correlations), clean_column_name)
      
      # Store the top N predictors and their corresponding correlation values
      predictors_with_values[[col]] <- data.frame(
        Predictor = predictors,
        Correlation = sorted_correlations
      )
    }
  }
  
  return(predictors_with_values)
}

# Main loop for extracting top predictors
s_all_predictors_values <- list()

for (base_stat in base_stat_list) {
  # Validate if the base stat exists in any cleaned column names
  clean_colnames <- sapply(colnames(s_cor_matrices$`2021`), clean_column_name)  # Assumes consistent column names
  if (base_stat %in% clean_colnames) {
    s_all_predictors_values[[base_stat]] <- list()  # Initialize for each base stat
    
    # Process each season
    for (season in names(s_cor_matrices)) {
      predictors_values <- s_get_top_predictors_with_values(s_cor_matrices[[season]], base_stat)
      
      # Add a Year column to each tibble for ordering
      if (length(predictors_values) > 0) {
        predictors_values <- lapply(predictors_values, function(df) {
          df$Year <- season
          return(df)
        })
        
        s_all_predictors_values[[base_stat]] <- c(s_all_predictors_values[[base_stat]], predictors_values)
      }
    }
    
    # Combine and filter results
    combined_data <- do.call(rbind, s_all_predictors_values[[base_stat]])
    
    # Ensure ordering by Year
    combined_data <- combined_data |>
      group_by(Predictor, Year) |>
      arrange(Year, desc(Correlation)) |>
      slice_head(n = 5) |>
      ungroup()
    
    # Save the filtered results
    s_all_predictors_values[[base_stat]] <- combined_data
  }
}

#
# Extract Top 10 overall predictors

# Combine all predictors across base stats into a single data frame
s_all_predictors_combined <- bind_rows(s_all_predictors_values, .id = "Base_Stat")

# Create a function to calculate top 10 predictors for each stat
get_top_10_per_stat <- function(data) {
  data |>
    group_by(Predictor) |>                      # Group by each predictor
    summarize(
      Frequency = n(),                           # Count how many times the predictor appears
      Avg_Correlation = mean(Correlation)        # Calculate the average correlation value
    ) |>
    arrange(desc(Avg_Correlation), desc(Frequency)) |>  # Rank by frequency, then avg correlation
    slice_head(n = 10)                            # Take the top 10 predictors
}

# Apply the function to each stat
s_top_predictors_by_stat <- s_all_predictors_combined |>
  group_by(Base_Stat) |>                        # Group by each stat
  group_modify(~ get_top_10_per_stat(.x)) |>     # Apply the function to each group
  ungroup()


#
#
#

#
#
#

#Pitcher Stats
pitcherstats <- pitcherstatsmain |>
  select(3, 1, 60, 4:28, 30:58, 61:66) |>
  filter(PA >= 30) |>
  rename(Season = year) |>
  unique() 


#2019-20

p1920 <- pitcherstats |>
  filter(Season == '2019' | Season == "2020") |>
  select(name, Season, 3:63) |>
  pivot_wider(names_from = Season, values_from = 3:63)


#2020-21 

p2021 <- pitcherstats |>
  filter(Season == '2020' | Season == "2021") |>
  select(name, Season, 3:63) |>
  pivot_wider(names_from = Season, values_from = 3:63)


#2021-22

p2122 <- pitcherstats |>
  filter(Season == '2021' | Season == "2022") |>
  select(name, Season, 3:63) |> 
  pivot_wider(names_from = Season, values_from = 3:63)


#2022-23

p2223 <- pitcherstats |>
  filter(Season == '2022' | Season == "2023") |>
  select(name, Season, 3:63) |> 
  pivot_wider(names_from = Season, values_from = 3:63)


#2023-24

p2324 <- pitcherstats |>
  filter(Season == '2023' | Season == "2024") |>
  select(name, Season, 3:63) |> 
  pivot_wider(names_from = Season, values_from = 3:63)


# Subset data to remove non-numeric columns
#named ds for data subset, followed by corresponding tag

#2019-20
pds1920 <- p1920 |>
  select(6:123) |>
  na.omit()


#2020-21 
pds2021 <- p2021 |>
  select(6:123) |>
  na.omit()


#2021-22
pds2122 <- p2122 |>
  select(6:123) |>
  na.omit()


#2022-23
pds2223 <- p2223 |>
  select(6:123) |>
  na.omit()


#2023-24 dfs
pds2324 <- p2324 |>
  select(6:123) |>
  na.omit()

# Identify yearly columns separately
pcols_2019 <- grep("_2019$", names(pds1920), value = TRUE)
pcols_2020 <- grep("_2020$", names(pds1920), value = TRUE)
pcols_2021 <- grep("_2021$", names(pds2122), value = TRUE)
pcols_2022 <- grep("_2022$", names(pds2122), value = TRUE)
pcols_2023 <- grep("_2023$", names(pds2324), value = TRUE)
pcols_2024 <- grep("_2024$", names(pds2324), value = TRUE)

#
#


#Standardizing data
pdfs <- list(pds1920, pds2021, pds2122, pds2223, pds2324)

# Function to standardize a dataframe
standardize_df <- function(df) {
  # Apply scale function to all numeric columns in the dataframe
  df_standardized <- df
  numeric_cols <- sapply(df, is.numeric)  # Identify numeric columns
  df_standardized[numeric_cols] <- scale(df[numeric_cols])
  
  return(df_standardized)
}

# Apply the standardization function to each dataframe
spds1920 <- standardize_df(pds1920)
spds2021 <- standardize_df(pds2021)
spds2122 <- standardize_df(pds2122)
spds2223 <- standardize_df(pds2223)
spds2324 <- standardize_df(pds2324)


# Compute correlation matrix (2019 stats vs. 2020 stats)
pcm1920 <- cor(spds1920[, pcols_2019], 
               spds1920[, pcols_2020], 
               use = "complete.obs")


# Compute correlation matrix (2020 stats vs. 2021 stats)
pcm2021 <- cor(pds2021[, pcols_2020], 
               pds2021[, pcols_2021], 
               use = "complete.obs")

#
# Compute correlation matrix (2021 stats vs. 2022 stats)
pcm2122 <- cor(pds2122[, pcols_2021], 
               pds2122[, pcols_2022], 
               use = "complete.obs")


# Compute correlation matrix (2022 stats vs. 2023 stats)
pcm2223 <- cor(pds2223[, pcols_2022], 
               pds2223[, pcols_2023], 
               use = "complete.obs")


# Compute correlation matrix (2023 stats vs. 2024 stats)
pcm2324 <- cor(pds2324[, pcols_2023], 
               pds2324[, pcols_2024], 
               use = "complete.obs")


#
#
# Identifying Top 10 Predictors for Each Stat in Seasons 2021-2024
p_cor_matrices <- list(
  "2021" = pcm2021,  
  "2022" = pcm2122,  
  "2023" = pcm2223,
  "2024" = pcm2324 
)

# Manually define the base stats
pbase_stat_list <- c("G", "formatted_ip", "PA", "AB", "H", "X1B", "X2B", "X3B", 
                     "HR", "SO", "BB", "K_pct", "BB_pct", "AVG", "SLG", "OBP",
                     "OPS", "ISO", "BABIP", "SV", "BSV", "ERA", "SB", "QS", 
                     "xBA", "xSLG", "wOBA", "xwOBA", "xOBP", "xISO", "EV", "LA",
                     "SS_pct", "Barrel_pct", "HardHit_pct", "avg_best_speed",
                     "avg_hyper_speed", "z_swing_pct", "z_swing_miss_pct", 
                     "oz_swing_pct", "oz_swing_miss_pct", "oz_contact_pct",
                     "iz_contact_pct", "whiff_pct", "swing_pct", "f_strike_pct",
                     "GB_pct", "FB_pct", "LD_pct", "PU_pct", "xba_minus_ba_diff",
                     "xslg_minus_slg_diff", "xwoba_minus_woba_diff", "LOB_pct",
                     "HR_FB", "FIP", "xFIP", "SIERA", "WAR") 

## Function to clean column names by removing year suffix (handles multiple underscores)
clean_column_name <- function(col_name) {
  # Remove the last part if it matches a year suffix (_####)
  sub("_(\\d{4})$", "", col_name)
}

# Function to extract top predictors and their correlation values for each base stat in a given season
p_get_top_predictors_with_values <- function(cor_matrix, base_stat, top_n = 10) {
  predictors_with_values <- list()
  
  # Clean column names to remove year suffix
  clean_colnames <- sapply(colnames(cor_matrix), clean_column_name)
  
  for (col in colnames(cor_matrix)) {
    # Exclude the column itself to avoid self-correlation
    correlations <- cor_matrix[, col]
    correlations <- correlations[!names(correlations) %in% col]
    
    # Match the cleaned column name with the base stat
    if (clean_column_name(col) == base_stat) {
      # Sort the correlations for this predictor in descending order
      sorted_correlations <- sort(abs(correlations), decreasing = TRUE)[1:top_n]
      
      # Modify predictor names to clean them
      predictors <- sapply(names(sorted_correlations), clean_column_name)
      
      # Store the top N predictors and their corresponding correlation values
      predictors_with_values[[col]] <- data.frame(
        Predictor = predictors,
        Correlation = sorted_correlations
      )
    }
  }
  
  return(predictors_with_values)
}

# Main loop for extracting top predictors
p_all_predictors_values <- list()

for (base_stat in base_stat_list) {
  # Validate if the base stat exists in any cleaned column names
  clean_colnames <- sapply(colnames(p_cor_matrices$`2021`), clean_column_name)  # Assumes consistent column names
  if (base_stat %in% clean_colnames) {
    s_all_predictors_values[[base_stat]] <- list()  # Initialize for each base stat
    
    # Process each season
    for (season in names(p_cor_matrices)) {
      predictors_values <- p_get_top_predictors_with_values(p_cor_matrices[[season]], base_stat)
      
      # Add a Year column to each tibble for ordering
      if (length(predictors_values) > 0) {
        predictors_values <- lapply(predictors_values, function(df) {
          df$Year <- season
          return(df)
        })
        
        p_all_predictors_values[[base_stat]] <- c(p_all_predictors_values[[base_stat]], predictors_values)
      }
    }
    
    # Combine and filter results
    combined_data <- do.call(rbind, p_all_predictors_values[[base_stat]])
    
    # Ensure ordering by Year
    combined_data <- combined_data |>
      group_by(Predictor, Year) |>
      arrange(Year, desc(Correlation)) |>
      slice_head(n = 5) |>
      ungroup()
    
    # Save the filtered results
    p_all_predictors_values[[base_stat]] <- combined_data
  }
}

#
# Extract Top 10 overall predictors

# Combine all predictors across base stats into a single data frame
p_all_predictors_combined <- bind_rows(p_all_predictors_values, .id = "Base_Stat")

# Create a function to calculate top 10 predictors for each stat
get_top_10_per_stat <- function(data) {
  data |>
    group_by(Predictor) |>                      # Group by each predictor
    summarize(
      Frequency = n(),                           # Count how many times the predictor appears
      Avg_Correlation = mean(Correlation)        # Calculate the average correlation value
    ) |>
    arrange(desc(Avg_Correlation), desc(Frequency)) |>  # Rank by frequency, then avg correlation
    slice_head(n = 10)                            # Take the top 10 predictors
}

# Apply the function to each stat
p_top_predictors_by_stat <- p_all_predictors_combined |>
  group_by(Base_Stat) |>                        # Group by each stat
  group_modify(~ get_top_10_per_stat(.x)) |>     # Apply the function to each group
  ungroup()


