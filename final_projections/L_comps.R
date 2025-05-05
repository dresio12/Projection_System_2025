library(tidyverse)

#projection comparisons

#load in source projections from GitHub
projections <- read.csv("pitcher_source_projections.csv", stringsAsFactors = FALSE)

#load in REZ projections from GitHub
REZ_L <- read.csv("pred_L.csv", stringsAsFactors = FALSE, check.names = FALSE)


#create Source column for REZ dfs
REZ_L <- REZ_L |>
  mutate(Source = "REZ1") |>
  select(PlayerName, playerid, predicted_L, Source) |>
  rename(L = predicted_L)


#remove special characters
projections <- projections |>
  select(playerid, name, L, Source) |>
  mutate(name = gsub("á", "a", name),
         name = gsub("é", "e", name),
         name = gsub("í", "i", name),
         name = gsub("ó", "o", name),
         name = gsub("ú", "u", name),
         name = gsub("ñ", "n", name),
         name = gsub("-", "", name),
         name = gsub("\\.", "", name))

REZ_L <- REZ_L |>
  rename(name = PlayerName) 

REZ_L$playerid <- as.character(REZ_L$playerid)

#comparisons df
comparisons <- bind_rows(REZ_L, projections) |>
  arrange(name, desc(L), Source)

#only compare players i have projections for
comparisons <- comparisons |>
  group_by(name, playerid) |>
  filter(any(grepl("REZ", Source))) |>
  ungroup()


test <- comparisons |>
  group_by(name, playerid) |>
  filter(n() == 8) |>
  ungroup()


# Step 1: Filter for REZ sources and non-REZ sources
rez_projections <- test %>% filter(grepl("REZ", Source))
other_projections <- test %>% filter(!grepl("REZ", Source))

# Step 2: Join REZ and non-REZ projections by player name
comparison_df <- rez_projections %>%
  inner_join(other_projections, by = c("name", "playerid"), suffix = c("_REZ", "_Other")) %>%
  mutate(diff = L_REZ - L_Other)

# Step 3: Calculate mean differences
mean_diffs <- comparison_df %>%
  group_by(Source_REZ, Source_Other) %>%
  summarise(mean_diff = mean(diff), .groups = 'drop') %>%
  mutate(comparison = paste(Source_REZ, "vs", Source_Other)) %>%
  select(comparison, mean_diff)

rez1 <- comparison_df |> filter(Source_REZ == "REZ1")

#averaging all sources together to create final L prediction
averages <- bind_rows(REZ_L, projections) |>
  arrange(name, desc(L), Source)

newL <- averages %>%
  group_by(name, playerid) %>%
  mutate(
    num_sources = n(),  # Count number of sources
    weight = case_when(
      num_sources > 3 & Source == "REZ1" ~ 1.5,  # More than 4 sources → REZ1 gets 1.5, others 1
      num_sources <= 3 & Source != "REZ1" ~ 2.5, # 4 or fewer sources → Non-REZ1 gets 1.5, REZ1 stays 1.0
      TRUE ~ 1
    )
  ) %>%
  summarize(
    new_L = weighted.mean(L, weight),  # Always use weighted mean
    .groups = "drop"
  )

newL <- left_join(newL, REZ_L, by = c("name", "playerid"))

newL <- newL |>
  mutate(
    L_adj_rate = new_L / L,
    L_adj_rate = ifelse(L_adj_rate == 0, 1, L_adj_rate)  # Prevent division by zero
  )

write.csv(newL, "newL.csv" , row.names = FALSE)
