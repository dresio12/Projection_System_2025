library(tidyverse)

#projection comparisons

#load in source projections from GitHub
projections <- read.csv("pitcher_source_projections.csv", stringsAsFactors = FALSE)

#load in REZ projections from GitHub
REZ_SV <- read.csv("pred_SV.csv", stringsAsFactors = FALSE, check.names = FALSE)


#create Source column for REZ dfs
REZ_SV <- REZ_SV |>
  mutate(Source = "REZ1") |>
  select(PlayerName, playerid, predicted_SV, Source) |>
  rename(SV = predicted_SV)


#remove special characters
projections <- projections |>
  select(playerid, name, SV, Source) |>
  mutate(name = gsub("á", "a", name),
         name = gsub("é", "e", name),
         name = gsub("í", "i", name),
         name = gsub("ó", "o", name),
         name = gsub("ú", "u", name),
         name = gsub("ñ", "n", name),
         name = gsub("-", "", name),
         name = gsub("\\.", "", name))

REZ_SV <- REZ_SV |>
  rename(name = PlayerName) 

REZ_SV$playerid <- as.character(REZ_SV$playerid)

#comparisons df
comparisons <- bind_rows(REZ_SV, projections) |>
  arrange(name, desc(SV), Source)

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
  mutate(diff = SV_REZ - SV_Other)

# Step 3: Calculate mean differences
mean_diffs <- comparison_df %>%
  group_by(Source_REZ, Source_Other) %>%
  summarise(mean_diff = mean(diff), .groups = 'drop') %>%
  mutate(comparison = paste(Source_REZ, "vs", Source_Other)) %>%
  select(comparison, mean_diff)

rez1 <- comparison_df |> filter(Source_REZ == "REZ1")

#averaging all sources together to create final SV prediction
averages <- bind_rows(REZ_SV, projections) |>
  arrange(name, desc(SV), Source)

newSV <- averages %>%
  group_by(name, playerid) %>%
  mutate(
    num_sources = n(),  # Count number of sources
    weight = case_when(
      num_sources > 3 & Source != "REZ1" ~ 2.5,  # More than 4 sources → REZ1 gets 1.5, others 1
      num_sources <= 3 & Source != "REZ1" ~ 2.5, # 4 or fewer sources → Non-REZ1 gets 1.5, REZ1 stays 1.0
      TRUE ~ 1
    )
  ) %>%
  summarize(
    new_SV = weighted.mean(SV, weight),  # Always use weighted mean
    .groups = "drop"
  )

newSV <- left_join(newSV, REZ_SV, by = c("name", "playerid"))

newSV <- newSV |>
  mutate(
    SV_adj_rate = new_SV / SV,
    SV_adj_rate = ifelse(SV_adj_rate == 0, 1, SV_adj_rate),  # Prevent division by zero
    new_SV = round(ifelse(new_SV < 1, 0, new_SV))
  )

write.csv(newSV, "newSV.csv" , row.names = FALSE)
