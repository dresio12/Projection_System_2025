library(tidyverse)

#projection comparisons

#load in source projections from GitHub
projections <- read.csv("pitcher_source_projections.csv", stringsAsFactors = FALSE, check.names = FALSE)

#load in REZ projections from GitHub
`REZ_LOB%` <- read.csv("pred_LOB.csv", stringsAsFactors = FALSE, check.names = FALSE)


#create Source column for REZ dfs
`REZ_LOB%` <- `REZ_LOB%` |>
  mutate(Source = "REZ1") |>
  select(PlayerName, playerid, `predicted_LOB%`, Source) |>
  rename(`LOB%` = `predicted_LOB%`)


#remove special characters
projections <- projections |>
  select(playerid, name, `LOB%`, Source) |>
  mutate(name = gsub("á", "a", name),
         name = gsub("é", "e", name),
         name = gsub("í", "i", name),
         name = gsub("ó", "o", name),
         name = gsub("ú", "u", name),
         name = gsub("ñ", "n", name),
         name = gsub("-", "", name),
         name = gsub("\\.", "", name))

`REZ_LOB%` <- `REZ_LOB%` |>
  rename(name = PlayerName) 

`REZ_LOB%`$playerid <- as.character(`REZ_LOB%`$playerid)

#comparisons df
comparisons <- bind_rows(`REZ_LOB%`, projections) |>
  arrange(name, desc(`LOB%`), Source)

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
  mutate(diff = `LOB%_REZ` - `LOB%_Other`)

# Step 3: Calculate mean differences
mean_diffs <- comparison_df %>%
  group_by(Source_REZ, Source_Other) %>%
  summarise(mean_diff = mean(diff), .groups = 'drop') %>%
  mutate(comparison = paste(Source_REZ, "vs", Source_Other)) %>%
  select(comparison, mean_diff)

rez1 <- comparison_df |> filter(Source_REZ == "REZ1")

#averaging all sources together to create final `LOB%` prediction
averages <- bind_rows(`REZ_LOB%`, projections) |>
  arrange(name, desc(`LOB%`), Source)

`newLOB%` <- averages %>%
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
    `new_LOB%` = weighted.mean(`LOB%`, weight),  # Always use weighted mean
    .groups = "drop"
  )

`newLOB%` <- left_join(`newLOB%`, `REZ_LOB%`, by = c("name", "playerid"))

`newLOB%` <- `newLOB%` |>
  mutate(
    `LOB%_adj_rate` = `new_LOB%` / `LOB%`,
    `LOB%_adj_rate` = ifelse(`LOB%_adj_rate` == 0, 1, `LOB%_adj_rate`)  # Prevent division by zero
  )

write.csv(`newLOB%`, "newLOB.csv" , row.names = FALSE)
