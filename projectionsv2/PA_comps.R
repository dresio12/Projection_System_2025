library(tidyverse)

#projection comparisons

#load in source projections from GitHub
projections <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/source_projections.csv", stringsAsFactors = FALSE)

#load in REZ projections from GitHub
REZ_cu <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/clean_unclean.csv", stringsAsFactors = FALSE)
REZ_uuno <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/unclean_unclean_no_mle.csv", stringsAsFactors = FALSE)
REZ_uumle <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/unclean_unclean_mlev2.csv", stringsAsFactors = FALSE)
REZ_cleanlgavg <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/clean_lgavg.csv", stringsAsFactors = FALSE)
REZ_pf <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/projectionsv2/pf_adj_avg.csv", stringsAsFactors = FALSE)


#create Source column for REZ dfs
REZ_cu <- REZ_cu |>
  mutate(Source = "REZ1") |>
  select(name, predicted_avg, Source) |>
  rename(AVG = predicted_avg)

REZ_uumle <- REZ_uumle |>
  mutate(Source = "REZ2") |>
  select(name, predicted_avg, Source) |>
  rename(AVG = predicted_avg)

REZ_uuno <- REZ_uuno |>
  mutate(Source = "REZ3") |>
  select(name, predicted_avg, Source) |>
  rename(AVG = predicted_avg)

REZ_cleanlgavg <- REZ_cleanlgavg |>
  mutate(Source = "REZ4") |>
  select(name, predicted_avg, Source) |>
  rename(AVG = predicted_avg)

REZ_pf <- REZ_pf |>
  mutate(Source = "REZ5") |>
  select(name, predicted_avg, Source) |>
  rename(AVG = predicted_avg)

#remove special characters
projections <- projections |>
  select(name, AVG, Source) |>
  mutate(name = gsub("á", "a", name),
         name = gsub("é", "e", name),
         name = gsub("í", "i", name),
         name = gsub("ó", "o", name),
         name = gsub("ú", "u", name),
         name = gsub("ñ", "n", name),
         name = gsub("-", "", name),
         name = gsub("\\.", "", name))


#comparisons df
comparisons <- bind_rows(REZ_cu, REZ_uumle, REZ_uuno, REZ_cleanlgavg, REZ_pf, projections) |>
  arrange(name, desc(AVG), Source)

#only compare players i have projections for
comparisons <- comparisons |>
  group_by(name) |>
  filter(any(grepl("REZ", Source))) |>
  ungroup()


test <- comparisons |>
  group_by(name) |>
  filter(n() == 13) |>
  ungroup()


# Step 1: Filter for REZ sources and non-REZ sources
rez_projections <- test %>% filter(grepl("REZ", Source))
other_projections <- test %>% filter(!grepl("REZ", Source))

# Step 2: Join REZ and non-REZ projections by player name
comparison_df <- rez_projections %>%
  inner_join(other_projections, by = "name", suffix = c("_REZ", "_Other")) %>%
  mutate(diff = AVG_REZ - AVG_Other)

# Step 3: Calculate mean differences
mean_diffs <- comparison_df %>%
  group_by(Source_REZ, Source_Other) %>%
  summarise(mean_diff = mean(diff), .groups = 'drop') %>%
  mutate(comparison = paste(Source_REZ, "vs", Source_Other)) %>%
  select(comparison, mean_diff)

rez1 <- comparison_df |> filter(Source_REZ == "REZ1")
rez2 <- comparison_df |> filter(Source_REZ == "REZ2")
rez3 <- comparison_df |> filter(Source_REZ == "REZ3")
rez4 <- comparison_df |> filter(Source_REZ == "REZ4")
rez5 <- comparison_df |> filter(Source_REZ == "REZ5")
