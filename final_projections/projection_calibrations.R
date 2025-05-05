library(readxl)
library(tidyverse)

#load in my predictions
pred_IP <- read.csv("newIP.csv") |>
  select(1:3)
pred_K9 <- read.csv("pred_K9.csv", check.names = FALSE) |>
  select(1:4)
pred_BB9 <- read.csv("pred_BB9.csv", check.names = FALSE) |>
  select(1:4)
pred_HR9 <- read.csv("pred_HR9.csv", check.names = FALSE) |>
  select(1:4)
pred_ERA <- read.csv("pred_ERA.csv", check.names = FALSE) |>
  select(1:4)
pred_AVG <- read.csv("pred_pAVG.csv", check.names = FALSE) |>
  select(1:4)

pred_K9$playerid <- as.character(pred_K9$playerid)
pred_BB9$playerid <- as.character(pred_BB9$playerid)
pred_HR9$playerid <- as.character(pred_HR9$playerid)
pred_ERA$playerid <- as.character(pred_ERA$playerid)
pred_AVG$playerid <- as.character(pred_AVG$playerid)


pitch_proj <- left_join(pred_K9, pred_IP, by = c('playerid', 'PlayerName' = 'name')) |>
  select(1:3, 5, 4)
pitch_proj <- left_join(pitch_proj, pred_BB9)
pitch_proj <- left_join(pitch_proj, pred_HR9) 
pitch_proj <- left_join(pitch_proj, pred_ERA)
pitch_proj <- left_join(pitch_proj, pred_AVG)

pitch_proj <- pitch_proj |>
  mutate(
    K = new_IP * (`predicted_K/9` / 9),
    BB = new_IP * (`predicted_BB/9` / 9),
    HR =  new_IP * (`predicted_HR/9` / 9),
    ER = new_IP * (predicted_ERA / 9)
  )

#park factor adjust
pfs <- read_excel("park_factor_both.xlsx") 
pfs <- pfs |>
  filter(Season >= 2022) |>
  select(1, 3:12) |> 
  group_by(TeamName) |>
  summarise(across(everything(), mean, na.rm = TRUE))

projections <- read.csv("pitcher_source_projections.csv", stringsAsFactors = FALSE) |>
  select(2,4)

pitch_proj <- left_join(pitch_proj, projections) |>
  unique()

pitch_proj <- left_join(pitch_proj, pfs, by = c("team" = "TeamName"))

pitch_proj <- pitch_proj |>
  mutate(
    K = ifelse(!is.na(team), K * (Sopf/100), K),
    BB = ifelse(!is.na(team), BB * (BBpf/100), BB),
    predicted_AVG = ifelse(!is.na(team), predicted_AVG * (Hpf/100), predicted_AVG),
    ER = ifelse(!is.na(team), ER * (Rpf/100), ER),
    HR = ifelse(!is.na(team), HR * (HRpf/100), HR)
  )

#recalculate rate stats
pitch_proj <- pitch_proj |>
  mutate(
    `predicted_K/9` = (K / new_IP) * 9,
    `predicted_BB/9` = (BB / new_IP) * 9,
    `predicted_HR/9` = (HR / new_IP) * 9,
    predicted_ERA = (ER / new_IP) * 9
  ) |>
  select(1:13)

pitch_proj <- pitch_proj |>
  mutate(Source = "REZ1") |>
  select(1,2, 4:14) |>
  rename(IP = new_IP,
         `K/9` = `predicted_K/9`,
         `HR/9` = `predicted_HR/9`,
         `BB/9` = `predicted_BB/9`,
         ERA = predicted_ERA,
         AVG = predicted_AVG)

#average my proj with other proj
projections <- read.csv("pitcher_source_projections.csv", stringsAsFactors = FALSE, check.names = FALSE)

projections <- projections |>
  mutate(name = gsub("á", "a", name),
         name = gsub("é", "e", name),
         name = gsub("í", "i", name),
         name = gsub("ó", "o", name),
         name = gsub("ú", "u", name),
         name = gsub("ñ", "n", name),
         name = gsub("-", "", name),
         name = gsub("\\.", "", name)) |>
  select(playerid, name, IP, `K/9`, `HR/9`, `BB/9`, HR, SO, ERA, AVG, BB, ER, Source) |>
  rename(
    PlayerName = name,
    K = SO
  )


#averaging all sources together to create final FIP prediction
averages <- bind_rows(pitch_proj, projections) |>
  arrange(PlayerName, desc(IP), Source)

newdata <- averages %>%
  group_by(PlayerName, playerid) %>%
  mutate(
    num_sources = n(),  # Count number of sources
    weight = case_when(
      num_sources > 3 & Source == "REZ1" ~ 1.5,  # More than 4 sources → REZ1 gets 1.5, others 1
      num_sources <= 3 & Source != "REZ1" ~ 2.5, # 4 or fewer sources → Non-REZ1 gets 1.5, REZ1 stays 1.0
      TRUE ~ 1
    )
  ) %>%
  summarize(
    `new_K/9` = weighted.mean(`K/9`, weight),  # Always use weighted mean
    `new_HR/9` = weighted.mean(`HR/9`, weight),
    `new_BB/9` = weighted.mean(`BB/9`, weight),
    new_AVG = weighted.mean(AVG, weight),
    new_ERA = weighted.mean(ERA, weight),
    new_SO = weighted.mean(K, weight),
    new_BB = weighted.mean(BB, weight),
    new_HR = weighted.mean(HR, weight),
    new_ER = weighted.mean(ER, weight),
    .groups = "drop"
  )

newdata <- newdata |> 
  rename_with(~ sub("^new_", "", .x))

#average my proj with other proj
projections <- read.csv("pitcher_source_projections.csv", stringsAsFactors = FALSE, check.names = FALSE)

projections <- projections |>
  mutate(name = gsub("á", "a", name),
         name = gsub("é", "e", name),
         name = gsub("í", "i", name),
         name = gsub("ó", "o", name),
         name = gsub("ú", "u", name),
         name = gsub("ñ", "n", name),
         name = gsub("-", "", name),
         name = gsub("\\.", "", name)) |>
  select(2, HLD, H, R, IBB, HBP)

projections <- projections |>
  group_by(playerid) |>
  summarise(
    HLD = mean(HLD),
    H = mean(H), 
    R = mean(R),
    IBB = mean(IBB),
    HBP = mean(HBP)
  ) |>
  ungroup() |>
  mutate(IBB = ifelse(is.na(IBB), 0, IBB))

newdata <- left_join(newdata, projections)

write.csv(newdata, "newprojaverages.csv" , row.names = FALSE)
