#load in my predictions
pred_AVG <- read.csv("AVG_preds.csv")
pred_OBP <- read.csv("OBP_preds.csv")
pred_SLG <- read.csv("SLG_preds.csv")
pred_OPS <- read.csv("OPS_preds.csv")
pred_wOBA <- read.csv("wOBA_preds.csv")
pred_BBpct <- read.csv("BB_pct_preds.csv")
pred_Kpct <- read.csv("K_pct_preds.csv")
pred_ISO <- read.csv("ISO_preds.csv")
pred_BABIP <- read.csv("BABIP_preds.csv")

#load in distributions
bats_dist <- read.csv("bats_dist.csv") |>
  filter(!is.na(dist1b))

#load in source projections from GitHub
projections <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/source_projections.csv", stringsAsFactors = FALSE)

#remove special characters
proj2 <- projections |>
  mutate(name = gsub("á", "a", name),
         name = gsub("é", "e", name),
         name = gsub("í", "i", name),
         name = gsub("ó", "o", name),
         name = gsub("Ó", "O", name),
         name = gsub("ú", "u", name),
         name = gsub("ñ", "n", name),
         name = gsub("-", "", name),
         name = gsub("\\.", "", name))

projections <- projections |>
  select(name, G, PA, AB, R, RBI, BB, HBP, SO, SB, CS, SF, SH, 
         wRC_plus, BaseRunning, WAR, Source) |>
  mutate(name = gsub("á", "a", name),
         name = gsub("é", "e", name),
         name = gsub("í", "i", name),
         name = gsub("ó", "o", name),
         name = gsub("Ó", "O", name),
         name = gsub("ú", "u", name),
         name = gsub("ñ", "n", name),
         name = gsub("-", "", name),
         name = gsub("\\.", "", name))


projections <- projections |>
  group_by(name) |>
  summarize(
    G = mean(G),
    PA = mean(PA),
    AB = mean(AB),
    R = mean(R),
    RBI = mean(RBI),
    BB = mean(BB),
    HBP = mean(HBP),
    SO = mean(SO),
    SB = mean(SB),
    CS = mean(CS),
    SF = mean(SF),
    SH = mean(SH),
    BaseRunning = mean(BaseRunning),
    wRC_plus = mean(wRC_plus),
    WAR = mean(WAR),
  ) |>
  ungroup()

df_list <- list(pred_AVG, pred_OBP, pred_SLG, pred_OPS, pred_wOBA, pred_BBpct,
                pred_Kpct, pred_ISO, pred_BABIP)

rez_df <- Reduce(function(x, y) merge(x, y), df_list)

rez_df <- left_join(rez_df, bats_dist)

rez_df <- left_join(rez_df, projections) |>
  filter(!is.na(dist1b) & G >= 1 & !is.na(predicted_AVG))
  
rez_df <- rez_df |>
  mutate(
    H = predicted_AVG * AB,
    X1B = H * dist1b,
    X2B = H * dist2b,
    X3B = H * dist3b,
    HR = H * distHR,
    IBB = BB * distIBB,
    )

pfs <- read.csv("pfs.csv")  

rez_df <- left_join(rez_df, pfs)

rez_df <- rez_df |>
  mutate(
    H = ifelse(!is.na(Hpf), H * Hpf / 100, H),
    X1B = ifelse(!is.na(X1Bpf), X1B * X1Bpf / 100, X1B),
    X2B = ifelse(!is.na(X2Bpf), X2B * X2Bpf / 100, X2B),
    X3B = ifelse(!is.na(X3Bpf), X3B * X3Bpf / 100, X3B),
    HR = ifelse(!is.na(HRpf), HR * HRpf / 100, HR),
    R = ifelse(!is.na(Rpf), R * Rpf / 100, R),
    BB = ifelse(!is.na(BBpf), BB * BBpf / 100, BB),
    SO = ifelse(!is.na(Sopf), SO * Sopf / 100, SO)
  )

rez_df <- rez_df |>
  mutate(
    AVG = round(H/AB, 3),
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
    SLG = round((X1B + (2*X2B) + (3*X3B) + (4*HR))/AB, 3),
    OPS = round(OBP + SLG, 3), 
    #woba coefficients 4-year avg of prev coefficients
    wOBA = round(((0.692 * (BB-IBB)) + (0.722 * HBP) + (0.882 * X1B) + (1.250 * X2B) + (1.582 * X3B) + (2.033 * HR)) / 
                   (AB + BB - IBB + SF + HBP), 3),
    BB_pct = round((BB / (AB + BB + HBP + SF + SH)) * 100, 1),
    K_pct = round((SO / (AB + BB + HBP + SF + SH)) * 100, 1),
    BB_K = round(BB/SO, 1),
    ISO = round(SLG-AVG, 3),
    BABIP = round((H - HR) / (AB - SO - HR + SF), 3)
  )

proj3 <- proj2 |>
  select(-Spd, -wRAA, -wRC, -season, -team_name)

proj2 <- proj2 |>
  select(-Source, -Spd, -wRAA, -wRC,  -season, -team_name) |>
  group_by(playerid, name, team) |>
  summarize(across(everything(), mean, na.rm = TRUE), .groups = "drop")

rez_df <- rez_df |>
  select(2, 1, 5, 20:40, 52:61)

rez_df$playerid <- as.character(rez_df$playerid)

# Filter rows in proj2 where the name is not already in rez_df
proj2_filtered <- proj2 %>%
  anti_join(rez_df, by = "playerid")

# Combine the filtered proj2 with rez_df
combined_df <- bind_rows(proj2_filtered, rez_df)

combined_df <- combined_df |>
  mutate(Source = "REZ")

final_hitters <- bind_rows(combined_df, proj3)

final_hitters <- final_hitters |>
  rename(Fangraphs_id = playerid,
         `1B`= X1B,
         `2B` = X2B,
         `3B` = X3B,
         `BB%` = BB_pct,
         `K%` = K_pct,
         `BB/K` = BB_K,
         `wRC+` = wRC_plus,
         Name = name,
         Team = team) |>
  select(Source, 1:34) |>
  arrange(desc(WAR), if_else(Source == "REZ", "", Source)) |>
  group_by(Fangraphs_id) |>
  filter(n() >= 3) |>
  ungroup() |>
  mutate(
    across(5:22, round, 0),   
    across(23:27, round, 3),  
    across(28:29, round, 2),  
    across(30:32, round, 3),  
    across(33, round, 1),     
    across(34, round, 0),    
    across(35, round, 1)
  )

test <- final_hitters |>
  group_by(Name) |>
  summarise(n = n()) |>
  filter(n >=4) |>
  ungroup()

final_hitters <- final_hitters |>
  filter(Name %in% test$Name)

write.csv(final_hitters, "final_hitter_projections.csv", row.names = FALSE)
