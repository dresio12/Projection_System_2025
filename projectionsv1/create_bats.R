library(jsonlite)
library(readxl)

#standard  
standard_api <- "https://www.fangraphs.com/api/leaders/major-league/data?pos=all&stats=bat&lg=all&type=0&month=0&ind=1&pageitems=2000000000&startdate=&enddate=&team=0%2Cto&qual=0&season1=2018&season=2024"
data <- jsonlite::fromJSON(standard_api)$data |>
  as_tibble() |>
  select(Season,
         playerid = playerid,
         team = TeamName,
         name = PlayerName,
         Bats,
         Age,
         G,
         AB,
         PA,
         H,
         X1B = `1B`,
         X2B = `2B`,
         X3B = `3B`,
         HR,
         R,
         RBI,
         BB,
         IBB,
         SO,
         HBP,
         SF,
         SH,
         IFH,
         BU,
         BUH,
         SB,
         CS,
         AVG,
         OBP,
         SLG,
         OPS,
         wOBA, 
         BB_pct = `BB%`,
         K_pct = `K%`,
         BB_K = `BB/K`,
         ISO,
         BABIP,
         Spd,
         BaseRunning,
         wRAA,
         wRC,
         wRC_plus = `wRC+`,
         Dollars,
         WAR,
         RE24,
         Clutch,
         o_swing_pct = `O-Swing%`,
         z_swing_pct = `Z-Swing%`,
         swing_pct = `Swing%`,
         o_contact_pct = `O-Contact%`,
         z_contact_pct = `Z-Contact%`,
         contact_pct = `Contact%`,
         zone_pct = `Zone%`, 
         f_strike_pct = `F-Strike%`,
         SwStr_pct = `SwStr%`,
         CStr_pct = `CStr%`,
         pull_pct = `Pull%`,
         cent_pct = `Cent%`,
         oppo_pct = `Oppo%`,
         soft_pct = `Soft%`,
         med_pct = `Med%`,
         hard_pct = `Hard%`,
         avg_ev = `EV`,
         avg_la = `LA`,
         barrels = `Barrels`,
         barrel_pct = `Barrel%`,
         maxEV,
         HardHit_pct = `HardHit%`
  ) 

write.csv(data, "all_the_stats.csv", row.names = FALSE)
#
#
#

#standard  
nonsplit_api <- "https://www.fangraphs.com/api/leaders/major-league/data?pos=all&stats=bat&lg=all&type=2&month=0&ind=1&team=0&pageitems=2000000000&startdate=&enddate=&season1=2018&season=2024&qual=0"
data_nosplit <- jsonlite::fromJSON(nonsplit_api)$data |>
  as_tibble() |>
  select(Season,
         playerid = playerid,
         name = PlayerName,
         Bats,
         Age,
         Spd,
         BaseRunning,
         wRAA,
         wRC,
         wRC_plus = `wRC+`,
         Dollars,
         WAR,
         RE24,
         Clutch,
         o_swing_pct = `O-Swing%`,
         z_swing_pct = `Z-Swing%`,
         swing_pct = `Swing%`,
         o_contact_pct = `O-Contact%`,
         z_contact_pct = `Z-Contact%`,
         contact_pct = `Contact%`,
         zone_pct = `Zone%`, 
         f_strike_pct = `F-Strike%`,
         SwStr_pct = `SwStr%`,
         CStr_pct = `CStr%`,
         pull_pct = `Pull%`,
         cent_pct = `Cent%`,
         oppo_pct = `Oppo%`,
         soft_pct = `Soft%`,
         med_pct = `Med%`,
         hard_pct = `Hard%`,
         avg_ev = `EV`,
         avg_la = `LA`,
         barrels = `Barrels`,
         barrel_pct = `Barrel%`,
         maxEV,
         HardHit_pct = `HardHit%`
  ) 



lhb_pf <- read_excel("lhb_pf.xlsx")
rhb_pf <- read_excel("rhb_pf.xlsx")

pfs_combined <- full_join(lhb_pf, rhb_pf, 
                          by = c("Season", "team"), 
                          suffix = c("_LHB", "_RHB"))

# Apply weighted park factor calculation
pfs_combined <- pfs_combined %>%
  mutate(across(ends_with("pf_LHB"), 
                ~ (.x * 0.67) + (get(sub("LHB", "RHB", cur_column())) * 0.33),
                .names = "{sub('_LHB', '', .col)}"))

pfs_combined <- pfs_combined |>
  select(1:2, 27:36) |>
  mutate(Bats = "B")

pfs <- bind_rows(lhb_pf, rhb_pf, pfs_combined)
#adjust stats
adj_data <- left_join(data, pfs)

adj_data <- adj_data |>
  mutate(
    R = R * (100 / Rpf),
    OBP = OBP * (100 / OBPpf),
    H = H * (100 / Hpf),
    X1B = X1B * (100 / X1Bpf),
    X2B = X2B * (100 / X2Bpf),
    X3B = X3B * (100 / X3Bpf),
    HR = HR * (100 / HRpf),
    BB = BB * (100 / BBpf),
    SO = SO * (100 / Sopf),
    HardHit_pct = HardHit_pct * (100 / HardHit_pctpf)
      
  )

#cleanup
adj_data <- adj_data |>
  filter(PA >= 1)

#combine multiple teams for single season into 1
adj_data <- adj_data |>
  select(-team) |>
  group_by(Season, playerid, name, Bats, Age) |>
  summarize(
    G = sum(G, na.rm = TRUE),
    AB = sum(AB, na.rm = TRUE),
    PA = sum(PA, na.rm = TRUE),
    H = sum(H, na.rm = TRUE),
    X1B = sum(X1B, na.rm = TRUE),
    X2B = sum(X2B, na.rm = TRUE),
    X3B = sum(X3B, na.rm = TRUE),
    HR = sum(HR, na.rm = TRUE),
    R = sum(R, na.rm = TRUE),
    RBI = sum(RBI, na.rm = TRUE),
    BB = sum(BB, na.rm = TRUE),
    IBB = sum(IBB, na.rm = TRUE),
    SO = sum(SO, na.rm = TRUE),
    HBP = sum(HBP, na.rm = TRUE),
    SF = sum(SF, na.rm = TRUE),
    SH = sum(SH, na.rm = TRUE),
    IFH = sum(IFH, na.rm = TRUE),
    BU = sum(BU, na.rm = TRUE),
    BUH = sum(BUH, na.rm = TRUE),
    SB = sum(SB, na.rm = TRUE),
    CS = sum(CS, na.rm = TRUE)
  )

   
adj_data <- adj_data |> 
  mutate(
    AVG = H/AB,
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
    SLG = (X1B + (2*X2B) + (3*X3B) + (4*HR))/AB,
    OPS = OBP + SLG,
    wOBA = case_when(
      Season == 2018 ~ ((0.690 * (BB-IBB)) + (0.719 * HBP) + (0.880 * X1B) + (1.247 * X2B) + (1.578 * X3B) + (2.031 * HR)) / 
        (AB + BB - IBB + SF + HBP),
      Season == 2019 ~ ((0.690 * (BB-IBB)) + (0.720 * HBP) + (0.870 * X1B) + (1.217 * X2B) + (1.529 * X3B) + (1.940 * HR)) / 
        (AB + BB - IBB + SF + HBP),
      Season == 2020 ~ ((0.699 * (BB-IBB)) + (0.728 * HBP) + (0.883 * X1B) + (1.238 * X2B) + (1.558 * X3B) + (1.979 * HR)) / 
        (AB + BB - IBB + SF + HBP),
      Season == 2021 ~ ((0.692 * (BB-IBB)) + (0.722 * HBP) + (0.879 * X1B) + (1.242 * X2B) + (1.568 * X3B) + (2.007 * HR)) / 
        (AB + BB - IBB + SF + HBP),
      Season == 2022 ~ ((0.689 * (BB-IBB)) + (0.720 * HBP) + (0.884 * X1B) + (1.261 * X2B) + (1.601 * X3B) + (2.072 * HR)) / 
        (AB + BB - IBB + SF + HBP),
      Season == 2023 ~ ((0.696 * (BB-IBB)) + (0.726 * HBP) + (0.883 * X1B) + (1.244 * X2B) + (1.569 * X3B) + (2.004 * HR)) / 
        (AB + BB - IBB + SF + HBP),
      Season == 2024 ~ ((0.689 * (BB-IBB)) + (0.720 * HBP) + (0.882 * X1B) + (1.254 * X2B) + (1.590 * X3B) + (2.050 * HR)) / 
        (AB + BB - IBB + SF + HBP),
    ),
    BB_pct = (BB / (AB + BB + HBP + SF + SH)) * 100,
    K_pct = (SO / (AB + BB + HBP + SF + SH)) * 100,
    BB_K = BB/SO,
    ISO = SLG-AVG,
    BABIP = (H - HR) / (AB - SO - HR + SF)
    )

adj_data <- adj_data |>
  unique() |>
  arrange(desc(Season), name)

adj_data <- left_join(adj_data, data_nosplit)

write.csv(adj_data, "bats.csv", row.names = FALSE)


bats <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/projectionsv2/bats.csv", stringsAsFactors = FALSE)

bats <- bats |>
  pivot_wider(names_from = Season, values_from = 5:67)

bats2 <- bats |>
  group_by(playerid) |>  # Group by player name
  summarize(across(everything(), ~ first(na.omit(.))), .groups = "drop")

# Extract and reorder columns by year in descending order
bats2 <- bats2[, 
                     order(
                       sapply(colnames(bats2), function(x) {
                         year <- stringr::str_extract(x, "\\d{4}")
                         as.numeric(year)  # Convert extracted year to numeric for sorting
                       }),
                       decreasing = TRUE,
                       na.last = TRUE
                     )
]

bats2 <- bats2 |>
  select(443,442,444, 1:441)

write.csv(bats2, "bats2.csv", row.names = FALSE)

#
#
#minor legaue data
#
#

ml_data_split <- read.csv("ml_data_split.csv")
ml_nosplit <- read.csv("ml_data_no_split.csv")

baseballr::

lhb_pf <- read_excel("lhb_pf.xlsx")
rhb_pf <- read_excel("rhb_pf.xlsx")

pfs_combined <- full_join(lhb_pf, rhb_pf, 
                          by = c("Season", "team"), 
                          suffix = c("_LHB", "_RHB"))

# Apply weighted park factor calculation
pfs_combined <- pfs_combined %>%
  mutate(across(ends_with("pf_LHB"), 
                ~ (.x * 0.67) + (get(sub("LHB", "RHB", cur_column())) * 0.33),
                .names = "{sub('_LHB', '', .col)}"))

pfs_combined <- pfs_combined |>
  select(1:2, 27:36) |>
  mutate(Bats = "B")

pfs <- bind_rows(lhb_pf, rhb_pf, pfs_combined)
#adjust stats
adj_data <- left_join(ml_data, pfs)

adj_data <- adj_data |>
  mutate(
    R = R * (100 / Rpf),
    OBP = OBP * (100 / OBPpf),
    H = H * (100 / Hpf),
    X1B = X1B * (100 / X1Bpf),
    X2B = X2B * (100 / X2Bpf),
    X3B = X3B * (100 / X3Bpf),
    HR = HR * (100 / HRpf),
    BB = BB * (100 / BBpf),
    SO = SO * (100 / Sopf),
    HardHit_pct = HardHit_pct * (100 / HardHit_pctpf)
    
  )

#cleanup
adj_data <- adj_data |>
  filter(PA >= 1)

#combine multiple teams for single season into 1
adj_data <- adj_data |>
  select(-team) |>
  group_by(Season, playerid, name, Bats, Age) |>
  summarize(
    G = sum(G, na.rm = TRUE),
    AB = sum(AB, na.rm = TRUE),
    PA = sum(PA, na.rm = TRUE),
    H = sum(H, na.rm = TRUE),
    X1B = sum(X1B, na.rm = TRUE),
    X2B = sum(X2B, na.rm = TRUE),
    X3B = sum(X3B, na.rm = TRUE),
    HR = sum(HR, na.rm = TRUE),
    R = sum(R, na.rm = TRUE),
    RBI = sum(RBI, na.rm = TRUE),
    BB = sum(BB, na.rm = TRUE),
    IBB = sum(IBB, na.rm = TRUE),
    SO = sum(SO, na.rm = TRUE),
    HBP = sum(HBP, na.rm = TRUE),
    SF = sum(SF, na.rm = TRUE),
    SH = sum(SH, na.rm = TRUE),
    IFH = sum(IFH, na.rm = TRUE),
    BU = sum(BU, na.rm = TRUE),
    BUH = sum(BUH, na.rm = TRUE),
    SB = sum(SB, na.rm = TRUE),
    CS = sum(CS, na.rm = TRUE)
  )


adj_data <- adj_data |> 
  mutate(
    AVG = H/AB,
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
    SLG = (X1B + (2*X2B) + (3*X3B) + (4*HR))/AB,
    OPS = OBP + SLG,
    wOBA = case_when(
      Season == 2018 ~ ((0.690 * (BB-IBB)) + (0.719 * HBP) + (0.880 * X1B) + (1.247 * X2B) + (1.578 * X3B) + (2.031 * HR)) / 
        (AB + BB - IBB + SF + HBP),
      Season == 2019 ~ ((0.690 * (BB-IBB)) + (0.720 * HBP) + (0.870 * X1B) + (1.217 * X2B) + (1.529 * X3B) + (1.940 * HR)) / 
        (AB + BB - IBB + SF + HBP),
      Season == 2020 ~ ((0.699 * (BB-IBB)) + (0.728 * HBP) + (0.883 * X1B) + (1.238 * X2B) + (1.558 * X3B) + (1.979 * HR)) / 
        (AB + BB - IBB + SF + HBP),
      Season == 2021 ~ ((0.692 * (BB-IBB)) + (0.722 * HBP) + (0.879 * X1B) + (1.242 * X2B) + (1.568 * X3B) + (2.007 * HR)) / 
        (AB + BB - IBB + SF + HBP),
      Season == 2022 ~ ((0.689 * (BB-IBB)) + (0.720 * HBP) + (0.884 * X1B) + (1.261 * X2B) + (1.601 * X3B) + (2.072 * HR)) / 
        (AB + BB - IBB + SF + HBP),
      Season == 2023 ~ ((0.696 * (BB-IBB)) + (0.726 * HBP) + (0.883 * X1B) + (1.244 * X2B) + (1.569 * X3B) + (2.004 * HR)) / 
        (AB + BB - IBB + SF + HBP),
      Season == 2024 ~ ((0.689 * (BB-IBB)) + (0.720 * HBP) + (0.882 * X1B) + (1.254 * X2B) + (1.590 * X3B) + (2.050 * HR)) / 
        (AB + BB - IBB + SF + HBP),
    ),
    BB_pct = (BB / (AB + BB + HBP + SF + SH)) * 100,
    K_pct = (SO / (AB + BB + HBP + SF + SH)) * 100,
    BB_K = BB/SO,
    ISO = SLG-AVG,
    BABIP = (H - HR) / (AB - SO - HR + SF)
  )

adj_data <- adj_data |>
  unique() |>
  arrange(desc(Season), name)

adj_data <- left_join(adj_data, data_nosplit)

write.csv(adj_data, "bats.csv", row.names = FALSE)


bats <- read.csv("https://raw.githubusercontent.com/dresio12/Projection_System_2025/main/projectionsv2/bats.csv", stringsAsFactors = FALSE)

bats <- bats |>
  pivot_wider(names_from = Season, values_from = 5:67)

bats2 <- bats |>
  group_by(playerid) |>  # Group by player name
  summarize(across(everything(), ~ first(na.omit(.))), .groups = "drop")

# Extract and reorder columns by year in descending order
bats2 <- bats2[, 
               order(
                 sapply(colnames(bats2), function(x) {
                   year <- stringr::str_extract(x, "\\d{4}")
                   as.numeric(year)  # Convert extracted year to numeric for sorting
                 }),
                 decreasing = TRUE,
                 na.last = TRUE
               )
]

bats2 <- bats2 |>
  select(443,442,444, 1:441)

write.csv(bats2, "bats2.csv", row.names = FALSE)



