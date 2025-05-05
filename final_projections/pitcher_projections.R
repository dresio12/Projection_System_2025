pitch_proj <- read.csv("newprojaverages.csv", check.names = FALSE)


#bring in other proj for players I have
pred_IP <- read.csv("newIP.csv") |>
  select(1:3)
pred_W <- read.csv("newW.csv") |>
  select(1:3)
pred_L <- read.csv("newL.csv")|>
  select(1:3)
pred_G <- read.csv("newpG.csv")|>
  select(1:3)
pred_GS <- read.csv("newpGS.csv")|>
  select(1:3)
pred_SV <- read.csv("newSV.csv")|>
  select(1:3)
pred_BABIP <- read.csv("newpBABIP.csv")|>
  select(1:3)
pred_LOB <- read.csv("newLOB.csv", check.names = FALSE)|>
  select(1:3)
pred_FIP <- read.csv("newFIP.csv")|>
  select(1:3)
pred_WAR <- read.csv("newpWAR.csv")|>
  select(1:3)
pred_TBF <- read.csv("newTBF.csv")|>
  select(1:3)


df_list <- list(pred_IP, pred_W, pred_L, pred_G, pred_GS, pred_SV,
                pred_BABIP, pred_LOB, pred_FIP, pred_WAR, pred_TBF)

preds <- Reduce(function(x, y) merge(x, y), df_list)

preds <- preds |> 
  rename_with(~ sub("^new_", "", .x)) |>
  rename(PlayerName = name)

pitch_proj <- left_join(pitch_proj, preds) 

projections <- read.csv("pitcher_source_projections.csv", stringsAsFactors = FALSE) |>
  select(2,4)

pitch_proj <- left_join(pitch_proj, projections) |>
  unique()

pitch_proj <- pitch_proj |>
  mutate(
    W = round(W),
    L = round(L),
    G = round(G),
    GS = ifelse(GS < .5, 0, GS),
    GS = round(GS),
    IP = round(IP),
    `K/9` = round(`K/9`, 2),
    `BB/9` = round(`BB/9`, 2),
    `HR/9` = round(`HR/9`, 2),
    BABIP = round(BABIP, 3),
    `LOB%` = `LOB%` * 100,
    `LOB%` = round(`LOB%`, 1),
    ERA = round(ERA, 2),
    FIP = round(FIP, 2),
    WAR = round(WAR, 1),
    `K%` = round(SO/TBF*100, 1),
    `BB%` = round(BB/TBF*100, 1),
    `K/BB` = round(`K/9`/`BB/9`, 2),
    SO = round(SO),
    BB = round(BB),
    HR = round(HR),
    ER = round(ER),
    WHIP = round((H + BB)/IP, 2),
    H = round(H),
    R = round(R),
    HBP = round(HBP),
    TBF = round(TBF),
    HLD = round(HLD),
    IBB = round(IBB)
  ) |>
  rename(Name = PlayerName,
         Team = team) |>
  mutate(Source = 'REZ')


projections <- read.csv("pitcher_source_projections.csv", stringsAsFactors = FALSE, check.names = FALSE)

projections <- projections |>
  mutate(name = gsub("á", "a", name),
         name = gsub("Á", "A", name),
         name = gsub("É", "E", name),
         name = gsub("Í", "I", name),
         name = gsub("Ó", "O", name),
         name = gsub("Ú", "U", name),
         name = gsub("é", "e", name),
         name = gsub("í", "i", name),
         name = gsub("ó", "o", name),
         name = gsub("ú", "u", name),
         name = gsub("ñ", "n", name),
         name = gsub("-", "", name),
         name = gsub("\\.", "", name)) |>
  rename(Name = name,
         Team = team) |>
  mutate(
    `BB%` = `BB%` * 100,
    `K%` = `K%` * 100,
    `LOB%` = `LOB%` * 100
  )

pitch_proj <- bind_rows(pitch_proj, projections) |>
  select(-34:-37)

pitch_proj <- pitch_proj |>
  mutate(
    W = round(W),
    L = round(L),
    G = round(G),
    GS = round(GS),
    IP = round(IP),
    `K/9` = round(`K/9`, 2),
    `BB/9` = round(`BB/9`, 2),
    `HR/9` = round(`HR/9`, 2),
    BABIP = round(BABIP, 3),
    `LOB%` = round(`LOB%`, 1),
    ERA = round(ERA, 2),
    FIP = round(FIP, 2),
    WAR = round(WAR, 1),
    `K%` = round(`K%`, 1),
    `BB%` = round(`BB%`, 1),
    `K/BB` = round(`K/BB`, 2),
    SO = round(SO),
    BB = round(BB),
    HR = round(HR),
    ER = round(ER),
    WHIP = round(WHIP, 2),
    H = round(H),
    R = round(R),
    HBP = round(HBP),
    TBF = round(TBF),
    HLD = round(HLD),
    IBB = round(IBB),
    SV = round(SV),
    AVG = round(AVG, 3)
  )

pitch_proj <- pitch_proj |>
  mutate(Name = gsub("á", "a", Name),
         Name = gsub("Á", "A", Name),
         Name = gsub("É", "E", Name),
         Name = gsub("Í", "I", Name),
         Name = gsub("Ó", "O", Name),
         Name = gsub("Ú", "U", Name),
         Name = gsub("é", "e", Name),
         Name = gsub("í", "i", Name),
         Name = gsub("ó", "o", Name),
         Name = gsub("ú", "u", Name),
         Name = gsub("ñ", "n", Name),
         Name = gsub("-", "", Name),
         Name = gsub("ü", "u", Name),
         Name = gsub("\\.", "", Name))

test <- pitch_proj |>
  group_by(Name) |>
  summarise(n = n()) |>
  filter(n >=4) |>
  ungroup()

pitch_proj <- pitch_proj |>
  filter(Name %in% test$Name)


#save as csv
write.csv(pitch_proj, "final_pitcher_projections.csv", row.names = FALSE)
