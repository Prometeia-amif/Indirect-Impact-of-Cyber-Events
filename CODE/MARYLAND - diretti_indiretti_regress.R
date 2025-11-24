library(dplyr)

CAR_full_db <- CAR_all_ind %>%
  left_join(
    CAR_all,
    by = c(
      "Date" = "Date",
      "event_wdw_p" = "event_wdw_p",
      "direct_isin" = "Issuer"
    ),
    suffix = c("_ind", "_dir")
  ) %>%
  select(
    ISIN = ISIN_ind,
    ISIN_dir = ISIN_dir,
    CARi_ind = CARi_ind,
    CARi_dir = CARi_dir,
    event_wdw_p,
    Date = Date
  ) %>%
  filter(!is.na(CARi_dir))

ols_model <- lm(CARi_ind ~ CARi_dir, data = CAR_full_db)
summary(ols_model)

library(plm)

# Declare the panel data structure
CAR_panel <- pdata.frame(CAR_full_db, index = c("ISIN", "event_wdw_p"))

# Fixed effects (within) model
panel_fe <- plm(CARi_ind ~ CARi_dir, data = CAR_panel, model = "within")
summary(panel_fe)



sectors<- read.xlsx("G:/CLIMATE RISK & ESG/PROGETTI/Reputation_risk/REPUTATION/DATA/Sectors_MARYLAND.xlsx", sheet = "Sheet2")

CAR_all_ind<-CAR_all_ind %>% left_join(sectors,by = "ISIN")

nace_code <- read_csv("https://gist.githubusercontent.com/b-rodrigues/4218d6daa8275acce80ebef6377953fe/raw/99bb5bc547670f38569c2990d2acada65bb744b3/nace_rev2.csv")



nace_code1 = nace_code[nace_code$Level=="1",2:5]
nace_code2 = nace_code[nace_code$Level=="2",2:5]



nace_code <- nace_code %>%
  select(Level, Code, Description)
nace_code <- nace_code %>%
  mutate(Level1 = ifelse(Level == 1, Code, NA)) %>%
  fill(Level1, .direction = "down") %>%  
  mutate(Level2 = ifelse(Level == 2, Code, NA)) %>%
  fill(Level2, .direction = "down") %>% 
  mutate(Level3 = ifelse(Level == 3, Code, NA)) %>%
  fill(Level3, .direction = "down") %>% 
  mutate(Level4 = ifelse(Level == 4, Code, NA)) %>%  
  filter(!is.na(Level4))

nace_code4=nace_code

colnames(nace_code1)[4] <- "NACE1"
colnames(nace_code2)[4] <- "NACE2"


CAR_all_ind <- CAR_all_ind %>%
  mutate(nace_code = sub(".*\\(([^)]+)\\).*", "\\1", NACE.Classification))


CAR_all_ind <- CAR_all_ind %>%
  left_join(nace_code4, by = c("nace_code" = "Level4"))


CAR_all_ind <- CAR_all_ind %>%
  left_join(nace_code1, by = c("Level1" = "Code"))


CAR_all_ind <- CAR_all_ind %>%
  left_join(nace_code2, by = c("Level2" = "Code"))


CAR_all_ind_u <- CAR_all_ind %>%
  group_by(ISIN) %>%
  slice(1) %>%
  ungroup()

CAR_all_u <- CAR_all %>%
  group_by(ISIN) %>%
  slice(1) %>%
  ungroup()

CAR_all_u$NACE1_dir <- CAR_all_u$NACE1

CAR_full_db <- CAR_full_db %>%
  left_join(CAR_all_ind_u %>% select(ISIN,NACE1),by = "ISIN")

CAR_full_db <- CAR_full_db %>%
  left_join(CAR_all_u %>% select(ISIN,NACE1_dir),by = c("ISIN_dir"="ISIN"))


CAR_full_db$IS_FIN <- ifelse(CAR_full_db$NACE1=="FINANCIAL AND INSURANCE ACTIVITIES",1,0) 
CAR_full_db$IS_INTRA <- ifelse(CAR_full_db$NACE1==CAR_full_db$NACE1_dir,1,0) 

CAR_full_db$IS_FIN_dir <- ifelse(CAR_full_db$NACE1_dir=="FINANCIAL AND INSURANCE ACTIVITIES",1,0) 
CAR_full_db$IS_FIN_both <- ifelse(CAR_full_db$NACE1=="FINANCIAL AND INSURANCE ACTIVITIES" & CAR_full_db$NACE1_dir=="FINANCIAL AND INSURANCE ACTIVITIES",1,0) 


CAR_full_db_FIN <- CAR_full_db %>% filter(NACE1=="FINANCIAL AND INSURANCE ACTIVITIES")

ols_model <- lm(CARi_ind ~ CARi_dir*IS_FIN + CARi_dir*IS_FIN_dir, data = CAR_full_db)

summary(ols_model)

# Declare the panel data structure
CAR_panel <- pdata.frame(CAR_full_db, index = c("ISIN", "event_wdw_p"))

# Fixed effects (within) model
panel_fe <- plm(CARi_ind ~ CARi_dir, data = CAR_panel, model = "within")
summary(panel_fe)


interaction_model <- lm(
  CARi_ind ~ CARi_dir * IS_FIN_both,
  data = CAR_full_db
)

summary(interaction_model)

CAR_full_db_NO_INTRA <- CAR_full_db %>% filter(NACE1!=NACE1_dir)

ols_model <- lm(CARi_ind ~ CARi_dir*IS_FIN, data = CAR_full_db_NO_INTRA)
summary(ols_model)

CAR_full_db_NO_EXTRA <- CAR_full_db %>% filter(NACE1==NACE1_dir)

ols_model <- lm(CARi_ind ~ CARi_dir*IS_FIN, data = CAR_full_db_NO_EXTRA)
summary(ols_model)



ols_model <- lm(CARi_ind ~ CARi_dir*IS_FIN + CARi_dir*IS_INTRA + CARi_dir*IS_INTRA*IS_FIN, data = CAR_full_db)

summary(ols_model)