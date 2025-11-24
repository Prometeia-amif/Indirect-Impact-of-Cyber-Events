rm( list = ls() )


if ( !require ( openxlsx) ) {
  install.packages ( "openxlsx" )
}
if ( !require ( reshape2) ) {
  install.packages ( "reshape2" )
}
if ( !require ( dplyr) ) {
  install.packages ( "dplyr" )
}
if ( !require ( dbplyr) ) {
  install.packages ( "ddplyr" )
}
if ( !require ( lubridate) ) {
  install.packages ( "lubridate" )
}
if ( !require (xts) ) {
  install.packages ( "xts" )
}
if ( !require (PerformanceAnalytics) ) {
  install.packages ( "PerformanceAnalytics" )
}
if ( !require (ggplot2) ) {
  install.packages ( "ggplot2" )
}

if(!require(cowplot)){ 
  install.packages("cowplot")
}

if(!require(readr)){ 
  install.packages("readr")
}

if(!require(tidyr)){ 
  install.packages("tidyr")
}

#load stuff


load("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT_CDS/results_before_means.RData")


############FIX NACE

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


CAR_all <- CAR_all %>%
  mutate(nace_code = sub(".*\\(([^)]+)\\).*", "\\1", NACE.Classification))


CAR_all <- CAR_all %>%
  left_join(nace_code4, by = c("nace_code" = "Level4"))


CAR_all <- CAR_all %>%
  left_join(nace_code1, by = c("Level1" = "Code"))


CAR_all <- CAR_all %>%
  left_join(nace_code2, by = c("Level2" = "Code"))

CAR_all_CDS<-CAR_all



#load stuff


load("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/results_before_means_15days.RData")


############FIX NACE

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


CAR_all <- CAR_all %>%
  mutate(nace_code = sub(".*\\(([^)]+)\\).*", "\\1", NACE.Classification))


CAR_all <- CAR_all %>%
  left_join(nace_code4, by = c("nace_code" = "Level4"))


CAR_all <- CAR_all %>%
  left_join(nace_code1, by = c("Level1" = "Code"))


CAR_all <- CAR_all %>%
  left_join(nace_code2, by = c("Level2" = "Code"))


###############



###########################

merged_TAB <- merge(CAR_all, CAR_all_CDS, by = c("KEY"))


model <- lm(CARi.y ~ CARi.x, data = merged_TAB)

summary(model)



lev=read.xlsx("G:/CLIMATE RISK & ESG/PROGETTI/Reputation_risk/reprisk/prova_isin.xlsx", sheet="LeverageT")


merged_TAB <- merged_TAB %>%
  left_join(lev, by = c("ISIN.x" = "ISIN"))

merged_TAB$d_lev <- merged_TAB$lev2023 - merged_TAB$lev2022  

merged_TAB$d_lev_2 <- (merged_TAB$lev2023 - merged_TAB$lev2022) / merged_TAB$lev2022 *100  

model <- lm(CARi.y ~ CARi.x + d_lev + lev + CARi.x*d_lev, data = merged_TAB2)

summary(model)
# Calculate  vol

DB <- DB %>%
  group_by(ISIN) %>%
  mutate(year=year(ymd(Date))) %>% as.data.frame()

DB1 <- DB %>%
  filter(year>2021)
DB1 <- DB1 %>%
  filter(year<2024)

DB1 <- DB1%>%
  group_by(ISIN,year) %>%
  summarise(vol = var(Return, na.rm = TRUE)) %>% as.data.frame()

DB1 <- DB1%>%
  group_by(ISIN) %>%
  mutate(vol_m = mean(vol, na.rm = TRUE)) %>% as.data.frame()

DB1 <- DB1%>%
  group_by(ISIN) %>%
  mutate(d_vol = vol-dplyr::lag(vol,1)) %>% as.data.frame()

DB2 <- DB1[,c(1,4,5)]

DB2 <- DB2 %>%
  filter(complete.cases(.))

merged_TAB2 <- merged_TAB %>%
  left_join(DB2, by = c("ISIN.x" = "ISIN"))

model <- lm(CARi.y ~ CARi.x + d_lev + lev + d_vol + vol_m + CARi.x*d_lev, data = merged_TAB2)

summary(model)


