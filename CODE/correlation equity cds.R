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
### STATS by NACE1 sector AND category
rm("jj")
rm("jjj")

the_n_sectors=unique(CAR_all$NACE1)


tab_ENV_EVENTS_NACE_sect_cat_all <- data.frame(numeric(0))

for ( jjj in 144:147) {
  
  for ( jj in 1:length(the_n_sectors)) {
    
    tab_ENV_EVENTS_NACE_sect_cat <- CAR_all %>%
      filter(CAR_all[,jjj]=="T" & CAR_all$NACE1==the_n_sectors[jj]) %>%
      left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
      group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
      summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
                Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
                min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
                VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY),category = colnames(CAR_all[jjj]), NACE_sector = the_n_sectors[jj],
                Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
      )
    tab_ENV_EVENTS_NACE_sect_cat$variable <- "CAR"
    
    tab_ENV_EVENTS_NACE_sect_cat_all <-rbind(tab_ENV_EVENTS_NACE_sect_cat_all,tab_ENV_EVENTS_NACE_sect_cat)
  }
}

tab_ENV_EVENTS_NACE1_sect_cat_all_CDS<-tab_ENV_EVENTS_NACE_sect_cat_all

### STATS by NACE2 sector AND category
rm("jj")
rm("jjj")

the_n_sectors=unique(CAR_all$NACE2)


tab_ENV_EVENTS_NACE_sect_cat_all <- data.frame(numeric(0))

for ( jjj in 144:147) {
  
  for ( jj in 1:length(the_n_sectors)) {
    
    tab_ENV_EVENTS_NACE_sect_cat <- CAR_all %>%
      filter(CAR_all[,jjj]=="T" & CAR_all$NACE2==the_n_sectors[jj]) %>%
      left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
      group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
      summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
                Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
                min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
                VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY),category = colnames(CAR_all[jjj]), NACE_sector = the_n_sectors[jj],
                Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
      )
    tab_ENV_EVENTS_NACE_sect_cat$variable <- "CAR"
    
    tab_ENV_EVENTS_NACE_sect_cat_all <-rbind(tab_ENV_EVENTS_NACE_sect_cat_all,tab_ENV_EVENTS_NACE_sect_cat)
  }
}
tab_ENV_EVENTS_NACE2_sect_cat_all_CDS<-tab_ENV_EVENTS_NACE_sect_cat_all




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

### STATS by NACE1 sector AND category
rm("jj")
rm("jjj")

the_n_sectors=unique(CAR_all$NACE1)


tab_ENV_EVENTS_NACE_sect_cat_all <- data.frame(numeric(0))

for ( jjj in 143:146) {
  
  for ( jj in 1:length(the_n_sectors)) {
    
    tab_ENV_EVENTS_NACE_sect_cat <- CAR_all %>%
      filter(CAR_all[,jjj]=="T" & CAR_all$NACE1==the_n_sectors[jj]) %>%
      left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
      group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
      summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
                Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
                min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
                VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY),category = colnames(CAR_all[jjj]), NACE_sector = the_n_sectors[jj],
                Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
      )
    tab_ENV_EVENTS_NACE_sect_cat$variable <- "CAR"
    
    tab_ENV_EVENTS_NACE_sect_cat_all <-rbind(tab_ENV_EVENTS_NACE_sect_cat_all,tab_ENV_EVENTS_NACE_sect_cat)
  }
}

tab_ENV_EVENTS_NACE1_sect_cat_all<-tab_ENV_EVENTS_NACE_sect_cat_all

### STATS by NACE2 sector AND category
rm("jj")
rm("jjj")

the_n_sectors=unique(CAR_all$NACE2)


tab_ENV_EVENTS_NACE_sect_cat_all <- data.frame(numeric(0))

for ( jjj in 143:146) {
  
  for ( jj in 1:length(the_n_sectors)) {
    
    tab_ENV_EVENTS_NACE_sect_cat <- CAR_all %>%
      filter(CAR_all[,jjj]=="T" & CAR_all$NACE2==the_n_sectors[jj]) %>%
      left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
      group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
      summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
                Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
                min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
                VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY),category = colnames(CAR_all[jjj]), NACE_sector = the_n_sectors[jj],
                Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
      )
    tab_ENV_EVENTS_NACE_sect_cat$variable <- "CAR"
    
    tab_ENV_EVENTS_NACE_sect_cat_all <-rbind(tab_ENV_EVENTS_NACE_sect_cat_all,tab_ENV_EVENTS_NACE_sect_cat)
  }
}
tab_ENV_EVENTS_NACE2_sect_cat_all<-tab_ENV_EVENTS_NACE_sect_cat_all


tab_ENV_EVENTS_NACE1_sect_cat_all_15 <- tab_ENV_EVENTS_NACE1_sect_cat_all %>%
  filter(tab_ENV_EVENTS_NACE1_sect_cat_all$event_wdw_p==15  & tab_ENV_EVENTS_NACE1_sect_cat_all$avg<0)

tab_ENV_EVENTS_NACE1_sect_cat_all_CDS_15 <- tab_ENV_EVENTS_NACE1_sect_cat_all_CDS %>%
  filter(tab_ENV_EVENTS_NACE1_sect_cat_all_CDS$event_wdw_p==15 & tab_ENV_EVENTS_NACE1_sect_cat_all_CDS$avg>0)


merged_TAB <- merge(tab_ENV_EVENTS_NACE1_sect_cat_all_15, tab_ENV_EVENTS_NACE1_sect_cat_all_CDS_15, by = c("category", "NACE_sector"))

merged_TAB$ela=merged_TAB$avg.y/merged_TAB$avg.x


merged_TAB$CDS_el=merged_TAB$ela*merged_TAB$avg.x

merged_TAB$ela_m=mean(merged_TAB$ela)*merged_TAB$avg.x

merged_TAB <- merged_TAB %>% filter(merged_TAB$n_event.y>4)

#

tab_ENV_EVENTS_NACE2_sect_cat_all_15 <- tab_ENV_EVENTS_NACE2_sect_cat_all %>%
  filter(tab_ENV_EVENTS_NACE2_sect_cat_all$event_wdw_p==15  & tab_ENV_EVENTS_NACE2_sect_cat_all$avg<0)

tab_ENV_EVENTS_NACE2_sect_cat_all_CDS_15 <- tab_ENV_EVENTS_NACE2_sect_cat_all_CDS %>%
  filter(tab_ENV_EVENTS_NACE2_sect_cat_all_CDS$event_wdw_p==15  & tab_ENV_EVENTS_NACE2_sect_cat_all_CDS$avg>0)


merged_TAB_2 <- merge(tab_ENV_EVENTS_NACE2_sect_cat_all_15, tab_ENV_EVENTS_NACE2_sect_cat_all_CDS_15, by = c("category", "NACE_sector"))

merged_TAB_2$ela=merged_TAB_2$avg.y/merged_TAB_2$avg.x

merged_TAB_2$ela_m=mean(merged_TAB_2$ela)*merged_TAB_2$avg.x

merged_TAB_2 <- merged_TAB_2 %>% filter(merged_TAB_2$n_event.y>4)

ggplot(merged_TAB, aes(x=category, y=NACE_sector, fill=ela_m)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event.y)), color="black", size=5) +
  scale_fill_gradient2(low="white", mid="white", high="red", 
                       midpoint=0,limits=c(0,max(merged_TAB$ela_m))) +
  labs(x="Category", y="Sector", fill="ACAR (Δ bps)") +
  ggtitle("5 days window")+
  theme_minimal()+
  theme(text = element_text(size = 12))



ggplot(merged_TAB, aes(x = avg.x, y = avg.y)) +
  geom_point() +
  geom_text(aes(label = paste(category, NACE_sector, sep = "\n")), 
            vjust = -1, 
            hjust = 1.1, 
            size = 2) +
  labs(x = "Equity impact",
       y = "CDS impact") +
  theme_minimal()



# Create the scatterplot with annotations and colors based on C
p<-ggplot(merged_TAB, aes(x = avg.x, y = avg.y, color = category)) +
  geom_point(size = 2) +
  geom_text(aes(label = NACE_sector), 
            vjust = -1, 
            hjust = 0.25, 
            size = 2.5) +
 # scale_color_continuous(type = "viridis") + # or scale_color_gradient() for a continuous gradient
  labs(x = "Equity impact",
       y = "CDS impact") +
  theme_minimal()

p + theme(legend.position="bottom")



###########################




merged_TAB <- merge(CAR_all, CAR_all_CDS, by = c("KEY","event_wdw_p"))

merged_TAB<-merged_TAB %>%
  filter(merged_TAB$event_wdw_p==15)

model <- lm(CARi.y ~ CARi.x, data = merged_TAB)

summary(model)


ggplot(merged_TAB, aes(x = CARi.x, y = CARi.y)) +
  geom_point() +
  labs(x = "Equity impact",
       y = "CDS impact") +
  geom_smooth(method = "lm", se = FALSE, aes(x = CARi.x, y = CARi.y, color = "red")) +
  theme_minimal()


lev=read.xlsx("G:/CLIMATE RISK & ESG/PROGETTI/Reputation_risk/reprisk/prova_isin.xlsx", sheet="LeverageT")


merged_TAB <- merged_TAB %>%
  left_join(lev, by = c("ISIN.x" = "ISIN"))

merged_TAB$d_lev <- merged_TAB$lev2023 - merged_TAB$lev2022  

merged_TAB$d_lev_2 <- (merged_TAB$lev2023 - merged_TAB$lev2022) / merged_TAB$lev2022 *100  


model <- lm(CARi.y ~ CARi.x + d_lev, data = merged_TAB)

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

model <- lm(CARi.y ~ CARi.x + lev + d_lev + vol_m + CARi.x*d_lev + d_vol, data = merged_TAB2)

summary(model)

#CAR_all$CDS <- model[["coefficients"]][["CARi.x"]]*CAR_all$CARi

CAR_all$CDS <- -3.769147*CAR_all$CARi #-3.769147 corrisponde all'elasticità tab_ENV_EVENTS_CDS$avg[16]/tab_ENV_EVENTS$avg[16]


tab_ENV_EVENTS <- CAR_all %>%
  left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
  group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
  summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
            Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
            min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
            avg_CDS= mean(CDS, na.rm = TRUE),
            #severity = severity,
            VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))), n_event = n_distinct(KEY),
            Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
  )
tab_ENV_EVENTS$variable <- "CAR"
tab_ENV_EVENTS$avg_CDS



### STATS by NACE1 sector AND category
rm("jj")
rm("jjj")

the_n_sectors=unique(CAR_all$NACE1)


tab_ENV_EVENTS_NACE_sect_cat_all <- data.frame(numeric(0))

for ( jjj in 143:146) {
  
  for ( jj in 1:length(the_n_sectors)) {
    
    tab_ENV_EVENTS_NACE_sect_cat <- CAR_all %>%
      filter(CAR_all[,jjj]=="T" & CAR_all$NACE1==the_n_sectors[jj]) %>%
      left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
      group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
      summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
                Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
                min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
                avg_CDS= mean(CDS, na.rm = TRUE),
                VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY),category = colnames(CAR_all[jjj]), NACE_sector = the_n_sectors[jj],
                Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
      )
    tab_ENV_EVENTS_NACE_sect_cat$variable <- "CAR"
    
    tab_ENV_EVENTS_NACE_sect_cat_all <-rbind(tab_ENV_EVENTS_NACE_sect_cat_all,tab_ENV_EVENTS_NACE_sect_cat)
  }
}


tab_ENV_EVENTS_cat_all_15_eco <- tab_ENV_EVENTS_NACE_sect_cat_all %>%
  filter(tab_ENV_EVENTS_NACE_sect_cat_all$event_wdw_p=="15" & tab_ENV_EVENTS_NACE_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_15_eco$category <- factor(tab_ENV_EVENTS_cat_all_15_eco$category, levels = category_order, labels=category_labels)


my_midpoint=mean(tab_ENV_EVENTS_cat_all_15_eco$avg_CDS)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_NACE_15_CDS.jpg", width = 1200, height = 536)

ggplot(tab_ENV_EVENTS_cat_all_15_eco, aes(x=category, y=NACE_sector, fill=avg_CDS)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="white", mid="white", high="red", 
                       midpoint=0,limits=c(0,max(tab_ENV_EVENTS_cat_all_15_eco$avg_CDS))) +
  labs(x="Category", y="Sector", fill="ACAR (Δ bps)") +
  ggtitle("15 days window")+
  theme_minimal()+
  theme(text = element_text(size = 12))
dev.off() # Ensure panels are side by side



### STATS by NACE2 sector AND category
rm("jj")
rm("jjj")

the_n_sectors=unique(CAR_all$NACE2)


tab_ENV_EVENTS_NACE_sect_cat_all <- data.frame(numeric(0))

for ( jjj in 143:146) {
  
  for ( jj in 1:length(the_n_sectors)) {
    
    tab_ENV_EVENTS_NACE_sect_cat <- CAR_all %>%
      filter(CAR_all[,jjj]=="T" & CAR_all$NACE2==the_n_sectors[jj]) %>%
      left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
      group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
      summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
                Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
                min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
                avg_CDS= mean(CDS, na.rm = TRUE),
                VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY),category = colnames(CAR_all[jjj]), NACE_sector = the_n_sectors[jj],
                Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
      )
    tab_ENV_EVENTS_NACE_sect_cat$variable <- "CAR"
    
    tab_ENV_EVENTS_NACE_sect_cat_all <-rbind(tab_ENV_EVENTS_NACE_sect_cat_all,tab_ENV_EVENTS_NACE_sect_cat)
  }
}

tab_ENV_EVENTS_cat_all_15_eco <- tab_ENV_EVENTS_NACE_sect_cat_all %>%
  filter(tab_ENV_EVENTS_NACE_sect_cat_all$event_wdw_p=="15" & tab_ENV_EVENTS_NACE_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")
mean
tab_ENV_EVENTS_cat_all_15_eco$category <- factor(tab_ENV_EVENTS_cat_all_15_eco$category, levels = category_order, labels=category_labels)


my_midpoint=mean(tab_ENV_EVENTS_cat_all_15_eco$avg_CDS)
tab_ENV_EVENTS_cat_all_15_eco<-tab_ENV_EVENTS_cat_all_15_eco %>%
  filter(tab_ENV_EVENTS_cat_all_15_eco$n_event>51)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_NACE2_15_CDS.jpg", width = 1200, height = 536)

ggplot(tab_ENV_EVENTS_cat_all_15_eco, aes(x=category, y=NACE_sector, fill=avg_CDS)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="white", mid="white", high="red", 
                       midpoint=0,limits=c(0,max(tab_ENV_EVENTS_cat_all_15_eco$avg_CDS))) +
  labs(x="Category", y="Sector", fill="ACAR (Δ bps)") +
  ggtitle("15 days window")+
  theme_minimal()+
  theme(text = element_text(size = 12))
dev.off() # Ensure panels are side by side




