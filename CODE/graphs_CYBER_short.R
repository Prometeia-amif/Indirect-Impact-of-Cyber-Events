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


if(!require(egg)){ 
  install.packages("egg")
}

#load stuff


load("G:/CLIMATE RISK & ESG/PROGETTI/Reputation_risk/REPUTATION/OUTPUT/results_before_means_CYBER_15.RData")


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
colnames(nace_code4)[3] <- "NACE4"


CAR_all <- CAR_all %>%
  mutate(nace_code = sub(".*\\(([^)]+)\\).*", "\\1", NACE.Classification))


CAR_all <- CAR_all %>%
  left_join(nace_code4, by = c("nace_code" = "Level4"))


CAR_all <- CAR_all %>%
  left_join(nace_code1, by = c("Level1" = "Code"))


CAR_all <- CAR_all %>%
  left_join(nace_code2, by = c("Level2" = "Code"))


################################################################################
### OUTPUT #####################################################################
################################################################################

tab_ENV_EVENTS <- CAR_all %>%
  left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
  group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
  summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
            Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
            min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
            #std= mean(CARi_std, na.rm = TRUE),
            #severity = severity,
            VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))), n_event = n_distinct(KEY),
            Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
  )
tab_ENV_EVENTS$variable <- "CAR"
tab_ENV_EVENTS


### STATS by NACE1
rm("jj")
rm("jjj")

the_n_sectors=unique(CAR_all$NACE1)


tab_ENV_EVENTS_NACE_sect_cat_all <- data.frame(numeric(0))



for ( jj in 1:length(the_n_sectors)) {
  
  tab_ENV_EVENTS_NACE_sect_cat <- CAR_all %>%
    filter(CAR_all$NACE1==the_n_sectors[jj]) %>%
    left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
    group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
    summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
              Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
              min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
              VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY), NACE_sector = the_n_sectors[jj],
              Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
    )
  tab_ENV_EVENTS_NACE_sect_cat$variable <- "CAR"
  
  tab_ENV_EVENTS_NACE_sect_cat_all <-rbind(tab_ENV_EVENTS_NACE_sect_cat_all,tab_ENV_EVENTS_NACE_sect_cat)
}


tab_ENV_EVENTS_financials <- tab_ENV_EVENTS_NACE_sect_cat_all %>%
  filter(NACE_sector=="FINANCIAL AND INSURANCE ACTIVITIES")


a<-ggplot(tab_ENV_EVENTS, aes(x = event_wdw_p, y = avg)) +
  geom_ribbon(aes(ymin = avg-Std_err, ymax = avg+Std_err), fill='DarkGreen', alpha = 0.4) +
  geom_ribbon(aes(ymin = avg-(Std_err*1.96), ymax = avg+(Std_err*1.96)), fill='DarkGreen', alpha = 0.2) +
  geom_line(color="DarkGreen") +
  labs(x = "Days", y = "ACAR (%)") +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", 
             size=0.5)+
  scale_x_continuous(breaks=seq(0,30,5))+
  guides(color = FALSE)+
  theme(text = element_text(size = 20))+
  ylim(-5,1)



b<-ggplot(tab_ENV_EVENTS_financials, aes(x = event_wdw_p, y = avg)) +
  geom_ribbon(aes(ymin = avg-Std_err, ymax = avg+Std_err), fill='red', alpha = 0.4) +
  geom_ribbon(aes(ymin = avg-(Std_err*1.96), ymax = avg+(Std_err*1.96)), fill='red', alpha = 0.2) +
  geom_line(color="red") +
  labs(x = "Days", y = "ACAR (%)") +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", 
             size=0.5)+
  scale_x_continuous(breaks=seq(0,30,5))+
  guides(color = FALSE)+
  theme(text = element_text(size = 20))+
  ylim(-5,1)


ggarrange(a,b,
          labels = c("All sectors", "Financial and insurance activities"),
          ncol = 2, nrow = 1)


write.xlsx(tab_ENV_EVENTS_NACE_sect_cat_all %>% filter(event_wdw_p==15,n_event>20) %>% select(event_wdw_p,avg, Std_err,n_event,NACE_sector,Reject),"G:/CLIMATE RISK & ESG/PROGETTI/Reputation_risk/REPUTATION/OUTPUT/cyber_NACE1.xlsx")

           