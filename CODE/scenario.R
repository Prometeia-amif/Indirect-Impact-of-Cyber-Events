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


###############SCENARIO


### N EVENTI MEDI PER SETTORE E CATEGORIA
rm("jj")
rm("jjj")

the_n_sectors=unique(CAR_all$NACE1)


tab_ENV_EVENTS_NACE_sect_cat_all <- data.frame(numeric(0))

for ( jjj in 143:146) {
  
  for ( jj in 1:length(the_n_sectors)) {
    
    tab_ENV_EVENTS_NACE_sect_cat <- CAR_all %>%
      filter(CAR_all$Date>"2022-12-31" & CAR_all[,jjj]=="T" & CAR_all$NACE1==the_n_sectors[jj]) %>%
      left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
      group_by(est_wdw,event_wdw_m,event_wdw_p,Issuer) %>%
      summarise(
                              n_event = n_distinct(KEY), 
                              m_event = n_distinct(Event),
                              NACE_sector = the_n_sectors[jj],
                              category = colnames(CAR_all[jjj]))
    tab_ENV_EVENTS_NACE_sect_cat$variable <- "CAR"
    
    tab_ENV_EVENTS_NACE_sect_cat_all <-rbind(tab_ENV_EVENTS_NACE_sect_cat_all,tab_ENV_EVENTS_NACE_sect_cat)
  }
}


tab_ENV_EVENTS_NACE_sect_1 <- tab_ENV_EVENTS_NACE_sect_cat_all %>%
  filter(event_wdw_p==15)


tab_ENV_EVENTS_NACE_sect_1 <- tab_ENV_EVENTS_NACE_sect_1 %>%
  group_by(event_wdw_p, NACE_sector,category) %>%
  summarise(
    m_event_m = {
      # Calculate the 5th and 95th percentiles
      lower_bound <- quantile(m_event, 0.05)
      upper_bound <- quantile(m_event, 0.95)
      
      # Filter out the tails
      trimmed_data <- m_event[m_event >= lower_bound & m_event <= upper_bound]
      
      # Compute the mean of the trimmed data
      mean_value <- mean(trimmed_data, na.rm = TRUE)
      
      # Replace NaN with 1
      ifelse(is.nan(mean_value), median(m_event), mean_value)
    }
  )

# Print the result
print(tab_ENV_EVENTS_NACE_sect_1)


### GET THE ACARs
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

tab_ENV_EVENTS_NACE_sect_cat_all <- tab_ENV_EVENTS_NACE_sect_cat_all %>%
  filter(event_wdw_p==15)

tab_ENV_EVENTS_NACE_sect_1 = merge(tab_ENV_EVENTS_NACE_sect_1,tab_ENV_EVENTS_NACE_sect_cat_all,by = c("NACE_sector","category"))

period_scenario = 5 #years
stress = 1.28 # % increase in events

#ACAR_5_Y <- as.data.frame((1 + tab_ENV_EVENTS_NACE_sect_1$m_event_m * rep(tab_ENV_EVENTS$avg[16]/100,each=dim(tab_ENV_EVENTS_NACE_sect_1)[1]))^period_scenario -1)
ACAR_5_Y <-as.data.frame(
  
  ifelse(tab_ENV_EVENTS_NACE_sect_1$Reject==1,
         (1 + tab_ENV_EVENTS_NACE_sect_1$m_event_m * tab_ENV_EVENTS_NACE_sect_1$avg/100)^period_scenario -1,
         (1 + tab_ENV_EVENTS_NACE_sect_1$m_event_m * rep(tab_ENV_EVENTS$avg[16]/100,each=dim(tab_ENV_EVENTS_NACE_sect_1)[1]))^period_scenario -1)
  
  
)
  

#Ipotesi di stress: in uno scenario di transizione elevata il numero di eventi annui di ogni settore aumenta del 28% (file 2007_2023 events)

#ACAR_5_Y_stress <- as.data.frame((1 + tab_ENV_EVENTS_NACE_sect_1$m_event_m* stress * rep(tab_ENV_EVENTS$avg[16]/100,each=dim(tab_ENV_EVENTS_NACE_sect_1)[1]))^period_scenario -1)
ACAR_5_Y_stress <-as.data.frame(
  
  ifelse(tab_ENV_EVENTS_NACE_sect_1$Reject==1,
         (1 + tab_ENV_EVENTS_NACE_sect_1$m_event_m * stress * tab_ENV_EVENTS_NACE_sect_1$avg/100)^period_scenario -1,
         (1 + tab_ENV_EVENTS_NACE_sect_1$m_event_m * stress * rep(tab_ENV_EVENTS$avg[16]/100,each=dim(tab_ENV_EVENTS_NACE_sect_1)[1]))^period_scenario -1)
  
  
)

TAB_stress <- cbind(tab_ENV_EVENTS_NACE_sect_1$NACE_sector,tab_ENV_EVENTS_NACE_sect_1$category)

TAB_stress <- cbind(TAB_stress,tab_ENV_EVENTS_NACE_sect_1$avg)

TAB_stress <- cbind(TAB_stress,tab_ENV_EVENTS_NACE_sect_1$m_event_m)

TAB_stress <- cbind(TAB_stress,ACAR_5_Y*100)

TAB_stress <- cbind(TAB_stress,ACAR_5_Y_stress*100)

colnames(TAB_stress) <- c("Sector", "Category", "ACAR_15gg_EQ","Average n event x sector", "ACAR_5_Y", "ACAR_5_Y_stress")

TAB_stress$diff <- TAB_stress$ACAR_5_Y_stress - TAB_stress$ACAR_5_Y

print(TAB_stress)

print(mean(TAB_stress$diff))

TAB_stress$`Average n event x sector`= as.numeric(TAB_stress$`Average n event x sector`)

TAB_stress$`ACAR_15gg_EQ`= as.numeric(TAB_stress$`ACAR_15gg_EQ`)

TAB_stress$`ACAR_15gg_EQ` <- ifelse(tab_ENV_EVENTS_NACE_sect_1$Reject==1,
         tab_ENV_EVENTS_NACE_sect_1$avg,
         tab_ENV_EVENTS$avg[16])
  

#CDS

####CDS#####

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


################################################################################
### OUTPUT #####################################################################
################################################################################

tab_ENV_EVENTS_CDS <- CAR_all %>%
  left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
  group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
  summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
            Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
            min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
            std= mean(CARi_std, na.rm = TRUE),
            #severity = severity,
            VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))), n_event = n_distinct(KEY),
            Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
  )
tab_ENV_EVENTS_CDS$variable <- "CAR"
tab_ENV_EVENTS_CDS


TAB_stress_CDS <- TAB_stress

TAB_stress_CDS$ACAR_5_Y = TAB_stress_CDS$ACAR_5_Y * tab_ENV_EVENTS_CDS$avg[16]/tab_ENV_EVENTS$avg[16] 
TAB_stress_CDS$ACAR_5_Y_stress = TAB_stress_CDS$ACAR_5_Y_stress * tab_ENV_EVENTS_CDS$avg[16]/tab_ENV_EVENTS$avg[16] 
TAB_stress_CDS$diff <- TAB_stress_CDS$ACAR_5_Y_stress - TAB_stress_CDS$ACAR_5_Y
TAB_stress_CDS$ACAR_15gg_EQ <- TAB_stress_CDS$ACAR_15gg_EQ * tab_ENV_EVENTS_CDS$avg[16]/tab_ENV_EVENTS$avg[16]

colnames(TAB_stress) <- c("Sector", "Category", "ACAR_15gg_CDS","Average n event x sector", "ACAR_5_Y", "ACAR_5_Y_stress")

TAB_stress_CDS$PD <- (TAB_stress_CDS$ACAR_5_Y/0.6)/100
TAB_stress_CDS$PD_stress <- (TAB_stress_CDS$ACAR_5_Y_stress/0.6)/100
TAB_stress_CDS$diff_PD <- TAB_stress_CDS$PD_stress - TAB_stress_CDS$PD



TAB_stress[,c(3:7)] <- round(TAB_stress[,c(3:7)],2) 
TAB_stress_CDS[,c(3:10)] <- round(TAB_stress_CDS[,c(3:10)],2) 


print(TAB_stress_CDS)
print(mean(TAB_stress_CDS$diff))
print(mean(TAB_stress_CDS$diff_PD))



file <- "C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT_SCENARIO/scenario_tables_cat.xlsx"
write.xlsx(list( 
  Equity = TAB_stress,
  CDS = TAB_stress_CDS),
  file)



# Specify the desired order of categories
category_order <- c("ENVIRONMENTAL_IMPACT",
                    "CLIMATE_CHANGE_INACTION",
                    "RESOURCE_MANAGEMENT",
                    "ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")

category_labels <- c("Environmental impact",
                     "Climate change inaction",
                     "Resource management",
                     "Environmental reporting and transparency")

# Convert the category column to a factor with the specified order
TAB_stress_CDS$Category <- factor(TAB_stress_CDS$Category, levels = category_order, labels=category_labels)

png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT_SCENARIO/diff_PD.jpg", width = 1067, height = 536)
ggplot(TAB_stress_CDS, aes(y = Sector, x = diff_PD, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.6)) +
  labs( y = "Sector", x = "Difference in PD (%)") +
  geom_vline(xintercept = 0.169, linetype = "dotted", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 8))
dev.off()




partners_betas <- coefficients_df %>%
  filter(!is.na(PBE_partners))
  