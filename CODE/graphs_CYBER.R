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


load("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/results_before_means_CYBER.RData")


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

# Compute the average number of events x firm x year
event_count <- CAR_all %>%
  filter(CAR_all$event_wdw_p==0) %>%
  group_by(ISIN,company_name) %>%
  summarize(number_of_events = n(), .groups = 'drop')

mean(event_count$number_of_events)/3


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


### STATS by NACE4
rm("jj")
rm("jjj")

the_n_sectors=unique(CAR_all$NACE4)


tab_ENV_EVENTS_NACE4_sect_cat_all <- data.frame(numeric(0))



for ( jj in 1:length(the_n_sectors)) {
  
  tab_ENV_EVENTS_NACE4_sect_cat <- CAR_all %>%
    filter(CAR_all$NACE4==the_n_sectors[jj]) %>%
    left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
    group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
    summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
              Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
              min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
              VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY), NACE4_sector = the_n_sectors[jj],
              Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
    )
  tab_ENV_EVENTS_NACE4_sect_cat$variable <- "CAR"
  
  tab_ENV_EVENTS_NACE4_sect_cat_all <-rbind(tab_ENV_EVENTS_NACE4_sect_cat_all,tab_ENV_EVENTS_NACE4_sect_cat)
}


tab_ENV_EVENTS_banks <- tab_ENV_EVENTS_NACE4_sect_cat_all %>%
  filter(NACE4_sector=="Other monetary intermediation")

################################################################################
### GRAPHS #####################################################################
################################################################################


png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/ACAR_ts_30.jpg", width = 713, height = 475)

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

dev.off()



fileout <- paste0(folder,"OUTPUT/")
file <- paste0 ( fileout, "CYBER_count_event_study.xlsx") 
write.xlsx(list( 
  all = tab_ENV_EVENTS,
  fin = tab_ENV_EVENTS_financials),
  file)


###################################

EVENTS <- EVENTS %>%
  mutate(nace_code = sub(".*\\(([^)]+)\\).*", "\\1", NACE.Classification))

EVENTS <- EVENTS %>%
  left_join(nace_code4, by = c("nace_code" = "Level4"))

EVENTS <- EVENTS %>%
  left_join(nace_code1, by = c("Level1" = "Code"))

EVENTS$incident_date <- as.Date(EVENTS$`Incident.Date.(incident_date)`, format = "%Y-%m-%d")

EVENTS$year <- format(EVENTS$incident_date, "%Y")

# 1. Overall yearly events
yearly_events <- EVENTS %>%
  group_by(year) %>%
  summarise(event_count = n())

# 2. Yearly events for "FINANCIAL AND INSURANCE ACTIVITIES"
financial_events <- EVENTS %>%
  filter(NACE1 == "FINANCIAL AND INSURANCE ACTIVITIES") %>%
  group_by(year) %>%
  summarise(financial_event_count = n())

# Merge both datasets on the year
combined_events <- merge(yearly_events, financial_events, by = "year", all.x = TRUE)

# Replace NA with 0 for financial_event_count (in case some years have no financial events)
combined_events$financial_event_count[is.na(combined_events$financial_event_count)] <- 0

# Normalize both series (100 = value in 2015)
# First, convert 'year' to numeric for comparison
combined_events$year <- as.numeric(combined_events$year)

# Get the event counts for 2015
overall_2015 <- combined_events$event_count[combined_events$year == 2015]
financial_2015 <- combined_events$financial_event_count[combined_events$year == 2015]

# Normalize to 100 based on 2015 counts
combined_events$normalized_event_count <- (combined_events$event_count / overall_2015) * 100
combined_events$normalized_financial_event_count <- (combined_events$financial_event_count / financial_2015) * 100

# Create the plot with a legend and custom colors
ggplot(combined_events, aes(x = year)) +
  # Normalized overall yearly events
  geom_line(aes(y = normalized_event_count, color = "All events"), size = 1) +
  geom_point(aes(y = normalized_event_count, color = "All events"), size = 2) +
  # Normalized financial events
  geom_line(aes(y = normalized_financial_event_count, color = "Financial and Insurance events"), size = 1) +
  geom_point(aes(y = normalized_financial_event_count, color = "Financial and Insurance events"), size = 2) +
  # y-axis label and x-axis settings
  labs(x = "Year", y = "Normalized Event Count (100 = 2015)", 
       title = "Yearly Cyberattack Events (Normalized to 2015 = 100)",
       color = "Event Type") +  # This adds the legend title
  theme_minimal() +
  theme(
    text = element_text(size = 15),
    axis.title.y = element_text(color = "black"),  # General y-axis label
    axis.text.y = element_text(color = "black"),   # General y-axis ticks
    legend.position = "bottom"  # Places the legend at the bottom
  ) +
  # Specify colors for each event type
  scale_color_manual(values = c("All events" = "darkgreen", 
                                "Financial and Insurance events" = "red"))


# RESULTS + MODEL STATS
fileout <- paste0(folder,"OUTPUT/")
file <- paste0 ( fileout, "CYBER_count.xlsx") 
write.xlsx(list( 
  n_events = combined_events),
  file)


###################################

#SEVERITY


tab_ENV_EVENTS_sev <- CAR_all %>%
  left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
  group_by(est_wdw,event_wdw_m,event_wdw_p,`Severity.(severity)`) %>%
  summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
            Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
            min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
            VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))), n_event = n_distinct(KEY),
            Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
  )
tab_ENV_EVENTS_sev$variable <- "CAR"


category_labels <- c(paste("Severity 1 \nn of events:",tab_ENV_EVENTS_sev$n_event[dim(tab_ENV_EVENTS_sev)[1]-2]),paste("Severity 2 \nn of events:",tab_ENV_EVENTS_sev$n_event[dim(tab_ENV_EVENTS_sev)[1]-1]),paste("Severity 3 \nn of events:",tab_ENV_EVENTS_sev$n_event[dim(tab_ENV_EVENTS_sev)[1]]))
tab_ENV_EVENTS_sev$severity <- factor(tab_ENV_EVENTS_sev$`Severity.(severity)`, labels=category_labels)


png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/ACAR_sev_ts.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_sev, aes(x = event_wdw_p, y = avg, fill=severity)) +
  geom_ribbon(aes(ymin = avg - Std_err, ymax = avg + Std_err, fill = severity), alpha = 0.4) +
  geom_ribbon(aes(ymin = avg - (Std_err*1.96), ymax = avg + (Std_err*1.96), fill = severity), alpha = 0.2) +
  geom_line(aes(color = severity)) +
  labs(x = "Days", y = "ACAR (%)") +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", size=0.5) +
  scale_x_continuous(breaks=seq(0, 30, 5)) +
  scale_y_continuous(breaks = c(0, -0.5, -1, -1.5,-2, -2.5, -3, -3.5)) +  # Set specific values on the y-axis
  guides(fill = FALSE, color = FALSE) +  # Remove legends for fill and color
  theme(text = element_text(size = 20), legend.position = "none") +  # Ensure legend is removed
  facet_wrap(~severity, scales = "fixed")
dev.off()


###################################

#novelty


tab_ENV_EVENTS_novelty <- CAR_all %>%
  left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
  group_by(est_wdw,event_wdw_m,event_wdw_p,`Novelty.(novelty)`) %>%
  summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
            Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
            min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
            VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))), n_event = n_distinct(KEY),
            Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
  )
tab_ENV_EVENTS_novelty$variable <- "CAR"


category_labels <- c(paste("Novelty 1 \nn of events:",tab_ENV_EVENTS_novelty$n_event[1]),paste("Novelty 2 \nn of events:",tab_ENV_EVENTS_novelty$n_event[2]))
tab_ENV_EVENTS_novelty$novelty <- factor(tab_ENV_EVENTS_novelty$`Novelty.(novelty)`, labels=category_labels)


png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/ACAR_novelty_ts.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_novelty, aes(x = event_wdw_p, y = avg, fill=novelty)) +
  geom_ribbon(aes(ymin = avg - Std_err, ymax = avg + Std_err, fill = novelty), alpha = 0.4) +
  geom_ribbon(aes(ymin = avg - (Std_err*1.96), ymax = avg + (Std_err*1.96), fill = novelty), alpha = 0.2) +
  geom_line(aes(color = novelty)) +
  labs(x = "Days", y = "ACAR (%)") +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", size=0.5) +
  scale_x_continuous(breaks=seq(0, 15, 1)) +
  scale_y_continuous(breaks = c(0, -0.5, -1, -1.5,-2, -2.5, -3, -3.5)) +  # Set specific values on the y-axis
  guides(fill = FALSE, color = FALSE) +  # Remove legends for fill and color
  theme(text = element_text(size = 20), legend.position = "none") +  # Ensure legend is removed
  facet_wrap(~novelty, scales = "fixed")
dev.off()


###################################

CAR_all$SIZE2 <- ifelse(CAR_all$SIZE==">1B","Big","Small")
  
  
the_sizes=unique(CAR_all$SIZE2)

for ( jj in 1:length(the_sizes)) {
  
  tab_ENV_EVENTS_size <- CAR_all %>%
    filter(CAR_all$SIZE2==the_sizes[jj]) %>%
    left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
    group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
    summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
              Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
              min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
              VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY), size = the_sizes[jj],
              Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
    )
  tab_ENV_EVENTS_size$variable <- "CAR"
  
  if (jj==1){
    tab_ENV_EVENTS_size_all <- tab_ENV_EVENTS_size
  } else {tab_ENV_EVENTS_size_all <-rbind(tab_ENV_EVENTS_size_all,tab_ENV_EVENTS_size)}
}















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
tab_ENV_EVENTS_cat_all$category <- factor(tab_ENV_EVENTS_cat_all$category, levels = category_order, labels=category_labels)

png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/ACAR_cat_ts.jpg", width = 713, height = 475)
ggplot(tab_ENV_EVENTS_cat_all, aes(x = event_wdw_p, y = avg, fill=category)) +
  geom_ribbon(aes(ymin = avg - Std_err, ymax = avg + Std_err, fill = category), alpha = 0.4) +
  geom_ribbon(aes(ymin = avg - (Std_err*1.96), ymax = avg + (Std_err*1.96), fill = category), alpha = 0.2) +
  geom_line(aes(color = category)) +
  labs(x = "Days", y = "ACAR (%)") +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", size=0.5) +
  scale_x_continuous(breaks=seq(0, 15, 1)) +
  scale_y_continuous(breaks = c(0, -0.25, -0.5, -0.75, -1)) +  # Set specific values on the y-axis
  guides(fill = FALSE, color = FALSE) +  # Remove legends for fill and color
  theme(text = element_text(size = 20), legend.position = "none") +  # Ensure legend is removed
  facet_wrap(~category, scales = "fixed")  # Ensure all plots have the same y-axis
dev.off()

####
tab_ENV_EVENTS_5_10 <- tab_ENV_EVENTS %>%
  filter(tab_ENV_EVENTS$event_wdw_p=="5" | tab_ENV_EVENTS$event_wdw_p=="10" | tab_ENV_EVENTS$event_wdw_p=="15")

png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/bars_all.jpg", width = 419, height = 475)

ggplot(tab_ENV_EVENTS_5_10, aes(x=1, y=avg, fill=factor(event_wdw_p))) +
  geom_bar(stat="identity", position="dodge", width = 0.5) +
  geom_errorbar(aes(ymin=avg-Std_err, ymax=avg+Std_err), position=position_dodge(width=0.5), width=0.25) +
  scale_fill_manual(values=c("dodgerblue2",
                             "green4",
                             "gold")) +
  labs(y="ACAR (%)", fill="Event window", x="All events \n n of events: 8645") +
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 20))
dev.off()
####

tab_ENV_EVENTS_cat_all_5_10 <- tab_ENV_EVENTS_cat_all %>%
  filter(tab_ENV_EVENTS_cat_all$event_wdw_p=="5" | tab_ENV_EVENTS_cat_all$event_wdw_p=="10" | tab_ENV_EVENTS_cat_all$event_wdw_p=="15")


# Specify the order of categories
#category_order <- c("ENVIRONMENTAL_IMPACT","ENVIRONMENTAL_IMPACT","ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION", "CLIMATE_CHANGE_INACTION","CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT", "RESOURCE_MANAGEMENT","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
#category_labels <- c("Environmental impact","Environmental impact","Environmental impact", "Climate change inaction","Climate change inaction","Climate change inaction", "Resource management","Resource management","Resource management", "Environmental reporting and transparency","Environmental reporting and transparency","Environmental reporting and transparency")

# Create a new column with the combined label
#category_labels <- paste(category_labels, "\nn of events:", tab_ENV_EVENTS_cat_all_5_10$n_event)

# Convert categories to a factor with the specified order
#tab_ENV_EVENTS_cat_all_5_10$category <- factor(tab_ENV_EVENTS_cat_all_5_10$category, levels = category_order, labels=category_labels)

png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/bars_cat.jpg", width = 1200, height = 536)

ggplot(tab_ENV_EVENTS_cat_all_5_10, aes(x=category, y=avg, fill=factor(event_wdw_p))) +
  geom_bar(stat="identity", position="dodge", width = 0.5) +
  geom_errorbar(aes(ymin=avg-Std_err, ymax=avg+Std_err), position=position_dodge(width=0.5), width=0.25) +
  geom_errorbar(aes(ymin=avg-(Std_err*1.96), ymax=avg+(Std_err*1.96)), position=position_dodge(width=0.5), width=0.25,linetype="dashed",color = "red") +
  scale_fill_manual(values=c("dodgerblue2",
                             "green4",
                             "gold")) +
  labs(y="ACAR (%)", fill="Event window") +
  theme_minimal()+
  theme(text = element_text(size = 20))
dev.off()


#Heatmap 5wdw

tab_ENV_EVENTS_cat_all_5_eco <- tab_ENV_EVENTS_bus_sect_cat_all %>%
  filter(tab_ENV_EVENTS_bus_sect_cat_all$event_wdw_p=="5" & tab_ENV_EVENTS_bus_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_5_eco$category <- factor(tab_ENV_EVENTS_cat_all_5_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_5_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_bus_5.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_cat_all_5_eco, aes(x=category, y=business_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                      midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_5_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("5 days window")+
  theme_minimal()+
  theme(text = element_text(size = 20))
dev.off()


#10 wdw

tab_ENV_EVENTS_cat_all_10_eco <- tab_ENV_EVENTS_bus_sect_cat_all %>%
  filter(tab_ENV_EVENTS_bus_sect_cat_all$event_wdw_p=="10" & tab_ENV_EVENTS_bus_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_10_eco$category <- factor(tab_ENV_EVENTS_cat_all_10_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_10_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_bus_10.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_cat_all_10_eco, aes(x=category, y=business_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_10_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("10 days window")+
  theme_minimal()+  # Adjust vertical justification for better alignment
  theme(text = element_text(size = 20))
dev.off()


#15 wdw

tab_ENV_EVENTS_cat_all_15_eco <- tab_ENV_EVENTS_bus_sect_cat_all %>%
  filter(tab_ENV_EVENTS_bus_sect_cat_all$event_wdw_p=="15" & tab_ENV_EVENTS_bus_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_15_eco$category <- factor(tab_ENV_EVENTS_cat_all_15_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_15_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_bus_15.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_cat_all_15_eco, aes(x=category, y=business_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_15_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("15 days window")+
  theme_minimal()+  # Adjust vertical justification for better alignment
  theme(text = element_text(size = 20))
dev.off()





#Heatmap 5wdw eco cat

### STATS by business sector AND category
rm("jj")
rm("jjj")

tab_ENV_EVENTS_eco_sect_cat_all <- data.frame(numeric(0))

for ( jjj in 143:146) {
  
  for ( jj in 1:length(the_e_sectors)) {
    
    tab_ENV_EVENTS_eco_sect_cat <- CAR_all %>%
      filter(CAR_all[,jjj]=="T" & CAR_all$TRBC.Economic.Sector.Name==the_e_sectors[jj]) %>%
      left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
      group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
      summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
                Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
                min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
                VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY),category = colnames(CAR_all[jjj]), business_sector = the_e_sectors[jj],
                Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
      )
    tab_ENV_EVENTS_eco_sect_cat$variable <- "CAR"
    
    tab_ENV_EVENTS_eco_sect_cat_all <-rbind(tab_ENV_EVENTS_eco_sect_cat_all,tab_ENV_EVENTS_eco_sect_cat)
  }
}

tab_ENV_EVENTS_cat_all_5_eco <- tab_ENV_EVENTS_eco_sect_cat_all %>%
  filter(tab_ENV_EVENTS_eco_sect_cat_all$event_wdw_p=="5" & tab_ENV_EVENTS_eco_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_5_eco$category <- factor(tab_ENV_EVENTS_cat_all_5_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_5_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_eco_5.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_cat_all_5_eco, aes(x=category, y=business_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_5_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("5 days window")+
  theme_minimal()+
  theme(text = element_text(size = 20))
dev.off()


tab_ENV_EVENTS_cat_all_10_eco <- tab_ENV_EVENTS_eco_sect_cat_all %>%
  filter(tab_ENV_EVENTS_eco_sect_cat_all$event_wdw_p=="10" & tab_ENV_EVENTS_eco_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_10_eco$category <- factor(tab_ENV_EVENTS_cat_all_10_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_10_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_eco_10.jpg", width = 1200, height = 536)

ggplot(tab_ENV_EVENTS_cat_all_10_eco, aes(x=category, y=business_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_10_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("10 days window")+
  theme_minimal()+
  theme(text = element_text(size = 20))
dev.off()

tab_ENV_EVENTS_cat_all_15_eco <- tab_ENV_EVENTS_eco_sect_cat_all %>%
  filter(tab_ENV_EVENTS_eco_sect_cat_all$event_wdw_p=="15" & tab_ENV_EVENTS_eco_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_15_eco$category <- factor(tab_ENV_EVENTS_cat_all_15_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_15_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_eco_15.jpg", width = 1200, height = 536)

ggplot(tab_ENV_EVENTS_cat_all_15_eco, aes(x=category, y=business_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_15_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("15 days window")+
  theme_minimal()+
  theme(text = element_text(size = 20))
dev.off()


#SEVERITY


tab_ENV_EVENTS_sev <- CAR_all %>%
  left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
  group_by(est_wdw,event_wdw_m,event_wdw_p,severity) %>%
  summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
            Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
            min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
            VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))), n_event = n_distinct(KEY),
            Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
  )
tab_ENV_EVENTS_sev$variable <- "CAR"


category_labels <- c(paste("Severity 1 \nn of events:",tab_ENV_EVENTS_sev$n_event[dim(tab_ENV_EVENTS_sev)[1]-2]),paste("Severity 2 \nn of events:",tab_ENV_EVENTS_sev$n_event[dim(tab_ENV_EVENTS_sev)[1]-1]),paste("Severity 3 \nn of events:",tab_ENV_EVENTS_sev$n_event[dim(tab_ENV_EVENTS_sev)[1]]))
tab_ENV_EVENTS_sev$severity <- factor(tab_ENV_EVENTS_sev$severity, labels=category_labels)


png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/ACAR_sev_ts.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_sev, aes(x = event_wdw_p, y = avg, fill=severity)) +
  geom_ribbon(aes(ymin = avg - Std_err, ymax = avg + Std_err, fill = severity), alpha = 0.4) +
  geom_ribbon(aes(ymin = avg - (Std_err*1.96), ymax = avg + (Std_err*1.96), fill = severity), alpha = 0.2) +
  geom_line(aes(color = severity)) +
  labs(x = "Days", y = "ACAR (%)") +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", size=0.5) +
  scale_x_continuous(breaks=seq(0, 30, 5)) +
  scale_y_continuous(breaks = c(0, -0.5, -1, -1.5,-2, -2.5, -3, -3.5)) +  # Set specific values on the y-axis
  guides(fill = FALSE, color = FALSE) +  # Remove legends for fill and color
  theme(text = element_text(size = 20), legend.position = "none") +  # Ensure legend is removed
  facet_wrap(~severity, scales = "fixed")
dev.off()


#REACH


tab_ENV_EVENTS_reach <- CAR_all %>%
  left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
  group_by(est_wdw,event_wdw_m,event_wdw_p,`Reach.(reach)`,NACE1) %>%
  summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
            Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
            min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
            VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))), n_event = n_distinct(KEY),
            Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
  )
tab_ENV_EVENTS_reach$variable <- "CAR"


tab_ENV_EVENTS_reach <- tab_ENV_EVENTS_reach %>%
  filter(NACE1=="FINANCIAL AND INSURANCE ACTIVITIES")

category_labels <- c(paste("Reach 1 \nn of events:",tab_ENV_EVENTS_reach$n_event[1]),paste("Reach 2 \nn of events:",tab_ENV_EVENTS_reach$n_event[2]),paste("Reach 3 \nn of events:",tab_ENV_EVENTS_reach$n_event[3]))
tab_ENV_EVENTS_reach$reach <- factor(tab_ENV_EVENTS_reach$`Reach.(reach)`, labels=category_labels)


png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/ACAR_reach_ts.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_reach, aes(x = event_wdw_p, y = avg, fill=reach)) +
  geom_ribbon(aes(ymin = avg - Std_err, ymax = avg + Std_err, fill = reach), alpha = 0.4) +
  geom_ribbon(aes(ymin = avg - (Std_err*1.96), ymax = avg + (Std_err*1.96), fill = reach), alpha = 0.2) +
  geom_line(aes(color = reach)) +
  labs(x = "Days", y = "ACAR (%)") +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", size=0.5) +
  scale_x_continuous(breaks=seq(0, 15, 1)) +
  scale_y_continuous(breaks = c(0,-2,-4,-6,-8)) +  # Set specific values on the y-axis
  guides(fill = FALSE, color = FALSE) +  # Remove legends for fill and color
  theme(text = element_text(size = 20), legend.position = "none") +  # Ensure legend is removed
  facet_wrap(~reach, scales = "fixed")
dev.off()


fileout <- paste0(folder,"OUTPUT/")
file <- paste0 ( fileout, "CYBER_count_reach_fin.xlsx") 
write.xlsx(list( 
  reach = tab_ENV_EVENTS_reach),
  file)


#novelty


tab_ENV_EVENTS_novelty <- CAR_all %>%
  left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
  group_by(est_wdw,event_wdw_m,event_wdw_p,novelty) %>%
  summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
            Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
            min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
            VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))), n_event = n_distinct(KEY),
            Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
  )
tab_ENV_EVENTS_novelty$variable <- "CAR"


category_labels <- c(paste("Novelty 1 \nn of events:",tab_ENV_EVENTS_novelty$n_event[31]),paste("Novelty 2 \nn of events:",tab_ENV_EVENTS_novelty$n_event[32]))
tab_ENV_EVENTS_novelty$novelty <- factor(tab_ENV_EVENTS_novelty$novelty, labels=category_labels)


png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/ACAR_novelty_ts.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_novelty, aes(x = event_wdw_p, y = avg, fill=novelty)) +
  geom_ribbon(aes(ymin = avg - Std_err, ymax = avg + Std_err, fill = novelty), alpha = 0.4) +
  geom_ribbon(aes(ymin = avg - (Std_err*1.96), ymax = avg + (Std_err*1.96), fill = novelty), alpha = 0.2) +
  geom_line(aes(color = novelty)) +
  labs(x = "Days", y = "ACAR (%)") +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", size=0.5) +
  scale_x_continuous(breaks=seq(0, 15, 1)) +
  scale_y_continuous(breaks = c(0, -0.5, -1, -1.5,-2, -2.5, -3, -3.5)) +  # Set specific values on the y-axis
  guides(fill = FALSE, color = FALSE) +  # Remove legends for fill and color
  theme(text = element_text(size = 20), legend.position = "none") +  # Ensure legend is removed
  facet_wrap(~novelty, scales = "fixed")
dev.off()


#STD


tab_ENV_EVENTS_std <- CAR_all %>%
  left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
  group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
  summarise(std = sd(CARi, na.rm = TRUE))
tab_ENV_EVENTS_std$variable <- "std CAR"

png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/ACAR_ts.jpg", width = 713, height = 475)

ggplot(tab_ENV_EVENTS_std, aes(x = event_wdw_p, y = std)) +
  geom_line(color="DarkGreen",size=0.5) +
  labs(x = "Days", y = "Std CAR") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(0,15,1))+
  guides(color = FALSE)+
  theme(text = element_text(size = 20))
dev.off()

ggplot(tab_ENV_EVENTS, aes(x = event_wdw_p, y = std)) +
  geom_point(color="DarkGreen") +
  labs(x = "Days", y = "Std of AR cumulated  est wdw (%)") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(0,15,1))+
  guides(color = FALSE)+
  theme(text = element_text(size = 20))



####HEATMAPS WITH SEVERITY 3

### STATS by business sector AND category
rm("jj")
rm("jjj")
CAR_all_sev = CAR_all[CAR_all$severity=="3",]
tab_ENV_EVENTS_bus_sect_cat_sev_all <- data.frame(numeric(0))

for ( jjj in 143:146) {
  
  for ( jj in 1:length(the_b_sectors)) {
    
    tab_ENV_EVENTS_bus_sect_cat_sev <- CAR_all_sev %>%
      filter(CAR_all_sev[,jjj]=="T" & CAR_all_sev$TRBC.Business.Sector.Name==the_b_sectors[jj]) %>%
      left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
      group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
      summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
                Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
                min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
                VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY),category = colnames(CAR_all[jjj]), business_sector = the_b_sectors[jj],
                Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
      )
    tab_ENV_EVENTS_bus_sect_cat_sev$variable <- "CAR"
    
    tab_ENV_EVENTS_bus_sect_cat_sev_all <-rbind(tab_ENV_EVENTS_bus_sect_cat_sev_all,tab_ENV_EVENTS_bus_sect_cat_sev)
  }
}



#Heatmap 5wdw

tab_ENV_EVENTS_cat_all_5_eco <- tab_ENV_EVENTS_bus_sect_cat_sev_all %>%
  filter(tab_ENV_EVENTS_bus_sect_cat_sev_all$event_wdw_p=="5" & tab_ENV_EVENTS_bus_sect_cat_sev_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_5_eco$category <- factor(tab_ENV_EVENTS_cat_all_5_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_5_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_bus_5_sev3.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_cat_all_5_eco, aes(x=category, y=business_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_5_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("5 days window")+
  theme_minimal()+
  theme(text = element_text(size = 20))
dev.off()


#10 wdw

tab_ENV_EVENTS_cat_all_10_eco <- tab_ENV_EVENTS_bus_sect_cat_sev_all %>%
  filter(tab_ENV_EVENTS_bus_sect_cat_sev_all$event_wdw_p=="10" & tab_ENV_EVENTS_bus_sect_cat_sev_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_10_eco$category <- factor(tab_ENV_EVENTS_cat_all_10_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_10_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_bus_10_sev3.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_cat_all_10_eco, aes(x=category, y=business_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_10_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("10 days window")+
  theme_minimal()+  # Adjust vertical justification for better alignment
  theme(text = element_text(size = 20))
dev.off()


#15 wdw

tab_ENV_EVENTS_cat_all_15_eco <- tab_ENV_EVENTS_bus_sect_cat_sev_all %>%
  filter(tab_ENV_EVENTS_bus_sect_cat_sev_all$event_wdw_p=="15" & tab_ENV_EVENTS_bus_sect_cat_sev_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_15_eco$category <- factor(tab_ENV_EVENTS_cat_all_15_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_15_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_bus_15_sev3.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_cat_all_15_eco, aes(x=category, y=business_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_15_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("15 days window")+
  theme_minimal()+  # Adjust vertical justification for better alignment
  theme(text = element_text(size = 20))
dev.off()





#Heatmap 5wdw eco cat

### STATS by business sector AND category
rm("jj")
rm("jjj")
tab_ENV_EVENTS_eco_sect_cat_sev_all <- data.frame(numeric(0))

for ( jjj in 143:146) {
  
  for ( jj in 1:length(the_e_sectors)) {
    
    tab_ENV_EVENTS_eco_sect_cat_sev <- CAR_all_sev %>%
      filter(CAR_all_sev[,jjj]=="T" & CAR_all_sev$TRBC.Economic.Sector.Name==the_e_sectors[jj]) %>%
      left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
      group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
      summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
                Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
                min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
                VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY),category = colnames(CAR_all[jjj]), business_sector = the_e_sectors[jj],
                Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
      )
    tab_ENV_EVENTS_eco_sect_cat_sev$variable <- "CAR"
    
    tab_ENV_EVENTS_eco_sect_cat_sev_all <-rbind(tab_ENV_EVENTS_eco_sect_cat_sev_all,tab_ENV_EVENTS_eco_sect_cat_sev)
  }
}

tab_ENV_EVENTS_cat_all_5_eco <- tab_ENV_EVENTS_eco_sect_cat_sev_all %>%
  filter(tab_ENV_EVENTS_eco_sect_cat_sev_all$event_wdw_p=="5" & tab_ENV_EVENTS_eco_sect_cat_sev_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_5_eco$category <- factor(tab_ENV_EVENTS_cat_all_5_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_5_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_eco_5_sev3.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_cat_all_5_eco, aes(x=category, y=business_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_5_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("5 days window")+
  theme_minimal()+
  theme(text = element_text(size = 20))
dev.off()


tab_ENV_EVENTS_cat_all_10_eco <- tab_ENV_EVENTS_eco_sect_cat_sev_all %>%
  filter(tab_ENV_EVENTS_eco_sect_cat_sev_all$event_wdw_p=="10" & tab_ENV_EVENTS_eco_sect_cat_sev_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_10_eco$category <- factor(tab_ENV_EVENTS_cat_all_10_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_10_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_eco_10_sev3.jpg", width = 1200, height = 536)

ggplot(tab_ENV_EVENTS_cat_all_10_eco, aes(x=category, y=business_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_10_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("10 days window")+
  theme_minimal()+
  theme(text = element_text(size = 20))
dev.off()

tab_ENV_EVENTS_cat_all_15_eco <- tab_ENV_EVENTS_eco_sect_cat_sev_all %>%
  filter(tab_ENV_EVENTS_eco_sect_cat_sev_all$event_wdw_p=="15" & tab_ENV_EVENTS_eco_sect_cat_sev_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_15_eco$category <- factor(tab_ENV_EVENTS_cat_all_15_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_15_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_eco_15_sev3.jpg", width = 1200, height = 536)

ggplot(tab_ENV_EVENTS_cat_all_15_eco, aes(x=category, y=business_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_15_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("15 days window")+
  theme_minimal()+
  theme(text = element_text(size = 20))
dev.off()





#Heatmap 5wdw eco cat

tab_ENV_EVENTS_cat_all_5_eco <- tab_ENV_EVENTS_NACE_sect_cat_all %>%
  filter(tab_ENV_EVENTS_NACE_sect_cat_all$event_wdw_p=="5" & tab_ENV_EVENTS_NACE_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_5_eco$category <- factor(tab_ENV_EVENTS_cat_all_5_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_5_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_NACE_5.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_cat_all_5_eco, aes(x=category, y=NACE_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_5_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("5 days window")+
  theme_minimal()+
  theme(text = element_text(size = 12))
dev.off()


tab_ENV_EVENTS_cat_all_10_eco <- tab_ENV_EVENTS_NACE_sect_cat_all %>%
  filter(tab_ENV_EVENTS_NACE_sect_cat_all$event_wdw_p=="10" & tab_ENV_EVENTS_NACE_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_10_eco$category <- factor(tab_ENV_EVENTS_cat_all_10_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_10_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_NACE_10.jpg", width = 1200, height = 536)

ggplot(tab_ENV_EVENTS_cat_all_10_eco, aes(x=category, y=NACE_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_10_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("10 days window")+
  theme_minimal()+
  theme(text = element_text(size = 12))
dev.off()

tab_ENV_EVENTS_cat_all_15_eco <- tab_ENV_EVENTS_NACE_sect_cat_all %>%
  filter(tab_ENV_EVENTS_NACE_sect_cat_all$event_wdw_p=="15" & tab_ENV_EVENTS_NACE_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_15_eco$category <- factor(tab_ENV_EVENTS_cat_all_15_eco$category, levels = category_order, labels=category_labels)


my_midpoint=mean(tab_ENV_EVENTS_cat_all_15_eco$avg)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_NACE_15.jpg", width = 1200, height = 536)

ggplot(tab_ENV_EVENTS_cat_all_15_eco, aes(x=category, y=NACE_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_15_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
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
                VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY),category = colnames(CAR_all[jjj]), NACE_sector = the_n_sectors[jj],
                Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
      )
    tab_ENV_EVENTS_NACE_sect_cat$variable <- "CAR"
    
    tab_ENV_EVENTS_NACE_sect_cat_all <-rbind(tab_ENV_EVENTS_NACE_sect_cat_all,tab_ENV_EVENTS_NACE_sect_cat)
  }
}






#Heatmap 5wdw eco cat

tab_ENV_EVENTS_cat_all_5_eco <- tab_ENV_EVENTS_NACE_sect_cat_all %>%
  filter(tab_ENV_EVENTS_NACE_sect_cat_all$event_wdw_p=="5" & tab_ENV_EVENTS_NACE_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_5_eco$category <- factor(tab_ENV_EVENTS_cat_all_5_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_5_eco$avg)
tab_ENV_EVENTS_cat_all_5_eco<-tab_ENV_EVENTS_cat_all_5_eco %>%
  filter(tab_ENV_EVENTS_cat_all_5_eco$n_event>51)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_NACE2_5.jpg", width = 1200, height = 536)
ggplot(tab_ENV_EVENTS_cat_all_5_eco, aes(x=category, y=NACE_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_5_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("5 days window")+
  theme_minimal()+
  theme(text = element_text(size = 12))
dev.off()


tab_ENV_EVENTS_cat_all_10_eco <- tab_ENV_EVENTS_NACE_sect_cat_all %>%
  filter(tab_ENV_EVENTS_NACE_sect_cat_all$event_wdw_p=="10" & tab_ENV_EVENTS_NACE_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_10_eco$category <- factor(tab_ENV_EVENTS_cat_all_10_eco$category, levels = category_order, labels=category_labels)




my_midpoint=mean(tab_ENV_EVENTS_cat_all_10_eco$avg)
tab_ENV_EVENTS_cat_all_10_eco<-tab_ENV_EVENTS_cat_all_10_eco %>%
  filter(tab_ENV_EVENTS_cat_all_10_eco$n_event>51)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_NACE2_10.jpg", width = 1200, height = 536)

ggplot(tab_ENV_EVENTS_cat_all_10_eco, aes(x=category, y=NACE_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_10_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("10 days window")+
  theme_minimal()+
  theme(text = element_text(size = 12))
dev.off()

tab_ENV_EVENTS_cat_all_15_eco <- tab_ENV_EVENTS_NACE_sect_cat_all %>%
  filter(tab_ENV_EVENTS_NACE_sect_cat_all$event_wdw_p=="15" & tab_ENV_EVENTS_NACE_sect_cat_all$Reject=="1")

category_order <- c("ENVIRONMENTAL_IMPACT", "CLIMATE_CHANGE_INACTION","RESOURCE_MANAGEMENT","ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY")
category_labels <- c("Environmental \n impact", "Climate change \n inaction", "Resource \n management", "Environmental reporting \n and transparency")

tab_ENV_EVENTS_cat_all_15_eco$category <- factor(tab_ENV_EVENTS_cat_all_15_eco$category, levels = category_order, labels=category_labels)


my_midpoint=mean(tab_ENV_EVENTS_cat_all_15_eco$avg)
tab_ENV_EVENTS_cat_all_15_eco<-tab_ENV_EVENTS_cat_all_15_eco %>%
  filter(tab_ENV_EVENTS_cat_all_15_eco$n_event>51)
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/heat_NACE2_15.jpg", width = 1200, height = 536)

ggplot(tab_ENV_EVENTS_cat_all_15_eco, aes(x=category, y=NACE_sector, fill=avg)) +
  geom_tile() +
  geom_text(aes(label=paste("n of events:", n_event)), color="black", size=5) +
  scale_fill_gradient2(low="red", mid="white", high="white", 
                       midpoint=0,limits=c(min(tab_ENV_EVENTS_cat_all_15_eco$avg),0)) +
  labs(x="Category", y="Sector", fill="ACAR (%)") +
  ggtitle("15 days window")+
  theme_minimal()+
  theme(text = element_text(size = 12))
dev.off() # Ensure panels are side by side










