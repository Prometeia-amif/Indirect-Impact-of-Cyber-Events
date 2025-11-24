db_ISIN_list <- read.xlsx("C:/Users/ivan.delorenzo/Downloads/scarico_Refinitiv_LEI_def.xlsx", sheet = "Scarico_LEI" ) 
db_ISIN_list2 <- read.xlsx("C:/Users/ivan.delorenzo/Downloads/scarico_Refinitiv_LEI_def.xlsx", sheet = "Scarico_ULT_PAR" ) 


myDB<-ResVar %>% 
  filter(ISIN%in%db_ISIN_list2$ISIN)


myISIN2=unique(myDB$ISIN)

myEvents2=unique(CAR_all$KEY)


signM<-CAR_all %>% 
  filter(ISIN%in%myISIN2)

signM_e<-CAR_all %>% 
  filter(KEY%in%signM$KEY)

signM_e <- signM_e %>%
  filter(Reject==1)

signM <- signM %>% filter(signM$Reject==1 & signM$CARi>0 & signM$NACE2!="Financial service activities, except insurance and pension funding")

BNP <- CAR_all %>% filter(CAR_all$incident_date=="2023-02-27" & CAR_all$company_name=="BNP Paribas SA")
BASF <- CAR_all %>% filter(CAR_all$incident_date=="2023-02-27" & CAR_all$company_name=="BASF SE (formerly BASF AG)")

tab_ENV_EVENTS <- BASF %>%
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

ggplot(tab_ENV_EVENTS, aes(x = event_wdw_p, y = avg)) +
  geom_ribbon(aes(ymin = avg-Std_err, ymax = avg+Std_err), fill='DarkGreen', alpha = 0.4) +
  geom_ribbon(aes(ymin = avg-(Std_err*1.96), ymax = avg+(Std_err*1.96)), fill='DarkGreen', alpha = 0.2) +
  geom_line(color="DarkGreen") +
  labs(x = "Days", y = "ACAR (%)") +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", 
             size=0.5)+
  scale_x_continuous(breaks=seq(0,30,5))+
  guides(color = FALSE)+
  theme(text = element_text(size = 20))


# Filter data for the specific banks
group_banks <- CAR_all %>%
  filter(NACE1 == "FINANCIAL AND INSURANCE ACTIVITIES" & Event == "8217689")

# Join and calculate statistics for all banks
tab_ENV_EVENTS <- group_banks %>%
  left_join(ResVar[, c("KEY", "Var")], by = c("KEY" = "KEY")) %>%
  group_by(company_name, est_wdw, event_wdw_m, event_wdw_p) %>%
  summarise(
    avg = mean(CARi, na.rm = TRUE),
    med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
    Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),
    Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
    min = min(CARi, na.rm = TRUE),
    max = max(CARi, na.rm = TRUE),
    VaR = mean(CARi_VaR, na.rm = TRUE),
    Std_err = sqrt((mean(Var * (event_wdw_m + event_wdw_p + 1), na.rm = TRUE)) * (1 / n_distinct(KEY))),
    n_event = n_distinct(KEY),
    Reject = ifelse(
      mean(CARi, na.rm = TRUE) / (sqrt((mean(Var * (event_wdw_m + event_wdw_p + 1), na.rm = TRUE)) * (1 / n_distinct(KEY)))) <= -1.960 |
        mean(CARi, na.rm = TRUE) / (sqrt((mean(Var * (event_wdw_m + event_wdw_p + 1), na.rm = TRUE)) * (1 / n_distinct(KEY)))) >= 1.960,
      1, 0
    )
  )

tab_ENV_EVENTS$variable <- "CAR"

# Plot all banks in a grid
ggplot(tab_ENV_EVENTS, aes(x = event_wdw_p, y = avg)) +
  geom_ribbon(aes(ymin = avg - Std_err, ymax = avg + Std_err), fill = 'DarkGreen', alpha = 0.4) +
  geom_ribbon(aes(ymin = avg - (Std_err * 1.96), ymax = avg + (Std_err * 1.96)), fill = 'DarkGreen', alpha = 0.2) +
  geom_line(color = "DarkGreen") +
  labs(x = "Days", y = "ACAR (%)") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  theme(text = element_text(size = 6)) +
  facet_wrap(~company_name, ncol = 6)  # Adjust 'ncol' for number of columns




# Filter data for banks with at least one Reject == 1
filtered_banks <- CAR_all %>%
  filter(NACE1 == "FINANCIAL AND INSURANCE ACTIVITIES" & Event == "8217689") %>%
  group_by(company_name) %>%
  filter(any(Reject == 1)) %>%
  ungroup()

# Join and calculate statistics for the filtered banks
tab_ENV_EVENTS <- filtered_banks %>%
  left_join(ResVar[, c("KEY", "Var")], by = c("KEY" = "KEY")) %>%
  group_by(company_name, est_wdw, event_wdw_m, event_wdw_p) %>%
  summarise(
    avg = mean(CARi, na.rm = TRUE),
    med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
    Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),
    Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
    min = min(CARi, na.rm = TRUE),
    max = max(CARi, na.rm = TRUE),
    VaR = mean(CARi_VaR, na.rm = TRUE),
    Std_err = sqrt((mean(Var * (event_wdw_m + event_wdw_p + 1), na.rm = TRUE)) * (1 / n_distinct(KEY))),
    n_event = n_distinct(KEY),
    Reject = ifelse(
      mean(CARi, na.rm = TRUE) / (sqrt((mean(Var * (event_wdw_m + event_wdw_p + 1), na.rm = TRUE)) * (1 / n_distinct(KEY)))) <= -1.960 |
        mean(CARi, na.rm = TRUE) / (sqrt((mean(Var * (event_wdw_m + event_wdw_p + 1), na.rm = TRUE)) * (1 / n_distinct(KEY)))) >= 1.960,
      1, 0
    )
  )

tab_ENV_EVENTS$variable <- "CAR"

# Plot only filtered banks
ggplot(tab_ENV_EVENTS, aes(x = event_wdw_p, y = avg)) +
  geom_ribbon(aes(ymin = avg - Std_err, ymax = avg + Std_err), fill = 'DarkGreen', alpha = 0.4) +
  geom_ribbon(aes(ymin = avg - (Std_err * 1.96), ymax = avg + (Std_err * 1.96)), fill = 'DarkGreen', alpha = 0.2) +
  geom_line(color = "DarkGreen") +
  labs(x = "Days", y = "ACAR (%)") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  theme(text = element_text(size = 8)) +
  facet_wrap(~company_name, ncol = 4)  # Adjust 'ncol' for the grid layout
