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
if ( !require (changepoint) ) {
  install.packages ( "changepoint" )
}

if ( !require (tidyr) ) {
  install.packages ( "tidyr" )
}


data <- read.xlsx("C:/Users/ivan.delorenzo/Downloads/2007_2023_EU_events.xlsx")

#colnames(data[3:513])

EVENTS <- data

EVENTS$Date <-  as.Date(EVENTS$`Incident.Date.(incident_date)`, origin = "2007-01-01")

EVENTS_E = EVENTS %>%
  filter(EVENTS$`ENVIRONMENT.(environment)`=="T")



drafted1 <- as.Date("2015-11-30")  # Replace with your actual date
drafted2 <- as.Date("2015-12-12")  # Replace with your actual date
signed <- as.Date("2016-04-22")  # Replace with your actual date
effective <- as.Date("2016-11-04")  # Replace with your actual date


EVENTS_E <- EVENTS_E %>%
  mutate(Year = year(Date))

# Filter out rows where ISIN is "NA"
data_filtered <- EVENTS_E %>%
  filter(`Primary.ISIN.(primary_isin)`!= "NA") %>%
  filter(Date<drafted1)

# Compute the number of events for each ISIN and Year
event_count_per_year <- data_filtered %>%
  group_by(`Primary.ISIN.(primary_isin)`, Year) %>%
  summarize(number_of_events = n(), .groups = 'drop')

# Compute the average number of events per year for each ISIN
average_events_per_isin <- event_count_per_year %>%
  group_by(`Primary.ISIN.(primary_isin)`) %>%
  summarize(average_number_of_events = mean(number_of_events), .groups = 'drop')

# Print the results
print(event_count_per_year)
mean(average_events_per_isin$average_number_of_events)



#Number of incidents by year

ggplot(EVENTS_E, aes(x = Date)) +
  geom_bar(position = "dodge",fill="blue") + 
  theme_bw() + 
  labs(y = "Number of events",title="Global")+
  geom_vline(xintercept = as.numeric(drafted1), linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = as.numeric(drafted2), linetype = "dashed", color = "red", size = 1) + 
  geom_vline(xintercept = as.numeric(signed), linetype = "dashed", color = "red", size = 1) + 
  geom_vline(xintercept = as.numeric(effective), linetype = "dashed", color = "red", size = 1)


#quarters



EVENTS_E <- EVENTS_E %>%
  mutate(Quarter = paste0(year(Date), " Q", quarter(Date)))

# Summarize the number of events per quarter
quarterly_events <- EVENTS_E %>%
  group_by(Quarter) %>%
  summarize(Number_of_events = n())


Qdrafted1 <- paste0(year(drafted1), " Q", quarter(drafted1))
Qsigned <- paste0(year(signed), " Q", quarter(signed))
Qeffective <- paste0(year(effective), " Q", quarter(effective))

quarterly_events_pre_Paris <- quarterly_events %>%
  filter(quarterly_events$Quarter<Qdrafted1)

quarterly_events_post_Paris <- quarterly_events %>%
  filter(quarterly_events$Quarter>Qeffective)




# Plot the number of events each quarter
png("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/_graphs/2007_2023_Q_EU.jpg", width = 1200, height = 700)
ggplot(quarterly_events, aes(x = Quarter, y = Number_of_events)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_bw() +
  labs(x = "Quarter", y = "Number of E events",
       title=paste("EU E events. Post-Paris Agreement events are",round(sum(quarterly_events_post_Paris$Number_of_events)/sum(quarterly_events_pre_Paris$Number_of_events),2),"times the Pre-Paris Agreement events")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
geom_vline(xintercept = Qdrafted1, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = Qsigned, linetype = "dashed", color = "red", size = 1) + 
  geom_vline(xintercept = Qeffective, linetype = "dashed", color = "red", size = 1)
dev.off()



# Compute the number of events for each ISIN and Year
event_count_per_day <- EVENTS_E %>%
  group_by(Date) %>%
  summarize(number_of_events_date = n(), .groups = 'drop')

policy_events <- read.xlsx("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/DATA/db_dates_no_NA_short_descr.xlsx", sheet = "DATA" )

policy_events <- policy_events %>%
  filter(policy_events$Country=="WRD" | policy_events$Country=="EUR")

policy_events$Date1 = as.Date(policy_events$Date, origin = "1899-12-30" ) 

ggplot(EVENTS_E, aes(x = Date)) +
  geom_bar(position = "dodge",fill="blue") + 
  theme_bw() + 
  labs(y = "Number of events",title="Global")+
  geom_vline(xintercept = as.numeric(policy_events$Date1), linetype = "dashed", color = "red", size = 1)
 



# Function to filter data and calculate means
filter_and_calculate_means <- function(date) {
  start_date <- as.Date(date) - 7
  end_date <- as.Date(date) + 7
  
  filtered_data <- EVENTS_E %>%
    filter(Date >= start_date & Date <= end_date) %>%
    mutate(RelativeDay = as.numeric(Date - as.Date(date)),
           Period = ifelse(Date < as.Date(date), "Before", "After"),
           PolicyDate = date)
  
  means <- filtered_data %>%
    group_by(PolicyDate, Period) %>%
    summarize(MeanEvents = mean(n())) %>%
    pivot_wider(names_from = Period, values_from = MeanEvents)
  
  list(data = filtered_data, means = means)
}

# Apply function to each policy date
results <- lapply(policy_events$Date1, filter_and_calculate_means)

# Combine data and means
combined_data <- bind_rows(lapply(results, `[[`, "data"))
mean_data <- bind_rows(lapply(results, `[[`, "means"))

# Plot with faceting

policy_events$PolicyDate <- policy_events$Date1
combined_data <- combined_data %>%
  left_join(policy_events, by = "PolicyDate")

# Then, use the Summary.description column in facet_wrap
p <- ggplot(combined_data, aes(x = RelativeDay)) +
  geom_bar(position = "dodge", fill = "blue") + 
  theme_bw() + 
  labs(y = "Number of events", x = "Days Relative to Policy Date", title = "Global Events around Policy Dates") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  scale_x_continuous(breaks = seq(-7, 7, by = 1), limits = c(-7, 7)) +
  facet_wrap(~ Summary.description, scales = "free_y", ncol = 5)


p <- ggplot(combined_data, aes(x = RelativeDay)) +
  geom_bar(position = "dodge", fill = "blue") + 
  theme_bw() + 
  labs(y = "Number of events", x = "Days Relative to Policy Date", title = "Global Events around Policy Dates") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  scale_x_continuous(breaks = seq(-7, 7, by = 1), limits = c(-7, 7)) +
  facet_wrap(~ PolicyDate, scales = "free_y", ncol = 5)

# Add mean text annotations
#for(i in 1:nrow(mean_data)) {
#  p <- p + 
#    annotate("text", x = -10, y = max(combined_data$Count, na.rm = TRUE) * 0.8, 
#             label = paste("Mean Before:", round(mean_data$Before[i], 2)), size = 1, color = "blue") +
#    annotate("text", x = 10, y = max(combined_data$Count, na.rm = TRUE) * 0.8, 
#             label = paste("Mean After:", round(mean_data$After[i], 2)), size = 1, color = "blue")
#}

print(p)

##TESTS

# Function to filter data and calculate means
filter_and_calculate_means <- function(date) {
  start_date <- as.Date(date) - 7
  end_date <- as.Date(date) + 7
  
  filtered_data <- EVENTS_E %>%
    filter(Date >= start_date & Date <= end_date) %>%
    mutate(RelativeDay = as.numeric(Date - as.Date(date)),
           Period = ifelse(Date < as.Date(date), "Before", "After"),
           PolicyDate = date)
  
  filtered_data
}

# Apply function to each policy date and combine data
combined_data <- bind_rows(lapply(policy_events$Date1, filter_and_calculate_means))

# Summarize the mean number of events before and after each policy date
mean_data <- combined_data %>%
  group_by(PolicyDate, Period) %>%
  summarize(MeanEvents = mean(n()))

# Separate before and after data
before_data <- mean_data %>% filter(Period == "Before")
after_data <- mean_data %>% filter(Period == "After")

# Perform paired t-test
t_test_result <- t.test(after_data$MeanEvents, before_data$MeanEvents, paired = TRUE)
print(t_test_result)

# Perform Wilcoxon signed-rank test (non-parametric alternative)
wilcox_test_result <- wilcox.test(after_data$MeanEvents, before_data$MeanEvents, paired = TRUE)
print(wilcox_test_result)

