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

if(!require(readr)){ 
  install.packages("readr")
}


if(!require(tidyr)){ 
  install.packages("tidyr")
}

if(!require(egg)){ 
  install.packages("egg")
}

library(purrr)
library(stringr)


###############################################################################

ISINs <- read.xlsx("G:/CLIMATE RISK & ESG/PROGETTI/Reputation_risk/REPUTATION/CODE/matched_names_results_emm_num_candidates_2014_2025_checked.xlsx")

ISINs <- ISINs %>% arrange(uid)

ISINs$ISIN <- ifelse(ISINs$check==2, ISINs$fixed.ISIN, ISINs$ISIN.Code)

ISINs <- ISINs %>% select(name,ISIN,check)

EVENTS1 <- read.xlsx("G:/CLIMATE RISK & ESG/PROGETTI/Reputation_risk/REPUTATION/CODE/Cyber Events Database - 2014-2024 + Jan_Aug_Sept 2025.xlsx",detectDates = FALSE)

EVENTS1 <- EVENTS1 %>%
  mutate(
    event_date = case_when(
      grepl("^[0-9]+$", event_date) ~ as.Date(as.numeric(event_date), origin = "1899-12-30"),
      grepl("^\\d{4}-\\d{2}-\\d{2}$", event_date) ~ ymd(event_date),
      TRUE ~ NA_Date_
    )
  )

EVENTS1 <- EVENTS1 %>%
  mutate(
    reported_date = case_when(
      grepl("^[0-9]+$", reported_date) ~ as.Date(as.numeric(reported_date), origin = "1899-12-30"),
      grepl("^\\d{4}-\\d{2}-\\d{2}$", reported_date) ~ ymd(reported_date),
      TRUE ~ NA_Date_
    )
  )

EVENTS1$event_date[14733]<-"2025-07-24"
EVENTS1$event_date[14734]<-"2025-06-01"
EVENTS1$reported_date[14733]<-"2025-08-05"


EVENTS <- cbind(EVENTS1,ISINs)

EVENTS_check <- ifelse(EVENTS$name!=EVENTS$organization,1,0)

the_sum<-sum(EVENTS_check) #ok

EVENTS <- EVENTS %>% filter(check!=0)


file <- "G:/CLIMATE RISK & ESG/PROGETTI/Reputation_risk/REPUTATION/OUTPUT/EVENTS_DIRECT_2014_2025.xlsx"
write.xlsx(list( 
  events=EVENTS),
  file)


file <- "G:/CLIMATE RISK & ESG/PROGETTI/Reputation_risk/REPUTATION/OUTPUT/EVENTS_DIRECT_2014_2025_ISINs.xlsx"
write.xlsx(list( 
  ISIN=unique(EVENTS$ISIN)),
  file)