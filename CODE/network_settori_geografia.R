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

folder <- "G:/CLIMATE RISK & ESG/PROGETTI/Reputation_risk/REPUTATION/"


EVENTS1    <- read.xlsx( paste0(folder,"DATA/maryland_db_with_ISIN_50k_2.xlsx"), sheet = "events", colNames = TRUE, detectDates = TRUE, na.strings = c( "NA", "#N/A", "N/D" ) )

sectors<- read.xlsx("G:/CLIMATE RISK & ESG/PROGETTI/Reputation_risk/REPUTATION/DATA/Sectors_MARYLAND.xlsx", sheet = "Sheet3")


EVENTS <- EVENTS1 %>%
  inner_join(
    sectors %>% select(X1, NACE.Classification, Country.of.Headquarters),
    by = c("NACE.Classification", "Country.of.Headquarters")
  )


EVENTS <- EVENTS %>% filter(NACE.Classification!="NULL")

EVENTS <- EVENTS %>%
  rename(
    primary_isin = X1,
    direct_isin = ISIN.Code
  )

EVENTS$Date <- as.Date(EVENTS$event_date, origin = "1970-01-01")
EVENTS <- EVENTS %>%
  mutate(story_id = sample(1e7:(1e8 - 1), n(), replace = FALSE))
EVENTS$ISIN_event_id = paste(EVENTS$primary_isin, EVENTS$story_id, sep = "_")


file <- "C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/EVENTS_spill_sector_geo.xlsx"
write.xlsx(list( 
  events=EVENTS),
  file)
