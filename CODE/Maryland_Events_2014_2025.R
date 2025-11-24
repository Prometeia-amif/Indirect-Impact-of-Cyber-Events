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




EVENTS   <- read.xlsx("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/DATA/Cyber Events Database - 2014-2024 + Jan_Aug_Sept 2025.xlsx", colNames = TRUE, detectDates = TRUE, na.strings = c( "NA", "#N/A", "N/D" ) )
EVENTS$Date <- as.Date(paste(EVENTS$year, EVENTS$month, "01", sep = "-"))
EVENTS$Quarter <- paste0(year(EVENTS$Date), " Q", quarter(EVENTS$Date))

events_by_quarter <- EVENTS %>%
  group_by(Quarter) %>%
  summarise(count = n())

ggplot(events_by_quarter, aes(x = Quarter, y = count)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Events by Quarter",
    x = "Quarter",
    y = "Number of Events"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



EVENTS$organization