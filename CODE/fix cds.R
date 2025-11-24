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
if ( !require ( tidyr) ) {
  install.packages ( "tidyr" )
}



data <- read.xlsx("G:/CLIMATE RISK & ESG/PROGETTI/Reputation_risk/reprisk/13678385 - CDS Premium of ISIN for Annalisa.xlsx",sheet = "Sheet1",colNames = TRUE, detectDates = TRUE, na.strings = c( "NA", "#N/A", "N/D" ))


# Texts to check
texts_to_check <- c(
  "The universe does not support the following fields: [PAR_MID1].",
  "User has no permission.",
  "The universe is not found."
)

# Function to check if any cell in a column contains any of the texts
contains_text <- function(column, texts) {
  any(sapply(texts, function(text) any(grepl(text, column, fixed = TRUE))))
}

# Identify columns to drop
columns_to_drop <- sapply(data, contains_text, texts = texts_to_check)

# Drop the identified columns
data <- data[, !columns_to_drop]


isins <- unique(data[, 1])

for (i in seq_along(data)) {
  # If the column name contains "X", rename it using values from column "A"
  if (grepl("X", names(data)[i])) {
    ric <- data[1,i]
    ric_pos<-which(data$Primary.CDS.RIC==ric)
    names(data)[i] <- data$ISIN[ric_pos]
  }
}


df= data[2:dim(data)[1],3:dim(data)[2]]

df <- df %>%
  mutate(across(contains("Timestamp"), ~ as.Date(.x, origin = "2017-12-01")))


df <- df[, !apply(df, 2, function(x) all(is.na(x)))]


is_date_column <- function(df, the_column) {
  inherits(df[,the_column], "Date")
}

timestamp_cols_all <- data.frame(numeric(0))

for (i in seq_along(df)) { 
if (is_date_column(df, i)) {
  
  timestamp_cols <-i
  timestamp_cols_all <- rbind(timestamp_cols_all,timestamp_cols)
}
  
}

data_cols_all <- data.frame(numeric(0))

for (i in seq_along(df)) { 
  if (is_date_column(df, i)=="FALSE") {
    
    data_cols <-i
    data_cols_all <- rbind(data_cols_all,data_cols)
  }
  
}

timestamp_cols <- names(df)[seq(1, ncol(df), 2)]
data_cols <- names(df)[seq(2, ncol(df), 2)]

# Create a long format dataframe for all timestamps and data
long_df_list <- lapply(seq_along(timestamp_cols), function(i) {
  timestamp_col <- timestamp_cols[i]
  data_col <- data_cols[i]
  df %>%
    select(!!timestamp_col, !!data_col) %>%
    rename(Timestamp = !!timestamp_col, Data = !!data_col) %>%
    mutate(Data_Type = data_col)
})

# Combine all long format dataframes into one
long_df <- bind_rows(long_df_list)

# Spread the long format dataframe back to wide format
wide_df <- long_df %>%
  pivot_wider(names_from = Data_Type, values_from = Data)

# Arrange by Timestamp for a neat output
wide_df <- wide_df %>%
  arrange(Timestamp)

# Rename the Timestamp column
wide_df <- wide_df %>%
  rename(Date = Timestamp)

wide_df <- wide_df %>%
  mutate(across(-1, ~ ifelse(.x == "NULL", NA, .x)))

wide_df <- wide_df[1:dim(wide_df)[1]-1,]

# RESULTS + MODEL STATS
file <- "C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/DATA/CDS_Prices.xlsx" 
write.xlsx(list( 
  CDS_Prices=wide_df),
  file,na.string = "NA")

