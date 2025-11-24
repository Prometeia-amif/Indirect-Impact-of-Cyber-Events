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

threshold <- 0.8
NO_FILTER<-FALSE #keep false to speed up the code, allows pre-filtering for <0.5 correlated series

my_method = "spearman"

load("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/results_before_means_CYBER_MARYLAND_2017_2024.RData")

my_ISINs <- unique(CAR_all$ISIN)

library(openxlsx)

# Define the file path
#file_path <- "C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/DATA/equity_1k_2012_2024.xlsx"
file_path <- "G:/CLIMATE RISK & ESG/PROGETTI/Reputation_risk/REPUTATION/DATA/equity_3k_2012_2024.xlsx"
# Create an empty list to store data frames
data_list <- list()

# Loop through sheets "Batch_1" to "Batch_19" for first 1k firms
#for (i in 1:19) {
for (i in 1:70) {
  sheet_name <- paste0("Batch_", i)  # Create sheet name
  
  # Read the sheet
  data_list[[i]] <- read.xlsx(file_path, sheet = sheet_name, 
                              colNames = TRUE, detectDates = TRUE, 
                              na.strings = c("NA", "#N/A", "N/D"))
}

# Combine all sheets column-wise (bind by columns)
Xi <- do.call(cbind, data_list)

colnames(Xi) <- gsub("\\(PI\\)~E$", "", colnames(Xi))

#Make sure not to peak ISINs of the original db

Xi_o   <- read.xlsx( paste0(folder,"DATA/check_matched_50k_726.xlsx"), sheet = "equity", colNames = TRUE, detectDates = TRUE, na.strings = c( "NA", "#N/A", "N/D" ) )
colnames(Xi_o) <- gsub("\\(PI\\)~E$", "", colnames(Xi_o))

# Get the column names of Xi and Xi_o
columns_Xi <- colnames(Xi)
columns_Xi_o <- colnames(Xi_o)

# Find the columns in Xi that are not in Xi_o
columns_to_keep <- setdiff(columns_Xi, columns_Xi_o)

# Subset Xi to keep only the columns that are not in Xi_o
Xi_filtered <- Xi[, columns_to_keep]

Xi <- Xi_filtered

Xi2 <- as.matrix(Xi)

# Proper merge with ISINs in Maryland
Xi_o <- Xi_o[which(Xi_o$Code=="2015-01-01"):dim(Xi_o)[1],]
last_date <- as.Date(Xi_o$Code[dim(Xi_o)[1]])
Xi2 <-Xi2[which(Xi2[,1]=="2015-01-01"):which(Xi2[,1]==as.character(last_date)),]
colnames(Xi2)[1] = "Code"

Xi2 <- cbind(Xi2,Xi_o)

#Xi2 <- Xi2[, colnames(Xi2) %in% my_ISINs]
#Xi2 <- Xi2[, colnames(Xi2) %in% my_ISINs | colnames(Xi2) == "Code"]


# Step 1: Identify columns to keep
columns_to_keep <- colSums(!is.na(Xi2)) > 0 & colSums(!(Xi2 == -1 | is.na(Xi2))) > 0

# Step 2: Subset the data to keep only desired columns
Xi2 <- Xi2[, columns_to_keep]

# Find the position of the last NA in each column
last_na_col_positions <- apply(Xi2, 2, function(col) {
  # Find the index of the last NA in the column
  last_na_index <- which(is.na(col))
  if(length(last_na_index) > 0) {
    return(last_na_index[length(last_na_index)])  # Return the last NA index
  } else {
    return(NA)  # Return NA if no NAs are found in the column
  }
})

# View the positions of the last NAs in each column
last_na_col_positions

# Count the number of NA values in last_na_col_positions
na_count <- sum(is.na(last_na_col_positions))

# View the count of NA values
na_count

# Keep only columns where last_na_col_positions is NA
Xi2 <- Xi2[, is.na(last_na_col_positions)]


# Subset Xi2 from column 2 to the last column
Xi2_sub <- Xi2[, 2:ncol(Xi2)]

# Find the first row with no NA in this subset
first_row_no_na <- apply(Xi2_sub, 1, function(row) all(!is.na(row)))

# Get the index of the first row with no NA
first_row_index <- which(first_row_no_na)[1]

# View the first row without NA
Xi2[first_row_index, ]

# Find columns that contain any NA values
columns_with_na <- which(colSums(is.na(Xi2)) > 0)

# View the names of the columns that contain NA
colnames(Xi2)[columns_with_na]

# Find columns that contain any NA values
columns_with_na <- which(colSums(is.na(Xi2)) > 0)

# Filter out columns with NA values
Xi2_filtered <- Xi2[, -columns_with_na]

# View the filtered data frame
head(Xi2_filtered)


# Step 1: Count NA values in each column
na_counts <- colSums(is.na(Xi2_filtered))

# Step 2: Get the maximum number of NA entries in any column
max_na <- max(na_counts)

# Output the result
max_na

#Xi2 <- Xi2_filtered
#clean_Xi2 <- Xi2[, !apply(Xi2, 2, anyNA)]
#clean_Xi2 <- clean_Xi2[,-1]
#clean_Xi2 <- apply(clean_Xi2, 2, as.numeric)

dates <- Xi2[, 1]  # First column contains dates

Xi2_numeric <- apply(Xi2[, -1], 2, as.numeric)

Xi2 <- cbind(dates, Xi2_numeric)

colnames(Xi2)[1] <- "Code"

# Remove columns with any NA values in Xi2
Xi2_filtered <- Xi2[, colSums(is.na(Xi2)) == 0]

# View the filtered data frame
head(Xi2_filtered)

Xi2 <- Xi2_filtered

xi3 <- as.matrix(Xi2[,-1]) 

xi3 <- apply(xi3, 2, as.numeric)


# Assuming `matrix_data` is your matrix
gXi2 <- apply(xi3, 2, function(x) {
  c(NA, diff(x) / x[-length(x)])  # Add NA for the first element
})


# Combine back the date column if needed
gXi2 <- cbind(Date = Xi2[, 1], gXi2)





gXi2 <- gXi2[-1,]



# Filter out columns that contain any NA values in gXi2
gXi2 <- gXi2[, colSums(is.na(gXi2)) == 0]

# Calculate the percentage of zeros in each column
zero_percentage <- colSums(gXi2 == 0) / nrow(gXi2)

# Filter out columns with more than 30% zeros
gXi2 <- gXi2[, zero_percentage <= 0.30]


# Load necessary library
library(lubridate)  # For working with dates
library(vars)  # VAR modeling

# If `gXi2` is a matrix, convert it to a data frame
gXi2 <- as.data.frame(gXi2)

# Assuming you have a Date column in the first column of `gXi2`
gXi2$Date <- as.Date(Xi2[2:2434,1])

# Ensure row names are Date objects
dates <- as.Date(gXi2$Date)

# Add a month-year column to group by month
gXi2$MonthYear <- format(dates, "%Y-%m")



# Remove columns that contain any NaN values
gXi2 <- gXi2[, !apply(gXi2, 2, function(x) any(x == "NaN"))]


# Market Index Returns
MKT <- read.xlsx( paste0(folder,"DATA/Market_Index_maryland.xlsx"), sheet = "DATA")
MKT_ts <- xts( MKT [,-1], as.Date( MKT [,1], origin = "1899-12-30" ) ) # time series
MKT_ts <- na.omit(Return.calculate(MKT_ts, method="log"))*100
colnames(MKT_ts) <- c("Mkt")
MKT <- data.frame(Date = index(MKT_ts), Value = coredata(MKT_ts))

# Risk-free Returns
RF <- read.xlsx( paste0(folder,"DATA/Risk_Free.xlsx") ,sheet = "DATA")
RF_ts <- xts( RF[,-1], as.Date( RF[,1], origin = "1899-12-30" ) ) # time series
RF_ts <- cbind( na.omit(Return.calculate(RF_ts[,c("Bund","Treasury","TOTMKWD","GOV_GLO_1_5","GOV_ITA_1_5")],method="log"))*100, # Total Rtn Idx Val
                log( ((1+RF_ts[,"T10"]/100))^(1/252) ), log(((1+RF_ts[,"B10"]/100))^(1/252)) ) # Yield
RF <- data.frame(Date = index(RF_ts), Value = coredata(RF_ts))
RF <- RF[, c("Date", "Value.Bund")]


# Fama-French Factors
FF_Factors    <- read.xlsx( paste0(folder,"DATA/Europe_3_Factors_Daily.xlsx"), sheet = "Europe_3_Factors_Daily", colNames = TRUE, detectDates = TRUE, na.strings = c( "-99.99") )
FFF_ts <- xts( FF_Factors[,-1], as.Date( as.character(FF_Factors[,1]), format = "%Y%m%d" ) )
rm(FF_Factors)

# Fama-French Factors
ExchangeRates    <- read.xlsx( paste0(folder,"DATA/Exchange_Rates.xlsx"), sheet = "DATA", colNames = TRUE, detectDates = TRUE)
ER_ts <- xts( ExchangeRates[,-1], as.Date( ExchangeRates[,1], origin = "1899-12-30" ) )
rm(ExchangeRates)

# Convert (divide by USD to EUR or multiply by EUR to USD)
FFF_ts <- merge(FFF_ts, ER_ts, join='left' ) # Merge the market
FFF_ts$SMBe <- (1/(1+FFF_ts$USEURSP))*FFF_ts$SMB
FFF_ts$HMLe <- (1/(1+FFF_ts$USEURSP))*FFF_ts$HML

FFF_ts$SMB <- FFF_ts$SMBe 
FFF_ts$HML <- FFF_ts$HMLe

# Fama-French Proxy
FF_Proxy <- read.xlsx( paste0(folder,"DATA/Growth_Value_Index.xlsx"), sheet = "DATA") # MSCI World (focus on developed countries)
FFP_ts <- xts( FF_Proxy [,-1], as.Date( FF_Proxy[,1], origin = "1899-12-30" ) ) # time series
FFP_ts <- na.omit(Return.calculate(FFP_ts, method="discrete"))*100
colnames(FFP_ts) <- c("LG", "SG", "LV",  "SV") # Large Growth / Small Growth / Large Value / Small Value
# Small Minus Big (firms with small market capitalization can earn higher returns than firms with high market capitalization)
FFP_ts$SMB_p <- (FFP_ts$SV + FFP_ts$SG)/2 - (FFP_ts$LV + FFP_ts$LG)/2
# High Minus Low (firms with high book-to-market ratio can earn higher returns than firms with low book-to-market ratio)
FFP_ts$HML_p <- (FFP_ts$LV + FFP_ts$SV)/2 - (FFP_ts$LG + FFP_ts$SG)/2

Proxy <- merge( FFP_ts[, c("SMB_p", "HML_p")], FFF_ts[, c("SMB", "HML")], join='left')
Proxy$SMBdef <- ifelse(is.na(Proxy$SMB), Proxy$SMB_p, Proxy$SMB)
Proxy$HMLdef <- ifelse(is.na(Proxy$HML), Proxy$SMB_p, Proxy$HML)

rm(FFF_ts)
FFF_ts <- cbind(Proxy$SMBdef, Proxy$HMLdef  )
colnames(FFF_ts) <- c("SMB", "HML")
FFF <- data.frame(Date = index(FFF_ts), Value = coredata(FFF_ts))
FFF <- FFF[, c("Date", "Value.SMB", "Value.HML")]


db_regre <- left_join(gXi2,MKT,by="Date")
db_regre <- left_join(db_regre,RF,by="Date")
db_regre <- left_join(db_regre,FFF,by="Date")
db_regre$Mkt_minus_Bund <- db_regre$Mkt - db_regre$Value.Bund

# Create an empty list to store residuals
residuals_list <- list()

# Loop over all columns except 'Date', 'MonthYear', and 'Mkt'
for (col in setdiff(names(db_regre), c("Date", "MonthYear", "Mkt", "Value.SMB", "Value.HML", "Value.Bund","Mkt_minus_Bund", 'UA4000067300', 'UA4000166011'))) {
  
  # Extract the stock return series (y) and the market return series (X)
  y <- db_regre[[col]]
  X <- db_regre[, c("Mkt", "Value.SMB", "Value.HML", "Value.Bund")]
  
  # Check if `y` contains NA values
  if (any(is.na(y))) {
    # If `y` contains NA, assign NA to the residuals for this column and continue to the next
    residuals_list[[col]] <- rep(NA, length(y))
    next  # Skip to the next iteration
  }
  
  # Perform regression (lm) on stock return (y) against the market return (Mkt)
  model <- lm(y ~  Value.Bund + Mkt_minus_Bund + Value.SMB + Value.HML, data = db_regre)
  
  # Extract residuals
  residuals_list[[col]] <- residuals(model)
}

# Convert residuals into a data frame
residuals_df <- as.data.frame(residuals_list)

residuals_df$Date <- db_regre$Date[762:2433]
residuals_df$MonthYear <- db_regre$MonthYear[762:2433]

gXi2 <- residuals_df


# Initialize Results List
results <- list()

if(NO_FILTER==TRUE){
  # Loop Through Each Unique Month
  for (month in unique(gXi2$MonthYear)) {
    
    subset_data <- gXi2[gXi2$MonthYear == month, ]
    subset_data$MonthYear <- NULL
    subset_data$Date <- NULL
    
    # Ensure data is numeric
    subset_data <- apply(subset_data, 2, as.numeric)
    
    # List of ISINs
    ISIN_list <- colnames(subset_data)
    
    # Initialize matrix for residual correlations
    cor_matrix <- matrix(NA, ncol = length(ISIN_list), nrow = length(ISIN_list))
    colnames(cor_matrix) <- ISIN_list
    rownames(cor_matrix) <- ISIN_list
    
    # Loop through ISIN pairs and fit VAR model
    for (i in 1:(length(ISIN_list) - 1)) {
      for (j in (i + 1):length(ISIN_list)) {
        
        # Extract two equity return series
        eq1 <- subset_data[, i]
        eq2 <- subset_data[, j]
        
        # Check for sufficient data
        if (sum(!is.na(eq1)) > 10 & sum(!is.na(eq2)) > 10) {  
          
          # Prepare VAR data
          var_data <- na.omit(data.frame(eq1, eq2))
          
          # Fit VAR model
          var_model <- VAR(var_data, lag.max = 3, type = "const", ic = "AIC")
          
          # Extract residuals
          residuals_var <- residuals(var_model)
          
          # Compute correlation of residuals
          cor_resid <- cor(residuals_var[, 1], residuals_var[, 2], method = my_method)
          
          # Store in correlation matrix
          cor_matrix[i, j] <- cor_resid
          cor_matrix[j, i] <- cor_resid
        }
      }
    }
    
    # Create adjacency matrix
    adj_matrix <- cor_matrix
    adj_matrix[abs(cor_matrix) < threshold] <- 0
    adj_matrix[abs(cor_matrix) >= threshold] <- 1
    diag(adj_matrix) <- 0
    
    results[[month]] <- adj_matrix
  }
  
}else{ 
  
  
  # Loop Through Each Unique Month
  for (month in unique(gXi2$MonthYear)) {
    
    subset_data <- gXi2[gXi2$MonthYear == month, ]
    subset_data$MonthYear <- NULL
    subset_data$Date <- NULL
    
    # Ensure data is numeric
    subset_data <- apply(subset_data, 2, as.numeric)
    
    # List of ISINs
    ISIN_list <- colnames(subset_data)
    
    # Compute raw correlation matrix
    raw_corr_matrix <- cor(subset_data, method = my_method, use = "complete.obs")
    
    # Initialize matrix for residual correlations
    cor_matrix <- matrix(NA, ncol = length(ISIN_list), nrow = length(ISIN_list))
    colnames(cor_matrix) <- ISIN_list
    rownames(cor_matrix) <- ISIN_list
    
    # Loop through ISIN pairs and fit VAR model only if raw correlation ≥ 0.5
    for (i in 1:(length(ISIN_list) - 1)) {
      for (j in (i + 1):length(ISIN_list)) {
        
        # Skip pairs with low raw correlation
        #if (abs(raw_corr_matrix[i, j]) < 0.5) next
        if (is.na(raw_corr_matrix[i, j]) || abs(raw_corr_matrix[i, j]) < 0.5) next
        
        # Extract two equity return series
        eq1 <- subset_data[, i]
        eq2 <- subset_data[, j]
        
        # Check for sufficient data
        if (sum(!is.na(eq1)) > 10 & sum(!is.na(eq2)) > 10) {  
          
          # Prepare VAR data
          var_data <- na.omit(data.frame(eq1, eq2))
          
          # Fit VAR model
          var_model <- VAR(var_data, lag.max = 3, type = "const", ic = "AIC")
          
          # Extract residuals
          residuals_var <- residuals(var_model)
          
          # Compute correlation of residuals
          cor_resid <- cor(residuals_var[, 1], residuals_var[, 2], method = my_method)
          
          # Store in correlation matrix
          cor_matrix[i, j] <- cor_resid
          cor_matrix[j, i] <- cor_resid
        }
      }
    }
    
    # Create adjacency matrix
    adj_matrix <- cor_matrix
    adj_matrix[abs(cor_matrix) < threshold] <- 0
    adj_matrix[abs(cor_matrix) >= threshold] <- 1
    diag(adj_matrix) <- 0
    
    results[[month]] <- adj_matrix
  }
  
}

# Access the results for a specific month, e.g., "2023-01"
results[["2023-01"]]

save.image("C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/network_resid_VAR_other_firms_MKT_FF.RData")


# Load data.table for fast operations
library(data.table)
library(readr)
# Convert gXi2 to a data.table (for fast subsetting and operations)
gXi2 <- as.data.table(gXi2)



# Events of interest
EVENTS    <- read.xlsx( paste0(folder,"DATA/maryland_db_with_ISIN_50k_2.xlsx"), sheet = "events", colNames = TRUE, detectDates = TRUE, na.strings = c( "NA", "#N/A", "N/D" ) )
#EVENTS    <- read.xlsx( paste0(folder,"DATA/maryland_db_with_ISIN_spill.xlsx"), sheet = "events", colNames = TRUE, detectDates = TRUE, na.strings = c( "NA", "#N/A", "N/D" ) )
#names(EVENTS) <- gsub("\\.", "_", names(EVENTS))
#names(EVENTS) <- gsub("\\(", "", names(EVENTS))
#names(EVENTS) <- gsub("\\)", "", names(EVENTS))
EVENTS$Date <-  as.Date(EVENTS$event_date, origin = "2021-01-01")
#EVENTS <- EVENTS[!duplicated(EVENTS$Date), ]
EVENTS$primary_isin <- EVENTS$ISIN.Code
EVENTS$story_id <- EVENTS$slug
EVENTS$ISIN_event_id <- paste(EVENTS$primary_isin,EVENTS$story_id,sep="_")
# Convert EVENTS to a data.table for faster lookup
setDT(EVENTS)

# Initialize the EVENTS_SPILL data table
EVENTS_SPILL <- data.table(primary_isin = character(), Date = character(), story_id = integer(),direct_isin = character())

# Loop over each unique month
for (month in unique(gXi2$MonthYear)) {
  
  # Get the adjacency matrix for the current month
  adj_matrix <- results[[month]]  # This is the adjacency matrix for the current month
  
  # Find the source ISINs and recipient ISINs for the current adjacency matrix
  source_isins <- rownames(adj_matrix)
  recipient_isins <- colnames(adj_matrix)
  
  # Identify pairs of ISINs where there is a relationship (correlation >= threshold)
  relationships <- which(adj_matrix == 1, arr.ind = TRUE)
  
  # Loop over the relationships and add data to EVENTS_SPILL
  for (i in seq_len(nrow(relationships))) {
    source_isin <- source_isins[relationships[i, 1]]
    recipient_isin <- recipient_isins[relationships[i, 2]]
    
    # Find the dates for the source ISIN in EVENTS (use fast lookup in data.table)
    source_dates <- EVENTS[primary_isin == source_isin, Date]
    
    # Check if any source_date falls within the current month
    if (length(source_dates) > 0) {
      # Loop over each source_date for the source ISIN
      for (source_date in source_dates) {
        # Extract month and year from source_date
        source_month_year <- format(as.Date(source_date), "%Y-%m")
        
        # If source_date falls within the current month
        if (source_month_year == month) {
          # Append to EVENTS_SPILL
          new_entry <- data.table(
            primary_isin = recipient_isin,
            Date = source_date,
            story_id = sample(1e7:1e8 - 1, 1),
            #ISIN_event_id = paste(recipient_isin, sample(1e7:1e8 - 1, 1), sep = "_"),
            direct_isin = source_isin
            #slug = EVENTS[primary_isin == source_isin & Date == source_date, slug]
          )
          
          # Ensure the column types in new_entry match those in EVENTS_SPILL
          new_entry$primary_isin <- as.character(new_entry$primary_isin)
          new_entry$Date <- as.Date(new_entry$Date)
          new_entry$story_id <- as.integer(new_entry$story_id)
          #new_entry$ISIN_event_id <- as.character(new_entry$ISIN_event_id)
          new_entry$direct_isin <- as.character(new_entry$direct_isin)
          
          # Ensure EVENTS_SPILL columns are consistent as well
          EVENTS_SPILL$primary_isin <- as.character(EVENTS_SPILL$primary_isin)
          EVENTS_SPILL$Date <- as.Date(EVENTS_SPILL$Date)
          EVENTS_SPILL$story_id <- as.integer(EVENTS_SPILL$story_id)
          #EVENTS_SPILL$ISIN_event_id <- as.character(EVENTS_SPILL$ISIN_event_id)
          EVENTS_SPILL$direct_isin <- as.character(EVENTS_SPILL$direct_isin)
          
          # Append new_entry to EVENTS_SPILL (faster than rbind)
          EVENTS_SPILL <- rbindlist(list(EVENTS_SPILL, new_entry), use.names = TRUE, fill = TRUE)
        }
      }
    }
  }
}

# Convert the Date column to Date type (if not already)
EVENTS_SPILL$Date <- as.Date(EVENTS_SPILL$Date, origin = "1970-01-01")
EVENTS_SPILL$ISIN_event_id = paste(EVENTS_SPILL$primary_isin, EVENTS_SPILL$story_id, sep = "_")
EVENTS_SPILL$combine = paste(EVENTS_SPILL$direct_isin, EVENTS_SPILL$Date, sep = "_")

EVENTS_subset <- EVENTS %>%
  dplyr::select(primary_isin, Date, actor, actor_type, motive, event_type, event_subtype)
EVENTS_subset$combine = paste(EVENTS_subset$primary_isin, EVENTS_subset$Date, sep = "_")

EVENTS_subset <- EVENTS_subset %>%
  distinct(combine, .keep_all = TRUE)

EVENTS_subset <- EVENTS_subset %>%
  dplyr::select(combine, actor, actor_type, motive, event_type, event_subtype)

EVENTS_SPILL <- EVENTS_SPILL %>%
  left_join(EVENTS_subset, by = c("combine" = "combine"))


##Exclude all firms of the original db

EVENTS_SPILL_filtered <- EVENTS_SPILL %>%
  filter(!(primary_isin %in% colnames(Xi_o)))

EVENTS_SPILL <- EVENTS_SPILL_filtered
# Print the result
print(EVENTS_SPILL)


# file <- "C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/EVENTS_SPILL_08_spearman_with_info_VAR_model_other_firms2_MKT_prova1.xlsx"
# write.xlsx(list( 
#   events=EVENTS_SPILL),
#   file)

file <- "C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/OUTPUT/EVENTS_SPILL_08_spearman_with_info_VAR_model_other_firms2_MKT_more_firms_FF.xlsx"
write.xlsx(list( 
  events=EVENTS_SPILL),
  file)


