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

# Paths
root <- "C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/"
# folder      <- "//COR-FS-BO-01/Amif/CLIMATE RISK & ESG/PROGETTI/ISP_Market_Risk/EVENT_STUDY/EQUITY/"
folder      <- "C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/REPUTATION/"
filein <- paste0(folder,"INPUT/")
fileout <- paste0(folder,"OUTPUT_CDS/")

# Parameters
riskfree <- "Bund" 
event_wdw_m <- 5 # Event window PRE
event_wdw_p <- c(0:15) # Event window POST
est_wdw <- 252*3 # Estimation window

# Options
UPDATA  <- TRUE
EXPORT  <- TRUE
FFproxy<- TRUE
Exclude_Shell<-FALSE

##################
### LOAD DATA ####
##################

if(UPDATA){
  
  # Market Index Returns
  MKT <- read.xlsx( paste0(folder,"DATA/Market_Index.xlsx"), sheet = "DATA")
  MKT_ts <- xts( MKT [,-1], as.Date( MKT [,1], origin = "1899-12-30" ) ) # time series
  MKT_ts <- na.omit(Return.calculate(MKT_ts, method="log"))*100
  colnames(MKT_ts) <- c("Mkt")
  plot.xts(MKT_ts)
  
  # Risk-free Returns
  RF <- read.xlsx( paste0(folder,"DATA/Risk_Free.xlsx") ,sheet = "DATA")
  RF_ts <- xts( RF[,-1], as.Date( RF[,1], origin = "1899-12-30" ) ) # time series
  RF_ts <- cbind( na.omit(Return.calculate(RF_ts[,c("Bund","Treasury","TOTMKWD","GOV_GLO_1_5","GOV_ITA_1_5")],method="log"))*100, # Total Rtn Idx Val
                  log( ((1+RF_ts[,"T10"]/100))^(1/252) ), log(((1+RF_ts[,"B10"]/100))^(1/252)) ) # Yield
  plot.xts(RF_ts$B10)
  # per T10 e B10: ritorno giornaliero di un investimento a tasso flessibile che ogni giorno rende lo ytm del bund (nella formula è implicito un /1 che consente di ottenere un log return)
  
  
  # Bond Market Index Returns
  MKT_BD <- read.xlsx( paste( folder, "DATA/CR_UEM_daily.xlsx", sep = "" ), sheet = "DB", colNames = TRUE )
  MKT_BD_ts <- xts( MKT_BD [,-1], as.Date( MKT_BD [,1], origin = "1899-12-30" ) ) # time series
  # MKT_BD_ts <-log(((1+MKT_BD_ts[,"TSIG_10_15"]/100))^(1/252))
  MKT_BD_ts <- MKT_BD_ts[,"TSIG_10_15"]/252
  # colnames(MKT_BD_ts) <- c("Mkt_bd")
  plot.xts(MKT_BD_ts)
  summary(coredata(MKT_BD_ts))
  format(mean(MKT_BD_ts, na.rm = TRUE))
  
  # Sovereign yields
  GOV <- read.xlsx( paste0(folder,"DATA/gov_py_scarico.xlsx") ,sheet = "scarico.D_DEU")
  GOV <- GOV[-1,]
  GOV_ts <- xts( GOV[,-1], as.Date( GOV[,1], origin = "1899-12-30" ) ) # time series
  # GOV_ts <- cbind(log(((1+GOV_ts[,"TRBD3MT"]/100))^(1/252)),log(((1+GOV_ts[,"BMBD10Y(RY)"]/100))^(1/252))) 
  GOV_ts <- GOV_ts[,c("TRBD3MT","BMBD10Y(RY)")]/252
  colnames(GOV_ts) <- c("m03_DEU_g","y10_DEU_g")
  plot.xts(GOV_ts)
  summary(coredata(GOV_ts))
  format(mean(GOV_ts, na.rm = TRUE))
  
  # Default Premium (Credit Spread)
  DEF_ts <- MKT_BD_ts-GOV_ts[,"y10_DEU_g"]
  colnames(DEF_ts) <- c("Def")
  plot(DEF_ts)
  
  # SLOPE
  TERM_ts <- GOV_ts[,"y10_DEU_g"]-GOV_ts[,"m03_DEU_g"]
  colnames(TERM_ts) <- c("Term")
  plot(TERM_ts)
  
  
  # Events of interest
  EVENTS    <- read.xlsx( paste0(folder,"DATA/Events.xlsx"), sheet = "Sheet1", colNames = TRUE, detectDates = TRUE, na.strings = c( "NA", "#N/A", "N/D" ) )
  #names(EVENTS) <- gsub("\\.", "_", names(EVENTS))
  #names(EVENTS) <- gsub("\\(", "", names(EVENTS))
  #names(EVENTS) <- gsub("\\)", "", names(EVENTS))
  EVENTS$Date <-  as.Date(EVENTS$incident_date, origin = "2021-01-01")
  #EVENTS <- EVENTS[!duplicated(EVENTS$Date), ]
  EVENTS$ISIN_event_id <- paste(EVENTS$primary_isin,EVENTS$story_id,sep="_")
  
  # Our categories
  
  EVENTS$ENVIRONMENTAL_IMPACT <- ifelse (EVENTS$animal_mistreatment=="T" 
                                         | EVENTS$impacts_on_landscapes=="T" 
                                         | EVENTS$local_pollution=="T","T","F")
  
  EVENTS$CLIMATE_CHANGE_INACTION <- ifelse (EVENTS$climate_ghg_pollution=="T","T","F")
  
  EVENTS$RESOURCE_MANAGEMENT <- ifelse (EVENTS$overuse_and_wasting=="T"
                                        | EVENTS$waste_issues=="T","T","F")
  
  EVENTS$ENVIRONMENTAL_REPORTING_AND_TRANSPARENCY <- ifelse (EVENTS$misleading_communication=="T","T","F")
  
  #Merge sectors and size
  
  Xi   <- read.xlsx( paste0(folder,"DATA/Sectors_and_size.xlsx"), sheet = "Sheet1", colNames = TRUE, detectDates = TRUE, na.strings = c( "NA", "#N/A", "N/D", "NULL") )
  Xi <- apply(Xi, c(1,2), function(x) if(grepl("Unable", x)) NA else x)
  
  Xi <-data.frame(Xi)
  
  EVENTS <- EVENTS %>%
    left_join(., Xi[, c(colnames(Xi[,]))], by = c ("primary_isin" = "primary_isin") )
  
  rm("Xi")
  
  
  classify_revenue2 <- function(rev) {
    if (is.na(rev)) {
      return(NA)
    } else if (rev <= 500000) {
      return("<0.5M")
    } else if (rev > 500000 & rev <= 5000000) {
      return("0.5M-5M")
    } else if (rev > 5000000 & rev <= 50000000) {
      return("5M-50M")
    } else if (rev > 50000000 & rev <= 1000000000) {
      return("50M-1B")
    } else if (rev > 1000000000) {
      return(">1B")
    }
  }
  
  EVENTS$SIZE <- sapply(as.numeric(EVENTS$Revenue.from.Business.Activities...Total), classify_revenue2)
  

  # CDS Spread mid
  # se il cds è 100pb, vuol dire che il compratore paga l'1% all'anno sull'ammontare per cui vuole coprirsi
  CDS   <- read.xlsx( paste0(folder,"DATA/CDS_Prices.xlsx"), sheet = "CDS_Prices", colNames = TRUE, detectDates = TRUE, na.strings = c( "NA", "#N/A", "N/D" , "NULL") )
  CDS_ts <- xts( CDS[,-1], as.Date( CDS[,1], origin = "2021-01-01" ) )
  # Exspressed in bps -> *100
  #CDS_ts <- (CDS_ts[,c("y05_ENI", "y07_ENI", "y10_ENI", "y05_STL", "y07_STL", "y10_STL")])
  # CDS_ts <- (na.omit(Return.calculate(CDS_ts, method="log")))*100
  
  CDS_ts        <- melt(as.matrix(CDS_ts), id.vars = rownames(X))
  colnames(CDS_ts) <- c("Date","ISIN","Value")
  
  # Calculate  Returns
  
  CDS_ts <- CDS_ts %>%
    mutate(Value = as.numeric(Value))
  
#  DBPD <- CDS_ts %>%
#    group_by(ISIN) %>%
#    mutate(Return = log(Value/dplyr::lag(Value,1))*100) %>% as.data.frame()  

  #DELTA bp
  
  DBPD <- CDS_ts %>%
    group_by(ISIN) %>%
    mutate(Return = Value - dplyr::lag(Value,1)) %>% as.data.frame() 
  
  if(FFproxy==FALSE){
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
    FFF_ts$Mkt.RF <- NULL  
    FFF_ts$RF <- NULL  
    FFF_ts$USEURSP <- NULL  
    FFF_ts$EUDOLLR <- NULL  
    FFF_ts$SMBe <- NULL  
    FFF_ts$HMLe <- NULL  
    colnames(FFF_ts) <- c("SMB", "HML")
  }else{
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
  }
  plot.xts(na.omit(FFF_ts))
  
  graphics.off()
  
  save("MKT_ts", "RF_ts", "EVENTS", "DBPD", "FFF_ts", "DEF_ts", "TERM_ts", file = paste0(folder, "INPUT/CDS_DB_REPRISK.Rdata") )

  # rm(list= ls()[!(ls() %in% c("folder", "filein", "MKT_ts", "RF_ts", "ANA",  "EVENTS", "DB", "FFF_ts"))])
  
}else {
  tmp.env   <- new.env()
  load( paste0(folder,"INPUT/CDS_DB_REPRISK.Rdata" ), envir=tmp.env)
  DB <- get("DB", pos = tmp.env)
  MKT_ts <- get("MKT_ts", pos = tmp.env)
  RF_ts <- get("RF_ts", pos = tmp.env)
  EVENTS <- get("EVENTS", pos = tmp.env)
  FFF_ts <- get("FFF_ts", pos = tmp.env)
  DEF_ts <- get("DEF_ts", pos = tmp.env)
  TERM_ts <- get("TERM_ts", pos = tmp.env)
  rm(tmp.env)
}

########################
### GENERAL CLEANING ###
#######################

# 1. Escludo gli emittenti che hanno un rendimento maggiore del 50% giornaliero 

ISIN_toclean_s1 <- DBPD %>%
  filter(abs(Return) > 50) %>%
  select(ISIN)%>%
  distinct()

DSET <- DBPD %>% 
  filter(!ISIN %in% as.data.frame(ISIN_toclean_s1)[,1] ) %>% 
  select(Date, ISIN, Return) %>%
  dcast(Date ~ ISIN, fun.aggregate = mean, value.var = "Return") # cast (ISIN = columns)


#2. Escludo gli emittenti con tutti NA

#DSET <- DSET[, colSums(is.na(DSET)) != nrow(DSET)]

#3. Escludo gli emittenti con tutti 0 (rendimenti costanti)

#DSET <- DSET[, c(TRUE,colSums(DSET[,-1],na.rm = TRUE) != 0)]

ISIN <-names( DSET[-1] )
n_ISIN <- length(ISIN) 

# print( paste0("Cleaning STEP 1: ", nrow(ISIN_toclean_s1), " ISIN removed. ISIN left: ", n_ISIN) ) # CHECK: quanti ISIN escludo?

EVENTS$week_day <- wday(EVENTS$Date, label=TRUE)

EVENTS <- EVENTS %>%
  filter(!week_day %in% c("Sat","Sun") ) 

#Escludo eventi per i cui ISIN non abbiamo equity prices

EVENTS <- EVENTS[EVENTS$primary_isin %in% colnames(DSET), ]


#

repetitions_isins=as.data.frame(table(EVENTS$primary_isin))

#TEST: escludo Shell che ha tanti eventi

if(Exclude_Shell==TRUE)
  EVENTS <- EVENTS[EVENTS$primary_isin!="GB00BP6MXD84", ]


####################
### EVENT STUDY ####
###################

for ( k in 1:length(event_wdw_p)) {
  
  print(k)
  
  event_wdw <- event_wdw_m+event_wdw_p[k]+1
  
  t  <-  as.Date(DSET[,1], origin = "1899-12-30" ) 
  r <- xts( DSET[,2:ncol(DSET)], t ) # DSET as ts
  
  r <- merge(r, MKT_ts, join='left' ) # Merge the market
  r <- merge(r, RF_ts[, riskfree], join='left' ) # Merge the riskfree
  r <- merge(r, FFF_ts[, c("SMB", "HML")], join='left' ) # Merge the FF Factors
  r <- merge(r, DEF_ts, join='left' ) # Merge the Default Premium
  r <- merge(r, TERM_ts, join='left' ) # Merge the Slope
  
  Issuer <- colnames(DSET[,-1])
  
  #for ( i in 1:length(Issuer)) {
  
  # print(Issuer[i])
  
  for ( j in 1:nrow(EVENTS)) {
    
    event <- EVENTS[j,"Date"] 
    
    Issuer_of_event <- EVENTS[j, "primary_isin"]
    print(Issuer_of_event)
    
    ev_start <- t[ as.numeric( which( t==event ) ) - event_wdw_m ] # Event window start
    ev_end <- t[ as.numeric(which(t==event)) + event_wdw_p[k] ] # Event window end
    
    # print( paste0("Event: ", event, " Event window start: ", ev_start, " Event window end: ", ev_end)  )
    
    est_start <- t[ as.numeric( which( t==ev_start) ) - est_wdw ] # Estimation window start
    est_end <- t[ as.numeric(which(t==ev_start)) - 1 ] # Estimation window end
    
    # print( paste0("Event: ", event, " Estimation window start: ", est_start, " Estimation window end: ", est_end)  )
    
    ###################
    ### RW CLEANING ###
    ##################
    
    r_rw_temp <- r [, c(Issuer_of_event, "Mkt",riskfree, "SMB" ,  "HML", "Def",  "Term")]
    r_rw_temp <- r_rw_temp[ paste0( est_start, "/", ev_end ) ] 
    
    # 2. Closed sample (tolgo ISIN che all'inizio della estimation window o alla fine dell'event window sono NaN)
    r_rw_temp <- r_rw_temp[, which( !is.na (r_rw_temp[ est_start , ]) ) ]
    r_rw_temp <- r_rw_temp[, which(!is.na( r_rw_temp[ ev_end, ]) ) ]
    
    # print( paste0("Cleaning STEP 2: ", 1 - (ncol(r_rw_temp)-2), " ISIN removed. ISIN left: ", ncol(r_rw_temp)-2 ) ) # CHECK: quanti ISIN escludo?
    
    # 3. Escludo gli emittenti  che hanno più del 30% di rendimenti giornalieri NA 
    checksum <- apply(r_rw_temp, 2, function(x)sum(is.na(x))) # sommo il numero dei NaN che ci sono nella ts di ogni fondo
    checksum_toclean <- apply( as.data.frame(checksum), 1, max ) > length(index(r_rw_temp))*30/100 # check se tale numero ? maggiore del 30% dei rendimenti totali
    ISIN_toclean_s3  <- as.data.frame( names(checksum_toclean[checksum_toclean==TRUE]) )
    
    if (nrow(ISIN_toclean_s3) > 0) 
      r_rw_temp  <- r_rw_temp[ , -which( names(r_rw_temp) %in% as.data.frame(ISIN_toclean_s3)[,1] ) ]
    
    # print( paste0("Cleaning STEP 3: ", nrow(ISIN_toclean_s3), " ISIN removed. ISIN left: ", ncol(r_rw_temp)-2 ) ) # CHECK: quanti ISIN escludo?
    
    # 4. Escludo gli emittenti che hanno pi? del 30% di rendimenti giornalieri 0
    checksum_z <- apply(r_rw_temp, 2, function(x)sum(x==0)) # sommo il numero dei 0 che ci sono nella ts di ogni fondo
    checksum_z_toclean <- apply( as.data.frame(checksum_z), 1, max ) > length(index(r_rw_temp))*30/100 # check se tale numero ? maggiore del 30% dei rendimenti totali
    ISIN_toclean_s4  <- as.data.frame( names(checksum_z_toclean[checksum_z_toclean==TRUE]) )
    
    if (nrow(ISIN_toclean_s4) > 0) 
      r_rw_temp  <- r_rw_temp[ , -which( names(r_rw_temp) %in% as.data.frame(ISIN_toclean_s4)[,1] ) ]
    
    # print( paste0("Cleaning STEP 4: ", nrow(ISIN_toclean_s4), " ISIN removed. ISIN left: ", ncol(r_rw_temp)-2) ) # CHECK: quanti ISIN escludo?
    
    r_rw <- r_rw_temp
    rm(r_rw_temp)
    
    if(ncol(r_rw) > 6){ # se non è stato eliminato per qualche motivo l'emittente
      
      # Returns in event window
      r_ev <- r_rw[ paste0( ev_start , "/", ev_end)  ]
      
      # Returns pre in estimation window 
      r_est  <-  r_rw[ paste0( est_start, "/", est_end ) ] 
      
      # Returns pre in estimation window cumulated to calculate VaR ************************************************************
      r_est_c  <- rollapply(r_est[,Issuer_of_event], width = event_wdw_m+event_wdw_p[k], sum, by.column = TRUE, align = "right")
      # Calculate VaR
      COR_VaR_event <- data.frame(quantile(r_est_c, probs = c(0.05), na.rm = TRUE)) # **********************************************
      
      # Market in the event window
      ones <- matrix( rep(1, nrow(r_ev),ncol=1) )
      X    <- cbind(ones, r_ev[, "Mkt"], r_ev[, "SMB"], r_ev[, "HML"], r_ev[, "Def"], r_ev[, "Term"]) 
      
      # Estimate linear model at bond level in the estimation window
      mm_event   <- lm( r_est[, Issuer_of_event] - r_est[, riskfree]  ~ (r_est[, "Mkt"] - r_est[, riskfree]) +  r_est[, "SMB"] + r_est[, "HML"] +  r_est[, "Def"] + r_est[, "Term"]) 
      beta_event <- matrix( coef(mm_event), nrow=1 ) # save coefficient estimates
      
      # Calculate Normal Return at fund level in the event window 
      NR_event <- X %*% t(beta_event)  
      
      # Calculate Abnormal Return at fund level in the event window
      AR_event<- matrix( r_ev[,Issuer_of_event], ncol=1 ) - NR_event 
      
      # Observed Return
      OR_event<- matrix( r_ev[,Issuer_of_event], ncol=1 )
      
      # Market in the estimation window ****************************************************************************************
      ones_est <- matrix( rep(1, nrow(r_est),ncol=1) )
      X_est  <- cbind(ones_est, r_est[, "Mkt"], r_est[, "SMB"], r_est[, "HML"], r_est[, "Def"], r_est[, "Term"]) 
      
      # Calculate Normal Return at fund level in the estimation window 
      NR_event_est <- X_est %*% t(beta_event)  
      
      # Calculate Abnormal Return at fund level in the estimation window
      AR_event_est <- matrix( r_est[,Issuer_of_event], ncol=1 ) - NR_event_est 
      
      # AR pre in estimation window cumulated to calculate VaR 
      AR_event_est_c  <- rollapply(AR_event_est, width = event_wdw_m+event_wdw_p[k], sum, by.column = TRUE, align = "right")
      
      #Calculate std of AR pre in estimation window cumulated
      
      #AR_event_est_c_std = data.frame(sd(AR_event_est_c, na.rm = TRUE))
      AR_event_est_c_std = data.frame(sd(AR_event_est_c[AR_event_est_c > 0], na.rm = TRUE))
      
      # Calculate VaR
      CAR_VaR_event <- data.frame(quantile(AR_event_est_c, probs = c(0.05), na.rm = TRUE)) # **********************************************
      
      # Store statistics
      SUMM_event  <- as.data.frame( coef( summary(mm_event))[2, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")] )
      names(SUMM_event) <- c("value")
      # Get the R^2
      Rsquared <- t(data.frame(summary(mm_event)$r.squared, summary(mm_event)$adj.r.squared))
      rownames(Rsquared) <- c("Rsquared", "adjRsquared")
      colnames(Rsquared) <- c("value")
      SUMM_event  <- rbind(SUMM_event, Rsquared)
      
      # Residual variance (residuals extracts model residuals, and then take variance)
      ResVar_event <- as.data.frame(var(residuals(mm_event))) # Variance of the residuals from the market model
      
      # Store results
      
      colnames(NR_event) <- paste(Issuer_of_event,EVENTS[j,"story_id"],sep="_")
      colnames(AR_event) <- paste(Issuer_of_event,EVENTS[j,"story_id"],sep="_")
      colnames(SUMM_event) <- paste(Issuer_of_event,EVENTS[j,"story_id"],sep="_")
      colnames(ResVar_event) <- paste(Issuer_of_event,EVENTS[j,"story_id"],sep="_")
      colnames(OR_event) <- paste(Issuer_of_event,EVENTS[j,"story_id"],sep="_")
      colnames(COR_VaR_event) <- paste(Issuer_of_event,EVENTS[j,"story_id"],sep="_")
      colnames(CAR_VaR_event) <- paste(Issuer_of_event,EVENTS[j,"story_id"],sep="_")
      colnames(AR_event_est_c_std) <- paste(Issuer_of_event,EVENTS[j,"story_id"],sep="_")
      
      if(!exists("NR_Issuer")) {
        #NR_Issuer <- c(NR_event,rep(NA,event_wdw_p[length(event_wdw_p)]-event_wdw_p[1]))
        #AR_Issuer <- c(AR_event,rep(NA,event_wdw_p[length(event_wdw_p)]-event_wdw_p[1]))
        NR_Issuer <- NR_event
        AR_Issuer <- AR_event
        SUMM_Issuer <- SUMM_event  
        ResVar_Issuer <- ResVar_event 
        #OR_Issuer <- c(OR_event,rep(NA,event_wdw_p[length(event_wdw_p)]-event_wdw_p[1]))
        OR_Issuer <- OR_event
        COR_VaR_Issuer <- COR_VaR_event
        CAR_VaR_Issuer <- CAR_VaR_event
        AR_Issuer_est_c_std <- AR_event_est_c_std
      } else {
        #NR_Issuer <- cbind(NR_Issuer, c(NR_event,rep(NA,event_wdw_p[length(event_wdw_p)]-event_wdw_p[1])))	
        #AR_Issuer <- cbind(AR_Issuer, c(AR_event,rep(NA,event_wdw_p[length(event_wdw_p)]-event_wdw_p[1])))
        NR_Issuer <- cbind(NR_Issuer,NR_event)
        AR_Issuer <- cbind(AR_Issuer,AR_event)
        SUMM_Issuer <- cbind(SUMM_Issuer, SUMM_event)
        ResVar_Issuer <- cbind(ResVar_Issuer, ResVar_event)
        #OR_Issuer <- cbind(OR_Issuer, c(OR_event,rep(NA,event_wdw_p[length(event_wdw_p)]-event_wdw_p[1])))	
        OR_Issuer <- cbind(OR_Issuer,OR_event)
        COR_VaR_Issuer <- cbind(COR_VaR_Issuer, COR_VaR_event)	
        CAR_VaR_Issuer <- cbind(CAR_VaR_Issuer, CAR_VaR_event)
        AR_Issuer_est_c_std <- cbind(AR_Issuer_est_c_std, AR_event_est_c_std)
      } 
      
      #else if(exists("NR_Issuer") & k==1) {
      #else {
      #   NR_Issuer <- cbind(NR_Issuer, NR_event)	
      #    AR_Issuer <- cbind(AR_Issuer, AR_event)
      #   SUMM_Issuer <- cbind(SUMM_Issuer, SUMM_event)
      #    ResVar_Issuer <- cbind(ResVar_Issuer, ResVar_event)
      #    OR_Issuer <- cbind(OR_Issuer, OR_event)	
      #    COR_VaR_Issuer <- cbind(COR_VaR_Issuer, COR_VaR_event)	
      #    CAR_VaR_Issuer <- cbind(CAR_VaR_Issuer, CAR_VaR_event)
      #  } 
      
    }
    
  } # for j event
  
  #  if(!exists("NR")) {
  NR <- NR_Issuer
  AR <- AR_Issuer
  SUMM_temp <- SUMM_Issuer
  ResVar_temp <- ResVar_Issuer
  OR <- OR_Issuer
  COR_VaR_temp <- COR_VaR_Issuer
  CAR_VaR_temp <- CAR_VaR_Issuer
  AR_est_c_std <- AR_Issuer_est_c_std
  #  } else {
  #    NR <- cbind(NR, NR_Issuer)	
  #    AR <- cbind(AR, AR_Issuer)	
  #    SUMM_temp <- cbind(SUMM_temp, SUMM_Issuer)
  #    ResVar_temp <- cbind(ResVar_temp, ResVar_Issuer)
  #    OR <- cbind(OR, OR_Issuer)
  #    COR_VaR_temp <- cbind(COR_VaR_temp, COR_VaR_Issuer)
  #    CAR_VaR_temp<- cbind(CAR_VaR_temp, CAR_VaR_Issuer)
  #  } 
  
  #} # for i issuer
  
  # rm(list= ls()[!(ls() %in% c("folder", "filein", "event_wdw_m", "event_wdw_p", "est_wdw", "k",
  #                             "MKT_ts", "RF_ts", "ANA",  "EVENTS", "DB", "FFF_ts",
  #                             "NR", "AR", "OR",
  #                             "SUMM_temp", "ResVar_temp", "COR_VaR_temp", "CAR_VaR_temp"))])
  
  rm("NR_Issuer")
  rm("AR_Issuer" )
  rm("SUMM_Issuer")
  rm("ResVar_Issuer") 
  rm("OR_Issuer") 
  rm("COR_VaR_Issuer") 
  rm("CAR_VaR_Issuer") 
  rm("AR_Issuer_est_c_std")
  
  # Model Summary
  SUMM <- melt( data.frame(rownames(SUMM_temp), SUMM_temp), id.vars = 1 )
  SUMM <- cbind(replicate(ncol(SUMM)-1, event_wdw_m), replicate(ncol(SUMM)-1, event_wdw_p[k]), replicate(ncol(SUMM)-1, est_wdw), SUMM)
  colnames(SUMM) <- c("event_wdw_m", "event_wdw_p", "est_wdw", "variable", "KEY", "value")
  SUMM$ISIN <- substr(SUMM$KEY,1,12)
  SUMM$Event <- substr(SUMM$KEY,14,20)
  SUMM$Issuer <- SUMM$ISIN
  rm("SUMM_temp")
  
  # Residual variance
  ResVar <- data.frame(rownames( t( ResVar_temp ) ),  t( ResVar_temp ) )
  rownames(ResVar) <- c()
  ResVar <- cbind(replicate(ncol(ResVar)-1, event_wdw_m), replicate(ncol(ResVar)-1, event_wdw_p[k]), replicate(ncol(ResVar)-1, est_wdw), ResVar)
  colnames(ResVar) <- c("event_wdw_m", "event_wdw_p", "est_wdw","KEY", "Var")
  ResVar$ISIN <- substr(ResVar$KEY,1,12)
  ResVar$Event <- substr(ResVar$KEY,14,20)
  ResVar$Issuer <- ResVar$ISIN
  rm("ResVar_temp")
  
  ### Cumulative Abnormal Return
  CAR <- as.data.frame( cbind( replicate(ncol(AR), event_wdw_m), replicate(ncol(AR), event_wdw_p[k]),replicate(ncol(AR), est_wdw),
                               colnames(AR), data.frame( colSums (AR, na.rm = TRUE) ) ) )
  rownames(CAR) <- c()
  colnames(CAR) <- c("event_wdw_m", "event_wdw_p", "est_wdw", "KEY", "CARi")
  CAR$ISIN <- substr(CAR$KEY,1,12)
  CAR$Event <- substr(CAR$KEY,14,20)
  CAR$Issuer <- CAR$ISIN
  
  CAR_VaR <- melt( data.frame(rownames(CAR_VaR_temp), CAR_VaR_temp), id.vars = 1 )
  colnames(CAR_VaR) <- c("p", "KEY", "CARi_VaR") 
  rm("CAR_VaR_temp")
  
  CAR_std <- melt( data.frame(rownames( AR_est_c_std),  AR_est_c_std), id.vars = 1 )
  colnames(CAR_std) <- c("p", "KEY", "CARi_std") 
  rm("AR_est_c_std")
  
  # Join CAR
  CAR <- CAR %>%
    left_join(., CAR_VaR[, c("KEY", "CARi_VaR")], by = c ("KEY" = "KEY") ) 
  
  CAR <- CAR %>%
    left_join(., CAR_std[, c("KEY", "CARi_std")], by = c ("KEY" = "KEY") ) 
  
  ### Cumulative Observed Return
  COR <- as.data.frame( cbind( replicate(ncol(OR), event_wdw_m), replicate(ncol(OR), event_wdw_p[k]),replicate(ncol(OR), est_wdw),
                               colnames(OR), data.frame( colSums (OR, na.rm = TRUE) ) ) )
  rownames(COR) <- c()
  colnames(COR) <- c("event_wdw_m", "event_wdw_p", "est_wdw", "KEY", "CORi")
  COR$ISIN <- substr(COR$KEY,1,12)
  COR$Event <- substr(COR$KEY,14,20)
  COR$Issuer <- COR$ISIN
  
  COR_VaR <- melt( data.frame(rownames(COR_VaR_temp), COR_VaR_temp), id.vars = 1 )
  colnames(COR_VaR) <- c("p", "KEY", "CORi_VaR") 
  rm("COR_VaR_temp")
  
  # Join CAR
  COR <- COR %>%
    left_join(., COR_VaR[, c("KEY", "CORi_VaR")], by = c ("KEY" = "KEY") ) 
  
  ### Z Test
  ZTestCAR <- CAR %>%
    left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) ) 
  ZTestCAR$Var <- ZTestCAR$Var*event_wdw # Estimate variance of CAR
  ZTestCAR$ZStat <- ZTestCAR$CARi/sqrt(ZTestCAR$Var) # Calculate test stat (Standard Normal dist)
  ZTestCAR$Reject <- ifelse( ZTestCAR$ZStat <= -1.960 | ZTestCAR$ZStat >= 1.960, 1, 0 ) # 2 tailed test with alpha = 0.05 (1 = Reject, 0 = non-Reject)
  # Sign of the CARi
  ZTestCAR$Sign <- ifelse( ZTestCAR$CARi >0, "Pos","Neg" ) 
  
  
  ############
  ### ACAR ###
  ############
  
  tab_ACAR <- CAR %>%
    summarise(ACAR = mean(CARi, na.rm = TRUE),KEY)
  #tab_ACAR
  
  # by ISIN 
  tab_ACAR_ISIN <- CAR %>%
    group_by(ISIN) %>%
    summarise(ACAR = mean(CARi, na.rm = TRUE),KEY)
  #tab_ACAR_ISIN
  
  # hist(tab_ACAR_ISIN$ACAR)
  
  #tab_ACAR_PAESE <- CAR %>%
  #  group_by(PAESE) %>%
  #    summarise(ACAR = mean(CARi, na.rm = TRUE))
  #  tab_ACAR_PAESE
  
  # hist(tab_ACAR_PAESE$ACAR)
  
  ######################
  ### Z Test of ACAR ###
  ######################
  
  ZTestACAR <- CAR %>%
    left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
    summarise(ACAR = mean(CARi, na.rm = TRUE), ACAR_Var_temp = mean(Var*event_wdw, na.rm = TRUE), n = n_distinct(KEY),KEY) %>%
    mutate(ACAR_Var = ACAR_Var_temp*(1/n) ) %>%
    mutate(ZStat = ACAR/sqrt(ACAR_Var)) %>%
    mutate(Reject = ifelse( ZStat <= -1.960 | ZStat >= 1.960, 1, 0 )) %>% 
    mutate(KEY=KEY) %>%
    select( n, ACAR, ACAR_Var, ZStat, Reject, KEY)
  # Test Results
  #ZTestACAR
  
  # by Issuer 
  ZTestACAR_I <- CAR %>%
    left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
    group_by(Issuer) %>%
    summarise(ACAR = mean(CARi, na.rm = TRUE), ACAR_Var_temp = mean(Var*event_wdw, na.rm = TRUE), n = n_distinct(KEY),KEY) %>%
    mutate(ACAR_Var = ACAR_Var_temp*(1/n) ) %>%
    mutate(ZStat = ACAR/sqrt(ACAR_Var)) %>%
    mutate(Reject = ifelse( ZStat <= -1.960 | ZStat >= 1.960, 1, 0 )) %>% 
    mutate(KEY=KEY) %>%
    select(Issuer, n, ACAR, ACAR_Var, ZStat, Reject,KEY)
  # Test Results
  #ZTestACAR_I
  
  
  # Join CAR
  CAR <- CAR %>%
    left_join(., ZTestCAR[, c("KEY", "Reject")], by = c ("KEY" = "KEY") ) %>% 
    left_join(., COR[, c("KEY", "CORi", "CORi_VaR")],
              by = c ("KEY" = "KEY") ) 
  
  
  
  if(k==1){
    ZTestCAR_all = ZTestCAR
    SUMM_all = SUMM
    ResVar_all = ResVar
    CAR_all = CAR
    COR_all = COR
    ACAR_all = tab_ACAR
    ACAR_ISIN_all = tab_ACAR_ISIN
    ZTestACAR_all = ZTestACAR
    ZTestACAR_I_all = ZTestACAR_I
  } else {
    ZTestCAR_all = rbind(ZTestCAR_all,ZTestCAR)
    SUMM_all = rbind(SUMM_all,SUMM)
    ResVar_all = rbind(ResVar_all,ResVar)
    CAR_all = rbind(CAR_all,CAR)
    COR_all = rbind(COR_all,COR)
    ACAR_all = rbind(ACAR_all,tab_ACAR)
    ACAR_ISIN_all = rbind(ACAR_ISIN_all,tab_ACAR_ISIN)
    ZTestACAR_all = rbind(ZTestACAR_all,ZTestACAR)
    ZTestACAR_I_all = rbind(ZTestACAR_I_all,ZTestACAR_I)
  }
  
  
  
  
  
} # for k event_wdw_p

CAR_all <- CAR_all %>%
  left_join(., EVENTS,
            by = c ( "KEY" = "ISIN_event_id") ) 

COR_all <- COR_all %>%
  left_join(., EVENTS,
            by = c ( "KEY" = "ISIN_event_id") ) 

ZTestCAR_all <- ZTestCAR_all %>%
  left_join(., EVENTS,
            by = c ( "KEY" = "ISIN_event_id") ) 

ACAR_all <- ACAR_all %>%
  left_join(., EVENTS,
            by = c ( "KEY" = "ISIN_event_id") )

ACAR_ISIN_all <- ACAR_ISIN_all %>%
  left_join(., EVENTS,
            by = c ( "KEY" = "ISIN_event_id") )

ZTestACAR_all <- ZTestACAR_all %>%
  left_join(., EVENTS,
            by = c ( "KEY" = "ISIN_event_id") )

ZTestACAR_I_all <- ZTestACAR_I_all %>%
  left_join(., EVENTS,
            by = c ( "KEY" = "ISIN_event_id") )

ResVar_all <- ResVar_all %>%
  left_join(., EVENTS,
            by = c ( "KEY" = "ISIN_event_id") )


save.image(file = paste0(folder,  "OUTPUT_CDS/results_before_means.RData"))


################################################################################
### OUTPUT #####################################################################
################################################################################

tab_ENV_EVENTS <- CAR_all %>%
  left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
  group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
  summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
            Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
            min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
            VaR = mean(CARi_VaR, na.rm = TRUE), std = mean(CARi_std, na.rm = TRUE),
            Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))), n_event = n_distinct(KEY),
            Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
  )
tab_ENV_EVENTS$variable <- "CAR"
tab_ENV_EVENTS

### STATS by RepRisk CATEGORY 

for ( jj in 29:141) {
  
  tab_ENV_EVENTS_RepRisk_cat <- CAR_all %>%
    filter(CAR_all[,jj]=="T") %>%
    left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
    group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
    summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
              Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
              min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
              VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY), category = colnames(CAR_all[jj]),
              Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
    )
  tab_ENV_EVENTS_RepRisk_cat$variable <- "CAR"
  #tab_ENV_EVENTS_RepRisk_cat
  
  if (jj==29){
    tab_ENV_EVENTS_RepRisk_cat_all <- tab_ENV_EVENTS_RepRisk_cat
  } else {tab_ENV_EVENTS_RepRisk_cat_all <-rbind(tab_ENV_EVENTS_RepRisk_cat_all,tab_ENV_EVENTS_RepRisk_cat)}
}


### STATS by our CATEGORIES 

for ( jj in 144:147) {
  
  tab_ENV_EVENTS_cat <- CAR_all %>%
    filter(CAR_all[,jj]=="T") %>%
    left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
    group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
    summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
              Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
              min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
              VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY), category = colnames(CAR_all[jj]),
              Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
    )
  tab_ENV_EVENTS_cat$variable <- "CAR"
  #tab_ENV_EVENTS_cat
  
  if (jj==144){
    tab_ENV_EVENTS_cat_all <- tab_ENV_EVENTS_cat
  } else {tab_ENV_EVENTS_cat_all <-rbind(tab_ENV_EVENTS_cat_all,tab_ENV_EVENTS_cat)}
}

### STATS by econ sector

the_e_sectors=unique(CAR_all$TRBC.Economic.Sector.Name)

for ( jj in 1:length(the_e_sectors)) {
  
  tab_ENV_EVENTS_eco_sect <- CAR_all %>%
    filter(CAR_all$TRBC.Economic.Sector.Name==the_e_sectors[jj]) %>%
    left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
    group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
    summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
              Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
              min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
              VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY), economic_sector = the_e_sectors[jj],
              Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
    )
  tab_ENV_EVENTS_eco_sect$variable <- "CAR"
  
  if (jj==1){
    tab_ENV_EVENTS_eco_sect_all <- tab_ENV_EVENTS_eco_sect
  } else {tab_ENV_EVENTS_eco_sect_all <-rbind(tab_ENV_EVENTS_eco_sect_all,tab_ENV_EVENTS_eco_sect)}
}

### STATS by business sector

the_b_sectors=unique(CAR_all$TRBC.Business.Sector.Name)

for ( jj in 1:length(the_b_sectors)) {
  
  tab_ENV_EVENTS_bus_sect <- CAR_all %>%
    filter(CAR_all$TRBC.Business.Sector.Name==the_b_sectors[jj]) %>%
    left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
    group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
    summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
              Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
              min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
              VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY), business_sector = the_b_sectors[jj],
              Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
    )
  tab_ENV_EVENTS_bus_sect$variable <- "CAR"
  
  if (jj==1){
    tab_ENV_EVENTS_bus_sect_all <- tab_ENV_EVENTS_bus_sect
  } else {tab_ENV_EVENTS_bus_sect_all <-rbind(tab_ENV_EVENTS_bus_sect_all,tab_ENV_EVENTS_bus_sect)}
}

### STATS by industry sector

the_i_sectors=unique(CAR_all$TRBC.Industry.Group.Name)

for ( jj in 1:length(the_i_sectors)) {
  
  tab_ENV_EVENTS_ind_sect <- CAR_all %>%
    filter(CAR_all$TRBC.Industry.Group.Name==the_i_sectors[jj]) %>%
    left_join(., ResVar[, c("KEY", "Var")], by = c ( "KEY"= "KEY" ) )  %>%
    group_by(est_wdw,event_wdw_m,event_wdw_p) %>%
    summarise(avg = mean(CARi, na.rm = TRUE), med = quantile(CARi, probs = c(0.50), na.rm = TRUE),
              Q1 = quantile(CARi, probs = c(0.25), na.rm = TRUE),  Q3 = quantile(CARi, probs = c(0.75), na.rm = TRUE),
              min = min(CARi, na.rm = TRUE), max = max(CARi, na.rm = TRUE),
              VaR = mean(CARi_VaR, na.rm = TRUE), Std_err = sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))),n_event = n_distinct(KEY), industry_sector = the_i_sectors[jj],
              Reject = ifelse ( mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY)))) <= -1.960 | mean(CARi, na.rm = TRUE)/(sqrt((mean(Var*(event_wdw_m+event_wdw_p+1), na.rm = TRUE))*(1/n_distinct(KEY))))>= 1.960, 1, 0 )
    )
  tab_ENV_EVENTS_ind_sect$variable <- "CAR"
  
  if (jj==1){
    tab_ENV_EVENTS_ind_sect_all <- tab_ENV_EVENTS_ind_sect
  } else {tab_ENV_EVENTS_ind_sect_all <-rbind(tab_ENV_EVENTS_ind_sect_all,tab_ENV_EVENTS_ind_sect)}
}

### STATS by size

the_sizes=unique(CAR_all$SIZE)

for ( jj in 1:length(the_sizes)) {
  
  tab_ENV_EVENTS_size <- CAR_all %>%
    filter(CAR_all$SIZE==the_sizes[jj]) %>%
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

##############
### EXPORT ###
##############

if (EXPORT){
  
  # RESULTS + MODEL STATS
  file <- paste0 ( fileout, "_raw/CDS_Rep_Climate_5FactorsModel_15days.xlsx") 
  write.xlsx(list( 
    ZTestCAR = ZTestCAR_all,
    ZTestACAR = ZTestACAR_all,
    ZTestACAR_I = ZTestACAR_I_all,
    SUMMARY = SUMM_all,
    ResVar = ResVar_all,
    CAR = CAR_all,
    Environmental_events = tab_ENV_EVENTS,
    Environmental_events_cat=tab_ENV_EVENTS_cat_all,
    Environmental_evs_cat_RepRisk=tab_ENV_EVENTS_RepRisk_cat_all,
    tab_ENV_EVENTS_eco_sect=tab_ENV_EVENTS_eco_sect_all,
    tab_ENV_EVENTS_bus_sect=tab_ENV_EVENTS_bus_sect_all,
    tab_ENV_EVENTS_ind_sect=tab_ENV_EVENTS_ind_sect_all,
    tab_ENV_EVENTS_size=tab_ENV_EVENTS_size_all),
    file)
  
  
  
  
  
}



tab_ENV_EVENTS$avg_over_std=abs(tab_ENV_EVENTS$avg)/tab_ENV_EVENTS$std*100

sev_3=tab_ENV_EVENTS_sev$avg[tab_ENV_EVENTS_sev$severity=="3"]

tab_ENV_EVENTS$avg_over_std_sev3=abs(sev_3)/tab_ENV_EVENTS$std*100


my_plot=ggplot(tab_ENV_EVENTS, aes(x = event_wdw_p, y = avg_over_std)) +
  geom_line(color="Green") +
  labs(x = "Days", y = "ACAR / std ACAR (%)") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(1,15,1))+
  guides(color = FALSE)

my_plot + theme_bw()

my_plot=ggplot(tab_ENV_EVENTS, aes(x = event_wdw_p, y = avg_over_std_sev3)) +
  geom_line(color="Green") +
  labs(x = "Days", y = "ACAR sev 3 / std ACAR (%)") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(1,15,1))+
  guides(color = FALSE)

my_plot + theme_bw()




