library(GDELTtools)

FIN <- CAR_all %>%
  filter(CAR_all$NACE1=="FINANCIAL AND INSURANCE ACTIVITIES")

bank_list = unique(FIN$company_name)


df2 <- GetGDELT(start_date="2021-01-05", end_date="2021-01-06")



data <- read_csv("C:/Users/ivan.delorenzo/Downloads/Gdelt_download.csv")

file ="C:/Users/ivan.delorenzo/Downloads/dataG.xlsx"
write.xlsx(list( 
  df2 = df2 ),
  file)