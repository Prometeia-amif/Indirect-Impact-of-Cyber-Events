devtools::install_github("hadley/devtools")
devtools::install_github("hadley/dplyr")
devtools::install_github("hafen/trelliscopejs")


devtools::install_github("abresler/gdeltr2")

library(gdeltr2)
library(lubridate)


my_Data <- get_data_gdelt_periods_event(
  periods = c(20210101, 20220101),
  remove_files = T,
  empty_trash = T,
  return_message = T
)


start_time <- now()

my_data2 <-
  get_data_gkg_days_detailed(
    dates = c('2021-01-05'),
    remove_files = T,
    empty_trash = T,
    return_message = T
  )


elaps_time <- now() - start_time
print(elaps_time)


row_numbers <- which(grepl("\\bUBS\\b", my_data2$organizations, ignore.case = TRUE))
