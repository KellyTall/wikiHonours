library(tidyverse)
library(jsonlite)
library(lubridate)

date <- allData %>% 
        filter(wpPage=="Yes") %>% 
        select(fullName, OADate, WPPageCreate) %>% 
        mutate(OADate = as_date(OADate)) 
        # mutate(dateDiff = WPPageCreate - OADate) %>% 
        # mutate(dayDiff = as.integer(dateDiff)) %>% 
        # select(-dateDiff) %>% 
        slice_sample(n=100) 
        

compareDate <- toJSON(date)

write(compareDate, "compareDate.json")

write_csv(compareDate, "compareDate.csv")
