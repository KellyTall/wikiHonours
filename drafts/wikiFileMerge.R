library(tidyverse)
library(lubridate)

OAList <- read_csv("order-of-australia_Alex.csv")  %>% 
        select(-ID, -ClaspLevel, -ClaspText) %>% 
        mutate(AwardId = as.integer(AwardId))

View(OAList)

wdList <- read_csv("wikiDataLink.cvs") %>% 
        rename(AwardId = refurl,
               wdID = person,
               description = personDescription,
               AwardName = orderausLabel) %>% 
        select(AwardId, wdID, personLabel, description, date_awarded) %>% 
        mutate(AwardId = as.integer(AwardId)) %>% 
        mutate(wdEntry = "Yes")
        

View(wdList)

wpList <- read_csv("wpPage.csv") %>% 
        mutate(wpPage = "Yes")

View(wpList)

wpID <- read_csv("WPUrlID.csv") %>% 
        mutate(wpID = as.integer(pageid )) %>% 
        rename(wpURL = url) %>% 
        select(-pageid)

wpCreate <- read_csv("wpCreation.csv") %>% 
        rename(wpID =wikipediaPageID) %>% 
        mutate(wpID = as.integer(wpID))



View(wpCreate)


OAwithWd <- left_join(OAList, wdList, by="AwardId")

OAWDwithWP <- left_join(OAwithWd, wpList, by="wdID")

wpDetails <- left_join(wpCreate, wpID, by="wpID")

allData <- left_join(OAWDwithWP, wpDetails, by="wpURL")

View(allData)

check <- wdList %>% 
        filter(!is.na(AwardId))

check2 <- wdList %>% 
        filter(is.na(AwardId))

View(check2)
View(check)