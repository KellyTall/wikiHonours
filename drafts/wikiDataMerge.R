library(tidyverse)
library(ggplot2)
library(lubridate)

##honours data sets
OAqBday <- read_csv("OA_QueenBDay.csv")
OAausDay <- read_csv("OA_AusDay.csv")

##honours list merged and reformtted
OAFullList <- bind_rows(OAqBday, OAausDay)

View(OAFulllist)

OA <- OAFullList %>% 
        select(AwardId, AwardedOn, AwardName, AwardAbbr, GazetteGivenName, GazetteSurname, GazetteState) %>% 
        rename(OAID =AwardId,
               OADate = AwardedOn) %>% 
        mutate(
                Award = case_when(
                        AwardAbbr == "AD" | AwardAbbr == "AK" ~ "ADK",
                        AwardAbbr == "AC" ~ "AC",
                        AwardAbbr == "AM" ~ "AM",
                        AwardAbbr == "AO" ~ "AO",
                        AwardAbbr == "OAM" ~ "OAM"
                )) %>% 
        mutate(GazetteState = toupper(GazetteState) )%>% 
        mutate(
                state = case_when(
                        GazetteState == "NSW" |GazetteState == "BULLI" |GazetteState == "ARMIDALE" |
                                GazetteState == "CASTEL HILL" | GazetteState == "COFFS HARBOUR" 
                        | GazetteState == "GOULBURN" | GazetteState == "WAHROONGA" ~ "NSW",
                        GazetteState =="VIC" | GazetteState =="PRAHRAN" ~ "VIC",
                        GazetteState =="TAS" | GazetteState =="KING ISLAND TAS" ~ "TAS",
                        GazetteState =="WA" ~ "WA",
                        GazetteState =="SA" ~ "SA",
                        GazetteState =="QLD" | GazetteState =="CANUNGRA" |GazetteState =="HERSTON, QLD"  ~ "QLD",
                        GazetteState =="NT" ~ "NT",
                        GazetteState =="ACT" |GazetteState =="BRUCE" |GazetteState =="KINGSTON" ~ "ACT",
                        TRUE ~ "Other"
                )
        ) %>% 
        mutate(surname = tolower(GazetteSurname) ,
               firstName = tolower(GazetteGivenName)) %>% 
        mutate(fullName = paste(firstName, surname, sep=" ")) %>% 
        select(-GazetteState, -GazetteSurname, -AwardAbbr, -GazetteGivenName, -firstName, -surname ) %>% 
        mutate(OADate = dmy_hms(OADate))


##wikiDataAus List import and reformat

wikidata <- read_csv("wikidataAusMatch.csv")

wikidata <- wikidata %>% 
        rename(wdID = id,
               name = label,
               fullName =  matchText,
               wdURL = url,
               wdDescription = description) %>% 
        mutate(fullName = tolower(fullName), 
               name = tolower(name)) %>% 
        select(-wikidataMatch,  -wikidataAusMatch, -PossibleAlias) %>% 
        mutate(wdEntry = "Yes")
        
view(wikidata)


## data using short name to help merge

wikiShortName <-  read_csv("wikiShortName.csv") %>% 
  distinct() %>% 
  rename(wdID = wd,
         shortName = value) %>% 
  mutate(shortName = tolower(shortName)) %>% 
  distinct()

View(wikiShortName)

##wikipedia List import and reformat

wikipedia1 <- read_csv("wikipediaArticleID.csv")  %>% 
  rename(shortName = displayName,
         wpURL = url,
         wpID = pageid) %>% 
  mutate(shortName = tolower(shortName)) %>% 
  mutate(wpEntry="Yes") %>% 
  distinct() 

View(wikipedia1)

pageCreation <- read_csv("pageCreation.csv") %>% 
  mutate(name = tolower(name)) %>% 
  rename(wpID = wikipediaPageID,
         shortName = name) %>% 
  distinct() %>% 
  rename(WPPageCreate = pageCreation) %>% 
  select(-shortName)

View(pageCreation)

wikipedia <- left_join(wikipedia1, wikiShortName, by="shortName") %>% 
  left_join(., pageCreation, by="wpID") %>% 
  select(shortName, wpURL, wpID, wdID, WPPageCreate)

View(wikipedia)

# fixedCreationDate <- read_csv("fixedCreationDate.csv") %>% 
#   rename(WPPageCreate = pageCreation)
# 
# wikipedia <- left_join(wikipedia2, fixedCreationDate, by="wpID")

# write_csv(wikipedia, "fixCreationDate.csv")
##wiki page creation

      
##merging wikidata with wikipedia

wiki <- left_join(wikipedia, wikidata, by="wdID")


##merging OA with wiki


allData <- left_join(OA, wiki,  by="fullName")

View(allData)

allData <- allData %>% 
  mutate(wpPage = case_when(
    wpID > 0 ~ "Yes",
    TRUE ~ "No")) %>% 
  mutate(wdEntry = case_when(
    wdEntry =="Yes" ~ "Yes",
    TRUE ~ "No"
  ))

write_csv(allData, "mergedData.csv")