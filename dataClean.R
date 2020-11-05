library(tidyverse)
library(lubridate)
library(WikipediR)
library(httr)
library(RSelenium)
library(rvest)
library(xml2)


honoursList <- read_csv("OrderOfAus15Oct.csv") %>% 
        select(-X1) %>% 
        rename(honoursID=AwardId) 
        

# View(honoursList)

wikipediaPageList <- read_csv("wikipediaQuery15Oct.csv") %>% 
        rename(honoursID=honsid) %>% 
        mutate(wikipediaPage = case_when(!is.na(sitelink) ~ "Yes",
                                         TRUE ~ "NA")) %>% 
        mutate(wikipediaPage = str_replace_na(wikipediaPage)) %>% 
        mutate(honoursID2 = str_remove_all(refurl, "https://honours.pmc.gov.au/honours/awards/")) %>% 
        mutate(honoursID2 = str_replace_all(honoursID2, "http://old.gg.gov.au/.*", "NA")) %>% 
        mutate(honoursID2 = str_replace_all(honoursID2, "https://.*", "NA")) %>% 
        mutate(honoursID2= str_replace_na(honoursID2)) %>% 
        mutate(honoursID2= as.numeric(honoursID2)) %>% 
        mutate(honoursID = ifelse(is.na(honoursID), honoursID2, honoursID)) 


# wikiPagecheck <- wikipediaPageList %>% 
#         filter(is.na(honoursID)) %>% 
#         mutate(honoursID2 = str_remove_all(refurl, "https://honours.pmc.gov.au/honours/awards/")) %>% 
#         mutate(honoursID2 = str_replace_all(honoursID2, "http://old.gg.gov.au/.*", "NA")) %>% 
#         mutate(honoursID2 = str_replace_all(honoursID2, "https://.*", "NA")) %>% 
#         mutate(honoursID2= str_replace_na(honoursID2)) %>% 
#         mutate(honoursID2= as.numeric(honoursID2)) %>% 
#         mutate(honoursID = ifelse(is.na(honoursID), honoursID2, honoursID)) 

        

        
View(wikiPagecheck)




wikiHonoursMerge <- full_join(honoursList, wikipediaPageList, by="honoursID") 




# View(wikiHonoursMerge)

wikiHonours <- wikiHonoursMerge %>%
        # filter(wikipediaPage=="Yes") %>% 
        rename(wikipediaURL = sitelink) %>% 
        mutate(name = str_remove(wikipediaURL, "https://en.wikipedia.org/wiki/")) %>% 
        mutate(name = str_replace_all(name, "_", " ")) %>% 
        mutate(name = str_replace_all(name, "%27", "'")) %>% 
        mutate(name = str_replace_all(name, "%C3%B8", "ø")) %>% 
        mutate(name = str_replace_all(name, "%C3%96", "Ö")) %>% 
        mutate(name = str_replace_all(name, "%C3%A3", "ã")) %>% 
        mutate(name = str_replace_all(name, "%C5%8D", "ō")) %>% 
        mutate(name = str_replace_all(name, "%C5%8C", "Ō")) %>% 
        mutate(name = str_replace_all(name, "%C3%A0", "à")) %>% 
        mutate(name = str_replace_all(name, "%C3%A9", "é")) %>% 
        mutate(name = str_replace_all(name, "%22", '"')) %>% 
        mutate(name = str_replace_all(name, "%C3%B3", 'ó')) %>% 
        mutate(name = str_replace_all(name, "%E2%80%93", '–')) %>% 
        mutate(name = str_replace_all(name, "%C3%A1", 'á')) %>% 
        mutate(name = str_replace_all(name, "%C5%99%C3%AD", 'ří')) %>% 
        mutate(name = str_replace_all(name, "%C3%B6", 'ö'))

WPCreation <- read_csv("wpCreation.csv") 

View(WPCreation)
  

wpDateCheck <- allData %>% 
        filter(is.na(WPDate)) %>% 
        filter(wikipediaPage =="Yes") %>% 
        filter(is.na(wikipediaPageID)) %>% 
        select(name, wikipediaURL) %>% 
        mutate(search = str_remove(wikipediaURL, "https://en.wikipedia.org/wiki/")) 




wpDateCheck2 <- lapply(wpDateCheck$search, function (i) {
        
        
        getinfo <- page_info("en", "wikipedia", page= i , clean_response = TRUE)
        # getinfo <- page_info("en", "wikipedia", page="Mark_Opitz", clean_response = TRUE)
        
})

wpDateCheck3 <- unlist(wpDateCheck2, recursive=FALSE)

View(wpDateCheck4)

wpDateCheck4 <- tibble (
        id = map(wpDateCheck3, "pageid"),
        wpURL = map(wpDateCheck3, "fullurl"),
        name = map(wpDateCheck3, "displaytitle")) %>% 
        mutate(
                pageid = map_chr(id, 1, .default = NA),
                wpURL    = map_chr(wpURL, 1, .default = NA),
                displayName = map_chr(name, 1, .default = NA)
        ) %>% 
        select (-id, -name) %>% 
        filter(!is.na(wpURL)) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%27", "'")) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%C3%B8", "ø")) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%C3%96", "Ö")) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%C3%A3", "ã")) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%C5%8D", "ō")) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%C5%8C", "Ō")) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%C3%A0", "à")) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%C3%A9", "é")) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%22", '"')) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%C3%B3", 'ó')) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%E2%80%93", '–')) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%C3%A1", 'á')) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%C5%99%C3%AD", 'ří')) %>% 
        mutate(wpURL = str_replace_all(wpURL, "%C3%B6", 'ö')) %>% 
        rename(wikipediaPageID = pageid)




pageCreation2 <- wpDateCheck4 %>% 
        mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
        mutate(searchURL=paste(base, wikipediaPageID, sep = "")) %>% 
        select(searchURL) 


ExtractPageCreation1 <- lapply(pageCreation2$searchURL, function(i){
        
        webpage <- read_html(i)
        
        EntryInfo <- html_nodes(webpage, ".s2") %>% 
                html_nodes(xpath="./text()[normalize-space()]") %>% 
                html_text(trim=TRUE) %>% 
                as_tibble() %>% 
                slice_tail(n=9) %>% 
                rownames_to_column() %>%
                pivot_longer(-rowname) %>%
                pivot_wider(names_from = rowname, values_from=value) %>% 
                select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
                rename(pageID=`2`,
                       name = `6`,
                       pageCreation = `9`)
        
})


comboPageCreation <- bind_rows(ExtractPageCreation1)

WPpageCreationUpdate  <- comboPageCreation %>%  
        mutate(pageID =str_remove_all(pageID , "[[\\p{P}][\\p{S}]]"),
               name =str_remove_all(name , "[[\\p{P}][\\p{S}]]"),
               pageCreation =str_remove_all(pageCreation , "\"" )) %>% 
        mutate(pageCreation = date(pageCreation)) %>%
        rename(wikipediaPageID = pageID) %>% 
        mutate(wikipediaPageID = as.numeric(wikipediaPageID)) 




WPCreation2 <- bind_rows(WPpageCreationUpdate, WPCreation) %>% 

  





WPID1 <- read_csv("wikipediaID.csv") %>% 
        rename(name = displayName) %>% 
        rename(wikipediaPageID=pageid)

wpDateCheck4 <- wpDateCheck4 %>% 
        mutate(wikipediaPageID = as.numeric(wikipediaPageID)) %>% 
        rename(name = displayName) 
        
WPID <- bind_rows(WPID1, wpDateCheck4)



mergeWPID <- left_join(wikiHonours, WPID, by="name") %>% 
        rename(wikipediaPageID = pageid) 
        

View(mergeWPID)

mergePageCreation <- left_join(mergeWPID, WPCreation2, by="wikipediaPageID") 

mergePageCreation %>%   mutate(pageCreation = ymd(pageCreation, tz = "Australia/Sydney"))

View(mergePageCreation)






# wpcheck <- mergePageCreation %>% 
#         filter(wikipediaPage=="Yes") %>% 
#         tally()

View(mergePageCreation)

allData2   <- mergePageCreation %>% 
        select(-c(ClaspLevel, ClaspText, GazetteGivenName, GazetteSurname, AdditionalInfo, personLabel, refurl, orderaus,  
                  date_awarded, wpURL, GazettePostcode, GazetteSuburb, AwardSystem, GazettePostcode, name.y)) %>% 
        rename(state = GazetteState, 
               wikiDataURL = person , 
               wikiDataDescription = personDescription,
               name=name.x,
               fullName = GazetteName,
               division = Division,
               citation = Citation,
               WPDate = pageCreation,
               honoursDate = AwardedOn,
               gender = Gender) %>% 
        mutate(
                state = case_when(
                        state == "NSW" |state == "Nsw" |state == "BULLI" |state == "ARMIDALE" |
                                state == "CASTEL HILL" | state == "CASTLE HILL" | state == "COFFS HARBOUR" 
                        | state == "GOULBURN" | state == "WAHROONGA" | state =="NORFOLK ISLAND" ~ "NSW",
                        state =="VIC" | state =="Vic" | state =="Victoria"| state =="PRAHRAN" ~ "VIC",
                        state =="TAS" |state =="Tas" | state =="KING ISLAND TAS" ~ "TAS",
                        state =="WA" ~ "WA",
                        state =="SA" ~ "SA",
                        state =="QLD" | state =="Qld" | state =="CANUNGRA" |state =="HERSTON, QLD" | state =="CAIRNS"   ~ "QLD",
                        state =="NT" ~ "NT",
                        state =="ACT" |state =="BRUCE" |state =="KINGSTON" ~ "ACT",
                        TRUE ~ "Other"
                )) %>% 
        mutate(
                AwardAbbr2 = case_when(
                        orderausLabel == "Companion of the Order of Australia" ~ "AC",
                        orderausLabel == "Member of the Order of Australia" ~ "AM",
                        orderausLabel == "Officer of the Order of Australia" | orderausLabel == "Honorary Officer of the Order of Australia" ~ "AO",
                        orderausLabel == "Medal of the Order of Australia" ~ "OAM"
                )) %>% 
        mutate(AwardAbbr = ifelse(is.na(AwardAbbr), AwardAbbr2, AwardAbbr)) %>%  
        
        mutate(
                awardComb = case_when(
                        AwardAbbr == "AD" | AwardAbbr == "AK" ~ "ADK",
                        AwardAbbr == "AC" ~ "AC",
                        AwardAbbr == "AM" ~ "AM",
                        AwardAbbr == "AO" ~ "AO",
                        AwardAbbr == "OAM" ~ "OAM"
                )) %>% 
        mutate(state = factor(state, levels = c("NSW", "VIC", "QLD", "SA", "TAS", "WA", "ACT", "NT", "Other"))) %>% 
        mutate(awardComb = factor(awardComb, levels = c("ADK", "AC", "AO", "AM", "OAM"))) %>% 
        select(orderausLabel,AwardAbbr, name, awardComb, WPDate, honoursDate, wikipediaPage, gender, state, wikipediaPageID, AwardName, fullName, AnnouncementEvent, division, citation, 
               wikiDataDescription, wikipediaURL, wikipediaPageID) %>% 
        mutate(name = ifelse(!is.na(name), name, fullName)) %>% 
        mutate(WPDate = ymd(WPDate),
               honoursDate = dmy_hms(honoursDate),
               honoursDate = as.Date(honoursDate, tz = "Australia/Sydney")) %>% 
        mutate(wikipediaPage = case_when(
                wikipediaPage =="Yes" ~ "Yes",
                TRUE ~"No"
        )) %>% 
        mutate(honoursYear = year(honoursDate),
               wikipediaYear = year(WPDate)) %>% 
        select(1:4, honoursYear, wikipediaYear, 5:17) %>% 
        arrange(name,honoursDate, division) %>% 
        group_by(name) %>% 
        fill(division, .direction="updown") %>% 
        ungroup() %>% 
        mutate(division2 = case_when(grepl("Air Commodore", name) |
                                             grepl("Air Vice-Marshal", name) |
                                             grepl("Air Vice Marshal", name) |
                                             grepl("Brigadier", name) |
                                             grepl("Captain", name) |
                                             grepl("Colonel", name) |
                                             grepl("Commander", name) |
                                             grepl("Lieutenant", name) |
                                             grepl("Major", name) |
                                             grepl("Warrant Officer", name) 
                                     ~ "M")) %>% 
        mutate(division = ifelse(!is.na(division), division, division2)) %>% 
        select(-c(orderausLabel , AwardAbbr)) %>% 
        filter(!is.na(honoursDate))  
  


##page creation fix

pageCreateFix1 <- allData %>% 
  select(wikipediaPageID) %>% 
  filter(!is.na(wikipediaPageID)) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(searchURL=paste(base, wikipediaPageID, sep = "")) %>% 
  select(searchURL) %>% 
  slice(1:1000)


ExtractPageCreation1 <- lapply(pageCreateFix1$searchURL, function(i){
  
  webpage <- read_html(i)
  
  EntryInfo <- html_nodes(webpage, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

pageCreateFix2 <- allData %>% 
  select(wikipediaPageID) %>% 
  filter(!is.na(wikipediaPageID)) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(searchURL=paste(base, wikipediaPageID, sep = "")) %>% 
  select(searchURL) %>% 
  slice(1001:2000)


ExtractPageCreation2 <- lapply(pageCreateFix2$searchURL, function(i){
  
  webpage <- read_html(i)
  
  EntryInfo <- html_nodes(webpage, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

pageCreateFix3 <- allData %>% 
  select(wikipediaPageID) %>% 
  filter(!is.na(wikipediaPageID)) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(searchURL=paste(base, wikipediaPageID, sep = "")) %>% 
  select(searchURL) %>% 
  slice(2001:4596)


ExtractPageCreation3 <- lapply(pageCreateFix3$searchURL, function(i){
  
  webpage <- read_html(i)
  
  EntryInfo <- html_nodes(webpage, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})


pageFixBind<- bind_rows(ExtractPageCreation1, ExtractPageCreation2, ExtractPageCreation3)

WPpageCreationUpdate  <- pageFixBind %>%  
  mutate(pageID =str_remove_all(pageID , "[[\\p{P}][\\p{S}]]"),
         name =str_remove_all(name , "[[\\p{P}][\\p{S}]]"),
         pageCreation =str_remove_all(pageCreation , "\"" )) %>% 
  mutate(pageCreation = ymd_hms(pageCreation, tz="UTC"),
         pageCreation1 = with_tz(pageCreation, "Australia/Sydney"),
         WPpageCreation = as_date(pageCreation1)) %>% 
  rename(wikipediaPageID = pageID) %>% 
  mutate(wikipediaPageID = as.numeric(wikipediaPageID)) %>% 
  select(wikipediaPageID, WPpageCreation)

allData <- left_join(allData2, WPpageCreationUpdate, by="wikipediaPageID") %>% 
  select(-WPDate) %>% 
  rename(WPDate = WPpageCreation) 


# View(allData)
# 
# allDataDateCheck <- allData %>%
#         filter(wikipediaPage == "Yes") %>% 
#         filter(is.na(WPDate))
# 
# View(allDataDateCheck)


write_csv(allData, "allData.csv")









# awardlevelcheck <- allData %>% 
#         filter(is.na(awardComb)) %>% 
#         select(awardComb, AwardAbbr, orderausLabel)
        


# View(allData)



##combining multiple award holders so data is recipient level

recipient <- allData %>% 
        arrange(name, honoursDate) %>% 
        group_by(name) %>% 
        add_tally() %>% 
        # filter(n>1) %>%
        mutate(firstAwardDate = min(honoursDate)) %>% 
        mutate(highestAward1 = case_when(awardComb == "OAM" ~1,
                                         awardComb == "AM" ~2,
                                         awardComb == "AO" ~3,
                                         awardComb == "AC" ~4,
                                         awardComb == "ADK" ~5)) %>% 
        mutate(highestAward2 = max(highestAward1)) %>% 
        mutate(highestAward = case_when(highestAward2 == 1 ~"OAM",
                                        highestAward2 == 2 ~"AM" ,
                                        highestAward2 == 3~"AO",
                                        highestAward2 == 4~"AC",
                                        highestAward2 == 5 ~"ADK")) %>% 
        filter(highestAward1 == max(highestAward1)) %>% 
        rename(numberOfHonours = n) %>% 
        mutate(honoursYear = year(firstAwardDate),
               wikipediaYear = year(WPDate)) %>% 
        select(-c(highestAward1, highestAward2 , highestAward)) %>% 
        mutate(prePostWikiPedia = case_when(firstAwardDate<"2001-01-15" ~ "Pre",
                                   firstAwardDate>="2001-01-15" ~ "Post")) %>% 
        ungroup() %>% 
        select(1:7, firstAwardDate, prePostWikiPedia, 8:21)



        
# View(recipient)





write_csv(recipient, "recipient.csv")
        




wikipedia <- recipient %>% 
        filter(wikipediaPage=="Yes") %>% 
        mutate(wikiCreationDate = "2001-01-15") %>% 
        mutate(wikiCreationDate = ymd(wikiCreationDate)) %>% 
        mutate(newAwardDate = replace(firstAwardDate, firstAwardDate<"2001-01-15", "2001-01-15")) %>% 
        mutate(newAwardYear = year(newAwardDate),
               honoursYear = year(honoursDate)) %>% 
        mutate(newTimeDiff = WPDate - newAwardDate) %>% 
        mutate(newYearDiff = wikipediaYear - newAwardYear) %>% 
  select(1:8, 23:26, 9:22, -wikiCreationDate)



        
write_csv(wikipedia, "wikipedia.csv")        





anon1 <- allData %>% 
        filter_all(any_vars((str_detect(.,"Anonymous")))) 
      


anonWP <- recipient %>% 
        filter_all(any_vars((str_detect(.,"Anonymous")))) %>% 
        filter(wikipediaPage =="Yes")




View(anon1)


View(wikipediaPageList)
prePostAward <- wikipedia %>% 
        # select(yearDiff) %>% 
        group_by(yearDiff) %>% 
        tally()
        


ggplot(prePostAward, aes(yearDiff, n)) +
        geom_col()

anon2 <- allData %>% 
        filter(wikipediaPage =="Yes") %>% 
        filter_all(any_vars((str_detect(.,"TERMINATED")))) 


View(allData)

View(WPpageCreationUpdate)




# multiNameCheck <- wikipediaPageList %>% 
#         select(personLabel, refurl, orderausLabel) %>% 
#         group_by(personLabel) %>% 
#         add_tally() %>% 
#         filter(n>1)
#         
# View(multiNameCheck)
# 
# honoursIDFix <- wikipediaPageList %>%
#         filter(is.na(honoursID)) %>%
#         select(refurl, personLabel, honoursID) %>%
#         mutate(honoursID = str_remove_all(refurl, "https://honours.pmc.gov.au/honours/awards/")) %>%
#         mutate(honoursID = na_if(honoursID, "http://old.gg.gov.au/sites/default/files/files/honours/aaagazattes/1983-1993/Order%20of%20Australia%20-%20Australia%20Day%201988%20(AD88)%20-%20Gazette%20S20.pdf"))
#         

# View(honoursIDFix)
# View(wikipediaPageList)

# aggregating data to weeks


yearAndweeks <- wikipedia %>% 
  select(WPDate, newAwardDate) %>% 
  mutate(WPweek = strftime(WPDate, format = "%Y-W%V"),
         honWeek = strftime(newAwardDate, format = "%Y-W%V"),
         weekDiff = interval(newAwardDate, WPDate) / dweeks(1),
         weekDiffRound = floor(weekDiff))



