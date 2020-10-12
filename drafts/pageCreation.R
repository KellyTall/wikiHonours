# install.packages("httr")
# install.packages("RSelenium")

# startng docker - enter below in terminal
#1 -  docker run -d -p 4445:4444 selenium/standalone-chrome
#2 - docker ps

# setwd("~/Documents/UTS/Cook")
library(httr)
library(RSelenium)
library(rvest)
library(xml2)
library(tidyverse)
library(lubridate)
library(WikipediR)

# reading in data ---------------------------------------------------- 
pageCreation1 <- read_csv("wikipediaMatch.csv")

View(pageCreation1)
pageCreation2 <- pageCreation1 %>% 
        drop_na(pageid) 
# pasting in action query search ---------------------------------------------------- 




View(wikipediaLink1)


wikipediaLink1 <- wikidataAusMatch %>% 
        select(id) %>% 
        mutate(wdlink = paste("https://www.wikidata.org/wiki/", id, sep="")) 

        
 

wikipediaLink2 <- lapply(wikipediaLink1$wdlink, function (i) {
        
        webpage <- read_html(i)

        wpInfo <- webpage %>% 
                html_nodes(".wikibase-sitelinkview-link-enwiki") %>% 
                html_nodes("span") %>% 
                html_nodes("a") %>%
                xml_attrs("character[1][1]") %>% 
                unlist() %>% 
                as_tibble() %>% 
                rownames_to_column() 
        
        # View(wdInfo)
        
})

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()


##HERE WE ARE SLEEPY KELLY
##getting wikidata ID to match with short name
wikipediaLinkAdd <- lapply(wikipediaLink1$wdlink, function (i) {
        
        webpage <- read_html(i)
        # webpage <- read_html("https://www.wikidata.org/wiki/Q42947998")
        
        wpInfo <- webpage %>% 
                html_nodes(".wikibase-sitelinkview-link-enwiki") %>% 
                html_nodes("span") %>% 
                html_nodes("a") %>%
                xml_attrs("character[1][1]") %>% 
                unlist() %>% 
                as_tibble() %>% 
                mutate(wd = i) %>% 
                rownames_to_column() 
                # mutate(wd = i)
                
})





wikipediaLink3Add <- Filter(nrow, wikipediaLinkAdd) 

wikipediaLink4Add <- bind_rows(wikipediaLink3Add) %>% 
        filter(value !="en") %>% 
        mutate(wd = str_remove(wd, "https://www.wikidata.org/wiki/")) %>% 
        filter(rowname!=1) %>% 
        select(-rowname)
        
write_csv(wikipediaLink4Add, "wikiShortName.csv")

# wikipediaLink4Add <- wikipediaLink3Add %>% 
#         reduce(left_join, by="rowname") %>% 
#         pivot_longer(-rowname) %>% 
#         pivot_wider(names_from = rowname, values_from=value) %>%
#         select(-name, -`1`) %>% 
#         rename(name = `2`,
#                wdID = `3`) %>% 
#         mutate (wdID = str_remove(wdID, "[(]" )) %>% 
#         mutate (wdID = str_remove(wdID, "[)]" ))

write_csv(wikipediaLink4Add, "wdIDLink.csv")

write_csv(wikipediaLink4,  "wikipediaMatch.csv")

##get wikipedia page ID usng new link

wikipediaID <- wikipediaLink4 %>% 
        mutate(search = str_remove(wikipediaURL, "https://en.wikipedia.org/wiki/"))


wikipediaID1 <- lapply(wikipediaID$search, function (i) {
        
        
        getinfo <- page_info("en", "wikipedia", page= i , clean_response = TRUE)
        # getinfo <- page_info("en", "wikipedia", page="Mark_Opitz", clean_response = TRUE)
        
})

wikipediaID2 <- unlist(wikipediaID1, recursive=FALSE)

View(wikipediaID2)

wikipediaID3 <- tibble (
        id = map(wikipediaID2, "pageid"),
        url = map(wikipediaID2, "fullurl"),
        name = map(wikipediaID2, "displaytitle")) %>% 
        mutate(
                pageid = map_chr(id, 1, .default = NA),
                url = map_chr(url, 1, .default = NA),
                displayName = map_chr(name, 1, .default = NA)
        ) %>% 
        select (-id, -name) %>% 
        drop_na(pageid) 

##this is the file to use with wikipedia info - full url and article ID with name field

write_csv(wikipediaID3, "wikipediaArticleID.csv")

##then run the edit date query

View(pageCreation1)
pageCreation1 <- wikipediaID3 %>% 
        mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
        mutate(searchURL=paste(base, pageid, sep = "")) 


# scraper set up ---------------------------------------------------- 

# startng docker - enter below in terminal
#1 -  docker run -d -p 4445:4444 selenium/standalone-chrome
#2 - docker ps


remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()

# 
# round1 <- pageCreation3 %>%
#         slice(1:20)

webpage <- read_html()

# starting scraper ---------------------------------------------------- 

pageCreation2 <- lapply(pageCreation1$searchURL, function(i){
        
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

pageCreation3 <- bind_rows(pageCreation2)

pageCreation4  <- pageCreation3 %>%  
        mutate(pageID =str_remove_all(pageID , "[[\\p{P}][\\p{S}]]"),
               name =str_remove_all(name , "[[\\p{P}][\\p{S}]]"),
               pageCreation =str_remove_all(pageCreation , "\"" )) %>% 
        mutate(pageCreation = date(pageCreation)) %>% 
        rename(wikipediaPageID = pageID)

write_csv(pageCreation4, "pageCreation.csv")


##new page creation fixing the disambiguation pages

fixCreation <- read_csv("fixCreationDate.csv") %>% 
        filter(is.na(WPPageCreate)) %>% 
        slice(1:10)

View(fixCreation)

fixCreation1 <- fixCreation %>% 
        mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
        mutate(searchURL=paste(base, wpID, sep = "")) 


fixCreation2 <- lapply(fixCreation1$searchURL, function(i){
        
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
                rename(wpID=`2`,
                       shortName = `6`,
                       pageCreation = `9`)
        
})

fixCreation3 <- bind_rows(fixCreation2)

fixCreation4  <- fixCreation3 %>%  
        mutate(wpID =str_remove_all(wpID , "[[\\p{P}][\\p{S}]]"),
               shortName =str_remove_all(shortName , "[[\\p{P}][\\p{S}]]"),
               pageCreation =str_remove_all(pageCreation , "\"" )) %>% 
        mutate(pageCreation = date(pageCreation)) 

write_csv(fixCreation4, "fixedCreationDate.csv")