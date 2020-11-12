library(tidyverse)
library(lubridate)
library(WikipediR)
library(httr)
library(RSelenium)
library(rvest)
library(xml2)
library(janitor)
library(WikidataQueryServiceR)
library(plotly)
library(scales)
##importing honours_list

honours_list_import <- read_csv("OrderOfAus15Oct.csv") 


##tidying file and 
honours_list <-  honours_list_import %>% 
        select(-X1) %>% 
        rename(honsid=AwardId) %>% 
        clean_names()




##extracting details from wikidata - using WikidataQueryServiceR

award_url <- query_wikidata("SELECT ?person ?personLabel ?personDescription ?refurl ?orderaus ?award_name ?honsid ?date_awarded ?sitelink 
WHERE {
  ?person wdt:P31 wd:Q5 .
  ?person p:P166 ?award .
  ?award ps:P166 ?orderaus .
  ?orderaus wdt:P361 wd:Q1141149 .
  OPTIONAL {?award pq:P585 ?date_awarded}.
  OPTIONAL {?award prov:wasDerivedFrom ?ref .
  ?ref pr:P854 ?refurl } .
  OPTIONAL {?award prov:wasDerivedFrom ?ref .
  ?ref pr:P4766 ?honsid } .
  OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  SERVICE wikibase:label { bd:serviceParam wikibase:language 'en' }
}")


## what records have no honsid on file
award_url_hons_id_fix_check <- award_url %>% 
        filter(is.na(honsid))
        


##fixing honours ID

award_url_hons_id_fix <- award_url %>% 
        mutate(honsid2 = str_remove_all(refurl, "https://honours.pmc.gov.au/honours/awards/")) %>% 
        mutate(honsid2 = str_replace_all(honsid2, "http://old.gg.gov.au/.*", "NA")) %>% 
        mutate(honsid2 = str_replace_all(honsid2, "https://.*", "NA")) %>% 
        mutate(honsid2= str_replace_na(honsid2)) %>% 
        mutate(honsid2= as.numeric(honsid2)) %>% 
        mutate(honsid = ifelse(is.na(honsid), honsid2, honsid)) %>% 
        select(-honsid2)

##removes all without necessary detail like honours ID for merging back with rest of data
award_url_hons_id_fix_check2 <- award_url_hons_id_fix %>% 
        filter(!is.na(honsid))


# View(award_url_hons_id_fix_check2)
# check_id <- award_url %>% 
#         filter(is.na(honsid))
# 
# View(check_id)

##tidying variable names
wiki_url <-  clean_names(award_url_hons_id_fix_check2)


# View(awardurl)


### fixing / tidying extracted info 
### removing all with no wikipedia page

wikipedia_page_list <- wiki_url %>% 
        mutate(wikipedia_page = case_when(!is.na(sitelink) ~ "Yes",
                                 TRUE ~ "No")) %>% 
        rename(wikidata_link = person) %>% 
        filter(wikipedia_page =="Yes") %>% 
        rename(wikipedia_url = sitelink) %>% 
        distinct()


# wikipediaPageListCheck <- wikipediaPageList%>% 
#         select(wikipedia_page) %>% 
#         group_by(wikipedia_page) %>% 
#         tally()

## preparing data to extract wikipage ID

wikipedia_page_query <- wikipedia_page_list %>%
        # rename(wikipedia_url = sitelink) %>% 
        select(wikipedia_url) %>% 
        mutate(name = str_remove(wikipedia_url, "https://en.wikipedia.org/wiki/")) %>% 
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
        mutate(name = str_replace_all(name, "%C3%B6", 'ö')) %>% 
        mutate(name = str_remove_all(name , "[[\\p{P}][\\p{S}]]")) %>% 
        distinct()

## getting page info using query

wikipedia_page_extraction <- lapply(wikipedia_page_query$name, function (i) {


                getinfo <- page_info("en", "wikipedia", page= i , clean_response = TRUE)
                # getinfo <- page_info("en", "wikipedia", page="Jacques_Cousteau", clean_response = TRUE)

        })

wikipedia_page_extraction_unlist <- unlist(wikipedia_page_extraction, recursive=FALSE)



wikipedia_page_extraction_format <- tibble (
        id = map(wikipedia_page_extraction_unlist, "pageid"),
        wpURL = map(wikipedia_page_extraction_unlist, "fullurl"),
        name = map(wikipedia_page_extraction_unlist, "displaytitle")) %>%
        mutate(
                pageid = map_chr(id, 1, .default = NA),
                wpURL    = map_chr(wpURL, 1, .default = NA),
                displayName = map_chr(name, 1, .default = NA)
        ) %>%
        select (-id, -name) %>%
        clean_names() %>% 
        mutate( wp_url   = str_replace_all( wp_url  , "%27", "'")) %>%
        mutate( wp_url = str_replace_all(wp_url, "%C3%B8", "ø")) %>%
        mutate(wp_url = str_replace_all(wp_url, "%C3%96", "Ö")) %>%
        mutate(wp_url = str_replace_all(wp_url, "%C3%A3", "ã")) %>%
        mutate(wp_url = str_replace_all(wp_url, "%C5%8D", "ō")) %>%
        mutate(wp_url = str_replace_all(wp_url, "%C5%8C", "Ō")) %>%
        mutate(wp_url = str_replace_all(wp_url, "%C3%A0", "à")) %>%
        mutate(wp_url = str_replace_all(wp_url, "%C3%A9", "é")) %>%
        mutate(wp_url = str_replace_all(wp_url, "%22", '"')) %>%
        mutate(wp_url = str_replace_all(wp_url, "%C3%B3", 'ó')) %>%
        mutate(wp_url = str_replace_all(wp_url, "%E2%80%93", '–')) %>%
        mutate(wp_url = str_replace_all(wp_url, "%C3%A1", 'á')) %>%
        mutate(wp_url = str_replace_all(wp_url, "%C5%99%C3%AD", 'ří')) %>%
        mutate(wp_url = str_replace_all(wp_url, "%C3%B6", 'ö')) %>%
        rename(wp_pageid = pageid)

### formatting page id to search for page creation date, removing duplicates from multi-honors holders
### splitting file into four as large file hangs

wp_page_create_search1 <- wikipedia_page_extraction_format %>% 
        select(wp_pageid) %>% 
        distinct() %>% 
        mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
        mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
        select(search_url) %>% 
        slice(1:1000)

wp_page_create_search2 <- wikipedia_page_extraction_format %>% 
        select(wp_pageid) %>% 
        distinct() %>% 
        mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
        mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
        select(search_url) %>% 
        slice(1001:2000)


wp_page_create_search3 <- wikipedia_page_extraction_format %>% 
        select(wp_pageid) %>% 
        distinct() %>% 
        mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
        mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
        select(search_url) %>% 
        slice(2001:3000)



wp_page_create_search4 <- wikipedia_page_extraction_format %>% 
        select(wp_pageid) %>% 
        distinct() %>% 
        mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
        mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
        select(search_url) %>% 
        slice(3001:4473)

## querying WP API for page creation date - 1

wp_page_create_query1 <- lapply(wp_page_create_search1$search_url, function(i){
        
        date <- read_html(i)
        
        EntryInfo <- html_nodes(date, ".s2") %>% 
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

## querying WP API for page creation date - 2

wp_page_create_query2 <- lapply(wp_page_create_search2$search_url, function(i){
        
        date <- read_html(i)
        
        EntryInfo <- html_nodes(date, ".s2") %>% 
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

## querying WP API for page creation date - 3

wp_page_create_query3 <- lapply(wp_page_create_search3$search_url, function(i){
        
        date <- read_html(i)
        
        EntryInfo <- html_nodes(date, ".s2") %>% 
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

## querying WP API for page creation date - 4

wp_page_create_query4 <- lapply(wp_page_create_search4$search_url, function(i){
        
        date <- read_html(i)
        
        EntryInfo <- html_nodes(date, ".s2") %>% 
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

# Bind files together
wp_page_create_bind <- bind_rows(wp_page_create_query1, wp_page_create_query2, wp_page_create_query3, wp_page_create_query4)


###NEED TO UPDATE VARIABLE NAMES HERE

## format file - fixing time zone from UTC to Sydney to ensure correct time calculation

wp_page_create_format  <- wp_page_create_bind %>%  
        mutate(pageID =str_remove_all(pageID , "[[\\p{P}][\\p{S}]]"),
               name =str_remove_all(name , "[[\\p{P}][\\p{S}]]"),
               pageCreation =str_remove_all(pageCreation , "\"" )) %>% 
        mutate(WPpageCreation = ymd_hms(pageCreation, tz="UTC")) %>% 
        rename(wikipediaPageID = pageID) %>% 
        mutate(wikipediaPageID = as.numeric(wikipediaPageID)) %>% 
        select(wikipediaPageID, WPpageCreation, name) %>% 
        clean_names()

wikipedia_page_date <- left_join(wp_page_create_format, wikipedia_page_query, by="name")


wikipedia_complete <- left_join(wikipedia_page_list, wikipedia_page_date, by="wikipedia_url")

wikipedia_complete <- wikipedia_complete %>% 
        

all_data_merge_honsid <- left_join(honours_list, wikipedia_complete, by="honsid") 


# wpCheck <- all_data_merge_honsid %>% 
#         filter(wikipedia_page=="Yes") %>% 
#         select(wikipedia_url) %>% 
#         group_by(wikipedia_url) %>% 
#         distinct()


##removing unecessary columns
data_prep_1 <- all_data_merge_honsid %>% 
        select(-c(award_system, clasp_level, clasp_text, gazette_postcode, additional_info, gazette_given_name, gazette_surname)) 

##recoding state etc

data_prep_2 <- data_prep_1 %>% 
        rename(state = gazette_state) %>% 
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
                award_abbr2 = case_when(
                        award_name == "Companion of the Order of Australia" ~ "AC",
                        award_name == "Member of the Order of Australia" ~ "AM",
                        award_name == "Officer of the Order of Australia" | award_name == "Honorary Officer of the Order of Australia" ~ "AO",
                        award_name == "Medal of the Order of Australia" ~ "OAM"
                )) %>% 
        mutate(award_abbr = ifelse(is.na(award_abbr), award_abbr2, award_abbr)) %>%  
        
        mutate(
                awardComb = case_when(
                        award_abbr == "AD" | award_abbr == "AK" ~ "ADK",
                        award_abbr == "AC" ~ "AC",
                        award_abbr == "AM" ~ "AM",
                        award_abbr == "AO" ~ "AO",
                        award_abbr == "OAM" ~ "OAM"
                )) %>% 
        clean_names() %>% 
        mutate(state = factor(state, levels = c("NSW", "VIC", "QLD", "SA", "TAS", "WA", "ACT", "NT", "Other"))) %>% 
        mutate(award_comb = factor(award_comb, levels = c("ADK", "AC", "AO", "AM", "OAM"))) 
        
##filling in missing names from name variable

data_prep_3 <- data_prep_2 %>% 
        mutate(name = ifelse(!is.na(name), name, gazette_name)) %>% 
        mutate(wikipedia_page = case_when(wikipedia_page=="Yes" ~ "Yes",
                                          TRUE ~ "No")) 



##formatting dates into Sydney / Aus time 

# View(data_prep_4)

data_prep_4 <- data_prep_3 %>% 
        # filter(wikipedia_page=="Yes") %>% 
        rename(wp_page_creation=w_ppage_creation) %>% 
        mutate(wp_page_creation_full = ymd_hms(wp_page_creation, tz="UTC"),
               wp_creation_date = with_tz(wp_page_creation, "Australia/Sydney"),
               wp_creation_date = as_date(wp_creation_date, "Australia/Sydney")) %>%
        mutate(awarded_on_full = dmy_hms(awarded_on, tz="Australia/Sydney"),
               honours_date = as_date(awarded_on_full, tz="Australia/Sydney")) %>% 
        select(-date_awarded, -wp_page_creation_full, -awarded_on_full, -awarded_on, -wp_page_creation) %>% 
        mutate(honours_year = year(honours_date),
               wikipedia_creation_year = year(wp_creation_date))
        

##re aranging cols
data_prep_5 <- data_prep_4 %>% 
        select(name, gender, wikipedia_page, award_comb, state, wikipedia_url, wikipedia_page_id, honsid, wikipedia_page_id, wp_creation_date, 
               honours_date, wikipedia_creation_year, honours_year, award_name, announcement_event, division, citation, person_description)
        

# View(all_data)
write_csv(data_prep_5, "all_data.csv")

##creating recipient file

recipient <- data_prep_5 %>% 
        arrange(name, honours_date) %>% 
        group_by(name) %>% 
        add_tally() %>% 
        # filter(n>1) %>%
        mutate(first_honours_date = min(honours_date)) %>% 
        mutate(highestAward1 = case_when(award_comb == "OAM" ~1,
                                         award_comb == "AM" ~2,
                                         award_comb == "AO" ~3,
                                         award_comb == "AC" ~4,
                                         award_comb == "ADK" ~5)) %>% 
        mutate(highestAward2 = max(highestAward1)) %>% 
        mutate(highestAward = case_when(highestAward2 == 1 ~"OAM",
                                        highestAward2 == 2 ~"AM" ,
                                        highestAward2 == 3~"AO",
                                        highestAward2 == 4~"AC",
                                        highestAward2 == 5 ~"ADK")) %>% 
        filter(highestAward1 == max(highestAward1)) %>% 
        rename(numberOfHonours = n) %>% 
        mutate(first_honours_year = year(first_honours_date) )%>% 
        select(-c(highestAward1, highestAward2 , highestAward)) %>% 
        mutate(prePostWikipedia = case_when(first_honours_date<"2001-01-15" ~ "Pre",
                                            first_honours_date>="2001-01-15" ~ "Post")) %>% 
   mutate(new_honours_date = replace(first_honours_date, first_honours_date<"2001-01-15", "2001-01-15")) %>% 
  mutate(new_honours_year = year(new_honours_date)) %>% 
        ungroup() %>% 
        clean_names() %>% 
        select(1:5, first_honours_date, first_honours_year, pre_post_wikipedia, 6:23)
        
write_csv(recipient, "recipient.csv")        

View(wikipedia)

wikipedia <- recipient %>% 
        filter(wikipedia_page=="Yes") %>% 
        mutate(new_honours_date = replace(first_honours_date, first_honours_date<"2001-01-15", "2001-01-15")) %>% 
        mutate(new_honours_year = year(new_honours_date),
               wp_creation_year = year(wp_creation_date),
               time_diff = wp_creation_date - new_honours_date,
               year_diff = wp_creation_year - new_honours_year) %>% 
        mutate(wikipedia_week = strftime(wp_creation_date, format = "%Y-W%V"),
               new_honours_week = strftime(new_honours_date, format = "%Y-W%V"),
               week_diff = interval(new_honours_date, wp_creation_date) / dweeks(1),
               week_diff = floor(week_diff))

write_csv(wikipedia, "wikipedia.csv")


post_wikipedia_awards <- wikipedia %>%                 
        filter(first_honours_date>"2001-01-15") %>% 
        mutate(before_after = case_when(time_diff >=0 ~ "on or after honours",
                                       time_diff<0 ~ "before Honours")) %>%
        mutate(before_after = factor(before_after, levels = c("before Honours","on or after honours"))) 

write_csv(post_wikipedia_awards, "post_wikipedia_awards.csv")



### checking data and small extractions below


View(ac_missing_check)
ac_missing_check <- recipient %>% 
  filter(award_comb=="AC") %>% 
  filter(wikipedia_page=="No")


day_zero <- wikipedia %>% 
  filter(time_diff == 0)
  

week_zero <- wikipedia %>% 
  filter(week_diff == 0) %>% 
  select(name, wikipedia_page_id, wikipedia_url, award_name)

write_csv(week_zero, "week_zero.csv")  


# View(week_zero)

gender_before_after  <-  wikipedia %>% 
  filter(gender!="U") %>% 
  mutate(before_after = case_when(time_diff >=0 ~ "on or after honours",
                                  time_diff<0 ~ "before Honours")) %>%
  mutate(before_after = factor(before_after, levels = c("before Honours","on or after honours"))) %>% 
  group_by(gender) %>% 
  add_tally() %>% 
  group_by(gender, before_after, n) %>% 
  tally() %>% 
  filter(!is.na(before_after)) %>% 
  mutate(prop = nn/n)


total_before_after  <-  wikipedia %>% 
  filter(gender!="U") %>% 
  mutate(before_after = case_when(time_diff >=0 ~ "on or after honours",
                                  time_diff<0 ~ "before Honours")) %>%
  mutate(before_after = factor(before_after, levels = c("before Honours","on or after honours"))) %>% 
  add_tally() %>% 
  group_by(before_after, n) %>% 
  tally() %>% 
  filter(!is.na(before_after)) %>% 
  mutate(prop = nn/n) %>% 
  filter()






gender_ba_check <- ggplot(gender_before_after, aes(gender, prop, text = percent(prop))) +
  geom_col() +
  facet_wrap(~before_after)+
  geom_hline(data=total_before_after, aes(yintercept = prop))+
  custom_theme

  

ggplotly(gender_ba_check, tooltip="text")
  

wiki_honours <- wikipedia %>% 
  select(time_diff) %>% 
  group_by(time_diff) %>% 
  tally(name="freq")

write_csv(wiki_honours, "wiki_honours.csv")


wiki_honours_postwp <- post_wikipedia_awards %>% 
  select(time_diff) %>% 
  group_by(time_diff) %>% 
  tally(name="freq")

write_csv(wiki_honours_postwp, "wiki_honours_postwp.csv")


pre_post_sum <- wikipedia %>% 
  select(pre_post_wikipedia) %>% 
  group_by(pre_post_wikipedia) %>% 
  tally()

  
pre_post_check <- wikipedia %>% 
  select(time_diff, pre_post_wikipedia) %>% 
  mutate(before_after = case_when(time_diff >=0 ~ "on or after honours",
                                  time_diff<0 ~ "before Honours")) %>%
  group_by(before_after, pre_post_wikipedia) %>% 
  tally()