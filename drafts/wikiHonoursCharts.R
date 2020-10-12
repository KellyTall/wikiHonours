library(tidyverse)
library(ggplot2)
library(lubridate)

read_csv()

OAFulllist <- OAFullList %>% 
        mutate(
                AwardAbbr2 = case_when(
                        AwardAbbr == "AD" | AwardAbbr == "AK" ~ "ADK",
                        AwardAbbr == "AC" ~ "AC",
                        AwardAbbr == "AM" ~ "AM",
                        AwardAbbr == "AO" ~ "AO",
                        AwardAbbr == "OAM" ~ "OAM"
                ))


OAFulllist <- OAFulllist %>% 
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
        )

orderSum2 <- OAFulllist %>%  
        group_by(AwardAbbr2) %>% 
        tally()

honoursTotal <- ggplot(orderSum2, aes(reorder(AwardAbbr2, n), n)) +
        geom_col()

orderSum3 <- OAFulllist %>%  
        group_by(state, AwardAbbr2) %>% 
        tally()

honoursByState <- ggplot(orderSum3, aes(reorder(AwardAbbr2, n), n)) +
        geom_col() +
        facet_wrap(~state)

View(orderSum3)


