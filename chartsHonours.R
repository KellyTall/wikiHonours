library(lubridate)
library(tidyverse)
library(DataExplorer)
library(plotly)
library(scales)
library(GGally)
library(ggraph)
library(tidygraph)

colours <- c("#2d4059", "#ea5455")

custom_theme <-  theme(
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill=("#e5e5e5")),
        strip.background = element_blank())


allDataBig <- read_csv("allDataBig.csv") %>% 
        select(-X1) %>% 
        mutate(state = factor(state, levels = c("NSW", "VIC", "QLD", "SA", "TAS", "WA", "ACT", "NT", "Other"))) %>% 
        mutate(awardComb = factor(awardComb, levels = c("ADK", "AC", "AO", "AM", "OAM"))) %>% 
        mutate (honourYear = year(honourDate),
                pageYear = year(pageCreation))


##filtered data ste nly including cases w wikipedia pages plus deleting duplicates, fixing missing names
smallData <- allDataBig %>% 
        filter(wikipediaPage=="Yes" & prePost!="No wiki") %>% 
        filter(fullName !="Anonymous") %>% 
        distinct(wikipediaURL, .keep_all = TRUE) %>% 
        mutate(name = ifelse(!is.na(name), name, fullName)) 
        
# View(smallData)        





allAwards <- allDataBig %>% 
        select(awardComb, honourDate) %>% 
        group_by(awardComb) 

allAwardsNames <- allDataBig %>% 
        filter(fullName !="Anonymous") %>% 
        select(awardComb, honourDate, fullName, wikipediaPage) %>% 
        group_by(fullName, wikipediaPage) %>% 
        tally() %>% 
        filter(n>1)

View(allAwardsNames)

allAwardees <- allDataBig %>% 
        select(fullName, awardComb, honourDate) %>% 
        group_by(fullName, honourDate) %>% 
        tally() %>% 
        filter(n>1)


numberAwards <- ggplot(allDataBig, aes(awardComb)) +
        geom_bar(fill="#2d4059") +
        custom_theme+
        labs(title="All Orders of Australia issued (1975 - 2020)",
             x="Order of Australia (membership levels)", y="number of awards", 
             caption="total number of awards n=41,816") +
        scale_fill_manual(values=colours)






# View(allDataBig)


# description and characteristics of the data set

##miltary v non-miltary

military <- allDataBig %>% 
        select(AwardAbbr, division) %>% 
        group_by(division) %>% 
        tally()

##anonymous orders

anon <- allDataBig %>% 
        select(fullName, division) %>% 
        filter(fullName == "Anonymous") %>% 
        group_by(division) %>% 
        tally() %>% 
        rename(Total = n)

anon_Split <- allDataBig %>% 
        select(fullName, division, AwardAbbr) %>% 
        filter(fullName == "Anonymous") %>% 
        group_by(division, AwardAbbr) %>% 
        tally() %>% 
        rename(Total = n)


##anon by division
anonDivision <- allDataBig %>% 
        select(division, fullName) %>% 
        filter(fullName == "Anonymous") %>% 
        group_by(division) %>% 
        tally()


anonDivision <- allDataBig %>% 
        select(AwardAbbr, fullName) %>% 
        filter(fullName == "Anonymous") %>% 
        group_by(AwardAbbr) %>% 
        tally()


StrippedDivision <- allDataBig %>% 
        select(AwardAbbr, fullName, citation) %>% 
        filter(str_detect(citation,  "(?i)terminated")) %>% 
        group_by(AwardAbbr) %>% 
        tally()

##anon military
        
anonMilitary <- allDataBig %>% 
        select(division, AwardAbbr, fullName) %>% 
        filter(fullName == "Anonymous") %>% 
        filter( division==  "HM" | division==  "M" ) %>% 
        group_by(AwardAbbr, division) %>% 
        tally()


##division breakdown

divisionBreakDown <- allDataBig %>% 
        select(division) %>% 
        group_by(division) %>% 
        tally()


divisionByOrder <- allDataBig %>% 
        select(division, AwardAbbr, wikipediaPage) %>% 
        group_by(division, AwardAbbr, wikipediaPage) %>% 
        tally()


divisionByWikipedia <- allDataBig %>% 
        select(division, wikipediaPage) %>% 
        group_by(division, wikipediaPage) %>% 
        tally()


##multiple awards

multiple <- allDataBig %>% 
        filter(fullName != "Anonymous") %>% 
        mutate(name = ifelse(!is.na(name), name, fullName)) %>% 
        select(name) %>% 
        group_by(name) %>% 
        tally() %>% 
        filter(n>1)

multipleWP <- allDataBig %>% 
        filter(fullName != "Anonymous") %>% 
        select(wikipediaURL) %>% 
        group_by(wikipediaURL) %>% 
        tally() %>% 
        filter(n>1)

multipleWP_List <- allDataBig %>% 
        filter(fullName != "Anonymous") %>% 
        select(wikipediaURL, name2) %>% 
        group_by(wikipediaURL, name2) %>% 
        tally() %>% 
        filter(n>1) 


##fixing dates of multiple award holders
View(multiple_awards_fix)

recipientData <- allDataBig %>% 
        filter(fullName != "Anonymous") %>% 
        mutate(name = ifelse(!is.na(name), name, fullName)) %>% 
        arrange(name, honourDate) %>% 
        group_by(name) %>% 
        add_tally() %>% 
        # filter(n>1) %>%
        mutate(firstAwardDate = min(honourDate)) %>% 
        mutate(highestAward1 = case_when(awardComb == "OAM" ~1,
                                        awardComb == "AM" ~2,
                                        awardComb == "AO" ~3,
                                        awardComb == "AC" ~4,
                                        awardComb == "AKD" ~5)) %>% 
        mutate(highestAward2 = max(highestAward1)) %>% 
        mutate(highestAward = case_when(highestAward2 == 1 ~"OAM",
                                        highestAward2 == 2 ~"AM" ,
                                        highestAward2 == 3~"AO",
                                        highestAward2 == 4~"AC",
                                        highestAward2 == 5 ~"AKD")) %>% 
        filter(highestAward1 == max(highestAward1)) %>% 
        mutate(daysDiff = pageCreation - firstAwardDate) %>% 
        mutate(daysDiff = str_remove(daysDiff, "days")) %>% 
        mutate(daysDiff = as.numeric(daysDiff)) %>% 
        select(wikipediaPage:pageCreation, firstAwardDate, daysDiff:highestAward) %>% 
        mutate(honourYear = year(firstAwardDate)) %>% 
        mutate(pageYear = year(pageCreation)) %>% 
        mutate(prePost = case_when(daysDiff >=0 ~ "Post",
                                   daysDiff < 0 ~ "Pre")) %>% 
        select(wikipediaPage:prePost, honourYear, pageYear, AwardAbbr:highestAward)


levelTest <- recipientData %>% 
        ungroup() %>% 
        # filter(honourYear >2000) %>% 
        select(awardComb) %>% 
        group_by(awardComb) %>% 
        tally()

multipleList <- allDataBig %>% 
        filter(fullName != "Anonymous") %>% 
        mutate(name = ifelse(!is.na(name), name, fullName)) %>% 
        arrange(name, dateAwarded) %>% 
        group_by(name) %>% 
        add_tally()
        
        select(wikipediaURL, name2) %>% 
        group_by(wikipediaURL, name2) %>% 
        tally() %>% 
        filter(n>1) 




multiple_awards_fix2 <- allDataBig %>% 
        # filter(fullName != "Anonymous") %>% 
        mutate(name = ifelse(!is.na(name), name, fullName)) %>% 
        arrange(name, dateAwarded) %>% 
        # filter(wikipediaPage =="Yes") %>% 
        # select(name, dateAwarded, pageCreation, AwardAbbr, daysDiff) %>%
        group_by(name) %>% 
        tally() %>% 
        arrange(desc(n))
        # filter(n>1) %>%
        mutate(firstAwardDate = min(dateAwarded)) %>% 
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
        mutate(daysDiff = pageCreation - firstAwardDate) %>% 
        mutate(daysDiff = str_remove(daysDiff, "days")) %>% 
        mutate(daysDiff = as.numeric(daysDiff)) %>% 
        select(wikipediaPage:pageCreation, firstAwardDate, daysDiff:highestAward) %>% 
        mutate(honourYear = year(firstAwardDate)) %>% 
        mutate(pageYear = year(pageCreation)) %>% 
        mutate(prePost = case_when(daysDiff >=0 ~ "Post",
                                   daysDiff < 0 ~ "Pre")) %>% 
        select(wikipediaPage:prePost, honourYear, pageYear, AwardAbbr:highestAward) %>% 
        tally()


        



View(multiple_awards_fix)
View(multiple_awards_fix2)
View(allDataBig)



colours <- c("#2d4059", "#ea5455")

custom_theme <-  theme(
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill=("#e5e5e5")),
        strip.background = element_blank(),
        panel.spacing  = unit(2, "lines"))




awardSummaryTotal <- allDataBig %>%
        select(awardComb) %>%
        group_by(awardComb) %>%
        tally()

awardSummaryDivisionTally <- allDataBig %>%
        select(awardComb, division) %>%
        group_by(awardComb, division) %>%
        tally()

divisionSummary <- ggplot(awardSummaryDivisionTally, aes(division, awardComb, size=n)) +
        geom_point(color="#515BA5") +
        scale_x_discrete(position = "top") +
        scale_size(range = c (1, 20))
        


t <- list(
        family = "Helvetica",
        size = 14)

ggplotly(divisionSummary, tooltip="n") %>% 
        layout(font=t,
               annotations = 
                       list(x = 1, y = -0.1, text = "total number of awards n=41,816", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=1, yshift=2,
                            font=list(size=10))) 

View(awardSummaryDivision)

awardSummaryState <- allDataBig %>%
        group_by(state) %>% 
        add_tally() %>% 
        rename(stateAwardTotal = n) %>% 
        select(awardComb, state, stateAwardTotal) %>%
        mutate(chartLabel = paste(state, " n=",stateAwardTotal, sep="")) %>% 
        group_by(state, awardComb, chartLabel) %>%
        tally() %>% 
        mutate(state = factor(state, levels = c("NSW", "VIC", "QLD", "SA", "TAS", "WA", "ACT", "NT", "Other"))) %>% 
        mutate(chartLabel = as_factor(chartLabel))

 



awardTotal <-  ggplot(awardSummaryTotal, aes(reorder(awardComb, n), n))+
        geom_col(fill="#2d4059") +
        geom_text(aes(label=n), hjust=-.3)+
        custom_theme+
        labs(title="Number of award recipients by level of membership",
             x="Order of Australia (membership levels)", y="Number of Honours", caption = "Knights and Dames 
             are combined into one level. Total number of honours n=41,816")+
        coord_flip()




awardState <- ggplot(awardSummaryState, aes(reorder(awardComb, n), n))+
        geom_col(fill="#2d4059") +
        facet_wrap(~chartLabel)+
        custom_theme+
        labs(title="Number of award recipients by level of membership by state",
             x="Order of Australia (membership levels)", y="Number of Honours", 
             caption = "'Other' refers to honourary awards, and/or those not residing in Australia, 
             or where there was no location information provided in the record. Total number of honours n=41,816") +
        coord_flip()+
        # geom_text(aes(label=n), hjust=-.3, size=3) +
        scale_y_continuous(limits = c(0, 10100))


ggplotly(awardState, tooltip = "n")





#### How many wikipedia pages are there for Order of Australia recipients?


pagePropTotal <- allDataBig %>% 
        select(awardComb, wikipediaPage) %>% 
        group_by(awardComb, wikipediaPage) %>% 
        tally()

pagePropState <- allDataBig %>% 
        select(awardComb, wikipediaPage, state) %>% 
        group_by(awardComb, wikipediaPage, state) %>% 
        tally()



awardWPNum <- ggplot(pagePropTotal, aes(reorder(awardComb, -n), n)) +
        geom_col(aes(fill=wikipediaPage)) +
        custom_theme+
        labs(title="Number of award recipients that have a wikipedia article page", 
             x="Order of Australia (membership levels)", y="number of awards") +
        scale_fill_manual(values=colours)



ggplotly(awardWPNum, tooltip = "n")


awardWPNumState <- ggplot(pagePropState, aes(reorder(awardComb, -n), n)) +
        geom_col(aes(fill=wikipediaPage)) +
        facet_wrap(~state)+
        custom_theme+
        labs(title="Number of award recipients that have a wikipedia article page by state", 
             x="Order of Australia (membership levels)", y="number of awards")+
        scale_fill_manual(values=colours)



ggplotly(awardWPNumState, tooltip = "n")

awardWPProp <- ggplot(pagePropTotal, aes(reorder(awardComb, -n), n, fill=wikipediaPage)) +
        geom_bar(position="fill", stat="identity") +
        custom_theme+
        labs(title="Proportion of award recipients that have a wikipedia article page", 
             x="Order of Australia (membership levels)", y="number of awards") +
        scale_y_continuous(labels = percent)+
        scale_fill_manual(values=colours)


ggplotly(awardWPProp, tooltip="n")

awardWPPropState <- ggplot(pagePropState, aes(reorder(awardComb, n), n, fill=wikipediaPage)) +
        geom_bar(position="fill", stat="identity") +
        facet_wrap(~state)+
        custom_theme+
        labs(title="Proportion of award recipients that have a wikipedia article page by state", 
             x="Order of Australia (membership levels)", y="number of awards") +
        scale_y_continuous(labels = percent)+
        scale_fill_manual(values=colours)


ggplotly(awardWPPropState, tooltip="n")


prePost <- allDataBig %>%
        filter(wikipediaPage=="Yes") %>% 
        filter(prePost!="No wiki") %>% 
        select(prePost) %>% 
        group_by(prePost) %>% 
        tally()

awardprePost <- ggplot(prePost, aes(prePost, n)) +
        geom_col(fill="#2d4059") +
        custom_theme +
        labs(title="Number of pages created pre / post award", 
             y="number of wikiPedia Articles", caption="Pre is calculated as less than zero days before the award was issued. 
             Post is calculated as 0 or greater than days after award issued") +
        scale_fill_manual(values=colours)



ggplotly(awardprePost, tooltip = "n")

timeline <- allDataBig %>% 
        filter(wikipediaPage == "Yes") %>% 
        select(pageCreation, wikipediaPage) %>% 
        group_by(pageCreation, wikipediaPage) %>% 
        tally()


timelinePlot <- ggplot(timeline, aes(pageCreation, n)) +
        geom_line()

ggplotly(timelinePlot, tooltip = "n")

cumulative <- allDataBig %>% 
        filter(wikipediaPage == "Yes") %>% 
        select(pageCreation, wikipediaPage) %>% 
        group_by(pageCreation) %>% 
        arrange(pageCreation) %>% 
        tally() %>% 
        mutate(n= as.numeric(n)) %>% 
        mutate(cumulate = cumsum(n))

cumulativePlot <- ggplot(cumulative, aes(pageCreation, cumulate) )+
        geom_line()

ggplotly(cumulativePlot, tooltip = "cumulate")



prePostGender <- allDataBig %>%
        filter(wikipediaPage=="Yes") %>% 
        filter(prePost!="No wiki") %>% 
        select(prePost, gender) %>% 
        group_by(prePost, gender) %>% 
        tally()

awardprePostGender <- ggplot(prePostGender, aes(prePost, n)) +
        geom_col(aes(fill=gender)) +
        custom_theme +
        labs(title="Number of pages created pre / post award", 
             y="number of wikiPedia Articles", caption="Pre is calculated as less than zero days before the award was issued. 
             Post is calculated as 0 or greater than days after award issued") +
        scale_fill_manual(values=colours)

ggplotly(awardprePostGender, tooltip = "n")


cumulativeGender <- allDataBig %>% 
        filter(wikipediaPage == "Yes") %>% 
        select(pageCreation, wikipediaPage, gender) %>% 
        group_by(gender, pageCreation) %>% 
        arrange(pageCreation) %>% 
        tally() %>% 
        mutate(n= as.numeric(n)) %>% 
        mutate(cumulate = cumsum(n))

cumulativePlotGender <- ggplot(cumulativeGender, aes(pageCreation, cumulate) )+
        geom_line(aes(colour=gender))+
        custom_theme +
        scale_fill_manual(values=colours)

ggplotly(cumulativePlotGender, tooltip = "cumulate")




####  slope chart

slopeData <- smallData %>% 
        select(honourYear, pageYear, prePost, gender, awardComb)

ggparcoord(slopeData, columns = 1:2, groupColumn="prePost", alphaLines = 0.04) 
        
ggparcoord(slopeData, columns = 1:2, groupColumn="prePost", alphaLines = 0.05) +
        facet_wrap(~gender)

ggparcoord(slopeData, columns = 1:2, groupColumn="prePost", alphaLines = 0.05) +
        facet_wrap(~awardComb)



##arcplot

arcData <- smallData %>% 
        select(honourDate, pageCreation, prePost, gender, awardComb) %>% 
        mutate (from = year(honourDate),
                to = year(pageCreation)) %>% 
        # slice_sample(n = 2) %>% 
        group_by(from, to, gender, awardComb, prePost) %>% 
        tally()  %>% 
        rename(weight=n) %>% 
        na.omit()


arcPlot <- tbl_graph(edges=arcData, directed=TRUE)


arcGender <- ggraph(arcPlot, layout = 'linear') +
        geom_edge_arc(aes(colour=gender), alpha=.3, width=.2) 

arcAward <- ggraph(arcPlot, layout = 'linear') +
        geom_edge_arc(aes(colour=awardComb), alpha=.3, width=.2) 



arcGenderSplit <- ggraph(arcPlot, layout = 'linear') +
        geom_edge_arc(aes(colour=gender), alpha=.3, width=.2) +
        facet_wrap(~gender)

arcAwardSplitSplit <- ggraph(arcPlot, layout = 'linear') +
        geom_edge_arc(aes(colour=gender), alpha=.3, width=.2) +
        facet_grid(~awardComb)





### length of time

timeData <- smallData %>% 
        select(daysDiff, gender, awardComb) 

ggplot(timeData, aes(daysDiff)) +
        geom_histogram(binwidth = 500) +
        facet_grid(gender~awardComb) 



ggplot(timeData, aes(daysDiff, after_stat(count), group=gender, fill=gender)) +
        geom_density(alpha=0.5)  +
        facet_grid(~awardComb)



##range

inRange <- allDataBig %>% 
        filter(wikipediaPage=="Yes" & prePost!="No wiki")  %>% 
        select(daysDiff, gender, awardComb, name) %>% 
        filter(daysDiff >=-1 & daysDiff<14)



##violin

ggplot(timeData, aes(gender, daysDiff)) +
        geom_violin()+
        geom_jitter(alpha=.02)
        


ggplot(timeData, aes(gender, daysDiff)) +
        geom_boxplot()+
        geom_jitter(alpha=.5)


##pre wiki / post wiki

prePost <- allDataBig %>% 
        filter(wikipediaPage=="Yes" & prePost!="No wiki")  %>% 
        select(honourDate, pageCreation, prePost, gender, awardComb) %>% 
        mutate (honourYear = year(honourDate),
                pageYear = year(pageCreation)) %>% 
        mutate(awardPreWP = 
                       case_when(honourYear <2001 ~ "PreWP",
                                 honourYear >=2001 ~ "PostWP")) %>% 
        group_by(awardPreWP, pageCreation) %>% 
        arrange(awardPreWP, pageCreation) %>% 
       tally() %>% 
        mutate(n = as.numeric(n)) %>% 
        mutate(cum = cumsum(n))
        

ggplot(prePost, aes(pageCreation, cum)) +
        geom_line(aes(colour = awardPreWP))



prePostALL <- allDataBig %>% 
        select(honourDate,prePost, gender, awardComb) %>% 
        mutate (honourYear = year(honourDate)) %>% 
        mutate(awardPreWP = 
                       case_when(honourYear <2001 ~ "PreWP",
                                 honourYear >=2001 ~ "PostWP")) %>% 
        group_by(awardPreWP) %>% 
        arrange(awardPreWP) %>% 
        tally() %>% 
        mutate(n = as.numeric(n)) 

ggplot(prePostALL, aes(pageCreation, cum)) +
        geom_line(aes(colour = awardPreWP))



##greatest difference

difference <- smallData %>% 
        filter(wikipediaPage=="Yes" & prePost!="No wiki") %>% 
        select(honourDate, pageCreation) %>% 
        arrange(pageCreation, honourDate) %>% 
        group_by(honourDate, pageCreation) %>% 
        add_tally() 
        
        
        
        
recipient <- allDataBigAnonRemoved %>% 
        mutate(name = ifelse(!is.na(name), name, fullName)) %>% 
        select(-dateAwarded) %>% 
        arrange(name, honourDate) %>% 
        group_by(name) %>% 
        add_tally() %>% 
        mutate(firstAwardDate = min(honourDate)) %>% 
        mutate(highestAward1 = case_when(awardComb == "OAM" ~1,
                                         awardComb == "AM" ~2,
                                         awardComb == "AO" ~3,
                                         awardComb == "AC" ~4,
                                         awardComb == "AKD" ~5)) %>% 
        mutate(highestAward2 = max(highestAward1)) %>% 
        mutate(highestAward = case_when(highestAward2 == 1 ~"OAM",
                                        highestAward2 == 2 ~"AM" ,
                                        highestAward2 == 3~"AO",
                                        highestAward2 == 4~"AC",
                                        highestAward2 == 5 ~"AKD")) %>% 
        filter(highestAward1 == max(highestAward1)) %>% 
        mutate(daysDiff = pageCreation - firstAwardDate) %>% 
        mutate(daysDiff = str_remove(daysDiff, "days")) %>% 
        mutate(daysDiff = as.numeric(daysDiff)) %>% 
        select(wikipediaPage:pageCreation, firstAwardDate, daysDiff:highestAward) %>% 
        mutate(honourYear = year(firstAwardDate)) %>% 
        mutate(pageYear = year(pageCreation)) %>% 
        mutate(prePost = case_when(daysDiff >=0 ~ "Post",
                                   daysDiff < 0 ~ "Pre")) %>% 
        select(wikipediaPage:prePost, honourYear, pageYear, AwardAbbr:highestAward)

write.csv(recipient, "recipient.csv")

View(recipient)

multipleHonoursList <- allDataAnonRemoved %>% 
        mutate(name = ifelse(!is.na(name), name, fullName)) %>% 
        arrange(name, dateAwarded) %>% 
        group_by(name) %>% 
        tally() %>% 
        filter(n>1) 

        
propAll <- recipient %>% 
        select(wikipediaPage) %>% 
        add_tally() %>% 
        rename(total =n) %>% 
        group_by(wikipediaPage, total) %>% 
        tally() %>% 
        mutate(prop = n/total)


propAward <- recipient %>% 
        select(wikipediaPage, awardComb) %>% 
        group_by()

ggplot(propAll, aes(wikipediaPage, prop, fill=wikipediaPage)) +
        geom_col()+
        labs(title="11% of all Honours recipients have a wikipedia page",
             x="Have wikipedia Page", y="Proportion of recipients") +
        scale_fill_manual(values=colours)+
        scale_y_continuous(labels = percent)+
        custom_theme


checkRecipients <- recipient %>% 
        nrow()

genderCheck <- recipient %>% 
        select(gender) %>% 
        group_by(gender) %>% 
        # filter(gender !="U") %>% 
        tally()

