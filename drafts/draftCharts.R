library(ggplot2)
library(plotly)
library(scales)


##total award summaries

awardSummaryTotal <- allData %>% 
        select(Award) %>% 
        group_by(Award) %>% 
        tally()

awardSummary <- allData %>% 
        select(Award, state) %>% 
        group_by(Award) %>% 
        tally()


awardSummaryState <- allData %>% 
        select(Award, state) %>% 
        group_by(state, Award ) %>% 
        tally()



awardTotal <-  ggplot(awardSummaryTotal, aes(reorder(Award, -n), n))+
        geom_col() +
        theme_minimal()+
        ggtitle("Number of award recipients by level")
        
ggplotly(awardTotal)

awardState <- ggplot(awardSummaryTotal, aes(reorder(Award, -n), n))+
        geom_col() +
        facet_wrap(~state)+
        theme_minimal()+
        ggtitle("Number of award recipients by level by state")

ggplotly(awardState)



##page information
pageProp <- allData %>% 
        select(Award, wpPage, state) %>% 
        group_by(Award, wpPage, state) %>% 
        tally()

ggplot(pageProp, aes(reorder(Award, -n), n)) +
        geom_col(aes(fill=wpPage)) +
        theme_minimal()+
        ggtitle("Number of award recipients that have a wikipedia article page")

ggplot(pageProp, aes(reorder(Award, -n), n)) +
        geom_col(aes(fill=wpPage)) +
        facet_wrap(~state)+
        theme_minimal()+
        ggtitle("Number of award recipients that have a wikipedia article page by State")


ggplot(pageProp, aes(reorder(Award, -n), n, fill=wpPage)) +
        geom_bar(position="fill", stat="identity") +
        theme_minimal()+
        ggtitle("proportion of award recipients that have a wikipedia article page")
        

ggplot(pageProp, aes(reorder(Award, n), n, fill=wpPage)) +
        geom_bar(position="fill", stat="identity") +
        facet_wrap(~state)+
        theme_minimal()+
        ggtitle("proportion of award recipients that have a wikipedia article page - by State")

        
