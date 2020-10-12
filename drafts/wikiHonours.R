# install.packages("WikidataR")
# install.packages("WikipediR")
# install.packages("tidyverse")
# install.packages("rlist")
# install.packages("WikidataQueryServiceR")

library(WikidataR)
library(tidyverse)
library(WikipediR)
library(rlist)
library(WikidataQueryServiceR)


test <- find_item("bernard farrelly")

# load data ---------------------------------------------------------------

OAqBday <- read_csv("OA_QueenBDay.csv")
OAausDay <- read_csv("OA_AusDay.csv")


# Merge data --------------------------------------------------------------


OAFullList <- bind_rows(OAqBday, OAausDay)

View(OAFullList)

# write_csv(OAFullList, "OAFullList.csv")

## list of all column names in data
colNames <- colnames(OAFullList)



##prepre the search file

oaWiki <- OAFullList %>%
        mutate(GazetteSurname = tolower(GazetteSurname)) %>%
        mutate(GazetteSurname = tolower(GazetteSurname)) %>%
        select(GazetteGivenName, GazetteSurname, AwardAbbr) %>%
        mutate(fullName = paste(GazetteGivenName, GazetteSurname, sep = " "))


# function to create smaller files to use in loop for wikimedia query`

###LOOPS HAVE BEEN COMMENTED OUT SO THEY ARE NOT ACCIDENTALLY RUN AGAIN - NEED TO EXPORT AND REIMPORT

# rows 1 to 1000 ----------------------------------------------------------


# oa1 <- oaWiki %>%
#         slice((1:1000))
# 
# 
# oaWikiLoop1<- lapply(oa1$fullName, function (i) {
# 
#         item <- find_item(i)
# 
# })
# 
# oaBreak1 <- unlist(oaWikiLoop1, recursive=FALSE)
# 
# 
# 
# # rows 1001 to 2000 -------------------------------------------------------
# 
# 
# 
# oa2 <- oaWiki %>%
#         slice((1001:2000))
# 
# oaWikiLoop2<- lapply(oa2$fullName, function (i) {
# 
#         item <- find_item(i)
# 
# })
# 
# oaBreak2 <- unlist(oaWikiLoop2, recursive=FALSE)

# 
# 
# # rows 2001 to 3000 -------------------------------------------------------
# 
# 
# oa3 <- oaWiki %>%
#         slice((2001:3000))
# 
# oaWikiLoop3<- lapply(oa3$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak3 <- unlist(oaWikiLoop3, recursive=FALSE)
# 
# 
# # rows 3001 to 4000 -------------------------------------------------------
# 
# 
# oa4 <- oaWiki %>%
#         slice((3001:4000))
# 
# oaWikiLoop4<- lapply(oa4$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak4 <- unlist(oaWikiLoop4, recursive=FALSE)

# 
# # rows 4001 to 5000 -------------------------------------------------------
# 
# 
# oa5 <- oaWiki %>%
#         slice((4001:5000))
# 
# oaWikiLoop5<- lapply(oa5$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak5 <- unlist(oaWikiLoop5, recursive=FALSE)
# 
# 
# # rows 5001 to 6000 -------------------------------------------------------
# 
# 
# oa6 <- oaWiki %>%
#         slice((5001:6000))
# 
# oaWikiLoop6<- lapply(oa6$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak6 <- unlist(oaWikiLoop6, recursive=FALSE)
# # 
# # 
# # # rows 6001 to 7000 -------------------------------------------------------
# # 
# # 
# oa7 <- oaWiki %>%
#         slice((6001:7000))
# 
# oaWikiLoop7<- lapply(oa7$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak7 <- unlist(oaWikiLoop7, recursive=FALSE)
# 
# 
# # rows 7001 to 10000 -------------------------------------------------------
# 
# 
# oa8 <- oaWiki %>%
#         slice((7001:10000))
# 
# oaWikiLoop8<- lapply(oa8$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak8 <- unlist(oaWikiLoop8, recursive=FALSE)


# # rows 10001 to 11000 -------------------------------------------------------
# 
# 
# oa9 <- oaWiki %>%
#         slice((10001:11000))
# 
# oaWikiLoop9<- lapply(oa9$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak9 <- unlist(oaWikiLoop9, recursive=FALSE)
# 
# 
# # rows 11001 to 13000 -------------------------------------------------------
# 
# 
# oa10 <- oaWiki %>%
#         slice((11001:13000))
# 
# oaWikiLoop10<- lapply(oa10$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak10 <- unlist(oaWikiLoop10, recursive=FALSE)


# # rows 13001 to 15000 -------------------------------------------------------
#
#
# oa11 <- oaWiki %>%
#         slice((13001:15000))
# 
# oaWikiLoop11<- lapply(oa11$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak11 <- unlist(oaWikiLoop11, recursive=FALSE)


# # rows 15001 to 18000 -------------------------------------------------------
#
#
# oa12 <- oaWiki %>%
#         slice((15001:18000))
# 
# oaWikiLoop12<- lapply(oa12$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak12 <- unlist(oaWikiLoop12, recursive=FALSE)
# 
# 
# # # rows 18001 to 20000 -------------------------------------------------------
# #
# #
# oa13 <- oaWiki %>%
#         slice((18001:20000))
# 
# oaWikiLoop13<- lapply(oa13$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak13 <- unlist(oaWikiLoop13, recursive=FALSE)

# # # rows 20001 to 23000 -------------------------------------------------------
# #
# # #
# oa14 <- oaWiki %>%
#         slice((20001:23000))
# 
# oaWikiLoop14<- lapply(oa14$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak14 <- unlist(oaWikiLoop14, recursive=FALSE)
# # 

# # # rows 23001 to 25000 -------------------------------------------------------
# #
# #
# oa15 <- oaWiki %>%
#         slice((23001:25000))
# 
# oaWikiLoop15<- lapply(oa15$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak15 <- unlist(oaWikiLoop15, recursive=FALSE)



# # # rows 25001 to 28000 -------------------------------------------------------
# #
# #
# oa16 <- oaWiki %>%
#         slice((25001:28000))
# 
# oaWikiLoop16<- lapply(oa16$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak16 <- unlist(oaWikiLoop16, recursive=FALSE)


# # # rows 28001 to 30000 -------------------------------------------------------
# #
#
# oa17 <- oaWiki %>%
#         slice((28001:30000))
# 
# oaWikiLoop17<- lapply(oa17$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak17 <- unlist(oaWikiLoop17, recursive=FALSE)



# # # rows 30001 to 33000 -------------------------------------------------------
# #
#
# oa18 <- oaWiki %>%
#         slice((30001:33000))
# 
# oaWikiLoop18<- lapply(oa18$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak18 <- unlist(oaWikiLoop18, recursive=FALSE)


# # # rows 33001 to 36000 -------------------------------------------------------
# #
###have  not been able to extract these cases from 33001 to 36000
# oa19a <- oaWiki %>%
#         slice(33001:33100)
# #
# oaWikiLoop19a <- lapply(oa19a$fullName, function (i) {
#         item <- find_item(i)
#         })
# 
# oaBreak19a <- unlist(oaWikiLoop19a, recursive=FALSE)
# 
# oa19aa <- oaWiki %>%
#         slice(33101:33200)
# #
# oaWikiLoop19aa <- lapply(oa19aa$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19aa <- unlist(oaWikiLoop19aa, recursive=FALSE)

# oa19ab <- oaWiki %>%
#         slice(33201:33300)
# #
# oaWikiLoop19ab <- lapply(oa19ab$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19ab <- unlist(oaWikiLoop19ab, recursive=FALSE)

# oa19ac <- oaWiki %>%
#         slice(33301:33400)
# #
# oaWikiLoop19ac <- lapply(oa19ac$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19ac <- unlist(oaWikiLoop19ac, recursive=FALSE)
# 
# oa19ad <- oaWiki %>%
#         slice(33401:33600)
# #
# oaWikiLoop19ad <- lapply(oa19ad$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19ad <- unlist(oaWikiLoop19ad, recursive=FALSE)

# oa19ae <- oaWiki %>%
#         slice(33601:33800)
# #
# oaWikiLoop19ae <- lapply(oa19ae$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19ae <- unlist(oaWikiLoop19ae, recursive=FALSE)

# oa19af <- oaWiki %>%
#         slice(33801:34000)
# #
# oaWikiLoop19af <- lapply(oa19af$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19af <- unlist(oaWikiLoop19af, recursive=FALSE)

# oa19ag <- oaWiki %>%
#         slice(34001:34500)
# #
# oaWikiLoop19ag <- lapply(oa19ag$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19ag <- unlist(oaWikiLoop19ag, recursive=FALSE)

# oa19ah <- oaWiki %>%
#         slice(34501:34600)
# #
# oaWikiLoop19ah <- lapply(oa19ah$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19ah <- unlist(oaWikiLoop19ah, recursive=FALSE)

# oa19ai <- oaWiki %>%
#         slice(34601:34700)
# #
# oaWikiLoop19ai <- lapply(oa19ai$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19ai <- unlist(oaWikiLoop19ai, recursive=FALSE)


# oa19aj <- oaWiki %>%
#         slice(34701:34900)
# #
# oaWikiLoop19aj <- lapply(oa19aj$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19aj <- unlist(oaWikiLoop19aj, recursive=FALSE)

# oa19ak <- oaWiki %>%
#         slice(34901:34950)
# #
# oaWikiLoop19ak <- lapply(oa19ak$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19ak <- unlist(oaWikiLoop19ak, recursive=FALSE)

# oa19al <- oaWiki %>%
#         slice(34951:34975)
# #
# oaWikiLoop19al <- lapply(oa19al$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19al <- unlist(oaWikiLoop19al, recursive=FALSE)

# oa19am <- oaWiki %>%
#         slice(34976:35000)
# #
# oaWikiLoop19am <- lapply(oa19am$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19am <- unlist(oaWikiLoop19am, recursive=FALSE)


# oa19c <- oaWiki %>%
#         slice(35001:36000)
# #
# oaWikiLoop19c <- lapply(oa19c$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak19c <- unlist(oaWikiLoop19c, recursive=FALSE)


# # # rows 36001 to 38000 -------------------------------------------------------
# #
#
# oa20 <- oaWiki %>%
#         slice(36001:38000)
# 
# oaWikiLoop20 <- lapply(oa20$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak20 <- unlist(oaWikiLoop20, recursive=FALSE)


# # # rows 38001 to 39000 -------------------------------------------------------
# #
#
# oa21 <- oaWiki %>%
#         slice((38001:39000))
# 
# oaWikiLoop21 <- lapply(oa21$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak21 <- unlist(oaWikiLoop21, recursive=FALSE)



# # # rows 39001 to 41303 -------------------------------------------------------
# #
#
# oa22 <- oaWiki %>%
#         slice((39001:41303))
# 
# oaWikiLoop22 <- lapply(oa22$fullName, function (i) {
#         item <- find_item(i)
# })
# 
# oaBreak22 <- unlist(oaWikiLoop22, recursive=FALSE)

# )
# Join lists together -----------------------------------------------------


bert <- find_item("Albert Newton")
jenny <- find_item("Jennifer Margaret Kee")

View(jenny)

# breakBert <- unlist(bert,  recursive=FALSE)

joinList <- do.call(c, list(oaBreak1, oaBreak2, oaBreak3, oaBreak4, oaBreak5, oaBreak6, oaBreak7, oaBreak8, oaBreak9, oaBreak10, oaBreak11, oaBreak12, oaBreak13, oaBreak14, 
                            oaBreak15, oaBreak16 , oaBreak17, oaBreak18, oaBreak19a, oaBreak19aa,  oaBreak19ab, oaBreak19ac, oaBreak19ad, oaBreak19ae, oaBreak19af, oaBreak19ag,
                            oaBreak19ah,oaBreak19ai, oaBreak19aj, oaBreak19ak, oaBreak19al,oaBreak19am, oaBreak19c, oaBreak20, oaBreak21, oaBreak22, bert, jenny))



View(joinList)
# create data file / tibble  ----------------------------------------------


wikidataMatch <- tibble (
        id = map_chr(joinList, "id"),
        label = map_chr(joinList, "label"),
        url=map_chr(joinList, "url"),
        description = map_chr(joinList, "description", .default = NA),
        match = map(joinList, "match"),
        aliases = map(joinList, "aliases")) %>% 
        mutate(
                matchText = map_chr(match, 3 , .default = NA),
                PossibleAlias = map_chr(aliases, 1, .default = NA)
        ) %>% 
        select(-match, -aliases) %>% 
        mutate(wikidataMatch = "Y") %>% 
        distinct()


view(wikidataNameMatch)

##write this csv regulalry so it can be used to import into markUp doc (otherise loops will need to be re-run if the environment is cleared)

write.csv(wikidataMatch, "wikidataMatch.csv")

# include / exclude words for match ---------------------------------------


string1 = c("Australia" , "NSW", " Victoria", "Australian", "Tasmania", "Queensland", "Melbourne", "New South Wales", 
            "Adelaide", "Victorian", "Sydney", "Aboriginal", "Fremantle", "australian", "Perth", "Italian-born", 
            "Indigenous education academic","Q42947998", "Q40391308", "Q41789383", "Q37375476", "Q91698743", "Q38803128",
            "Q57551242", "Q26704358","Q17318570", "Q23771328", "Q91208571", "Q56815961",
            "Q6375587", "Q53678222", "Q96187869", "Q4662139", "Q42636594", "Q59737370", 
            "Q60141411", "Q56440911", "Q91980063", "Q90094726", "Q14954682", "Q46851655",
            "Q18166645", "Q57904930", "Q69627764", "Q58418645", "Q40187053", "Q21165015", 
            "Q37373213", "Q89722146", "Q88086105", "Q61741188", "Q75782657", "Q70731997", 
            "Q39818657", "Q21932435", "Q57004917", "Q59684921", "Q58143378", "Q21606943", "Q6845925",
            "New South Wales politician and premier", "Norfolk Island politician", "Q18385372",
            "Q66725467", "Q82667", "Q56650757", "Q56440759", "Q2356897", "Q37393483", "Q70026805", "Q67064620",
            "Q37843077", "Q67231057", "Q24851642", "Q56254903", "Q18640515", "Q37839159", "Q43900835", "Q66685240",
            "Q5394642", "Q18645861", "Q21165814", "Q52727394", "Q59576600", "Q5546376", "Q3237300", "Q64684938", 
            "Q69627064", "Q69631432","Q27995816", "Q1411634", "Q50164000", "Q21598168", "Q55825496", "Q3339739", "Q6120159",
            "Q2638527", "Q27678467", "Q25659726", "Q63688819", "Q1174223", "Q333509", "Q6835037", "Q63213576", "Q23900847", "Q46200278",
            "Q57804862", "Q67819616", "Q1117915", "Q24041208","Q2075218", "Q1326252", "Q112877", "Q112877", "Q51914744", "Q39052654",
            "Q4758280", "Q43110705", "Q50663891", "Q2209560", "Q110124", "Q83833358", "Q37841925", "Q6251938",
            "Q16136210", "Q37831174", "Q4769065", "Q257049", "Q957612", "Q6238124", "Q59533008", "Q39061421",
            "Q202136", "Q2738939", "Q7175927", "Q3117430","Q16732157", "Q6828190", "Q21515490", "Q67179646",
            "Q56815974", "Q16105206", "Q263886","Q442605", "Q512658", "Q69526226", "Q58143294", "Q7153232", "Q5113163",
            "Q2143011","Q62606807", "Q5628527", "Q63705546", "Q54855", "Q64878016", "Q1022204", "Q6287144", "Q56887983",
            "Q16012545", "Q16104395", "Q944851", "Q504387", "Q2033551", "Q1903849", "Q18166458", "Q52157148",
            "Q4933267", "Q55999467", "Q21538464", "Q7687202")

string2 = c("Canadian", "disambiguation page", "Tibet", "British", "American", "South African", "Irish", "Leeds", "Tibetan writer", "Tibetan historian",
            "Italian", "Indian", "Swiss", "English", "United States", "German", "Barbadian", "Nepalese", "1941 harbor tug", "Greek", "Scotland", "Welsh", "Newfoundland", 
            "Scottish", "New Zealand", "Chile", "South Korean", "Ireland", "Argentina", "Azerbaijan", "Ethiopia", "Israeli", "Chinese", "South Korea",
            "Filipino", "Ohio", "Dorset", "Belgian", "Newton, MA", "Hungarian", "Brazilian", "Iddesleigh", "England", "Fijian", "The Mysteries of Laura", "Ming Dynasty", 
            "Slochteren", "Dutch", "news article", "French", "Spanish", "Suburb of", "painting by", "U.S", "Swedish", "Fiji", "Czech Republic",
            "Venezuela", "street in", "Albanian", "Croatia", "Jordan", "Days of our Lives", "Serbian", "Korean", "Thailand", "Brazil", "Jaipur",
            "New Jersey", "Danish", "Norwegian", "Woolwich", "Faroese", "scientific article", "Kenyan", "Bahamian", "Czech Republic",
            "Canada", "Vietnamese", "Czech", "Taiwanese", "Wikimedia albums discography", "artist discography",  "London", "album by", "Wikimedia list article",
            "singles discography", "compilation album", "discography", "television series", "Cymru", "Social Democratic Party", 
            "musical character", "Russian", "Yemeni",  "film directed by", "film by", "Peerage person", "Saskatchewan", 
            "Jamaican", "US chemist", "Austrian", "Minnesota", "US Army officer", "Connecticut", "Hong Kong", "Confederate", "theatrical character", "Malaysia", 
            "Moldovan", "Romanian", "Pittsburgh Post ", "Nova Scotia", "Tanzanian", "Thai", "novel by", "programming language", "Iriah scholar", 
            "Sailing cutter built in 1931", "Liberian", "South Carolina", "Zimbabwe", "SFMOMA", "US academic", "fictional character", "fictional race in Avatar", 
            "constructed language", "Colombia's", "human settlement", "Ming dynasty", "InterPro Domain", "protein family", " Japan's military", "portrait by", "InterPro Domain",
            "Bulgaria's", "naval warfare", "1964 documentary film about swimmer", "device that sucks up dust and dirt from floors", "Mexican", "Vietnam", "Ghana's military",
            "Georgia's military", "Myanmar", "Singaporean", "book by", "obituary in the New York Times", "Private art collection", "Sri Lankan", "United Kingdom General Election", "Aberdeen",
            "original composition by Henry Mancini", "instrumental track by Henry Mancini", "TED2013 talk", "Wine merchant and Mayor of Oxford", "Rattus norvegicus", "Greece's",
            "Japanese artist", "maritime warfare branch", "Upper Carniola, Slovenia", "Laos", "Yemen", "TV series", "Great-Great-Great-Great-Grandfather of Hoagy Carmichael", 
            "Great-Great-Great-Great-Great-Great-Grandfather of Hoagy Carmichael", "series in the National Archives and Records Administration's holdings", "alkali metal alloy which is liquid at room temperature",
            "North Carolina", "Agents that inhibit sodium-potassium-chloride symporters", "Finnish", "Ghanaian", "US Navy officer", "Accused of witchcraft", "North Macedonia",
            "third earl of Inchcape", "Member of Parliament (UK)", "THOMAS CHARLES RICHARDSON", "WILLIAM JOHN FORREST", "Vice-Chancellor of the University of Reading",
            "Chairman Ashtead Group", "association football player", "ming dynasty", "UCL", "Q60374467", "Q91698743", "Q61873164", "Q57321233", "Q57779111", 
            "Q76996007", "Q7154213", "Q65029568", "Q92061705", "Q7154217", "Q84611264", "Q5231286", 
            "Q57944494", "Q964427","Q89503368", "Q45692230", "Q89301920", "Q88009556", "Q90716274", 
            "Q21608566", "Q953319", "Q21606909", "Q42862690", "Q89349746", "Q84450869", "Q88444844", 
            "Q65218900", "Q76097092", "Q75560428", "Q75436918", "Q76164829", "Q42165364", "Q16014410",
            "Q65931639", "Q58424856", "Q75700615", "Q62559766", "Q21165716", "Q92892013", "Q24285826", 
            "Q51146528", "Q57942480", "Q88174800", "Q21604242", "Q87782215", "Q7687203", "Q59737370",
            "Q76040667", "Q76040722", "Q76040762", "Q20648325", "Q6849319", "Q75535419", "Q76165963", 
            "Q4708108", "Q15993676", "Q75631825", "Q91211584", "Q75412156", "Q75798499", "Q90949522",
            "Q92492291", "Q75325156", "Q57412324", "Q86592720", "Q65487304", "Q30349139","Q89739470",
            "Q75981847", "Q75980010", "Q6780373", "Q76290714", "Q89722146", "Q88086105", "Q75782657",
            "Q28361975", "Q37830454", "Q79516325", "Q75942491", "Q63148682", "Q96202014", "Q316939",
            "Q1750329", "Q76013152", "Q28797454", "Q76156787", "Q59631428", "Q1026840", "Q3869768",
            "Q2526431", "Q24778116", "Q83146375", "Q731647", "Q80481160", "Q61477840", 'Q53870700',
            "Q2864401", "Q75557052", "Q75267443", "Q76042765", "Q75353011", "Q84705510", "Q91075323",
            "Q58224002", "Q75999092", "Q75852922", "Q75852927", "Q28939700", "Q28843042", "Q61679437", 
            "Q90004554", "Q76043206", "Q73291725", "Q92446692", "Q76140625", "Q75792882", "Q24084216", 
            "Q6231958", "Q55007982", "Q4069118", "Q27525240", "Q4718932", "Q59198079", "Q92590086", 
            "Q43394670", "Q76370743", "Q43392250", "Q76287014", "Q72977651", "Q16135518", "Q30504158",
            "Q90775499", "Q21506181", "Q90710612", "Q42603882", "Q91279320", "Q91338872", "Q7650820",
            "Q24450210", "Q75640040", "Q86888883", "Q75703217", "Q91821254", "Q76204286", "Q42290048", 
            "Q55120392", "Q42306720", "Member of Parliament (UK)", "Q1699331", "Q6234587", "Q6249519", 
            "Q1453403", "Q16677910", "Q95876401", "accused of witchcraft", "Q332576", "Bishop of Shrewsbury",
            "NASA administrator", "miniature painter from the Southern Netherlands", "Inazuam Eleven character",
            "Baronet in the United Kingdom", "writer of children books from the US", "Ulster Unionist politician",
            "Q63296587", "University of Texas", "Q75353385", "Q4545158", "Q19326030", "Q22704344", "Q842267", "Q1330429",
            "Q41797564", "Q24772475", "Q24773222", "Q75780963", "Q7147502", "Q65553109", "Q1058391",
            "Q26202761", "Q62604062", "Q3900297", "Q55942091", "Q27903685", "Q56755230", "Q81338825", "Q149765",
            "Q981631", "Q14644710", "Q56704007", "Q1211948", "Q4545158", "Q55587675", "Q75741030",
            "Q366731", "Q1527827", "Q1688154", "Q5488196", "Q1539814", "Q2657431", "Q52534019", "Q6017554",
            "Q109186", "Q75999269", "Q56612499", "Q57014124", "Q7173038", "Q421922", "Q1146526", "Q237809", "Q445180",
            "Q3131721", "Q488093", "Q43109906", "Q6383947", "Q43064204", "Q86982749", "Q78124",
            "Q59609739", "Q58207142", "Q28927747", "Q2903332","Q66828232", "Q21166051", "Q86096704",
            "Q29014708", "Q41804648", "Q1438421","Q465846", "Q488093", "Q3160963", "Q62501724", "Q42327939", "Q18818012", 
            "Q94265282", "Q18165741", "Q53517846", "Q1700249", "Q56795264","Q16234060", "Q27120380", "Q76301832", "Q241620", 
            "Q7963355", "Q6765153", "Q5983280", "Q42303965", "Q3182241", "Q60684635", "Q96654375", "Q4772827", "Q667841",
            "Q83026833","Q59851338", "Q42243651", "Q59452505", "Q59834585", "Q87185437", "Q37390294", "Q38319860", "Q21166093", 
            "Q41283370", "Q54932365", "Q5650518", "Q1581440", "Q5650521", "Q60030727", "Q5293939", "Q5293672", "Q37837778",
            "Q47003123", "Q877332", "Q88803723", "Q5292633", "Q355750", "Q76331101", "Q9287906", "Q21538069", "Q57056207", 
            "Q6240012", "Q30071980", "Q86531408", "Q2483123", "Q6262426", "Q44077995", "Q3017941", "Q631455", "Q3498579",
            "Q37649265","Q268702", "Q1397635", "Q11727936", "Q6257473", "Q61740713", "Q7926310","Q56650695",
            "Q37837751","Q95629143", "Q6106354", "Q3619657","Q73657862","Q17278587","Q55640794", "Q18954185", "Q29642349", 
            "Q57408434", "Q42887432", "Q41775404", "Q65014938", "Q7325058", "Q555958", "Q62414557", "Q7608884", "Q7117330", "Q64001196",
            "Q4895060", "Q721219", "Q1889528", "Q41591689", "Q1674770", "Q1344009","Q57428177", "Q17432017","Q20646280", "Q18642387",
            "Q1655641", "Q7103784","Q380207", "Q63443073", "Q56670396", "Q37389702", "Q59823275", "Q79074636", "Q41905110", "Q45132679",
            "Q6234753", "Q60402466", "Q16196152", "Q97588822", "Q24039079", "Q6131576", "Q16026980",
            "Q60830362", "Q1929519", "Q372653", "Q88104666", "Q26258168", "Q1949095", "Q58425685", 
            "Q56642194", "Q26955595", "Q6530040", "Q731871", "Q7373509", "Q24054176", "Q3281581", "Q27042768",
            "Q60310937", "Q91317122", "Q38329590", "Q42544817", "Q5257734", "Q5606258", 
            "Q18530068", "Q22810729", "Q7764834", "Q28147789", "Q4773350", "Q62606807" , "Q5628527", "Q63705546", 
            "Q235184", "Q47255", "Q24068201", "Q52288518", "Q2393651", "Q1706799", "Q59773632", "Q18954655", "Q54811273",
            "Q18811283", "Q28939871", "Q53784250", "Q29867626", "Q22670438", "Q18683966", "Q28912369", "Q24044846", "Q6252816",
            "Q16626295", "Q91321888", "Q18534151", "Q37829427", "Q74037955", "Q7343289", "Q7153669", "Q28147110", 
            "Q27817494", "Q1509072", "Q1279986", "Q28164974", "Q42852215", "Q8011855", "anonymous anonymous")


# file type allocation ----------------------------------------------------

##wikiAus: these cases are all Aus based and match Australian people






wikiAus <- wikidataMatch %>% 
        filter_all(any_vars(str_detect (.,(paste(string1, collapse = "|" ))))) %>% 
        mutate(wikidataAusMatch = "Y") 


removeAus <- c("Q58906222", "Q4775087", "Q74168579", "Q94391240", "Q41464294", "Q58906222", "Q17653974", "Q56643328", "Q74168579",
               "Q58182007", "Q43865292", "Q58584779", "Q6996043", "Q45132679", "Q7307783", "Q741691", "Q30274272",
               "Q84352810", "Q97452952", "Q21937820", "Q21936333", "Q21931181", "Q21926548", "Q21970426", "Q21929766",
               "Q21972630", "Q21930205", "Q63756871", "Q17653974", "Q47475481", "Q97454495", "Q97454192", "Q97454186",
               "Q97455447", "Q21970768", "Q21935987", "Q23470306", "Q23808177", "Q7659975", "Q2596187", "Q964548", "Q19564293",
               "Q76271809", "Q16003690", "Q63162198", "32421283","Q5234356", "Q4962429", "Q21176202", "Q47546725", 
               "Q6225040", "Q91698743", "Q7173734", "Q7173729", "Q16030691", "Q4756975", "Q4757234",
               "Q4757503", "Q4799520", "Q4864838", "Q19879990", "Q4898960", "Q4907944", "Q16104381",
               "Q21069637", "Q22019523", "Q23620175", "Q4911421", "Q19872738", "Q4977354", "Q16866373", "Q4799520",
               "Q4864838", "Q19879990", "Q19667874", "Q59737370", "Q16095658", "Q5145207", "Q5231621",
               "Q5231622", "Q1174584", "Q56650757", "Q1177206", "Q18389679", "Q22017865", "Q5498723",
               "Q21228842", "Q5528086", "Q5534307", "Q5541783", "Q5671167", "Q16227719", "Q37393483", "Q37393483",
               "Q21068876", "Q5981275", "Q5981343", "Q87763198", "Q18352850", "Q21597782", "Q18685906", "Q16093707",
               "Q20983391", "Q96475076", "Q88086105", "Q6246295", "Q1392267", "Q16215177", "Q21664185", "Q16106274", "Q72973962",
               "Q17007366", "Q21078212", "Q17051663", "Q6243972", "Q6249527", "Q37736202", "Q6249992", "Q6255214", "Q3182377", "Q20741186",
               "Q6255812", "Q6256074", "Q3182407", "Q20737599", "Q6258255", "Q20856697", "Q16029985", "Q15486775", "Q6263851", "Q6303644",
               "Q6303644", "Q3183939", "Q4223", "Q1411634", "Q240001", "Q6384167", "Q60736180", "Q6385040", "Q18640561", "Q6388703", "Q6388703",
               "Q358208", "Q10533788", "Q6831820", "Q88172143", "Q7047210", "Q5809337", "Q7173115", "Q23822314", "Q3900832", "Q3900832",
               "Q18685814", "Q2534758", "Q7173889", "Q64797517", "Q7174489", "Q7174192", "Q3376673", "Q7175005", "Q7175792", "Q7175794", "Q93875090",
               "Q7324294", "Q21537285", "Q3497630", "Q7343627", "Q24598", "Q7349266", "Q7349269", "Q7349662", "Q67179646", "Q21664742", "Q16014448",
               "Q5355916", "Q2167601", "Q7369627", "Q72364832", "Q24087943", "Q19936242", "Q19975564", "Q963153", "Q8017885", "Q97457888", "Q63341445",
               "Q7173367", "Q8006012", "Q30125950", "Q1929043", "Q22019632", "Q17073306", "Q3081511", "Q7183429", "Q961079", "Q23023273",
               "Q19662105", "Q15497745", "Q18640488", "Q21536138", "Q6238793", "Q5534915", "Q22279128", "Q19974828", "Q24004581",
               "Q19871319", "Q22019246", "Q4934204", "Q19874881", "Q19662582", "Q6916393"
               )        

wikidataAusMatch <- wikiAus %>% 
        filter(!str_detect (id, paste(removeAus, collapse = "|" ))) %>% 
        # filter(!str_detect (fullName, "anonymous anonymous" )) %>% 
        mutate(wikidataAusMatch = "Y") %>% 
        distinct()

# View(wikidataAusMatch)

write_csv(wikidataAusMatch, "wikidataAusMatch.csv")
##wikiCheck: these cases need checking / unclear if correct person or not

# wikiCheck <- wikiNameMatch %>% 
#         filter_all(any_vars (str_detect (., paste(string1, collapse = "|"), negate=TRUE))) %>% 
#         filter_all(any_vars (!str_detect (., paste(string2, collapse = "|")))) 
# 
# wikiCheck <- wikiNameMatch %>% 
#         filter_all(all_vars(!str_detect (., paste(string1, collapse = "|")))) %>% 
#         filter_all(all_vars (!str_detect (., paste(string2, collapse = "|"))))  %>% 
#         distinct()
# 



##wikiNoMatch: these cases are not connected to an Australian person
# 
# wikiNoMatch <- wikiNameMatch %>% 
#         filter_all(any_vars (str_detect (.,  paste(string2, collapse = "|" )))) %>% 
#         # filter_all(any_vars (!str_detect (.,  paste(string1, collapse = "|" )))) %>% 
#         mutate(wikiAusMatch = "N") %>% 
#         distinct()
#         

# View(wikiNoMatch)
# View(wikiAus2)
# View(wikiNoMatch)

# write_csv(wikiCheck, "test.csv")
# 
# write_csv(wikiAus2, "aus.csv")
# 
# write_csv(wikiNoMatch, "noMatch.csv")
# 
# write_csv(wikiAus1, "ausTest.csv")




# linking to wikipedia ----------------------------------------------------

# TA <- page_info("en", "wikipedia", page = "Tony Abbott" , properties =c( "url", "displaytitle"), clean_response = TRUE)

# View(wikidataAusMatch)

wpcheck <- wikidataAusMatch %>% 
        slice(1:10)

View(wpcheck)

ausWikipediaLink <- lapply(wpcheck$label, function (i) {
        
        wikiPediaLoop <- page_info("en", "wikipedia", page = i , properties =c("url", "displaytitle"), clean_response = TRUE)
})      


testWP <- page_info("en", "wikipedia", page = "Philip Brady" , properties =c("url", "displaytitle"), clean_response = FALSE)

View(testWP)

View(ausWikipediaLink)


listBreak <- unlist(ausWikipediaLink, recursive=FALSE)

wikipediaMatch <- tibble (
        id = map(listBreak, "pageid"),
        url = map(listBreak, "fullurl"),
        name = map(listBreak, "displaytitle")) %>% 
        mutate(
                pageid = map_chr(id, 1, .default = NA),
                url = map_chr(url, 1, .default = NA),
                displayName = map_chr(name, 1, .default = NA)
        ) %>% 
        select (-id, -name) %>% 
        drop_na(pageid) %>% 
        mutate(wikipediaMatch = "Y") 
View(wikipediaMatch)        

write_csv(wikipediaMatch, "wikipediaMatch.csv" )

View(wikipediaMatch)
# breakLoop <- unlist(ausWikipediaLink, recursive=FALSE)
# breakLoop2 <- unlist(breakLoop, recursive=FALSE)
# breakLoop3 <- unlist(breakLoop2, recursive=FALSE)

# oaWikiTest <- oaWiki %>% 
#         slice(1:1000)
# 
# ausWikipediaLinkTest <- lapply(oaWikiTest$fullName, function (i) {
#         
#         wikiPediaLoop2 <- page_info("en", "wikipedia", page = i , properties =c( "url", "displaytitle"), clean_response = TRUE)
# })      
# 
# listBreak2 <- unlist(ausWikipediaLinkTest, recursive=FALSE)
# 
# wikipediaNameMatch <- tibble (
#         id = map(listBreak2, "pageid"),
#         url = map(listBreak2, "fullurl"),
#         name = map(listBreak2, "displaytitle")) %>% 
#         mutate(
#                 pageid = map_chr(id, 1, .default = NA),
#                 url = map_chr(url, 1, .default = NA),
#                 displayName = map_chr(name, 1, .default = NA)
#         ) %>% 
#         select (-id, -name)
# 
# write_csv(wikipediaNameMatch, "")
# 
# # wp_content <- revision_content("en","wikipedia", revisions = 552373187)
# # 
# # changeTest <- recent_changes("en", "wikipedia", page="32421283", dir="older")
# # 
# # breatTest <- unlist(changeTest, recursive=FALSE)

testWP <- page_info("en", "wikipedia", page= "George_Joseph_(Australian_politician)")


