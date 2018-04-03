#Install packages

#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tidyr")

#Load packages
library(dplyr)
library(stringr)
library(tidyr)

##################################################################
#CHECKPOINT 1 : Data Cleaning 1
##################################################################

#Load files
companies <- read.delim("companies.txt",header = TRUE,sep = "\t")
rounds2 <- read.csv("rounds2.csv")
mapping <- read.csv("mapping.csv")

str(companies)
str(rounds2)
str(mapping)

#Data cleaning

# Unique Companies in rounds2
length(unique(tolower(rounds2$company_permalink)))#66368

# Unique Companies in companies file
length(unique(tolower(companies$permalink)))#66368
#length(unique(tolower(companies$name)))#66038

# Difference in rounds2 vs companies: None
setdiff(tolower(rounds2$company_permalink) , tolower(companies$permalink)) #0

# Merge the frames
rounds2$company_permalink <- tolower(rounds2$company_permalink)
companies$permalink <- tolower(companies$permalink)

master_frame <- full_join(rounds2, companies, by = c("company_permalink" = "permalink"))

nrow(master_frame) #114949

#Remove NA as the column is needed a lot for mathematical calculations
#rounds2$raised_amount_usd <- sapply(rounds2$raised_amount_usd, function(x) if(is.na(x) == TRUE) x = 0 else x)

#Replace 0 to na in category_list column. Exception would be versions like 1.0 or 2.0 etc
mapping$category_list <- mapping$category_list %>% 
  str_replace_all("0", "na") %>%
  str_replace("\\.na", "\\.0") 

##################################################################
#CHECKPOINT 2 : Funding Type Analysis
##################################################################

funding_analysis_result <-master_frame %>% 
  group_by(funding_round_type) %>%
  summarise( numof_records= n(), raised_amount_usd = mean(raised_amount_usd, na.rm = TRUE)) %>%
  arrange(desc(raised_amount_usd))

funding_analysis_result %>% 
  filter(between(raised_amount_usd,5000000, 15000000)) %>%
  arrange(desc(raised_amount_usd))

#  Best Funding Type as per requirment: Venture

##################################################################
#CHECKPOINT 3 : Country Analysis
##################################################################

country_investment<-master_frame %>%
  group_by(funding_round_type,country_code) %>%
  filter(funding_round_type == 'venture') %>%
  summarise(total_investment = sum(raised_amount_usd,na.rm = T)) %>%
  arrange(desc(total_investment))
top9<-head(country_investment[-3,2],n=9L)
top9

#Top 3 Eng Speaking Countries (data identified from Countries_where_English_is_an_official_language.pdf)
#Country 1 : USA 
#Country 2 : GBR 
#Country 3 : IND

##################################################################
#CHECKPOINT 4: Sector Analysis 1
##################################################################
mapdata <- mapping %>% gather(main_sector, sectorval, 2:10)
mapdata <- mapdata[!(mapdata$sectorval == 0),]  
#mapdata

master_frame$primary_category <- sapply(master_frame$category_list,function(x) { if(regexpr("\\|",x) > 0) substring(x,1 ,regexpr("\\|", x) - 1) else as.character(x)})

mapdata[[1]] <- tolower(mapdata[[1]])
master_frame[[16]] <- tolower(master_frame[[16]])

master_frame <- left_join(master_frame %>% 
                            mutate(primary_category = tolower(primary_category)), mapdata %>% 
                            mutate(category_list = tolower(category_list)), by = c("primary_category" = "category_list"), all.x = T)

##################################################################
#Table 5.1  #Checkpoint 5: Sector Analysis 2
##################################################################

#Country 1 USA

D1 <- master_frame %>%
  filter(country_code == 'USA' & funding_round_type == 'venture' & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000) 

D1_no_of_investments <- D1 %>%
  group_by(main_sector) %>%
  summarise(length(raised_amount_usd)) %>%
  setNames(c("main_sector","no_of_investments")) %>%
  arrange(desc(no_of_investments))

D1_total_amt_invested <- D1 %>%
  group_by(main_sector) %>%
  summarise(sum(raised_amount_usd)) %>%
  setNames(c("main_sector","total_amt_invested")) %>%
  arrange(desc(total_amt_invested))

D1 <- D1 %>%
  left_join(D1_no_of_investments, by = c("main_sector" = "main_sector")) %>%
  left_join(D1_total_amt_invested, by = c("main_sector" = "main_sector"))

#Country 2 GBR

D2 <- master_frame %>%
  filter(country_code == 'GBR' & funding_round_type == 'venture' & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000) 

D2_no_of_investments <- D2 %>%
  group_by(main_sector) %>%
  summarise(length(raised_amount_usd)) %>%
  setNames(c("main_sector","no_of_investments")) %>%
  arrange(desc(no_of_investments))

D2_total_amt_invested <- D2 %>%
  group_by(main_sector) %>%
  summarise(sum(raised_amount_usd)) %>%
  setNames(c("main_sector","total_amt_invested")) %>%
  arrange(desc(total_amt_invested))

D2 <- D2 %>%
  left_join(D2_no_of_investments, by = c("main_sector" = "main_sector")) %>%
  left_join(D2_total_amt_invested, by = c("main_sector" = "main_sector"))

#Country 3 IND

D3 <- master_frame %>%
  filter(country_code == 'IND' & funding_round_type == 'venture' & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000) 

D3_no_of_investments <- D3 %>%
  group_by(main_sector) %>%
  summarise(length(raised_amount_usd)) %>%
  setNames(c("main_sector","no_of_investments")) %>%
  arrange(desc(no_of_investments))

D3_total_amt_invested <- D3 %>%
  group_by(main_sector) %>%
  summarise(sum(raised_amount_usd)) %>%
  setNames(c("main_sector","total_amt_invested")) %>%
  arrange(desc(total_amt_invested))

D3 <- D3 %>%
  left_join(D3_no_of_investments, by = c("main_sector" = "main_sector")) %>%
  left_join(D3_total_amt_invested, by = c("main_sector" = "main_sector"))

#Table 5.1
#Q1
length(D1$raised_amount_usd[!is.na(D1$raised_amount_usd)])
length(D2$raised_amount_usd[!is.na(D2$raised_amount_usd)])
length(D3$raised_amount_usd[!is.na(D3$raised_amount_usd)])

#Q2
sum(D1$raised_amount_usd[!is.na(D1$raised_amount_usd)])
sum(D2$raised_amount_usd[!is.na(D2$raised_amount_usd)])
sum(D3$raised_amount_usd[!is.na(D3$raised_amount_usd)])

#Top sectors for D1, D2 and D3
D1_sectors <- D1 %>% group_by(main_sector) %>%  summarise(count=n()) %>% arrange(desc(count))
D2_sectors <- D2 %>% group_by(main_sector) %>%  summarise(count=n()) %>% arrange(desc(count))
D3_sectors <- D3 %>% group_by(main_sector) %>%  summarise(count=n()) %>% arrange(desc(count))

#Q3
D1_sectors[1, "main_sector"]
D2_sectors[1, "main_sector"]
D3_sectors[1, "main_sector"]

#Q4
D1_sectors[2, "main_sector"]
D2_sectors[2, "main_sector"]
D3_sectors[2, "main_sector"]

#Q5
D1_sectors[3, "main_sector"]
D2_sectors[3, "main_sector"]
D3_sectors[3, "main_sector"]

#Q6
D1_sectors[1, "count"]
D2_sectors[1, "count"]
D3_sectors[1, "count"]

#Q7
D1_sectors[2, "count"]
D2_sectors[2, "count"]
D3_sectors[2, "count"]

#Q8
D1_sectors[3, "count"]
D2_sectors[3, "count"]
D3_sectors[3, "count"]

#Q9: For the top sector count-wise (point 3), which company received the highest investment?
D1 %>%
  filter(main_sector == 'Others') %>%
  group_by(tolower(name)) %>%
  summarise(sum(raised_amount_usd)) %>%
  setNames(c("name","total_investment")) %>%
  arrange(desc(total_investment)) %>%
  top_n(1, wt=total_investment) %>%
  select(name,total_investment)

D2 %>%
  filter(main_sector == 'Others') %>%
  group_by(tolower(name)) %>%
  summarise(sum(raised_amount_usd)) %>%
  setNames(c("name","total_investment")) %>%
  arrange(desc(total_investment)) %>%
  top_n(1, wt=total_investment) %>%
  select(name,total_investment)

D3 %>%
  filter(main_sector == 'Others') %>%
  group_by(tolower(name)) %>%
  summarise(sum(raised_amount_usd)) %>%
  setNames(c("name","total_investment")) %>%
  arrange(desc(total_investment)) %>%
  top_n(1, wt=total_investment) %>%
  select(name,total_investment)

#Q10:For the second-best sector count-wise (point 4), which company received the highest investment?
D1 %>%
  filter(main_sector == 'Social..Finance..Analytics..Advertising') %>%
  group_by(tolower(name)) %>%
  summarise(sum(raised_amount_usd)) %>%
  setNames(c("name","total_investment")) %>%
  arrange(desc(total_investment)) %>%
  top_n(1, wt=total_investment) %>%
  select(name,total_investment)

D2 %>%
  filter(main_sector == 'Social..Finance..Analytics..Advertising') %>%
  group_by(tolower(name)) %>%
  summarise(sum(raised_amount_usd)) %>%
  setNames(c("name","total_investment")) %>%
  arrange(desc(total_investment)) %>%
  top_n(1, wt=total_investment) %>%
  select(name,total_investment)

D3 %>%
  filter(main_sector == 'Social..Finance..Analytics..Advertising') %>%
  group_by(tolower(name)) %>%
  summarise(sum(raised_amount_usd)) %>%
  setNames(c("name","total_investment")) %>%
  arrange(desc(total_investment)) %>%
  top_n(1, wt=total_investment) %>%
  select(name,total_investment)

#######################################################
#Output files
write.csv(master_frame,"master_frame.csv", row.names = FALSE)