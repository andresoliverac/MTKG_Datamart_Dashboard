# clean enviorment
rm(list = ls())


library(data.table)

#To read SAS datasets 

library(haven)


library(lubridate)

library(tidyr)


library(dplyr)

#Package to get weekday

library(timeDate)
#Packaged for reduce (multiple merges)

library(purrr)
setwd("C:/Users/avandelaer/Documents/R open sources/OpenSourceProgramming/Group Assignment")
# Read dataset RawDataIIUserDailyAggregation as "aggegation"
aggregation <- read_sas("RawDataIIUserDailyAggregation.sas7bdat")
View(aggregation)
# Read dataset RawDataIIIPokerChipConversions as "conversions"
conversions <- read_sas('RawDataIIIPokerChipConversions.sas7bdat')
# Read dataset RawDataIDemographics as "demographics"
demographics <- read_sas("RawDataIDemographics.sas7bdat")
View(demographics)
# Read dataset AnalyticDataInternetGambling as "action"
activity <- read_sas("AnalyticDataInternetGambling.sas7bdat")
View(activity)

######################
### Data Cleansing ###
######################



#####################
## Variables types ##
#####################


# Aggregation #
# UserId from Numeric to CHARACTER
aggregation$UserID = as.character(aggregation$UserID)
# Date from character to DATE 
aggregation$Date = as.Date(aggregation$Date,"%Y%m%d")
# ProductID from Numeric to CHARACTER 
aggregation$ProductID = as.character(aggregation$ProductID)
sapply(aggregation,class)

# Demographic #
# UserId from Numeric to CHARACTER  
demographics$UserID = as.character(demographics$UserID)
# RegDate from text to DATE 
demographics$RegDate = as.Date(demographics$RegDate)
# Change all rows at once from text to DATE  
demographics$FirstPay = as.Date(demographics$FirstPay,"%Y%m%d")
demographics$FirstAct = as.Date(demographics$FirstAct,"%Y%m%d")
demographics$FirstSp = as.Date(demographics$FirstSp,"%Y%m%d")
demographics$FirstCa = as.Date(demographics$FirstCa,"%Y%m%d")
demographics$FirstGa = as.Date(demographics$FirstGa,"%Y%m%d")
demographics$FirstPo = as.Date(demographics$FirstPo,"%Y%m%d")
# Change all rows at once from text to DATE  
sapply(demographics,class)

# Conversion #
# UserId from Numeric to CHARACTER
conversions$UserID = as.character(conversions$UserID)
# UserId from Numeric to DATE
conversions$TransDateTime = as.POSIXlt(conversions$TransDateTime)
sapply(conversions,class)

# Activity #
activity = activity %>%
  rename(UserID =  USERID, Country = COUNTRY, Languague = LANGUAGE, Age = AGE, Gender = GENDER)
# UserId from Numeric to CHARACTER
activity$UserID = as.character(activity$UserID)
# From Numeric to DATE
activity$RegistrationDate = as.Date(activity$RegistrationDate)
activity$FOFirstActiveDate = as.Date(activity$FOFirstActiveDate)
activity$FOLastActiveDate = as.Date(activity$FOLastActiveDate)
activity$LAFirstActiveDate = as.Date(activity$LAFirstActiveDate)
activity$LALastActiveDate = as.Date(activity$LALastActiveDate)
activity$FirstSportsActiveDate = as.Date(activity$FirstSportsActiveDate)

activity$FOTotalWinnings = as.numeric(activity$FOTotalWinnings)
activity$FOTotalStakes = as.numeric(activity$FOTotalStakes)
activity$FOTotalBets = as.numeric(activity$FOTotalBets)
activity$LATotalStakes = as.numeric(activity$LATotalStakes)
activity$LATotalWinnings = as.numeric(activity$LATotalWinnings)
activity$LATotalBets = as.numeric(activity$LATotalBets)

sapply(activity,class)


####################
## MISSING VALUES ##
####################

#NA - Demographics
# DO IT FIRST THAT CHANGING THE TYPE TO DATE
# Replace NA with Character that gives info

demographics$FirstCa = as.character(demographics$FirstCa)
demographics$FirstCa <- demographics$FirstCa %>% 
  replace_na("Didn't play Casino")

demographics$FirstGa = as.character(demographics$FirstGa)
demographics$FirstGa <- demographics$FirstGa %>% 
  replace_na("Didn't play Games")

demographics$FirstPo = as.character(demographics$FirstPo)
demographics$FirstPo <- demographics$FirstPo %>% 
  replace_na("Didn't play Poker")


demographics$FirstSp = as.character(demographics$FirstSp)
demographics$FirstSp <- demographics$FirstSp %>% 
  replace_na("Didn't book Sports")


# Delete two columns that did not have information 
demographics <- demographics %>%
  filter(UserID != '1365903' & UserID != '1371535')


#NA - Activity

# Replace NA with Character that gives info
sum(is.na(activity))

#LA bets
activity$LATotalStakes <- activity$LATotalStakes %>% 
  replace_na(0)
activity$LATotalStakes = as.numeric(activity$LATotalStakes)

activity$LATotalWinnings <- activity$LATotalBets %>% 
  replace_na(0)
activity$LATotalWinnings = as.numeric(activity$LATotalWinnings)

activity$LAFirstActiveDate<- as.character(activity$LAFirstActiveDate)
activity$LAFirstActiveDate <- activity$LAFirstActiveDate %>% 
  replace_na("Didn't bet on Live-act")
activity$LALastActiveDate<- as.character(activity$LALastActiveDate)
activity$LALastActiveDate <- activity$LALastActiveDate %>% 
  replace_na("Didn't bet on Live-act")
activity$LATotalDaysActive <- activity$LATotalDaysActive %>% 
  replace_na("Didn't bet on Live-act")
activity$LATotalBets <- activity$LATotalBets %>%
  replace_na(0)
activity$LATotalBets = as.numeric(activity$LATotalBets)

#FO bets
activity$FOTotalStakes <- activity$FOTotalStakes %>% 
  replace_na(0)
activity$FOTotalStakes = as.numeric(activity$FOTotalStakes)

activity$FOTotalWinnings <- activity$FOTotalWinnings %>% 
  replace_na(0)
activity$FOTotalWinnings = as.numeric(activity$FOTotalWinnings)

activity$FOTotalBets <- activity$FOTotalBets %>% 
  replace_na(0)
activity$FOTotalBets = as.numeric(activity$FOTotalBets)

activity$FOFirstActiveDate<- as.character(activity$FOFirstActiveDate)
activity$FOFirstActiveDate <- activity$FOFirstActiveDate %>% 
  replace_na("Didn't bet on Fixed-odds")

activity$FOLastActiveDate<- as.character(activity$FOLastActiveDate)
activity$FOLastActiveDate <- activity$FOLastActiveDate %>% 
  replace_na("Didn't bet on Fixed-odds")

activity$FOTotalDaysActive <- activity$FOTotalDaysActive %>% 
  replace_na("Didn't bet on Fixed-odds")



# Aggreagation = 0 NAN
sum(is.na(aggregation))

# Conversions 0 NAN
sum(is.na(conversions))


# average age group by country
statCountry <- activity %>% 
  group_by(Country) %>% 
  summarise(avg_Age_for_country=mean(Age))
View(statCountry)
activity = merge(x=activity,y=statCountry ,by="Country", all.x = TRUE)


#######################################################
#Change codes for relevant words



library("readxl")

#Country
MyData <- read_excel("dictionary.xlsm", col_names = FALSE)

MyData = MyData %>%
  rename(Country =  ...1, Country_name = ...2)


demographics = merge(x=demographics,y=MyData ,by="Country", all.x = TRUE) 
demographics$Country <- NULL


#language
language <- read_excel("language.xlsm")

demographics = merge(x=demographics,y=language ,by="Language", all.x = TRUE) 
demographics$Language <- NULL

#ProductID
product_id <- read_excel("productid.xlsm")
aggregation = merge(x=aggregation,y=product_id ,by="ProductID", all.x = TRUE) 
aggregation$product_id <- NULL


#ApplicationID
ApplicationID <- read_excel("ApplicationID.xlsm")
demographics = merge(x=demographics,y=ApplicationID ,by="ApplicationID", all.x = TRUE) 
demographics$ApplicationID <- NULL

#Convert 124 to sell and 24 to buy 
demographics$Gender <-  gsub('1','Male',demographics$Gender)
demographics$Gender <-  gsub('0','Female',demographics$Gender)

#############################################################



##############    
## Outliers ##
##############

# Didn't change any outlier

#Function to identify outliers
source("https://goo.gl/4mthoF")
#outlierKD(data, variable)
# With Tukey's method (Outliers ranged aboce and below the 1.5*IQR)
# - nbr of outliers
# - mean of outliers
# - mean with outliers
# - mean without outliers
# Outliers will be replaced by NA 

# Aggregation
summary(aggregation)
outlierKD(aggregation, Winnings)
outlierKD(aggregation, Bets)
outlierKD(aggregation, Stakes)

# Conversions
summary(conversions)
outlierKD(conversions, TransAmount)

# Demographics
summary(demographics)

# Demographics
summary(activity)
outlierKD(activity, FOTotalWinnings)
outlierKD(activity, FOTotalStakes)
outlierKD(activity, FOTotalBets)
outlierKD(activity, LATotalStakes)
outlierKD(activity, LATotalWinnings)
outlierKD(activity, LATotalBets)

sapply(activity,class)

#aggregation %>% 
#group_by(ProductID) %>%
#summarize(count = n())

################
## ERRORS ##
################

# Some players have Winnings eventhough their Stake and their Bet is 0. It doesn't make sense. Gotta delete it.
#Proof of mistake / Comparison is with Total bets, winnings and stakes in productID 1 (Fixed-odds)
# Winnings are lower without inconsistencies

#totals without inconsistencies
aggregation %>%
  filter(!(Stakes == 0 & Bets == 0))%>%
  group_by(UserID) %>%
  filter(UserID == '1348062' & ProductID == 1) %>%
  summarize(s_bets = sum(Bets), s_sta = sum(Stakes), s_win = sum(Winnings))
#totals with inconsistencies
aggregation %>%
  filter(UserID == '1348062') %>%
  summarize(bet = sum(Bets), win = sum(Winnings), sta = sum(Stakes))
# Values from actiivity 
www <- activity %>%
  filter(UserID == '1348062')


#Nbr of clients with NO inconsistencies
aggregation %>%
  filter(!(Stakes == 0 & Winnings > 0 | Bets == 0 & Winnings > 0)) %>%
  select(UserID) %>%
  distinct(UserID) %>%
  count(n())
#Nbr of clients with  inconsistencies
aggregation %>%
  select(UserID) %>%
  distinct(UserID) %>%
  count(n())
# SAME AMOUNT OF CUSTOMERS AFTER MISTAKE FIXED 


#FIXED WINNINGS WITH NO BETS OR STAKES
aggregation <- aggregation %>%
  filter(!(Stakes == 0 & Winnings > 0 | Bets == 0 & Winnings > 0))



# Nbr of negative inconsistencies         
aggregation %>%
  filter(Stakes < 0 | Bets < 0 | Winnings < 0) %>%
  count(n())

# FIXED NEGATIVE INCONSISTENCIES
aggregation <- aggregation %>%
  filter(!(Stakes < 0 | Bets < 0 | Winnings < 0))


########################
## Feature Engineering ##
########################

# tot_appID <- aggregation2 %>%
#New dataframe to play freealy
aggregation2 <- aggregation
sapply(aggregation2, class)
#Add new date variables
aggregation2$Month <- months(aggregation2$Date)
aggregation2$year <- year(aggregation2$Date)

#Is weekday or Weekend?
aggregation2$weekday <- as.character(isWeekday(aggregation2$Date, wday=1:5))
#Convert 1 and 0 into more useable names
aggregation2$weekday <- gsub('TRUE','Wday',aggregation2$weekday)
aggregation2$weekday <-  gsub('FALSE','Wknd',aggregation2$weekday)

colnames(aggregation2)

#PIVOTING TO GET TOTALS AND FREQ
Tot_month <- aggregation2 %>%
  pivot_wider(UserID,names_from = Month, values_from =  c(Stakes,Winnings, Bets),names_prefix=('tot_month'), values_fill = list( Stakes = 0,Winnings = 0, Bets = 0 ),  values_fn = list(Stakes = sum, Winnings =sum, Bets = sum))

tot_year <- aggregation2 %>%
  pivot_wider(UserID,names_from = year, values_from = c(Stakes,Winnings, Bets), values_fill = list( Stakes = 0,Winnings = 0, Bets = 0 ),  values_fn = list(Stakes = sum, Winnings =sum, Bets = sum))

Freq_month <- aggregation2 %>%
  pivot_wider(UserID,names_from = Month, values_from =   Bets, names_prefix=('freq_month'),values_fill = list( Stakes = 0,Winnings = 0, Bets = 0 ),  values_fn = list( Bets = length))

Freq_Year <- aggregation2 %>%
  pivot_wider(UserID,names_from = year, values_from = Bets, names_prefix=('freq_year'),values_fill = list( Bets = 0 ),  values_fn = list(Bets = length))

Tot_prod <- aggregation2 %>%
  pivot_wider(UserID,names_from = `Product Description`, values_from =  c(Stakes,Winnings, Bets),names_prefix=('tot_Prod_'), values_fill = list( Stakes = 0,Winnings = 0, Bets = 0 ),  values_fn = list(Stakes = sum, Winnings =sum, Bets = sum))

Tot_prod_month <- aggregation2 %>%
  pivot_wider(UserID,names_from = c(`Product Description`, Month), values_from =  c(Stakes,Winnings, Bets),names_prefix=('tot_Prod_'), values_fill = list( Stakes = 0,Winnings = 0, Bets = 0 ),  values_fn = list(Stakes = sum, Winnings =sum, Bets = sum))

Freq_prod <- aggregation2 %>%
  pivot_wider(UserID,names_from = `Product Description`, values_from =   Bets,names_prefix=('freq_Prod_'), values_fill = list( Stakes = 0,Winnings = 0, Bets = 0 ),  values_fn = list(Bets = length))

Tot_week <- aggregation2 %>%
  pivot_wider(UserID,names_from = weekday, values_from =  c(Stakes,Winnings, Bets),names_prefix=('tot_'), values_fill = list( Stakes = 0,Winnings = 0, Bets = 0 ),  values_fn = list(Stakes = sum, Winnings =sum, Bets = sum))

Freq_week <- aggregation2 %>%
  pivot_wider(UserID,names_from = weekday, values_from =   Bets,names_prefix=('freq_'), values_fill = list( Stakes = 0,Winnings = 0, Bets = 0 ),  values_fn = list(Stakes = length, Winnings =length, Bets = length))
#Don't know if it's relevant.
#pivot_wider(UserID,names_from = ApplicationID, values_from =  c(Stakes,Winnings, Bets),names_prefix=('tot_AppID'), values_fill = list( Stakes = 0,Winnings = 0, Bets = 0 ),  values_fn = list(Stakes = sum, Winnings =sum, Bets = sum))

#New relevant variables
tot_year$Revenue <- tot_year$Stakes_2005 - tot_year$Winnings_2005
tot_year$Avg_stake <-  tot_year$Stakes_2005/tot_year$Bets_2005
tot_year$Avg_rev <-  tot_year$Revenue/tot_year$Bets_2005
#Tot_prod$Revenue <- Tot_prod$Stakes_2005 - tot_year$Winnings_2005
#Tot_prod$Avg <-  Tot_prod$Stakes_2005/tot_year$Bets_2005

# New variable with ranking
tot_year$rank_stakes <- as.integer(cut(tot_year$Stakes_2005, quantile(tot_year$Stakes_2005, probs=0:4/4)))
tot_year$rank_bets <- as.integer(cut(tot_year$Bets_2005, quantile(tot_year$Bets_2005, probs=0:4/4)))
tot_year$rank_rev <- as.integer(cut(tot_year$Revenue, quantile(tot_year$Revenue, probs=0:4/4)))
tot_year$rank_avg_stake <- as.integer(cut(tot_year$Avg_stake, quantile(tot_year$Avg_stake, probs=0:4/4)))
Freq_Year$rank_freq_y <- as.integer(cut(Freq_Year$freq_year2005, quantile(Freq_Year$freq_year2005, probs=0:4/4)))





# Left Join (basic)
# merge(x=aggregation2,y=j_1 ,by="UserID", all.x = TRUE) 


#Totals merged
totals_merged <- list(Tot_month, Tot_prod, tot_year, Tot_week, Tot_prod_month) %>% 
  reduce(inner_join, by = "UserID")

#Totals Frequencies merged
Frequencies_merged <- list(Freq_month, Freq_prod, Freq_week, Freq_Year) %>% 
  reduce(inner_join, by = "UserID")

#Merge all
first_draft <- merge(x=totals_merged,y=Frequencies_merged ,by="UserID") 

#See dimensions of all merged
dim(first_draft) 

#Add demographic info
new2 <- merge(x=first_draft,y=demographics ,by="UserID", all.x = TRUE) 



#####################################
#### DATA CLEANSING CONVERSIONS #####
#####################################


# Create new dataframe to play with
conversions2 <- conversions

#Convert 124 to sell and 24 to buy 
conversions2$TransType <-  gsub('124','sell',conversions2$TransType)
conversions2$TransType <-  gsub('24','buy',conversions2$TransType)

#Fix amount
#If type is buy is possitive and sell negative
for (x in 1:length(conversions2$TransType)){
  if(conversions2$TransType[x] == 'sell'){
    conversions2$TransAmount[x] <- (conversions2$TransAmount[x]*(-1))
  }
}


#Make TransDateTime a data type
conversions2$TransDateTime = as.Date(conversions2$TransDateTime,"%Y-%m-%d")
sapply(conversions2,class)

## NA ##
sum(is.na(conversions))

#Create new variable month and year out of date
conversions2$Month <- months(conversions2$TransDateTime)
conversions2$year <- year(conversions2$TransDateTime)

#Is weekday or Weekend?
conversions2$weekday <- as.character(isWeekday(conversions2$TransDateTime, wday=1:5))
#Convert 1 and 0 into more useable names
conversions2$weekday <- gsub('TRUE','Wday',conversions2$weekday)
conversions2$weekday <-  gsub('FALSE','Wknd',conversions2$weekday)


# Create new relevant variable with pivot
con_tot_type <- conversions2 %>%
  pivot_wider(UserID,names_from = TransType, values_from =  TransAmount,names_prefix=('poker_type_'), values_fill = list( TransAmount = 0 ),  values_fn = list(TransAmount = sum))

con_tot_month <- conversions2 %>%
  pivot_wider(UserID,names_from = Month, values_from =  TransAmount,names_prefix=('poker_rev_month'), values_fill = list( TransAmount = 0 ),  values_fn = list(TransAmount = sum))

conv_tot_year <- conversions2 %>%
  pivot_wider(UserID,names_from = year, values_from =  TransAmount,names_prefix=('poker_rev_year'), values_fill = list( TransAmount = 0 ),  values_fn = list(TransAmount = sum))

conv_tot_weekday <- conversions2 %>%
  pivot_wider(UserID,names_from = weekday, values_from =  TransAmount,names_prefix=('poker_rev_wday'), values_fill = list( TransAmount = 0 ),  values_fn = list(TransAmount = sum))

conv_freq_month <- conversions2 %>%
  pivot_wider(UserID,names_from = Month, values_from =   TransAmount,names_prefix=('poker_freq_month'), values_fill = list(TransAmount = 0 ),  values_fn = list(TransAmount = length))

conv_freq_year <- conversions2 %>%
  pivot_wider(UserID,names_from = year, values_from =   TransAmount,names_prefix=('poker_freq_year'), values_fill = list(TransAmount = 0 ),  values_fn = list(TransAmount = length))


#Merge all new tables created 
conv_merged <- list(con_tot_month, conv_tot_year, conv_tot_weekday, conv_freq_month,conv_freq_year,con_tot_type) %>% 
  reduce(inner_join, by = "UserID")

#Merge poker(conv) and other gambling(aggr)
datamart_1 = merge(x=new2,y=conv_merged ,by="UserID", all.x = TRUE) 

#datamart_1$Country_name = as.character(datamart_1$Country_name)
#datamart_1$Country_name <- datamart_1$Country_name %>% 
#  replace_na("UNKNOWN")
#datamart_1$Gender = as.character(datamart_1$Gender)
#datamart_1$Gender <- datamart_1$Gender %>% 
#  replace_na("UNKNOWN")
#datamart_1$Language.Description = as.factor(datamart_1$Language.Description)
#datamart_1$Language.Description <- datamart_1$Language.Description%>% 
 # replace_na("UNKNOWN")



acitivy

activity = subset(activity, select = -c(Languague,Country,Gender) )
datamart2 = merge(x=activity,y=datamart_1 ,by="UserID", no.dups = TRUE)
dim(activity)
datamart2$groupage <- datamart2$Age

for (x in 1:length(datamart2$Age)){
  if(datamart2$Age[x] >17 & datamart2$Age[x] <31){
    datamart2$groupage[x] <- '18-30'
  }else if (datamart2$Age[x] >30 & datamart2$Age[x] <41) {
    datamart2$groupage[x] <- '31-40'
  }else if (datamart2$Age[x] >40 & datamart2$Age[x] <51) {
    datamart2$groupage[x] <- '41-50'  
  }else if (datamart2$Age[x] >50 & datamart2$Age[x] <61) {
    datamart2$groupage[x] <- '51-60'  
  }else {
    datamart2$groupage[x] <- 'More than 60'  
  }
}
datamart2$Gender
datamart2$Gender<- as.factor(datamart2$Gender)
datamart2$UserID<- as.numeric(datamart2$UserID)
datamart2["Language Description"]<- as.factor(datamart2["Language Description"])
datamart2$Country_name<- as.factor(datamart2$Country_name)
#install.packages('rsconnect')
library(rsconnect)
#rsconnect::setAccountInfo(name='arnaudvandelaerproject',
#                       token='562BCC26B18900EDD9A862573925EC13',
#                      secret='JYSLMwtzHpJfrX1ZKyDi+EaZO1s+EJl8WlIyFQKP')
#rsconnect::deployApp('C:/Users/avandelaer/Documents/R open sources/group assingment/app2/appgambling/gambling')
