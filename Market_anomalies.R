# Calendar_Effect(17-18)
# 17:Identify January effects via dummy regression
setwd('/Users/danilavtonoskin/Desktop/Study/3 Semester/Empirical finance/HW/Codes')
getwd()
# Loading libraries
library("readxl")
library(dplyr)
library(stats)
library(tidyverse)
library(quantmod)
library(xts)
library(ggplot2)
# load data
month_effect <- read.csv('EMP_Finance_17_Avtonoshkin.csv', header = TRUE, sep = ';', dec = ',')
# convert data type into the right one
month_effect$Date = as.Date(month_effect$Date, format = '%Y-%m-%d')
month_effect_xts <- xts(month_effect[,2], order.by = month_effect$Date)
# divide a dataset into 2 periods (1980-1999 and 2000-2014)
#range(month_effect$Date)
month_effect_before_2000 <- month_effect %>%
  filter(Date < '2000-01-01')
#range(month_effect_before_2000$Date)
month_effect_after_2000 <- month_effect %>%
  filter(Date >= '2000-01-01')
#range(month_effect_after_2000$Date)
# extract months from a Date columns
month_effect_before_2000$Month <- format(as.Date(month_effect_before_2000$Date,
                                          format = '%Y-%m-%d'), '%m')
#head(month_effect_before_2000)
month_effect_after_2000$Month <- format(as.Date(month_effect_after_2000$Date,
                                                format = '%Y-%m-%d'), '%m')
#head(month_effect_after_2000)

# adding dummy variables
install.packages('fastDummies')
library(fastDummies)
month_effect_before_2000 <- dummy_cols(month_effect_before_2000,
                                       select_columns = 'Month')
names(month_effect_before_2000)[4:15] = c('January', 'February', 'March', 'April',
                                          'May', 'June', 'July', 'August',
                                          'September', 'October', 'November',
                                          'December')
#head(month_effect_before_2000)
# the same for the post 2000 period
month_effect_after_2000 <- dummy_cols(month_effect_after_2000, 
                                      select_columns = 'Month')
names(month_effect_after_2000)[4:15] = c('January', 'February', 'March', 'April',
                                          'May', 'June', 'July', 'August',
                                          'September', 'October', 'November',
                                          'December')
#head(month_effect_after_2000)
# building a first regression model for the period prior to 2000
reg_1 <- lm(month_effect_before_2000$S.P_Log_Returns ~ 0 + month_effect_before_2000$January +
              month_effect_before_2000$February + month_effect_before_2000$March +
              month_effect_before_2000$April + month_effect_before_2000$May + 
              month_effect_before_2000$June + month_effect_before_2000$July + 
              month_effect_before_2000$August + month_effect_before_2000$September +
              month_effect_before_2000$October + month_effect_before_2000$November + 
              month_effect_before_2000$December)
summary(reg_1)
# January tends to have a higher return with p value = 0.0178, Moreover, 
# May and December seem to have higher returns as well are characterized with higher returns which is surprising
# with p value for May = 0.0260 and p value for December = 0.0172
# now we will take a look at the period since 2000
reg_2 <- lm(month_effect_after_2000$S.P_Log_Returns ~ 0 + month_effect_after_2000$January +
              month_effect_after_2000$February + month_effect_after_2000$March +
              month_effect_after_2000$April + month_effect_after_2000$May + 
              month_effect_after_2000$June + month_effect_after_2000$July + 
              month_effect_after_2000$August + month_effect_after_2000$September +
              month_effect_after_2000$October + month_effect_after_2000$November + 
              month_effect_after_2000$December)
summary(reg_2)
# no calendar effect (monthly) can be identified anymore

# 18:Identify day-of-week effects
# load data
daily_effect <- read.csv('EMP_Finance_18_Avtonoshkin.csv', header = TRUE, sep = ';', dec = ',')
# convert data type into the right one
daily_effect$Date = as.Date(daily_effect$Date, format = '%Y-%m-%d')
daily_effect_xts <- xts(daily_effect[,2], order.by = daily_effect$Date)
# finding weekdays
daily_effect$Weekday <- wday(daily_effect$Date, label = TRUE)
head(daily_effect)
# creating dummy variables
daily_effect <- dummy_cols(daily_effect, 
                                      select_columns = 'Weekday')
# drop saturday and sunday columns
drop <- c('Weekday_Sun', 'Weekday_Sat')
daily_effect <- daily_effect[,!(names(daily_effect)%in% drop)]
#names(daily_effect)
dayoftheweek <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
names(daily_effect)[4:8] <- dayoftheweek
# build a regression to test day of the week effect on returns
reg_3 <- lm(daily_effect$IMOEX ~ 0 + daily_effect$Monday + 
              daily_effect$Tuesday + daily_effect$Wednesday + 
              daily_effect$Thursday + daily_effect$Friday)
summary(reg_3)
# we got a Tuesday effect observable on the Russian stock exchange with a positive slope
# and a p value of 0.0165

#19:Set up Winners/Losers portfolio
win_vs_lose <- read.csv('EMP_Finance_19_Avtonoshkin.csv', header = TRUE, sep = ';', dec = ',')
# convert data type into the right one
win_vs_lose$Date = as.Date(win_vs_lose$Date, format = '%Y-%m-%d')
win_vs_lose_xts <- xts(month_effect[,2], order.by = month_effect$Date)
# calculate for the first period 2000-2001 cum returns
head(win_vs_lose)
names(win_vs_lose)
first_period <- win_vs_lose[,1:16] %>%
  filter(Date < '2001-01-01')
head(first_period)
for(i in 2:length(names(first_period))){
  first_period[,i] = cumsum(first_period[,i])
}
#finding the best 3 stocks as winners and all others as losers
tail(first_period)
# extract the last row as a cum sum for the period
cum_ret_1 <- tail(first_period,1)
cum_ret_1
# drop the date column
cum_ret_1 <- cum_ret_1[,!(names(cum_ret_1)%in% c('Date'))]
cum_ret_1
# transpose the df
rownames(cum_ret_1) <- NULL
cum_ret_1 <- t(cum_ret_1)
# rank cumularive returns
cum_ret_1 <- as.data.frame(cum_ret_1)
cum_ret_1
cum_ret_1['Company'] <- rownames(cum_ret_1)
rownames(cum_ret_1) <- NULL
names(cum_ret_1)[1] <- c('Cum_Return')
cum_ret_1
cum_ret_1_sort <- cum_ret_1 %>%
  arrange(desc(Cum_Return))
cum_ret_1_sort
# Adobe, Merck, Pepsico are the winners for the 1 period
winners_1 <- head(cum_ret_1_sort,3)
winners_1
# everyone else is the loser for the 1 period
losers_1 <- tail(cum_ret_1_sort,12)
losers_1

# calculate for the second period 2001-2002 cum returns
second_period <- win_vs_lose[,1:16] %>%
  filter(Date < '2002-01-01' & Date > '2001-01-01')
head(second_period)
for(i in 2:length(names(second_period))){
  second_period[,i] = cumsum(second_period[,i])
}
#finding the best 3 stocks as winners and all others as losers
tail(second_period)
# extract the last row as a cum sum for the period
cum_ret_2 <- tail(second_period,1)
cum_ret_2
# drop the date column
cum_ret_2 <- cum_ret_2[,!(names(cum_ret_2)%in% c('Date'))]
cum_ret_2
# transpose the df
rownames(cum_ret_2) <- NULL
cum_ret_2 <- t(cum_ret_2)
# rank cumularive returns
cum_ret_2 <- as.data.frame(cum_ret_2)
cum_ret_2
cum_ret_2['Company'] <- rownames(cum_ret_2)
rownames(cum_ret_2) <- NULL
names(cum_ret_2)[1] <- c('Cum_Return')
cum_ret_2
cum_ret_2_sort <- cum_ret_2 %>%
  arrange(desc(Cum_Return))
cum_ret_2_sort
# Microsoft, Bank of America, Costco are the winners for the 2 period
winners_2 <- head(cum_ret_2_sort,3)
winners_2
# everyone else is the loser for the 2 period
losers_2 <- tail(cum_ret_2_sort,12)
losers_2

# calculate for the third period 2002-2003 cum returns
head(win_vs_lose)
names(win_vs_lose)
third_period <- win_vs_lose[,1:16] %>%
  filter(Date > '2002-01-01' & Date < '2003-01-01')
head(third_period)
for(i in 2:length(names(third_period))){
  third_period[,i] = cumsum(third_period[,i])
}
#finding the best 3 stocks as winners and all others as losers
tail(third_period)
# extract the last row as a cum sum for the period
cum_ret_3 <- tail(third_period,1)
cum_ret_3
# drop the date column
cum_ret_3 <- cum_ret_3[,!(names(cum_ret_3)%in% c('Date'))]
cum_ret_3
# transpose the df
rownames(cum_ret_3) <- NULL
cum_ret_3 <- t(cum_ret_3)
# rank cumularive returns
cum_ret_3 <- as.data.frame(cum_ret_3)
cum_ret_3
cum_ret_3['Company'] <- rownames(cum_ret_3)
rownames(cum_ret_3) <- NULL
names(cum_ret_3)[1] <- c('Cum_Return')
cum_ret_3
cum_ret_3_sort <- cum_ret_3 %>%
  arrange(desc(Cum_Return))
cum_ret_3_sort
# Bank of America, P&G, Merck are the winners for the 3 period
winners_3 <- head(cum_ret_3_sort,3)
winners_3
# everyone else is the loser for the 3 period
losers_3 <- tail(cum_ret_3_sort,12)
losers_3

# calculate for the forth period 2003-2004 cum returns
head(win_vs_lose)
names(win_vs_lose)
forth_period <- win_vs_lose[,1:16] %>%
  filter(Date > '2003-01-01' & Date < '2004-01-01')
head(forth_period)
for(i in 2:length(names(forth_period))){
  forth_period[,i] = cumsum(forth_period[,i])
}
#finding the best 3 stocks as winners and all others as losers
tail(forth_period)
# extract the last row as a cum sum for the period
cum_ret_4 <- tail(forth_period,1)
cum_ret_4
# drop the date column
cum_ret_4 <- cum_ret_4[,!(names(cum_ret_4)%in% c('Date'))]
cum_ret_4
# transpose the df
rownames(cum_ret_4) <- NULL
cum_ret_4 <- t(cum_ret_4)
# rank cumularive returns
cum_ret_4 <- as.data.frame(cum_ret_4)
cum_ret_4
cum_ret_4['Company'] <- rownames(cum_ret_4)
rownames(cum_ret_4) <- NULL
names(cum_ret_4)[1] <- c('Cum_Return')
cum_ret_4
cum_ret_4_sort <- cum_ret_4 %>%
  arrange(desc(Cum_Return))
cum_ret_4_sort
# Intel, Cisco, JP Morgan are the winners for the 4 period
winners_4 <- head(cum_ret_4_sort,3)
winners_4
# everyone else is the loser for the 4 period
losers_4 <- tail(cum_ret_4_sort,12)
losers_4

# calculate for the fifth period 2004-2005 cum returns
head(win_vs_lose)
names(win_vs_lose)
fifth_period <- win_vs_lose[,1:16] %>%
  filter(Date > '2004-01-01' & Date < '2005-01-01')
head(fifth_period)
for(i in 2:length(names(fifth_period))){
  fifth_period[,i] = cumsum(fifth_period[,i])
}
#finding the best 3 stocks as winners and all others as losers
tail(fifth_period)
# extract the last row as a cum sum for the period
cum_ret_5 <- tail(fifth_period,1)
cum_ret_5
# drop the date column
cum_ret_5 <- cum_ret_5[,!(names(cum_ret_5)%in% c('Date'))]
cum_ret_5
# transpose the df
rownames(cum_ret_5) <- NULL
cum_ret_5 <- t(cum_ret_5)
# rank cumularive returns
cum_ret_5 <- as.data.frame(cum_ret_5)
cum_ret_5
cum_ret_5['Company'] <- rownames(cum_ret_5)
rownames(cum_ret_5) <- NULL
names(cum_ret_5)[1] <- c('Cum_Return')
cum_ret_5
cum_ret_5_sort <- cum_ret_5 %>%
  arrange(desc(Cum_Return))
cum_ret_5_sort
# Adobe, Nike, Costco are the winners for the 5 period
winners_5 <- head(cum_ret_5_sort,3)
winners_5
# everyone else is the loser for the 5 period
losers_5 <- tail(cum_ret_5_sort,12)
losers_5

# calculate for the sixth period 2005-2006 cum returns
head(win_vs_lose)
names(win_vs_lose)
sixth_period <- win_vs_lose[,1:16] %>%
  filter(Date > '2005-01-01' & Date < '2006-01-01')
head(sixth_period)
for(i in 2:length(names(sixth_period))){
  sixth_period[,i] = cumsum(sixth_period[,i])
}
#finding the best 3 stocks as winners and all others as losers
tail(sixth_period)
# extract the last row as a cum sum for the period
cum_ret_6 <- tail(sixth_period,1)
cum_ret_6
# drop the date column
cum_ret_6 <- cum_ret_6[,!(names(cum_ret_6)%in% c('Date'))]
cum_ret_6
# transpose the df
rownames(cum_ret_6) <- NULL
cum_ret_6 <- t(cum_ret_6)
# rank cumularive returns
cum_ret_6 <- as.data.frame(cum_ret_6)
cum_ret_6
cum_ret_6['Company'] <- rownames(cum_ret_6)
rownames(cum_ret_6) <- NULL
names(cum_ret_6)[1] <- c('Cum_Return')
cum_ret_6
cum_ret_6_sort <- cum_ret_6 %>%
  arrange(desc(Cum_Return))
cum_ret_6_sort
# Adobe, Pepsico, Exxon are the winners for the 6 period
winners_6 <- head(cum_ret_6_sort,3)
winners_6
# everyone else is the loser for the 6 period
losers_6 <- tail(cum_ret_6_sort,12)
losers_6
# lets track the performance of winners vs losers vs S&P 5000 for 2000-2004 incl.
head(win_vs_lose)
# Adobe, Merck, Pepsico are the winners for the 1 period
# Microsoft, Bank of America, Costco are the winners for the 2 period
# Bank of America, P&G, Merck are the winners for the 3 period
# Intel, Cisco, JP Morgan are the winners for the 4 period
# Adobe, Nike, Costco are the winners for the 5 period
# Adobe, Pepsico, Exxon are the winners for the 6 period
# first year cumulative winners vs losers
names(second_period)
head(second_period)
tail(second_period)
first_year <- second_period %>%
  mutate(Winners = rowSums(second_period[,c('Adobe', 'Merck', 'Pepsico')])/3,
         Losers = rowSums(second_period[,!names(second_period)%in%c('Adobe', 'Merck', 'Pepsico', 'Date')])/12 )
head(first_year)
# second year cumulative winners vs losers
second_year <- third_period %>%
  mutate(Winners = rowSums(third_period[,c('Microsoft', 'Bank.of.America', 'Costco')])/3,
         Losers = rowSums(third_period[,!names(third_period)%in%c('Microsoft', 'Bank.of.America', 'Costco', 'Date')])/12 )
head(second_year)
# third year cumulative winners vs losers
third_year <- forth_period %>%
  mutate(Winners = rowSums(forth_period[,c('Bank.of.America', 'P.G', 'Merck')])/3,
         Losers = rowSums(forth_period[,!names(forth_period)%in%c('Bank.of.America', 'P.G', 'Merck', 'Date')])/12 )
head(third_year) 
# forth year cumulative winners vs losers
forth_year <- fifth_period %>%
  mutate(Winners = rowSums(fifth_period[,c('Intel', 'Cisco', 'JP.Morgan')])/3,
         Losers = rowSums(fifth_period[,!names(fifth_period)%in%c('Intel', 'Cisco', 'JP.Morgan', 'Date')])/12 )
head(forth_year) 
# fifth year cumulative winners vs losers
fifth_year <- sixth_period %>%
  mutate(Winners = rowSums(sixth_period[,c('Adobe', 'Nike', 'Costco')])/3,
         Losers = rowSums(sixth_period[,!names(sixth_period)%in%c('Adobe', 'Nike', 'Costco', 'Date')])/12 )
head(fifth_year)

# to combine data in one df we would need to add to the n+1 year cum returns for both winners and losers
# final cum return for the year n (final row)
year_1 <- tail(first_year,1)[c('Winners','Losers')]
year_1
tail(second_year)
# second year
second_year <- second_year %>%
  mutate(Winners = Winners + year_1[,1],
         Losers = Losers + year_1[,2])
tail(second_year)
year_2 <- tail(second_year,1)[c('Winners','Losers')]
year_2
# third year
third_year <- third_year %>%
  mutate(Winners = Winners + year_2[,1],
         Losers = Losers + year_2[,2])
tail(third_year)
year_3 <- tail(third_year,1)[c('Winners','Losers')]
year_3
# forth year
forth_year <- forth_year %>%
  mutate(Winners = Winners + year_3[,1],
         Losers = Losers + year_3[,2])
tail(forth_year)
year_4 <- tail(forth_year,1)[c('Winners','Losers')]
year_4
# fifth year
fifth_year <- fifth_year %>%
  mutate(Winners = Winners + year_4[,1],
         Losers = Losers + year_4[,2])
tail(fifth_year)
year_5 <- tail(fifth_year,1)[c('Winners','Losers')]
year_5
# join all the years in one df
portfolios <- first_year[,c('Date','Winners','Losers')]
tail(portfolios)
# adding the second year
portfolios <- union_all(portfolios, second_year[,c('Date','Winners','Losers')])
# adding the third year
portfolios <- union_all(portfolios, third_year[,c('Date','Winners','Losers')])
tail(portfolios)
# adding the forth year   
portfolios <- union_all(portfolios, forth_year[,c('Date','Winners','Losers')])
tail(portfolios)
# adding the fifth year   
portfolios <- union_all(portfolios, fifth_year[,c('Date','Winners','Losers')])
tail(portfolios)
range(portfolios$Date)
head(portfolios)
# adding S&P 500 cum return
head(win_vs_lose)
S_P500 <- win_vs_lose %>%
  filter(Date > '2001-01-01' & Date < '2006-01-01') %>%
  #select(S.P.500) %>%
  mutate(S.P.500 = cumsum(S.P.500),.keep = 'none')
  #select(win_vs_lose$S.P.500)
head(S_P500)
nrow(S_P500)
nrow(portfolios)
portfolios <- cbind(portfolios, S_P500)
# visualization of cumulative returns for Winners vs Losers vs S&P500
graph <- ggplot(portfolios, aes(Date)) + geom_line(aes(Date,Winners, colour = 'Winners')) +
  geom_line(aes(Date,Losers, colour = 'Losers')) + geom_line(aes(Date,S.P.500, colour = 'S&P500')) + 
  ylab('Accumulated_Returns') 
graph
# so losers outperform winners and S&P
# strategy of buying losers and selling winners should work at least for the period 2001 - 2006

  
  

                        


