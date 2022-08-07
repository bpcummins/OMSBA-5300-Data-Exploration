library(tidyverse)
library(fixest)
library(purrr)
library(vtable)
library(estimatr)
library(lubridate)

file_names <- list.files(path = 'Data/', pattern = 'trends_up_to', full.names = TRUE)

#read in google trends data
trend_df <- file_names %>% 
  map_df(read_csv)

#date format and months aggregation
trend_df <- trend_df %>%  
  mutate(monthorweek = str_sub(monthorweek,1,10))

trend_df$monthorweek <- trend_df$monthorweek %>% 
  ymd() %>% 
  floor_date(unit='month')

#index calc
trend_df <- trend_df %>% 
  group_by(schname, keyword) %>% 
  mutate(index = (index -mean(index, na.rm=TRUE))/sd(index, na.rm=TRUE))

#could index to keyword-month, school-week, etc same as above
#if ^ change line 41 to reflect


#read in other files
id_name_link = read_csv('Data/id_name_link.csv')
scorecard = read_csv('Data/Most+Recent+Cohorts+(Scorecard+Elements).csv')

#grouping and counting school names then filtering out ones that appear more than once
id_name <- id_name_link %>% 
  group_by(schname) %>% 
  mutate(n = n()) %>% 
  filter(n == 1)

#join trends to id
id_and_trends <- inner_join(id_name, trend_df, by='schname')

# join scorecard to id/trends
clean_df <- inner_join(id_and_trends, scorecard, by=c('unitid' = 'UNITID'))

#filter full data set
clean_df <- clean_df %>%
  filter(`md_earn_wne_p10-REPORTED-EARNINGS` != 'NULL') %>%
  filter(`md_earn_wne_p10-REPORTED-EARNINGS` != 'PrivacySuppressed') %>% 
  mutate(earnings = as.numeric(`md_earn_wne_p10-REPORTED-EARNINGS`))

#create dummy variable for before or after scorecard
clean_df$post_scorecard <- ifelse(clean_df$monthorweek >= '2015-9-1', 1, 0)


#filter to only be bach degrees
clean_df <- clean_df %>% 
  filter(PREDDEG == 3)  

#do this after the filter
median(clean_df$earnings, na.rm=TRUE) # 41700
clean_df$high_earner <- ifelse(clean_df$earnings >= 41700, 1, 0)

clean_df$ACTCMMID <- as.numeric(clean_df$ACTCMMID)
clean_df$SAT_AVG <- as.numeric(clean_df$SAT_AVG)


#lets do three income groups: low mid and high
# low is bottom quartile, high is upper, mid is IQR
quantile(clean_df$earnings, probs = c(0,0.25,0.5,0.75,1))
#     0%    25%    50%    75%   100% 
#  16700  36400  41700  49100 166200 

clean_df$low_income <- ifelse(clean_df$earnings < 36400, 1, 0)
clean_df$mid_income <- ifelse(clean_df$earnings >= 36400 & clean_df$earnings <= 49100, 1, 0)
clean_df$high_income <- ifelse(clean_df$earnings > 49100, 1, 0)


#only keeping columns I used in the technical appendix to make it lighter 
clean_df <-  clean_df %>% 
  subset(select = c(index, earnings, post_scorecard, high_earner, PBI, HSI, 
                  MENONLY, WOMENONLY, ACTCMMID, SAT_AVG, low_income, mid_income, high_income))

#saving out to be accessed in TA and write up
write.csv(clean_df, 'Data/clean_trends.csv',row.names=FALSE)
