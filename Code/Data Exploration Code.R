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
trend_df %>% trend_df %>%  
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
