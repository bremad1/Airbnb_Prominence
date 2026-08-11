library(dplyr)
library(lubridate) #funtion for date

setwd('C:/Users/brema/iCloudDrive/4-1/Airbnb/Test')
#################Combining raw data into one dataset########################
a = read.csv('raw CSV/230405 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
b = read.csv('raw CSV/230503 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
c = read.csv('raw CSV/230605 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
d = read.csv('raw CSV/230703 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
e = read.csv('raw CSV/230804 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
f = read.csv('raw CSV/230905 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
g = read.csv('raw CSV/231001 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
h = read.csv('raw CSV/231101 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
i = read.csv('raw CSV/231204 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
j = read.csv('raw CSV/240105 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
k = read.csv('raw CSV/240206 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
l = read.csv('raw CSV/240307 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
m = read.csv('raw CSV/240406 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
n = read.csv('raw CSV/240503 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
o = read.csv('raw CSV/240604 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
p = read.csv('raw CSV/240705 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
q = read.csv('raw CSV/240804 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
r = read.csv('raw CSV/240904 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
s = read.csv('raw CSV/241004 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
t = read.csv('raw CSV/241104 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))
u = read.csv('raw CSV/241204 NYC.csv', colClasses = c("id" = "character", 'host_id'='character'))

raw_data = rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)
rm(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)
save(raw_data, file='RData/1. raw_data.RData')
####################################
#load(RData/1. raw_data.RData)
raw_data = raw_data%>%rename(Date = last_scraped)
raw_data$Date <- ymd(raw_data$Date) #declare as Date data

ex <- raw_data %>%
  select(Date, host_id, host_is_superhost) %>% #keep necessary variable only
  distinct(Date, host_id, .keep_all = TRUE) %>%  # remove repeated (Date, host_id) combination
  arrange(host_id, Date) %>%
  group_by(host_id) %>%
  mutate( #create variable that indicates superhost status of last period
    ex_super = case_when( 
      # 2023년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 2 & year(Date) == 2023), 
               as.character(first(host_is_superhost[month(Date) == 2 & year(Date) == 2023])), 
               NA_character_),
      
      # 2023년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 5 & year(Date) == 2023), 
               as.character(first(host_is_superhost[month(Date) == 5 & year(Date) == 2023])), 
               NA_character_),
      
      # 2023년 10, 11, 12월에는 같은 해 8월의 host_is_superhost 값을 사용
      month(Date) %in% c(10, 11, 12) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 8 & year(Date) == 2023), 
               as.character(first(host_is_superhost[month(Date) == 8 & year(Date) == 2023])), 
               NA_character_),
      
      # 2024년 1, 2, 3월에는 2023년 11월의 host_is_superhost 값을 사용
      month(Date) %in% c(1, 2, 3) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 11 & year(Date) == 2023), 
               as.character(first(host_is_superhost[month(Date) == 11 & year(Date) == 2023])), 
               NA_character_),
      
      # 2024년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 2 & year(Date) == 2024), 
               as.character(first(host_is_superhost[month(Date) == 2 & year(Date) == 2024])), 
               NA_character_),
      
      # 2024년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 5 & year(Date) == 2024), 
               as.character(first(host_is_superhost[month(Date) == 5 & year(Date) == 2024])), 
               NA_character_),
      
      month(Date) %in% c(10, 11, 12) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 8 & year(Date) == 2024), 
               as.character(first(host_is_superhost[month(Date) == 8 & year(Date) == 2024])), 
               NA_character_),
      
      
      # 나머지는 NA로 설정
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

combined_data = raw_data%>%left_join(ex%>%select(Date,host_id,ex_super), by=c('Date','host_id'))

combined_data$price=as.numeric(gsub("\\$|,", "", combined_data$price))
combined_data$host_is_superhost2 <- ifelse(combined_data$host_is_superhost =='t', TRUE, FALSE)

save(combined_data, file = 'RData/2. combined_data.RData')

