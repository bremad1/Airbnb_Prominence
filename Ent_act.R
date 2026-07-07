# Active Statistics

library(dplyr)
library(lubridate)
library(purrr)
library(openxlsx)
library(writexl)
library(tidyr)
library(ggplot2)
library(rdrobust)
setwd('C:/Users/sim/Desktop/iCloudDrive/iCloudDrive/4-1/Airbnb/Test')
load('Entire2.RData')
# 2. 23 3Q ------------------------------------------------------------
pct =0.05
z <- data.frame()  # 결과를 저장할 빈 데이터 프레임
avglist = list()
exlist= list()
difflist = list()
results1 <- list()
results2 <- list()
#no_dup : id 하나에 여러개의 host_id 매칭 방지, obs 448666
no_dup <- Entire2 %>%
  group_by(id) %>%
  filter(n_distinct(host_id) == 1) %>%  # id가 여러 host_id와 매칭되지 않도록
  ungroup()

#valid_listings: remove incomplete observation in the past quarter, 25,656 id left
valid_listings <- no_dup %>%
  filter(year(Date) == 2023 & month(Date) %in% c(5, 6) & !is.na(price) & host_is_superhost!='' & !is.na(host_is_superhost)) %>%
  group_by(host_id) %>%
  filter(n_distinct(host_is_superhost) == 1) %>%
  distinct(id)%>%
  pull(id) # 해당 host_id만 추출

#monthly2: 4~9월 모두 관찰되었고 5,6월달에 슈퍼호스트 상태 안 바뀌었으며 (에러 x) 8,9월에도 슈퍼호스트 상태 안 바뀜, obs 33,060
temp_data<- no_dup %>%filter( (year(Date) ==2023 & month(Date) %in% c(4,5,6,7,8,9))& host_is_superhost!='' & !is.na(host_is_superhost)
                              & !is.na(price)) %>%group_by(id)%>%filter(all(c(4,5,6,7, 8, 9) %in% month(Date)))%>% 
  filter(year(Date) == 2023 & month(Date) %in% c(8, 9) & !is.na(price) & id %in% valid_listings) %>%
  group_by(host_id) %>%
  filter(n_distinct(host_is_superhost) == 1) %>%
  ungroup()

# id를 분기별로 하나만 남기기. avg_price 계산. obs 16,530
temp_data <- temp_data %>% group_by(id) %>%
  mutate(
    avg_price = ifelse(
      sum(!is.na(price)) > 1,
      mean(price, na.rm = TRUE),
      price[!is.na(price)])
  ) %>%
  slice(1) %>% # 그룹당 유일한 값 선택
  ungroup()
temp_data$ex_avg <- ifelse(
  month(temp_data$Date) %in% c(2, 5, 8, 11),
  rowMeans(cbind(temp_data$ex_price2, temp_data$ex_price3), na.rm = TRUE),
  ifelse(
    month(temp_data$Date) %in% c(1, 4, 7, 10),
    rowMeans(cbind(temp_data$ex_price1, temp_data$ex_price2), na.rm = TRUE),
    rowMeans(cbind(temp_data$ex_price3, temp_data$ex_price4), na.rm = TRUE)
  )
)

#obs 14,875
temp_data <- temp_data %>%
  filter(
    ((avg_price - ex_avg) / ex_avg) > quantile((avg_price - ex_avg) / ex_avg, probs = pct, na.rm = TRUE) &
      ((avg_price - ex_avg) / ex_avg) < quantile((avg_price - ex_avg) / ex_avg, probs = 1 - pct, na.rm = TRUE)) 

#obs 7,123
temp_data = temp_data%>%filter(
  first_month_ltm>=10)
temp_data <- temp_data %>%# group_by(ex_super)%>%
  mutate(
    ex_q1 = ifelse(ex_avg < quantile(ex_avg, probs = 0.25), 1, 0),
    ex_q2 = ifelse(ex_avg < quantile(ex_avg, probs = 0.5) & ex_avg >= quantile(ex_avg, probs = 0.25), 1, 0),
    ex_q3 = ifelse(ex_avg < quantile(ex_avg, probs = 0.75) & ex_avg >= quantile(ex_avg, probs = 0.5), 1, 0),
    ex_q4 = ifelse(ex_avg >= quantile(ex_avg, probs = 0.75), 1, 0)
  )
temp_data$price_diff = log(temp_data$avg_price)-log(temp_data$ex_avg)
Q323=temp_data
Q323_superhost_ratio_by_quartile <- temp_data %>%
  mutate(ex_q = case_when(
    ex_q1 == 1 ~ "ex_q1",
    ex_q2 == 1 ~ "ex_q2",
    ex_q3 == 1 ~ "ex_q3",
    ex_q4 == 1 ~ "ex_q4"
  )) %>%
  group_by(ex_q, host_is_superhost) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = host_is_superhost, values_from = count, values_fill = 0) %>%
  rename(Non_superhost = "f", Superhost = "t") %>%
  mutate(obs = Superhost + Non_superhost,
         Superhost_ratio = Superhost / (Superhost + Non_superhost))


Q323_summary_by_quartile=temp_data%>%
  mutate(ex_q = case_when(
    ex_q1 == 1 ~ "ex_q1",
    ex_q2 == 1 ~ "ex_q2",
    ex_q3 == 1 ~ "ex_q3",
    ex_q4 == 1 ~ "ex_q4"
  )) %>%
  group_by(ex_q) %>%
  summarise(mean_ex_avg = mean(ex_avg, na.rm = TRUE), 
            min_ex_avg = min(ex_avg, na.rm = TRUE),
            max_ex_avg = max(ex_avg, na.rm = TRUE),
            sd_ex_avg = sd(ex_avg, na.rm =T),
            mean_price = mean(avg_price, na.rm = TRUE), 
            min_price = min(avg_price, na.rm = TRUE),
            max_price = max(avg_price, na.rm = TRUE),
            sd_price = sd(avg_price, na.rm =T),
            mean_price_diff = mean(price_diff, na.rm = TRUE), 
            min_price_diff = min(price_diff, na.rm = TRUE),
            max_price_diff = max(price_diff, na.rm = TRUE),
            sd_price_diff = sd(price_diff, na.rm =T),
            .groups = "drop") %>%
  rename(mean__ex = mean_ex_avg, min__ex = min_ex_avg, max__ex = max_ex_avg, sd__ex = sd_ex_avg,
         mean__price = mean_price, min__price = min_price, max__price = max_price, sd__price = sd_price,
         mean__price_diff = mean_price_diff, min__price_diff = min_price_diff, max__price_diff = max_price_diff, sd__price_diff=sd_price_diff,
  ) %>%
  pivot_longer(cols = mean__ex:sd__price_diff, 
               names_to = c(".value", "stat"),
               names_sep = "__")



# 3. 23 4Q ------------------------------------------------------------
#no_dup : id 하나에 여러개의 host_id 매칭 방지, obs 448666
no_dup <- Entire2 %>%
  group_by(id) %>%
  filter(n_distinct(host_id) == 1) %>%  # id가 여러 host_id와 매칭되지 않도록
  ungroup()

#valid_listings: remove incomplete observation in the past quarter, 25,656 id left
valid_listings <- no_dup %>%
  filter(year(Date) == 2023 & month(Date) %in% c(8, 9) & !is.na(price) & host_is_superhost!='' & !is.na(host_is_superhost)) %>%
  group_by(host_id) %>%
  filter(n_distinct(host_is_superhost) == 1) %>%
  distinct(id)%>%
  pull(id) # 해당 host_id만 추출

#monthly2: 4~9월 모두 관찰되었고 5,6월달에 슈퍼호스트 상태 안 바뀌었으며 (에러 x) 8,9월에도 슈퍼호스트 상태 안 바뀜, obs 33,060
temp_data<- no_dup %>%filter( (year(Date) ==2023 & month(Date) %in% c(10,11,12,7,8,9))& host_is_superhost!='' & !is.na(host_is_superhost)
                              & !is.na(price)) %>%group_by(id)%>%filter(all(c(10,11,12,7, 8, 9) %in% month(Date)))%>% 
  filter(year(Date) == 2023 & month(Date) %in% c(11, 12) & !is.na(price) & id %in% valid_listings) %>%
  group_by(host_id) %>%
  filter(n_distinct(host_is_superhost) == 1) %>%
  ungroup()

# id를 분기별로 하나만 남기기. avg_price 계산. obs 16,530
temp_data <- temp_data %>% group_by(id) %>%
  mutate(
    avg_price = ifelse(
      sum(!is.na(price)) > 1,
      mean(price, na.rm = TRUE),
      price[!is.na(price)])
  ) %>%
  slice(1) %>% # 그룹당 유일한 값 선택
  ungroup()
temp_data$ex_avg <- ifelse(
  month(temp_data$Date) %in% c(2, 5, 8, 11),
  rowMeans(cbind(temp_data$ex_price2, temp_data$ex_price3), na.rm = TRUE),
  ifelse(
    month(temp_data$Date) %in% c(1, 4, 7, 10),
    rowMeans(cbind(temp_data$ex_price1, temp_data$ex_price2), na.rm = TRUE),
    rowMeans(cbind(temp_data$ex_price3, temp_data$ex_price4), na.rm = TRUE)
  )
)

#obs 14,875
temp_data <- temp_data %>%
  filter(
    ((avg_price - ex_avg) / ex_avg) > quantile((avg_price - ex_avg) / ex_avg, probs = pct, na.rm = TRUE) &
      ((avg_price - ex_avg) / ex_avg) < quantile((avg_price - ex_avg) / ex_avg, probs = 1 - pct, na.rm = TRUE)) 

#obs 7,123
temp_data = temp_data%>%filter(
  first_month_ltm>=10)
temp_data <- temp_data %>%# group_by(ex_super)%>%
  mutate(
    ex_q1 = ifelse(ex_avg < quantile(ex_avg, probs = 0.25), 1, 0),
    ex_q2 = ifelse(ex_avg < quantile(ex_avg, probs = 0.5) & ex_avg >= quantile(ex_avg, probs = 0.25), 1, 0),
    ex_q3 = ifelse(ex_avg < quantile(ex_avg, probs = 0.75) & ex_avg >= quantile(ex_avg, probs = 0.5), 1, 0),
    ex_q4 = ifelse(ex_avg >= quantile(ex_avg, probs = 0.75), 1, 0)
  )
temp_data$price_diff = log(temp_data$avg_price)-log(temp_data$ex_avg)
Q423=temp_data
Q423_superhost_ratio_by_quartile <- temp_data %>%
  mutate(ex_q = case_when(
    ex_q1 == 1 ~ "ex_q1",
    ex_q2 == 1 ~ "ex_q2",
    ex_q3 == 1 ~ "ex_q3",
    ex_q4 == 1 ~ "ex_q4"
  )) %>%
  group_by(ex_q, host_is_superhost) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = host_is_superhost, values_from = count, values_fill = 0) %>%
  rename(Non_superhost = "f", Superhost = "t") %>%
  mutate(obs = Superhost + Non_superhost,
         Superhost_ratio = Superhost / (Superhost + Non_superhost))


Q423_summary_by_quartile=temp_data%>%
  mutate(ex_q = case_when(
    ex_q1 == 1 ~ "ex_q1",
    ex_q2 == 1 ~ "ex_q2",
    ex_q3 == 1 ~ "ex_q3",
    ex_q4 == 1 ~ "ex_q4"
  )) %>%
  group_by(ex_q) %>%
  summarise(mean_ex_avg = mean(ex_avg, na.rm = TRUE), 
            min_ex_avg = min(ex_avg, na.rm = TRUE),
            max_ex_avg = max(ex_avg, na.rm = TRUE),
            sd_ex_avg = sd(ex_avg, na.rm =T),
            mean_price = mean(avg_price, na.rm = TRUE), 
            min_price = min(avg_price, na.rm = TRUE),
            max_price = max(avg_price, na.rm = TRUE),
            sd_price = sd(avg_price, na.rm =T),
            mean_price_diff = mean(price_diff, na.rm = TRUE), 
            min_price_diff = min(price_diff, na.rm = TRUE),
            max_price_diff = max(price_diff, na.rm = TRUE),
            sd_price_diff = sd(price_diff, na.rm =T),
            .groups = "drop") %>%
  rename(mean__ex = mean_ex_avg, min__ex = min_ex_avg, max__ex = max_ex_avg, sd__ex = sd_ex_avg,
         mean__price = mean_price, min__price = min_price, max__price = max_price, sd__price = sd_price,
         mean__price_diff = mean_price_diff, min__price_diff = min_price_diff, max__price_diff = max_price_diff, sd__price_diff=sd_price_diff,
  ) %>%
  pivot_longer(cols = mean__ex:sd__price_diff, 
               names_to = c(".value", "stat"),
               names_sep = "__")




# 4. 24 1Q ------------------------------------------------------------
#no_dup : id 하나에 여러개의 host_id 매칭 방지, obs 448666
no_dup <- Entire2 %>%
  group_by(id) %>%
  filter(n_distinct(host_id) == 1) %>%  # id가 여러 host_id와 매칭되지 않도록
  ungroup()

#valid_listings: remove incomplete observation in the past quarter,
valid_listings <- no_dup %>%
  filter(year(Date) == 2023 & month(Date) %in% c(11, 12) & !is.na(price) & host_is_superhost!='' & !is.na(host_is_superhost)) %>%
  group_by(host_id) %>%
  filter(n_distinct(host_is_superhost) == 1) %>%
  distinct(id)%>%
  pull(id) # 해당 host_id만 추출

temp_data<- no_dup %>%filter( (year(Date) ==2023 & month(Date) %in% c(10,11,12)) |
                                (year(Date) ==2024 & month(Date) %in% c(1,2,3))
                              & host_is_superhost!='' & !is.na(host_is_superhost)
                              & !is.na(price)) %>%group_by(id)%>%filter(all(c(1,2,3,10,11,12) %in% month(Date)))%>% 
  filter(year(Date) == 2024 & month(Date) %in% c(2, 3) & !is.na(price) & id %in% valid_listings) %>%
  group_by(host_id) %>%
  filter(n_distinct(host_is_superhost) == 1) %>%
  ungroup()

# id를 분기별로 하나만 남기기. avg_price 계산. obs 16,530
temp_data <- temp_data %>% group_by(id) %>%
  mutate(
    avg_price = ifelse(
      sum(!is.na(price)) > 1,
      mean(price, na.rm = TRUE),
      price[!is.na(price)])
  ) %>%
  slice(1) %>% # 그룹당 유일한 값 선택
  ungroup()
temp_data$ex_avg <- ifelse(
  month(temp_data$Date) %in% c(2, 5, 8, 11),
  rowMeans(cbind(temp_data$ex_price2, temp_data$ex_price3), na.rm = TRUE),
  ifelse(
    month(temp_data$Date) %in% c(1, 4, 7, 10),
    rowMeans(cbind(temp_data$ex_price1, temp_data$ex_price2), na.rm = TRUE),
    rowMeans(cbind(temp_data$ex_price3, temp_data$ex_price4), na.rm = TRUE)
  )
)

#obs 14,875
temp_data <- temp_data %>%
  filter(
    ((avg_price - ex_avg) / ex_avg) > quantile((avg_price - ex_avg) / ex_avg, probs = pct, na.rm = TRUE) &
      ((avg_price - ex_avg) / ex_avg) < quantile((avg_price - ex_avg) / ex_avg, probs = 1 - pct, na.rm = TRUE)) 

#obs 7,123
temp_data = temp_data%>%filter(
  first_month_ltm>=10)
temp_data <- temp_data %>%# group_by(ex_super)%>%
  mutate(
    ex_q1 = ifelse(ex_avg < quantile(ex_avg, probs = 0.25), 1, 0),
    ex_q2 = ifelse(ex_avg < quantile(ex_avg, probs = 0.5) & ex_avg >= quantile(ex_avg, probs = 0.25), 1, 0),
    ex_q3 = ifelse(ex_avg < quantile(ex_avg, probs = 0.75) & ex_avg >= quantile(ex_avg, probs = 0.5), 1, 0),
    ex_q4 = ifelse(ex_avg >= quantile(ex_avg, probs = 0.75), 1, 0)
  )
temp_data$price_diff = log(temp_data$avg_price)-log(temp_data$ex_avg)
Q124=temp_data
Q124_superhost_ratio_by_quartile <- temp_data %>%
  mutate(ex_q = case_when(
    ex_q1 == 1 ~ "ex_q1",
    ex_q2 == 1 ~ "ex_q2",
    ex_q3 == 1 ~ "ex_q3",
    ex_q4 == 1 ~ "ex_q4"
  )) %>%
  group_by(ex_q, host_is_superhost) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = host_is_superhost, values_from = count, values_fill = 0) %>%
  rename(Non_superhost = "f", Superhost = "t") %>%
  mutate(obs = Superhost + Non_superhost,
         Superhost_ratio = Superhost / (Superhost + Non_superhost))


Q124_summary_by_quartile=temp_data%>%
  mutate(ex_q = case_when(
    ex_q1 == 1 ~ "ex_q1",
    ex_q2 == 1 ~ "ex_q2",
    ex_q3 == 1 ~ "ex_q3",
    ex_q4 == 1 ~ "ex_q4"
  )) %>%
  group_by(ex_q) %>%
  summarise(mean_ex_avg = mean(ex_avg, na.rm = TRUE), 
            min_ex_avg = min(ex_avg, na.rm = TRUE),
            max_ex_avg = max(ex_avg, na.rm = TRUE),
            sd_ex_avg = sd(ex_avg, na.rm =T),
            mean_price = mean(avg_price, na.rm = TRUE), 
            min_price = min(avg_price, na.rm = TRUE),
            max_price = max(avg_price, na.rm = TRUE),
            sd_price = sd(avg_price, na.rm =T),
            mean_price_diff = mean(price_diff, na.rm = TRUE), 
            min_price_diff = min(price_diff, na.rm = TRUE),
            max_price_diff = max(price_diff, na.rm = TRUE),
            sd_price_diff = sd(price_diff, na.rm =T),
            .groups = "drop") %>%
  rename(mean__ex = mean_ex_avg, min__ex = min_ex_avg, max__ex = max_ex_avg, sd__ex = sd_ex_avg,
         mean__price = mean_price, min__price = min_price, max__price = max_price, sd__price = sd_price,
         mean__price_diff = mean_price_diff, min__price_diff = min_price_diff, max__price_diff = max_price_diff, sd__price_diff=sd_price_diff,
  ) %>%
  pivot_longer(cols = mean__ex:sd__price_diff, 
               names_to = c(".value", "stat"),
               names_sep = "__")




# 5. 24 2Q ------------------------------------------------------------
#no_dup : id 하나에 여러개의 host_id 매칭 방지, obs 448666
no_dup <- Entire2 %>%
  group_by(id) %>%
  filter(n_distinct(host_id) == 1) %>%  # id가 여러 host_id와 매칭되지 않도록
  ungroup()

#valid_listings: remove incomplete observation in the past quarter,
valid_listings <- no_dup %>%
  filter(year(Date) == 2024 & month(Date) %in% c(2, 3) & !is.na(price) & host_is_superhost!='' & !is.na(host_is_superhost)) %>%
  group_by(host_id) %>%
  filter(n_distinct(host_is_superhost) == 1) %>%
  distinct(id)%>%
  pull(id) # 해당 host_id만 추출

temp_data<- no_dup %>%filter( (year(Date) ==2024 & month(Date) %in% c(1,2,3,4,5,6))
                              & host_is_superhost!='' & !is.na(host_is_superhost)
                              & !is.na(price)) %>%group_by(id)%>%filter(all(c(1,2,3,4,5,6) %in% month(Date)))%>% 
  filter(year(Date) == 2024 & month(Date) %in% c(5, 6) & !is.na(price) & id %in% valid_listings) %>%
  group_by(host_id) %>%
  filter(n_distinct(host_is_superhost) == 1) %>%
  ungroup()

# id를 분기별로 하나만 남기기. avg_price 계산. obs 16,530
temp_data <- temp_data %>% group_by(id) %>%
  mutate(
    avg_price = ifelse(
      sum(!is.na(price)) > 1,
      mean(price, na.rm = TRUE),
      price[!is.na(price)])
  ) %>%
  slice(1) %>% # 그룹당 유일한 값 선택
  ungroup()
temp_data$ex_avg <- ifelse(
  month(temp_data$Date) %in% c(2, 5, 8, 11),
  rowMeans(cbind(temp_data$ex_price2, temp_data$ex_price3), na.rm = TRUE),
  ifelse(
    month(temp_data$Date) %in% c(1, 4, 7, 10),
    rowMeans(cbind(temp_data$ex_price1, temp_data$ex_price2), na.rm = TRUE),
    rowMeans(cbind(temp_data$ex_price3, temp_data$ex_price4), na.rm = TRUE)
  )
)

#obs 14,875
temp_data <- temp_data %>%
  filter(
    ((avg_price - ex_avg) / ex_avg) > quantile((avg_price - ex_avg) / ex_avg, probs = pct, na.rm = TRUE) &
      ((avg_price - ex_avg) / ex_avg) < quantile((avg_price - ex_avg) / ex_avg, probs = 1 - pct, na.rm = TRUE)) 

#obs 7,123
temp_data = temp_data%>%filter(
  first_month_ltm>=10)
temp_data <- temp_data %>%# group_by(ex_super)%>%
  mutate(
    ex_q1 = ifelse(ex_avg < quantile(ex_avg, probs = 0.25), 1, 0),
    ex_q2 = ifelse(ex_avg < quantile(ex_avg, probs = 0.5) & ex_avg >= quantile(ex_avg, probs = 0.25), 1, 0),
    ex_q3 = ifelse(ex_avg < quantile(ex_avg, probs = 0.75) & ex_avg >= quantile(ex_avg, probs = 0.5), 1, 0),
    ex_q4 = ifelse(ex_avg >= quantile(ex_avg, probs = 0.75), 1, 0)
  )
temp_data$price_diff = log(temp_data$avg_price)-log(temp_data$ex_avg)
Q224=temp_data
Q224_superhost_ratio_by_quartile <- temp_data %>%
  mutate(ex_q = case_when(
    ex_q1 == 1 ~ "ex_q1",
    ex_q2 == 1 ~ "ex_q2",
    ex_q3 == 1 ~ "ex_q3",
    ex_q4 == 1 ~ "ex_q4"
  )) %>%
  group_by(ex_q, host_is_superhost) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = host_is_superhost, values_from = count, values_fill = 0) %>%
  rename(Non_superhost = "f", Superhost = "t") %>%
  mutate(obs = Superhost + Non_superhost,
         Superhost_ratio = Superhost / (Superhost + Non_superhost))


Q224_summary_by_quartile=temp_data%>%
  mutate(ex_q = case_when(
    ex_q1 == 1 ~ "ex_q1",
    ex_q2 == 1 ~ "ex_q2",
    ex_q3 == 1 ~ "ex_q3",
    ex_q4 == 1 ~ "ex_q4"
  )) %>%
  group_by(ex_q) %>%
  summarise(mean_ex_avg = mean(ex_avg, na.rm = TRUE), 
            min_ex_avg = min(ex_avg, na.rm = TRUE),
            max_ex_avg = max(ex_avg, na.rm = TRUE),
            sd_ex_avg = sd(ex_avg, na.rm =T),
            mean_price = mean(avg_price, na.rm = TRUE), 
            min_price = min(avg_price, na.rm = TRUE),
            max_price = max(avg_price, na.rm = TRUE),
            sd_price = sd(avg_price, na.rm =T),
            mean_price_diff = mean(price_diff, na.rm = TRUE), 
            min_price_diff = min(price_diff, na.rm = TRUE),
            max_price_diff = max(price_diff, na.rm = TRUE),
            sd_price_diff = sd(price_diff, na.rm =T),
            .groups = "drop") %>%
  rename(mean__ex = mean_ex_avg, min__ex = min_ex_avg, max__ex = max_ex_avg, sd__ex = sd_ex_avg,
         mean__price = mean_price, min__price = min_price, max__price = max_price, sd__price = sd_price,
         mean__price_diff = mean_price_diff, min__price_diff = min_price_diff, max__price_diff = max_price_diff, sd__price_diff=sd_price_diff,
  ) %>%
  pivot_longer(cols = mean__ex:sd__price_diff, 
               names_to = c(".value", "stat"),
               names_sep = "__")




# 6. 24 3Q ------------------------------------------------------------
#no_dup : id 하나에 여러개의 host_id 매칭 방지, obs 448666
no_dup <- Entire2 %>%
  group_by(id) %>%
  filter(n_distinct(host_id) == 1) %>%  # id가 여러 host_id와 매칭되지 않도록
  ungroup()

#valid_listings: remove incomplete observation in the past quarter,
valid_listings <- no_dup %>%
  filter(year(Date) == 2024 & month(Date) %in% c(5, 6) & !is.na(price) & host_is_superhost!='' & !is.na(host_is_superhost)) %>%
  group_by(host_id) %>%
  filter(n_distinct(host_is_superhost) == 1) %>%
  distinct(id)%>%
  pull(id) # 해당 host_id만 추출

temp_data<- no_dup %>%filter( (year(Date) ==2024 & month(Date) %in% c(7,8,9,4,5,6))
                              & host_is_superhost!='' & !is.na(host_is_superhost)
                              & !is.na(price)) %>%group_by(id)%>%filter(all(c(7,8,9,4,5,6) %in% month(Date)))%>% 
  filter(year(Date) == 2024 & month(Date) %in% c(8, 9) & !is.na(price) & id %in% valid_listings) %>%
  group_by(host_id) %>%
  filter(n_distinct(host_is_superhost) == 1) %>%
  ungroup()

# id를 분기별로 하나만 남기기. avg_price 계산. obs 16,530
temp_data <- temp_data %>% group_by(id) %>%
  mutate(
    avg_price = ifelse(
      sum(!is.na(price)) > 1,
      mean(price, na.rm = TRUE),
      price[!is.na(price)])
  ) %>%
  slice(1) %>% # 그룹당 유일한 값 선택
  ungroup()
temp_data$ex_avg <- ifelse(
  month(temp_data$Date) %in% c(2, 5, 8, 11),
  rowMeans(cbind(temp_data$ex_price2, temp_data$ex_price3), na.rm = TRUE),
  ifelse(
    month(temp_data$Date) %in% c(1, 4, 7, 10),
    rowMeans(cbind(temp_data$ex_price1, temp_data$ex_price2), na.rm = TRUE),
    rowMeans(cbind(temp_data$ex_price3, temp_data$ex_price4), na.rm = TRUE)
  )
)

#obs 14,875
temp_data <- temp_data %>%
  filter(
    ((avg_price - ex_avg) / ex_avg) > quantile((avg_price - ex_avg) / ex_avg, probs = pct, na.rm = TRUE) &
      ((avg_price - ex_avg) / ex_avg) < quantile((avg_price - ex_avg) / ex_avg, probs = 1 - pct, na.rm = TRUE)) 

#obs 7,123
temp_data = temp_data%>%filter(
  first_month_ltm>=10)
temp_data <- temp_data %>%# group_by(ex_super)%>%
  mutate(
    ex_q1 = ifelse(ex_avg < quantile(ex_avg, probs = 0.25), 1, 0),
    ex_q2 = ifelse(ex_avg < quantile(ex_avg, probs = 0.5) & ex_avg >= quantile(ex_avg, probs = 0.25), 1, 0),
    ex_q3 = ifelse(ex_avg < quantile(ex_avg, probs = 0.75) & ex_avg >= quantile(ex_avg, probs = 0.5), 1, 0),
    ex_q4 = ifelse(ex_avg >= quantile(ex_avg, probs = 0.75), 1, 0)
  )
temp_data$price_diff = log(temp_data$avg_price)-log(temp_data$ex_avg)
Q324=temp_data
Q324_superhost_ratio_by_quartile <- temp_data %>%
  mutate(ex_q = case_when(
    ex_q1 == 1 ~ "ex_q1",
    ex_q2 == 1 ~ "ex_q2",
    ex_q3 == 1 ~ "ex_q3",
    ex_q4 == 1 ~ "ex_q4"
  )) %>%
  group_by(ex_q, host_is_superhost) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = host_is_superhost, values_from = count, values_fill = 0) %>%
  rename(Non_superhost = "f", Superhost = "t") %>%
  mutate(obs = Superhost + Non_superhost,
         Superhost_ratio = Superhost / (Superhost + Non_superhost))


Q324_summary_by_quartile=temp_data%>%
  mutate(ex_q = case_when(
    ex_q1 == 1 ~ "ex_q1",
    ex_q2 == 1 ~ "ex_q2",
    ex_q3 == 1 ~ "ex_q3",
    ex_q4 == 1 ~ "ex_q4"
  )) %>%
  group_by(ex_q) %>%
  summarise(mean_ex_avg = mean(ex_avg, na.rm = TRUE), 
            min_ex_avg = min(ex_avg, na.rm = TRUE),
            max_ex_avg = max(ex_avg, na.rm = TRUE),
            sd_ex_avg = sd(ex_avg, na.rm =T),
            mean_price = mean(avg_price, na.rm = TRUE), 
            min_price = min(avg_price, na.rm = TRUE),
            max_price = max(avg_price, na.rm = TRUE),
            sd_price = sd(avg_price, na.rm =T),
            mean_price_diff = mean(price_diff, na.rm = TRUE), 
            min_price_diff = min(price_diff, na.rm = TRUE),
            max_price_diff = max(price_diff, na.rm = TRUE),
            sd_price_diff = sd(price_diff, na.rm =T),
            .groups = "drop") %>%
  rename(mean__ex = mean_ex_avg, min__ex = min_ex_avg, max__ex = max_ex_avg, sd__ex = sd_ex_avg,
         mean__price = mean_price, min__price = min_price, max__price = max_price, sd__price = sd_price,
         mean__price_diff = mean_price_diff, min__price_diff = min_price_diff, max__price_diff = max_price_diff, sd__price_diff=sd_price_diff,
  ) %>%
  pivot_longer(cols = mean__ex:sd__price_diff, 
               names_to = c(".value", "stat"),
               names_sep = "__")




# 7. 24 4Q ------------------------------------------------------------
#no_dup : id 하나에 여러개의 host_id 매칭 방지, obs 448666
no_dup <- Entire2 %>%
  group_by(id) %>%
  filter(n_distinct(host_id) == 1) %>%  # id가 여러 host_id와 매칭되지 않도록
  ungroup()

#valid_listings: remove incomplete observation in the past quarter,
valid_listings <- no_dup %>%
  filter(year(Date) == 2024 & month(Date) %in% c(8, 9) & !is.na(price) & host_is_superhost!='' & !is.na(host_is_superhost)) %>%
  group_by(host_id) %>%
  filter(n_distinct(host_is_superhost) == 1) %>%
  distinct(id)%>%
  pull(id) # 해당 host_id만 추출

temp_data<- no_dup %>%filter( (year(Date) ==2024 & month(Date) %in% c(7,8,9,10,11,12))
                              & host_is_superhost!='' & !is.na(host_is_superhost)
                              & !is.na(price)) %>%group_by(id)%>%filter(all(c(7,8,9,10,11,12) %in% month(Date)))%>% 
  filter(year(Date) == 2024 & month(Date) %in% c(11, 12) & !is.na(price) & id %in% valid_listings) %>%
  group_by(host_id) %>%
  filter(n_distinct(host_is_superhost) == 1) %>%
  ungroup()

# id를 분기별로 하나만 남기기. avg_price 계산. obs 16,530
temp_data <- temp_data %>% group_by(id) %>%
  mutate(
    avg_price = ifelse(
      sum(!is.na(price)) > 1,
      mean(price, na.rm = TRUE),
      price[!is.na(price)])
  ) %>%
  slice(1) %>% # 그룹당 유일한 값 선택
  ungroup()
temp_data$ex_avg <- ifelse(
  month(temp_data$Date) %in% c(2, 5, 8, 11),
  rowMeans(cbind(temp_data$ex_price2, temp_data$ex_price3), na.rm = TRUE),
  ifelse(
    month(temp_data$Date) %in% c(1, 4, 7, 10),
    rowMeans(cbind(temp_data$ex_price1, temp_data$ex_price2), na.rm = TRUE),
    rowMeans(cbind(temp_data$ex_price3, temp_data$ex_price4), na.rm = TRUE)
  )
)

#obs 14,875
temp_data <- temp_data %>%
  filter(
    ((avg_price - ex_avg) / ex_avg) > quantile((avg_price - ex_avg) / ex_avg, probs = pct, na.rm = TRUE) &
      ((avg_price - ex_avg) / ex_avg) < quantile((avg_price - ex_avg) / ex_avg, probs = 1 - pct, na.rm = TRUE)) 

#obs 7,123
temp_data = temp_data%>%filter(
  first_month_ltm>=10)
temp_data <- temp_data %>%# group_by(ex_super)%>%
  mutate(
    ex_q1 = ifelse(ex_avg < quantile(ex_avg, probs = 0.25), 1, 0),
    ex_q2 = ifelse(ex_avg < quantile(ex_avg, probs = 0.5) & ex_avg >= quantile(ex_avg, probs = 0.25), 1, 0),
    ex_q3 = ifelse(ex_avg < quantile(ex_avg, probs = 0.75) & ex_avg >= quantile(ex_avg, probs = 0.5), 1, 0),
    ex_q4 = ifelse(ex_avg >= quantile(ex_avg, probs = 0.75), 1, 0)
  )
temp_data$price_diff = log(temp_data$avg_price)-log(temp_data$ex_avg)
Q424=temp_data
Q424_superhost_ratio_by_quartile <- temp_data %>%
  mutate(ex_q = case_when(
    ex_q1 == 1 ~ "ex_q1",
    ex_q2 == 1 ~ "ex_q2",
    ex_q3 == 1 ~ "ex_q3",
    ex_q4 == 1 ~ "ex_q4"
  )) %>%
  group_by(ex_q, host_is_superhost) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = host_is_superhost, values_from = count, values_fill = 0) %>%
  rename(Non_superhost = "f", Superhost = "t") %>%
  mutate(obs = Superhost + Non_superhost,
         Superhost_ratio = Superhost / (Superhost + Non_superhost))


Q424_summary_by_quartile=temp_data%>%
  mutate(ex_q = case_when(
    ex_q1 == 1 ~ "ex_q1",
    ex_q2 == 1 ~ "ex_q2",
    ex_q3 == 1 ~ "ex_q3",
    ex_q4 == 1 ~ "ex_q4"
  )) %>%
  group_by(ex_q) %>%
  summarise(mean_ex_avg = mean(ex_avg, na.rm = TRUE), 
            min_ex_avg = min(ex_avg, na.rm = TRUE),
            max_ex_avg = max(ex_avg, na.rm = TRUE),
            sd_ex_avg = sd(ex_avg, na.rm =T),
            mean_price = mean(avg_price, na.rm = TRUE), 
            min_price = min(avg_price, na.rm = TRUE),
            max_price = max(avg_price, na.rm = TRUE),
            sd_price = sd(avg_price, na.rm =T),
            mean_price_diff = mean(price_diff, na.rm = TRUE), 
            min_price_diff = min(price_diff, na.rm = TRUE),
            max_price_diff = max(price_diff, na.rm = TRUE),
            sd_price_diff = sd(price_diff, na.rm =T),
            .groups = "drop") %>%
  rename(mean__ex = mean_ex_avg, min__ex = min_ex_avg, max__ex = max_ex_avg, sd__ex = sd_ex_avg,
         mean__price = mean_price, min__price = min_price, max__price = max_price, sd__price = sd_price,
         mean__price_diff = mean_price_diff, min__price_diff = min_price_diff, max__price_diff = max_price_diff, sd__price_diff=sd_price_diff,
  ) %>%
  pivot_longer(cols = mean__ex:sd__price_diff, 
               names_to = c(".value", "stat"),
               names_sep = "__")

# Aggregating superhost ratio-------------------------------------------------------------

Q323_superhost_ratio_by_quartile <- Q323_superhost_ratio_by_quartile %>%
  summarise(ex_q = "Total", Non_superhost = sum(Non_superhost), Superhost = sum(Superhost), obs = sum(obs), Superhost_ratio = Superhost/obs) %>%
  bind_rows(Q323_superhost_ratio_by_quartile)
Q423_superhost_ratio_by_quartile <- Q423_superhost_ratio_by_quartile %>%
  summarise(ex_q = "Total", Non_superhost = sum(Non_superhost), Superhost = sum(Superhost), obs = sum(obs), Superhost_ratio = Superhost/obs) %>%
  bind_rows(Q423_superhost_ratio_by_quartile)
Q124_superhost_ratio_by_quartile <- Q124_superhost_ratio_by_quartile %>%
  summarise(ex_q = "Total", Non_superhost = sum(Non_superhost), Superhost = sum(Superhost), obs = sum(obs), Superhost_ratio = Superhost/obs) %>%
  bind_rows(Q124_superhost_ratio_by_quartile)
Q224_superhost_ratio_by_quartile <- Q224_superhost_ratio_by_quartile %>%
  summarise(ex_q = "Total", Non_superhost = sum(Non_superhost), Superhost = sum(Superhost), obs = sum(obs), Superhost_ratio = Superhost/obs) %>%
  bind_rows(Q224_superhost_ratio_by_quartile)
Q324_superhost_ratio_by_quartile <- Q324_superhost_ratio_by_quartile %>%
  summarise(ex_q = "Total", Non_superhost = sum(Non_superhost), Superhost = sum(Superhost), obs = sum(obs), Superhost_ratio = Superhost/obs) %>%
  bind_rows(Q324_superhost_ratio_by_quartile)
Q424_superhost_ratio_by_quartile <- Q424_superhost_ratio_by_quartile %>%
  summarise(ex_q = "Total", Non_superhost = sum(Non_superhost), Superhost = sum(Superhost), obs = sum(obs), Superhost_ratio = Superhost/obs) %>%
  bind_rows(Q424_superhost_ratio_by_quartile)

total_superhost_ratio = rbind(Q323_superhost_ratio_by_quartile,
                              Q423_superhost_ratio_by_quartile,
                              Q124_superhost_ratio_by_quartile,
                              Q224_superhost_ratio_by_quartile,
                              Q324_superhost_ratio_by_quartile,
                              Q424_superhost_ratio_by_quartile
)
total_superhost_ratio <- total_superhost_ratio %>%
  mutate(quarter = rep(c("Q323", "Q423", "Q124", "Q224","Q324","Q424"), each = 5)) %>%
  select(quarter, everything()) 

Q_sup_ratio <- "\\begin{table}[]\n \\TABLE \n\ {Superhost ratio by quarter \\label{Sup_ratio}} {\\begin{tabular}{lccccc}\n"
Q_sup_ratio <- paste(Q_sup_ratio, "\\hline\n", sep = "")
Q_sup_ratio <- paste(Q_sup_ratio, " Date & Price\\_Quarter & Non\\_Superhost& Superhost &obs &Ratio \\\\ \\hline\n", sep = "")

for (i in 1:30){
  if (i == 1) {
    Q_sup_ratio <- paste(Q_sup_ratio, " \\\\ \n", sep = "")
    Q_sup_ratio <- paste(Q_sup_ratio, " \\textbf{2023 Q3} \\\\ \n", sep = "")
  }
  if (i == 6) {
    Q_sup_ratio <- paste(Q_sup_ratio, " \\\\ \n", sep = "")
    Q_sup_ratio <- paste(Q_sup_ratio, " \\textbf{2023 Q4} \\\\ \n", sep = "")
  }
  if (i == 11) {
    Q_sup_ratio <- paste(Q_sup_ratio, " \\\\ \n", sep = "")
    Q_sup_ratio <- paste(Q_sup_ratio, " \\textbf{2024 Q1} \\\\ \n", sep = "")
  }  
  if (i == 16) {
    Q_sup_ratio <- paste(Q_sup_ratio, " \\\\ \n", sep = "")
    Q_sup_ratio <- paste(Q_sup_ratio, " \\textbf{2024 Q2} \\\\ \n", sep = "")
  }  
  if (i == 21) {
    Q_sup_ratio <- paste(Q_sup_ratio, " \\\\ \n", sep = "")
    Q_sup_ratio <- paste(Q_sup_ratio, " \\textbf{2024 Q3} \\\\ \n", sep = "")
  }  
  if (i == 26) {
    Q_sup_ratio <- paste(Q_sup_ratio, " \\\\ \n", sep = "")
    Q_sup_ratio <- paste(Q_sup_ratio, " \\textbf{2024 Q4} \\\\ \n", sep = "")
  }  
  Date = total_superhost_ratio$quarter[i]
  Price_Quarter = total_superhost_ratio$ex_q[i]
  Non = total_superhost_ratio$Non_superhost[i]
  Sup = total_superhost_ratio$Superhost[i]
  obs = total_superhost_ratio$obs[i]
  Ratio = round(total_superhost_ratio$Superhost_ratio[i]*100,3)
  Q_sup_ratio <- paste(Q_sup_ratio, paste("", "&", Price_Quarter, "&", Non, "&",Sup,"&",obs,"&",Ratio, "\\\\ \n", sep = ""))
  
}

Q_sup_ratio <- paste(Q_sup_ratio, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
writeLines(Q_sup_ratio, "Quarterly superhost ratio_act.tex")
cat(Q_sup_ratio)

# Aggregating price summary statistics ------------------------------------
total_price_summary= rbind(Q323_summary_by_quartile,
                           Q423_summary_by_quartile,
                           Q124_summary_by_quartile,
                           Q224_summary_by_quartile,
                           Q324_summary_by_quartile,
                           Q424_summary_by_quartile
)
total_price_summary <- total_price_summary %>%
  mutate(quarter = rep(c("Q323", "Q423", "Q124", "Q224","Q324","Q424"), each = 12)) %>%
  select(quarter, everything()) 

plot_data <- total_price_summary %>%
  filter(stat == "price_diff")
# ggplot 생성
ggplot(plot_data, aes(x = as.factor(quarter), y = mean, color = ex_q, group = ex_q)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
  labs(x = "Quarter", y = "Mean Price_diff", color = "",
       title = "Mean Price Difference by ex_q over Time") +
  scale_x_discrete(limits = c("Q323", "Q423", "Q124", "Q224", "Q324", "Q424"),
                   labels = c("Q323" = "2023 Q3", "Q423" = "2023 Q4", 
                              "Q124" = "2024 Q1", "Q224" = "2024 Q2", 
                              "Q324" = "2024 Q3", "Q424" = "2024 Q4")) +
  theme_minimal()+theme(panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank())
ggsave("price_diff_plot.eps", plot = last_plot(), device = "eps", width = 8, height = 6, units = "in")

# generate .tex
Q_price_summary1 <- "\\begin{table}[]\n \\TABLE \n\ {Superhost ratio by quarter} {\\begin{tabular}{lcccccc}\n"
Q_price_summary1 <- paste(Q_price_summary1, "\\hline\n", sep = "")
Q_price_summary1 <- paste(Q_price_summary1, " Date &Price_Quartile &Variable & Mean & Min & Max & SD \\\\ \\hline\n", sep = "")
for (i in 1:36){
  if (i == 1) {
    Q_price_summary1 <- paste(Q_price_summary1, " \\\\ \n", sep = "")
    Q_price_summary1 <- paste(Q_price_summary1, " \\textbf{2023 Q3} \\\\ \n", sep = "")
  }
  if (i == 13) {
    Q_price_summary1 <- paste(Q_price_summary1, " \\\\ \n", sep = "")
    Q_price_summary1 <- paste(Q_price_summary1, " \\textbf{2023 Q4} \\\\ \n", sep = "")
  }
  if (i == 25) {
    Q_price_summary1 <- paste(Q_price_summary1, " \\\\ \n", sep = "")
    Q_price_summary1 <- paste(Q_price_summary1, " \\textbf{2024 Q1} \\\\ \n", sep = "")
  }
  #Date = total_superhost_ratio$quarter[i]
  Ex_q = total_price_summary$ex_q[i]
  Variable = total_price_summary$stat[i]
  Mean = round(total_price_summary$mean[i],3)
  Min = round(total_price_summary$min[i],3)
  Max = round(total_price_summary$max[i],3)
  SD = round(total_price_summary$sd[i],3)
  Q_price_summary1 <- paste(Q_price_summary1, paste("","&", Ex_q,"&", Variable, "&", Mean, "&",Min,"&",Max,"&",SD, "\\\\ \n", sep = ""))
  
}

Q_price_summary1 <- paste(Q_price_summary1, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
writeLines(Q_price_summary1, "price summary1_act.tex")
cat(Q_price_summary1)

Q_price_summary2 <- "\\begin{table}[]\n \\TABLE \n\ {Superhost ratio by quarter} {\\begin{tabular}{lcccccc}\n"
Q_price_summary2 <- paste(Q_price_summary2, "\\hline\n", sep = "")
Q_price_summary2 <- paste(Q_price_summary2, " Date &Price\\_Quartile &Variable & Mean & Min & Max & SD \\\\ \\hline\n", sep = "")

for (i in 37:72){
  if (i == 37) {
    Q_price_summary2 <- paste(Q_price_summary2, " \\\\ \n", sep = "")
    Q_price_summary2 <- paste(Q_price_summary2, " \\textbf{2024 Q2} \\\\ \n", sep = "")
  }
  if (i == 49) {
    Q_price_summary2 <- paste(Q_price_summary2, " \\\\ \n", sep = "")
    Q_price_summary2 <- paste(Q_price_summary2, " \\textbf{2024 Q3} \\\\ \n", sep = "")
  }
  if (i == 61) {
    Q_price_summary2 <- paste(Q_price_summary2, " \\\\ \n", sep = "")
    Q_price_summary2 <- paste(Q_price_summary2, " \\textbf{2024 Q4} \\\\ \n", sep = "")
  }
  #Date = total_superhost_ratio$quarter[i]
  Ex_q = total_price_summary$ex_q[i]
  Variable = total_price_summary$stat[i]
  Mean = round(total_price_summary$mean[i],3)
  Min = round(total_price_summary$min[i],3)
  Max = round(total_price_summary$max[i],3)
  SD = round(total_price_summary$sd[i],3)
  Q_price_summary2 <- paste(Q_price_summary2, paste("","&", Ex_q,"&", Variable, "&", Mean, "&",Min,"&",Max,"&",SD, "\\\\ \n", sep = ""))
  
}

Q_price_summary2 <- paste(Q_price_summary2, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
writeLines(Q_price_summary2, "price summary2_act.tex")
cat(Q_price_summary2)
# Change of the superhost -------------------------------------------------
change <- bind_rows(
  Q323 %>% group_by(ex_super, host_is_superhost) %>% count() %>% mutate(quarter = "Q323"),
  Q423 %>% group_by(ex_super, host_is_superhost) %>% count() %>% mutate(quarter = "Q423"),
  Q124 %>% group_by(ex_super, host_is_superhost) %>% count() %>% mutate(quarter = "Q124"),
  Q224 %>% group_by(ex_super, host_is_superhost) %>% count() %>% mutate(quarter = "Q224"),
  Q324 %>% group_by(ex_super, host_is_superhost) %>% count() %>% mutate(quarter = "Q324"),
  Q424 %>% group_by(ex_super, host_is_superhost) %>% count() %>% mutate(quarter = "Q424")
)
change$ex_super = ifelse(change$ex_super=='t',1,0)
change$host_is_superhost = ifelse(change$host_is_superhost=='t',1,0)

quarter_order <- c("Q323", "Q423", "Q124", "Q224", "Q324", "Q424")

change <- change %>%
  group_by(quarter, ex_super) %>%  # 각 quarter와 ex_super별로 그룹화
  summarise(n2 = sum(n), .groups = "drop") %>%  # 각 그룹의 n 값을 합산
  left_join(
    change %>% 
      group_by(quarter, ex_super, host_is_superhost) %>%  # host_is_superhost별로 그룹화
      summarise(obs = sum(n), .groups = "drop"),  # 각 그룹의 n 값을 합산
    by = c("quarter", "ex_super")  # quarter와 ex_super로 join
  ) %>%
  mutate(ratio = obs / n2) %>%  # ratio 계산
  mutate(quarter = factor(quarter, levels = quarter_order)) %>%  # quarter 순서 지정
  arrange(quarter)%>%select(-n2)  # quarter 순서대로 정렬

Sup_change <- "\\begin{table}[]\n \\TABLE \n\ {Superhost Status Change} {\\begin{tabular}{lcccc}\n"
Sup_change <- paste(Sup_change, "\\hline\n", sep = "")
Sup_change <- paste(Sup_change, " Date &Ex\\_super &Superhost & Obs & Ratio \\\\ \\hline\n", sep = "")
for (i in 1:24){
  if (i == 1) {
    Sup_change <- paste(Sup_change, " \\\\ \n", sep = "")
    Sup_change <- paste(Sup_change, " \\textbf{2023 Q3} \\\\ \n", sep = "")
  }
  if (i == 5) {
    Sup_change <- paste(Sup_change, " \\\\ \n", sep = "")
    Sup_change <- paste(Sup_change, " \\textbf{2023 Q4} \\\\ \n", sep = "")
  }
  if (i == 9) {
    Sup_change <- paste(Sup_change, " \\\\ \n", sep = "")
    Sup_change <- paste(Sup_change, " \\textbf{2024 Q1} \\\\ \n", sep = "")
  }
  if (i == 13) {
    Sup_change <- paste(Sup_change, " \\\\ \n", sep = "")
    Sup_change <- paste(Sup_change, " \\textbf{2024 Q2} \\\\ \n", sep = "")
  }
  if (i == 17) {
    Sup_change <- paste(Sup_change, " \\\\ \n", sep = "")
    Sup_change <- paste(Sup_change, " \\textbf{2024 Q3} \\\\ \n", sep = "")
  }
  if (i == 21) {
    Sup_change <- paste(Sup_change, " \\\\ \n", sep = "")
    Sup_change <- paste(Sup_change, " \\textbf{2024 Q4} \\\\ \n", sep = "")
  }
  #Date = total_superhost_ratio$quarter[i]
  Ex_super = change$ex_super[i]
  Superhost = change$host_is_superhost[i]
  obs = change$obs[i]
  Ratio = round(change$ratio[i]*100,3)
  Sup_change <- paste(Sup_change, paste("","&", Ex_super,"&", Superhost, "&", obs, "&",Ratio, "\\\\ \n", sep = ""))
  
}

Sup_change <- paste(Sup_change, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
writeLines(Sup_change, "Sup Change_Act.tex")
cat(Sup_change)


# McCrary Test ------------------------------------------------------------

z <- data.frame()  # 결과를 저장할 빈 데이터 프레임
z=rbind(Q323,Q423,Q124,Q224,Q324,Q424)
z$date3 <- factor(z$Date)
ggplot(z, aes(x = running_scr)) +
  geom_density(color = "#4D4D4D", size = 1.2) +  # 밀도 곡선을 어두운 회색 선으로 그리기
  geom_vline(xintercept = 4.75, color = "black", linetype = "dashed", size = 1) +  # 컷오프 라인 회색 점선
  xlim(4, 5) + theme_classic() +
  labs(x = "Average rating within last 12 months", y = "Density") +
  scale_color_manual(values = c("black", "black")) +
  theme(legend.position = "none") # 기본적인 미니멀한 테마

ggsave("Density.eps", width = 6, height = 4, device = "eps")

library(rddensity)
test_result = rddensity(z$running_scr,c=4.75,massPoints = T)
summary(test_result)
summary(z$running_scr)
plot_density_test <- rdplotdensity(rdd = test_result, type = 'both',
                                   X = z$running_scr#,plotRange = c(4.7,4.85)
)

# Regression --------------------------------------------------------------
avglist = list()
exlist= list()
difflist = list()
results1 <- list()
results2 <- list()
results3 <- list()
results4 <- list()
results5 <- list()
results6 <- list()
conditions <- list(
  "FULL" = (z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1Q2" = (z$ex_q1 == 1 | z$ex_q2 == 1),
  "Q2Q3" = (z$ex_q3 == 1 | z$ex_q2 == 1),
  "Q3Q4" = (z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1" = (z$ex_q1 == 1),
  "Q2" = (z$ex_q2 == 1),
  "Q3" = (z$ex_q3 == 1),
  "Q4" = (z$ex_q4 == 1)
)
for (super_type in c('f', 't')) {
  for (condition_name in names(conditions)) {
    condition <- conditions[[condition_name]]
    
    filtered_data <- z %>%
      filter(ex_super == super_type & condition)
    
    if (nrow(filtered_data) > 0) {
      dummy_vars <- as.data.frame(model.matrix(~ date3 - 1, data = filtered_data))
      margin <- filtered_data$running_scr - 4.75
      result_name <- paste(pct, super_type, condition_name, sep = "_")
      
      tryCatch({

        
        est1 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          covs = cbind(dummy_vars),
          all = TRUE,
          cluster = filtered_data$id,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est2 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          covs = cbind(dummy_vars),
          h = c(2*est1[['bws']][1,1], 2*est1[['bws']][1,2]),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est3 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          covs = cbind(dummy_vars),
          h = c(0.2, 0.1),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est4 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          covs = cbind(dummy_vars),
          h = c(0.3, 0.15),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est5 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          covs = cbind(dummy_vars),
          h = c(0.4, 0.2),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est6 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        avg <- summary(filtered_data$avg_price)
        ex <- summary(filtered_data$ex_avg)
        diff <- summary((filtered_data$avg_price - filtered_data$ex_avg) / filtered_data$ex_avg)
        
        results1[[result_name]] <- est1
        results2[[result_name]] <- est2
        results3[[result_name]] <- est3
        results4[[result_name]] <- est4
        results5[[result_name]] <- est5
        results6[[result_name]] <- est6
        
        avglist[[result_name]] <- avg
        exlist[[result_name]] <- ex
        difflist[[result_name]] <- diff
        
      }, error = function(e) {
        message(sprintf("Error in %s: %s", result_name, e$message))
        # 오류가 나면 해당 반복만 건너뜀
      })
    }
  }
}

# Save regression results as .xlsx ----------------------------------------


results_list <-list()
bandwidths <- c("h")
bandwidths <- c("h")

# 현재 퍼센타일에 대한 결과를 저장할 데이터 프레임 초기화
estimate_matrix_df <- data.frame(
  "Data_Name" = rep(c("FULL", "Q1Q2", "Q2Q3","Q3Q4","Q1","Q2","Q3","Q4"), each = 2),  # "Full", "Q1Q2", "Q3Q4"에 대해 t, f 순서 반복
  "Ex_super" = rep(c("t", "f"), 8),  # Covariate 열 추가
  "N" = numeric(16),  # h/2에 해당하는 pv 열
  stringsAsFactors = FALSE
)
# results_list 초기화
results_list <- list()

for (j in 1:6) {
  list_name <- paste0('results', j)  # "results1"이 됩니다.
  
  # 각 Data_Name에 대해 Estimate 값과 Covariate 값을 채우기
  for (i in 1:nrow(estimate_matrix_df)) {
    data_name <- estimate_matrix_df$Data_Name[i]
    Ex_super <- estimate_matrix_df$Ex_super[i]  # t, f를 구분
    
    for (bw in bandwidths) {
      # bandwidth별로 결과 key 생성
      result_key <- paste('0.05', Ex_super, data_name, sep = "_") # Bandwidth와 type 포함된 key 형성
      
      # get()을 사용하여 해당 리스트를 가져옴
      result_data <- get(list_name)[[result_key]]
      
      # result_key가 존재하면 Estimate 값을 채움
      if (!is.null(result_data)) {
        estimate_matrix_df[i, paste0("coef_", bw, "_conv")] <- result_data[["Estimate"]][1]
        estimate_matrix_df[i, paste0("coef_", bw, "_bc")] <- result_data[["Estimate"]][2]
        estimate_matrix_df[i, paste0("se_", bw, "_conv")] <- result_data[["se"]][1]
        estimate_matrix_df[i, paste0("se_", bw, "_bc")] <- result_data[["se"]][2]
        estimate_matrix_df[i, paste0("pv_", bw, "_conv")] <- result_data[["pv"]][1]
        estimate_matrix_df[i, paste0("pv_", bw, "_bc")] <- result_data[["pv"]][2]
        estimate_matrix_df[i, paste0("beta_Y_p_l_", bw)] <- result_data[["beta_Y_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_Y_p_r_", bw)] <- result_data[["beta_Y_p_r"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_l_", bw)] <- result_data[["beta_T_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_r_", bw)] <- result_data[["beta_T_p_r"]][1]
        
        # N_h, N_b, h, b 값 형성
        estimate_matrix_df$N_h[i] <- paste("[", paste(result_data[["N_h"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$N_b[i] <- paste("[", paste(result_data[["N_b"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$h[i] <- paste("[", paste(round(result_data[["bws"]][1,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$b[i] <- paste("[", paste(round(result_data[["bws"]][2,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$obs_h[i] <- result_data[["N_h"]][1]+result_data[["N_h"]][2]
        estimate_matrix_df$obs_b[i] <- result_data[["N_b"]][1]+result_data[["N_b"]][2]
        
        # N 값 합산
        n_values <- result_data[["N"]]
        estimate_matrix_df$N[i] <- sum(n_values)
        
        # 결과 저장
        results_list[[as.character(j)]] <- estimate_matrix_df
      }
    }
  }
}

percentiles <- c( "0.05")

for (j in c(1:6)) {
  
  results_list[[j]][["coef_h_conv2"]] <- paste(
    round(results_list[[j]][["coef_h_conv"]], 3),ifelse(results_list[[j]][["pv_h_conv"]] < 0.01, "***",
                                                        ifelse(results_list[[j]][["pv_h_conv"]] < 0.05, "**",
                                                               ifelse(results_list[[j]][["pv_h_conv"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_conv"]], 3), ")",
    
    sep = ""
  )
  results_list[[j]][["coef_h_bc2"]] <- paste(
    round(results_list[[j]][["coef_h_bc"]], 3),ifelse(results_list[[j]][["pv_h_bc"]] < 0.01, "***",
                                                      ifelse(results_list[[j]][["pv_h_bc"]] < 0.05, "**",
                                                             ifelse(results_list[[j]][["pv_h_bc"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_bc"]], 3), ")",
    
    sep = ""
  )
  
}
combined_results_list <- list()

# 각 percentile에 대해 h_combined, 2h_combined, h/2_combined, covariate_combined만 추출
for (percentile in names(results_list)) {
  # 각 리스트에서 필요한 열만 추출
  combined_results_list[[percentile]] <- results_list[[percentile]][, c("Data_Name","Ex_super","N","N_h","N_b",
                                                                        "coef_h_conv2","coef_h_bc2", "h","b","beta_Y_p_l_h","beta_Y_p_r_h",
                                                                        "beta_T_p_l_h","beta_T_p_r_h"
  )]
}
summary_list = list()



# 현재 퍼센타일에 대한 결과를 저장할 데이터 프레임 초기화
estimate_matrix_df <- data.frame(
  "Data_Name" = rep(c("FULL", "Q1Q2", "Q2Q3","Q3Q4","Q1","Q2","Q3","Q4"), each = 2),  # "Full", "Q1Q2", "Q3Q4"에 대해 t, f 순서 반복
  "Ex_super" = rep(c("t", "f"), 8), 
  "Min" = NA,# Covariate 열 추가
  stringsAsFactors = FALSE
)

# 각 Data_Name에 대해 Estimate 값과 Covariate 값을 채우기
for (i in 1:nrow(estimate_matrix_df)) {
  data_name <- estimate_matrix_df$Data_Name[i]
  Ex_super <- estimate_matrix_df$Ex_super[i]  # t, f를 구분
  
  # bandwidth별로 결과 key 생성
  result_key <- paste('0.05',Ex_super,data_name, sep = "_") # Bandwidth와 type 포함된 key 형성
  
  # Estimate 값을 채움
  if (result_key %in% names(results1)) {
    estimate_matrix_df$Min[i] <- avglist[[result_key]][["Min."]]
    estimate_matrix_df$Median[i] <- avglist[[result_key]][["Median"]]
    estimate_matrix_df$Max[i] <- avglist[[result_key]][["Max."]]
    estimate_matrix_df$ex_Min[i] <- exlist[[result_key]][["Min."]]
    estimate_matrix_df$ex_Median[i] <- exlist[[result_key]][["Median"]]
    estimate_matrix_df$ex_Max[i] <- exlist[[result_key]][["Max."]]
    estimate_matrix_df$diff_Min[i] <- difflist[[result_key]][["Min."]]
    estimate_matrix_df$diff_Median[i] <- difflist[[result_key]][["Median"]]
    estimate_matrix_df$diff_Max[i] <- difflist[[result_key]][["Max."]]
    
    # 두 번째 Estimate 값
  }
  
}


wb <- createWorkbook()
percentiles <- c(0.01, 0.05, 0.1)

# 각 퍼센타일별로 시트 추가
for (j in c(1:2)) {
  sheet_name <- as.character(j)
  
  # 해당 퍼센타일의 데이터 프레임을 시트로 추가
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, combined_results_list[[sheet_name]], rowNames = FALSE)
}
sheet_name <- paste0("summary")

# 해당 퍼센타일의 데이터 프레임을 시트로 추가
addWorksheet(wb, sheet_name)
writeData(wb, sheet_name, estimate_matrix_df, rowNames = FALSE)
# 새로운 시트 추가 및 데이터 쓰기



saveWorkbook(wb, "1-1.Ent ltm_first_month 10.xlsx", overwrite = TRUE)




tex_data <- data.frame(
  "Condition" = rep(c("FULL", "Q1Q2", "Q2Q3", "Q3Q4", "Q1", "Q2", "Q3", "Q4"), each = 2),
  "Ex_super" = rep(c("t", "f"), 8),
  "coef_h_conv" = round(cbind(results_list[[1]]$coef_h_conv, results_list[[2]]$coef_h_conv),3),
  "se_h_conv" = round(cbind(results_list[[1]]$se_h_conv, results_list[[2]]$se_h_conv),3),
  "pv_h_conv" = cbind(results_list[[1]]$pv_h_conv, results_list[[2]]$pv_h_conv),
  "coef_h_bc" = round(cbind(results_list[[1]]$coef_h_bc, results_list[[2]]$coef_h_bc),3),
  "se_h_bc" = round(cbind(results_list[[1]]$se_h_bc, results_list[[2]]$se_h_bc),3),
  "pv_h_bc" = cbind(results_list[[1]]$pv_h_bc, results_list[[2]]$pv_h_bc)
  
)
tex_data <- tex_data %>%
  mutate(Ex_super = factor(Ex_super, levels = c("t", "f"))) %>%
  arrange(Ex_super)
# 텍스트 변환
tex_table <- "\\begin{table}[]\n \\TABLE \n\ {LTM 2} {\\begin{tabular}{lcccc}\n"
tex_table <- paste(tex_table, "\\hline\n", sep = "")
tex_table <- paste(tex_table, "Condition & h\\_conv & h\\_bc & cov\\_h\\_conv & cov\\_h\\_bc \\\\ \\hline\n", sep = "")
tex_table <- paste(tex_table, "\\\\ \n", sep = "")
tex_table <- paste(tex_table, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==TRUE}}\\\\ \n", sep = "")
tex_table <- paste(tex_table, "\\\\ \n", sep = "")


# 각 조건에 대해 행 추가
for (i in 1:8) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  
  coef_h_bc <- tex_data$coef_h_bc.1[i]
  se_h_bc <- tex_data$se_h_bc.1[i]
  pv_h_bc <- tex_data$pv_h_bc.1[i]
  
  cov_coef_h_conv <- tex_data$coef_h_conv.2[i]
  cov_se_h_conv <- tex_data$se_h_conv.2[i]
  cov_pv_h_conv <- tex_data$pv_h_conv.2[i]
  
  cov_coef_h_bc <- tex_data$coef_h_bc.2[i]
  cov_se_h_bc <- tex_data$se_h_bc.2[i]
  cov_pv_h_bc <- tex_data$pv_h_bc.2[i]
  # coef와 se에 별 추가
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_bc_star <- paste(coef_h_bc, ifelse(pv_h_bc < 0.01, "***", ifelse(pv_h_bc < 0.05, "**", ifelse(pv_h_bc < 0.1, "*", ""))), sep = "")
  cov_coef_h_conv_star <- paste(cov_coef_h_conv, ifelse(cov_pv_h_conv < 0.01, "***", ifelse(cov_pv_h_conv < 0.05, "**", ifelse(cov_pv_h_conv < 0.1, "*", ""))), sep = "")
  cov_coef_h_bc_star <- paste(cov_coef_h_bc, ifelse(cov_pv_h_bc < 0.01, "***", ifelse(cov_pv_h_bc < 0.05, "**", ifelse(cov_pv_h_bc < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table <- paste(tex_table, paste(condition,  "&", coef_h_conv_star, "&", coef_h_bc_star, "&",cov_coef_h_conv_star,"&",cov_coef_h_bc_star, "\\\\ \n","& ","(" ,se_h_conv,") & (",se_h_bc,") & (",cov_se_h_conv, ") & (",cov_se_h_bc, ") \\\\ \n",sep = ""), sep = "")
}
tex_table <- paste(tex_table, "\\\\ \n", sep = "")

tex_table <- paste(tex_table, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==FALSE}} \\\\ \n", sep = "")
tex_table <- paste(tex_table, "\\\\ \n", sep = "")

for (i in 9:16) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  
  coef_h_bc <- tex_data$coef_h_bc.1[i]
  se_h_bc <- tex_data$se_h_bc.1[i]
  pv_h_bc <- tex_data$pv_h_bc.1[i]
  
  cov_coef_h_conv <- tex_data$coef_h_conv.2[i]
  cov_se_h_conv <- tex_data$se_h_conv.2[i]
  cov_pv_h_conv <- tex_data$pv_h_conv.2[i]
  
  cov_coef_h_bc <- tex_data$coef_h_bc.2[i]
  cov_se_h_bc <- tex_data$se_h_bc.2[i]
  cov_pv_h_bc <- tex_data$pv_h_bc.2[i]
  # coef와 se에 별 추가
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_bc_star <- paste(coef_h_bc, ifelse(pv_h_bc < 0.01, "***", ifelse(pv_h_bc < 0.05, "**", ifelse(pv_h_bc < 0.1, "*", ""))), sep = "")
  cov_coef_h_conv_star <- paste(cov_coef_h_conv, ifelse(cov_pv_h_conv < 0.01, "***", ifelse(cov_pv_h_conv < 0.05, "**", ifelse(cov_pv_h_conv < 0.1, "*", ""))), sep = "")
  
  cov_coef_h_bc_star <- paste(cov_coef_h_bc, ifelse(cov_pv_h_bc < 0.01, "***", ifelse(cov_pv_h_bc < 0.05, "**", ifelse(cov_pv_h_bc < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table <- paste(tex_table, paste(condition,  "&", coef_h_conv_star, "&", coef_h_bc_star, "&",cov_coef_h_conv_star,"&",cov_coef_h_bc_star, "\\\\ \n","& ","(" ,se_h_conv,") & (",se_h_bc,") & (",cov_se_h_conv, ") & (",cov_se_h_bc, ") \\\\ \n",sep = ""), sep = "")
}
# 테이블 끝 마무리
tex_table <- paste(tex_table, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
writeLines(tex_table, "1-2 Ent ltm 2 Regression Results_act.tex")
# TeX 코드 출력
cat(tex_table)


# Entire detail ------------------------------------------------------------------

tex_data <- data.frame(
  "Condition" = rep(c("FULL", "Q1Q2", "Q2Q3", "Q3Q4", "Q1", "Q2", "Q3", "Q4"), each = 2),
  "Ex_super" = rep(c("t", "f"), 8),
  "coef_h_conv" = round(cbind(results_list[[1]]$coef_h_conv,
                              results_list[[2]]$coef_h_conv,
                              results_list[[3]]$coef_h_conv,
                              results_list[[4]]$coef_h_conv,
                              results_list[[5]]$coef_h_conv,
                              results_list[[6]]$coef_h_conv),3),
  "se_h_conv" = round(cbind(results_list[[1]]$se_h_conv, 
                            results_list[[2]]$se_h_conv,
                            results_list[[3]]$se_h_conv,
                            results_list[[4]]$se_h_conv,
                            results_list[[5]]$se_h_conv,
                            results_list[[6]]$se_h_conv),3),
  "pv_h_conv" = cbind(results_list[[1]]$pv_h_conv,
                      results_list[[2]]$pv_h_conv,
                      results_list[[3]]$pv_h_conv,
                      results_list[[4]]$pv_h_conv,
                      results_list[[5]]$pv_h_conv,
                      results_list[[6]]$pv_h_conv
  ),
  "bws_h" = cbind(results_list[[1]]$h),
  "N_h" = cbind(results_list[[1]]$N_h,
                results_list[[2]]$N_h,
                results_list[[3]]$N_h,
                results_list[[4]]$N_h,
                results_list[[5]]$N_h,
                results_list[[6]]$N_h
  ))
tex_data <- tex_data %>%
  mutate(Ex_super = factor(Ex_super, levels = c("t", "f"))) %>%
  arrange(Ex_super)
tex_table_detail <- "\\begin{table}[]\n \\TABLE \n\ {Entire Room Regression Results} {\\begin{tabular}{lcccccc}\n"
tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) & (5) &(6) \\\\ \\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==1}}\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")


# 각 조건에 대해 행 추가
for (i in 1:8) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  coef_h_conv6 <- tex_data$coef_h_conv.6[i]
  se_h_conv6 <- tex_data$se_h_conv.6[i]
  pv_h_conv6 <- tex_data$pv_h_conv.6[i]
  N_h6 <- tex_data$N_h.6[i]
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star6 <- paste(coef_h_conv6, ifelse(pv_h_conv6 < 0.01, "***", ifelse(pv_h_conv6 < 0.05, "**", ifelse(pv_h_conv6 < 0.1, "*", ""))), sep = "")
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5,"&",coef_h_conv_star6, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5,")&(",se_h_conv6, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&", bws_h_conv, "&", "&","&","&", "&","\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5,"&",N_h6, "\\\\ \n"), sep = "")
  
}

tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")

tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==FALSE}} \\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
for (i in 9:16) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  coef_h_conv6 <- tex_data$coef_h_conv.6[i]
  se_h_conv6 <- tex_data$se_h_conv.6[i]
  pv_h_conv6 <- tex_data$pv_h_conv.6[i]
  N_h6 <- tex_data$N_h.6[i]
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star6 <- paste(coef_h_conv6, ifelse(pv_h_conv6 < 0.01, "***", ifelse(pv_h_conv6 < 0.05, "**", ifelse(pv_h_conv6 < 0.1, "*", ""))), sep = "")
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5,"&",coef_h_conv_star6, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5,")&(",se_h_conv6, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&", bws_h_conv, "&", "&","&","&", "&","\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5,"&",N_h6, "\\\\ \n"), sep = "")
  
}

# 테이블 끝 마무리
tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
writeLines(tex_table_detail, "Ent detail_act.tex")
# TeX 코드 출력
cat(tex_table_detail)
quarterly[["Total"]]<-results_list

# 1-1. RD plot t---------------------------------------------------------------
z3=z%>%filter(ex_super=='t' &!is.na(running_scr))
#summary(z3$review_scores_rating)
z3$margin = z3$running_scr-4.75
z3= z3%>%filter(-results2[[9]]$bws[1,1]<= margin & margin <= results2[[9]]$bws[1,2])
data=z3
data$margin = data$running_scr-4.75
data= data%>%filter(-results2[[9]]$bws[1,1]<= margin & margin <= results2[[9]]$bws[1,2])
#data$denominator = results1[[1]]$beta_T_p_r[1,1]-results1[[1]]$beta_T_p_l[1,1]
rd_plot = rdplot(y=data$price_diff,x = data$margin , subset=-results2[[9]]$bws[1,1]<= margin & margin <= results2[[9]]$bws[1,2], 
                 kernel="tri", p=1, #binselect = 'esmv',
                 title="RD Plot: Airbnb", #scale=2,
                 y.label="Occupancy Rate",
                 x.label="Rating for Host",masspoints = 'off')
rd_plot
x <- rd_plot$vars_bins$rdplot_mean_bin
y <- rd_plot$vars_bins$rdplot_mean_y

left_data <- data%>%filter(margin>=-results2[[9]]$bws[1,1]& margin <0)
right_data <- data%>%filter(margin>=0& margin <results2[[9]]$bws[1,2])
#data=data%>%arrange(margin)

line_l <- results2[[9]]$beta_Y_p_l[2] * (left_data$margin) + results2[[9]]$beta_Y_p_l[1]
line_r <- results2[[9]]$beta_Y_p_r[2] * (right_data$margin) + results2[[9]]$beta_Y_p_r[1]

confi_l_u <- results2[[9]]$beta_Y_p_l[2] * (left_data$margin) + results2[[9]]$beta_Y_p_l[1] + results2[[9]]$se[1] * 1.96
confi_l_l <- results2[[9]]$beta_Y_p_l[2] * (left_data$margin) + results2[[9]]$beta_Y_p_l[1] - results2[[9]]$se[1] * 1.96

confi_r_u <- results2[[9]]$beta_Y_p_r[2] * (right_data$margin) + results2[[9]]$beta_Y_p_r[1] + results2[[9]]$se[1] * 1.96
confi_r_l <- results2[[9]]$beta_Y_p_r[2] * (right_data$margin) + results2[[9]]$beta_Y_p_r[1] - results2[[9]]$se[1] * 1.96


scatter_data <- data.frame(x = x, y = y)

fitted_data <- data.frame(
  x = c(left_data$margin, right_data$margin),
  y = c(line_l, line_r),
  group = c(rep("Left Fit", length(line_l)), rep("Right Fit", length(line_r)))
)

confidence_data <- data.frame(
  x = c(left_data$margin, right_data$margin),
  ymin = c(confi_l_l, confi_r_l),
  ymax = c(confi_l_u, confi_r_u)
)

confidence_data_left <- data.frame(
  x = left_data$margin,
  ymin = confi_l_l,
  ymax = confi_l_u
)

confidence_data_right <- data.frame(
  x = right_data$margin,
  ymin = confi_r_l,
  ymax = confi_r_u
)
library(ggplot2)
plot_t <- ggplot() +
  geom_point(data = scatter_data, aes(x = x, y = y), color = "black", alpha = 1.0) +
  geom_line(data = fitted_data, aes(x = x, y = y, color = group), size = 1) +
  #geom_line(data = confidence_data_left, aes(x = x, y = ymin), linetype = "dashed", color = "black") +
  #geom_line(data = confidence_data_left, aes(x = x, y = ymax), linetype = "dashed", color = "black") +
  #geom_line(data = confidence_data_right, aes(x = x, y = ymin), linetype = "dashed", color = "black") +
  #geom_line(data = confidence_data_right, aes(x = x, y = ymax), linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_classic() +
  labs(x = "Average rating within last 12 months", y = "Price Increment") +
  scale_color_manual(values = c("black", "black")) +
  theme(legend.position = "none") +ylim(-0.1,0.1)
plot_t
# 1-1. RD plot f---------------------------------------------------------------
data = z%>%filter(ex_super=='f' &!is.na(host_is_superhost) & !is.na(id) &
                    host_is_superhost!='' &!is.na(ex_avg) & ex_avg!='')
data$margin = data$running_scr-4.75
data= data%>%filter(-results2[[1]]$bws[1,1]<= margin & margin <= results2[[1]]$bws[1,2])
rd_plot = rdplot(y=data$price_diff,x = data$margin , subset=-results2[[1]]$bws[1,1]<= margin & margin <= results2[[1]]$bws[1,2], 
                 kernel="tri", p=1, scale=1,
                 title="RD Plot: Airbnb", 
                 y.label="Occupancy Rate",
                 x.label="Rating for Host",masspoints = 'off')
rd_plot
x <- rd_plot$vars_bins$rdplot_mean_bin
y <- rd_plot$vars_bins$rdplot_mean_y

left_data <- data%>%filter(margin>=-results2[[1]]$bws[1,1]& margin <0)
right_data <- data%>%filter(margin>=0& margin <results2[[1]]$bws[1,2])
#data=data%>%arrange(margin)

line_l <- results2[[1]]$beta_Y_p_l[2] * (left_data$margin) + results2[[1]]$beta_Y_p_l[1]
line_r <- results2[[1]]$beta_Y_p_r[2] * (right_data$margin) + results2[[1]]$beta_Y_p_r[1]

confi_l_u <- results2[[1]]$beta_Y_p_l[2] * (left_data$margin) + results2[[1]]$beta_Y_p_l[1] + results2[[1]]$se[1] * 1.96
confi_l_l <- results2[[1]]$beta_Y_p_l[2] * (left_data$margin) + results2[[1]]$beta_Y_p_l[1] - results2[[1]]$se[1] * 1.96

confi_r_u <- results2[[1]]$beta_Y_p_r[2] * (right_data$margin) + results2[[1]]$beta_Y_p_r[1] + results2[[1]]$se[1] * 1.96
confi_r_l <- results2[[1]]$beta_Y_p_r[2] * (right_data$margin) + results2[[1]]$beta_Y_p_r[1] - results2[[1]]$se[1] * 1.96


scatter_data <- data.frame(x = x, y = y)

fitted_data <- data.frame(
  x = c(left_data$margin, right_data$margin),
  y = c(line_l, line_r),
  group = c(rep("Left Fit", length(line_l)), rep("Right Fit", length(line_r)))
)

confidence_data <- data.frame(
  x = c(left_data$margin, right_data$margin),
  ymin = c(confi_l_l, confi_r_l),
  ymax = c(confi_l_u, confi_r_u)
)

confidence_data_left <- data.frame(
  x = left_data$margin,
  ymin = confi_l_l,
  ymax = confi_l_u
)

confidence_data_right <- data.frame(
  x = right_data$margin,
  ymin = confi_r_l,
  ymax = confi_r_u
)
library(ggplot2)
plot_f <- ggplot() +
  geom_point(data = scatter_data, aes(x = x, y = y), color = "black", alpha = 1.0) +
  geom_line(data = fitted_data, aes(x = x, y = y, color = group), size = 1) +
  #geom_line(data = confidence_data_left, aes(x = x, y = ymin), linetype = "dashed", color = "black") +
  #geom_line(data = confidence_data_left, aes(x = x, y = ymax), linetype = "dashed", color = "black") +
  #geom_line(data = confidence_data_right, aes(x = x, y = ymin), linetype = "dashed", color = "black") +
  #geom_line(data = confidence_data_right, aes(x = x, y = ymax), linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  ggtitle("Panel 2") +
  theme_classic() +
  labs(x = "Average rating within last 12 months", y = "Price Increment") +
  scale_color_manual(values = c("black", "black")) +
  theme(legend.position = "none")
plot_f
library(gridExtra)

postscript("Figure_1.eps", width = 10, height = 4, horizontal = FALSE, onefile = FALSE)
grid.arrange(plot_t, plot_f, ncol=2)  # 가로 배치 (세로 배치는 nrow=2)
dev.off()


# 23 3Q regression --------------------------------------------------------
z <- data.frame()  # 결과를 저장할 빈 데이터 프레임
z=rbind(Q323)
#summary(z$first_month_number_of_reviews)
avglist = list()
exlist= list()
difflist = list()
results1 <- list()
results2 <- list()
results3 <- list()
results4 <- list()
results5 <- list()

conditions <- list(
  "FULL" = (z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1Q2" = (z$ex_q1 == 1 | z$ex_q2 == 1),
  "Q2Q3" = (z$ex_q3 == 1 | z$ex_q2 == 1),
  "Q3Q4" = (z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1" = (z$ex_q1 == 1),
  "Q2" = (z$ex_q2 == 1),
  "Q3" = (z$ex_q3 == 1),
  "Q4" = (z$ex_q4 == 1)
)
summary(z$ex_q4)
#z=z%>%filter(first_month_ltm>=5)
for (super_type in c('f', 't')) {
  for (condition_name in names(conditions)) {
    condition <- conditions[[condition_name]]
    
    filtered_data <- z %>%
      filter(ex_super == super_type & condition)
    
    if (nrow(filtered_data) > 0) {
      margin <- filtered_data$running_scr - 4.75
      
      result_name <- paste(pct, super_type, condition_name, sep = "_")
      
      tryCatch({
        est1 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est2 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(2*est1[['bws']][1,1], 2*est1[['bws']][1,2]),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est3 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.2, 0.1),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est4 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.3, 0.15),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est5 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.4, 0.2),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        avg <- summary(filtered_data$avg_price)
        ex <- summary(filtered_data$ex_avg)
        diff <- summary((filtered_data$avg_price - filtered_data$ex_avg) / filtered_data$ex_avg)
        
        results1[[result_name]] <- est1
        results2[[result_name]] <- est2
        results3[[result_name]] <- est3
        results4[[result_name]] <- est4
        results5[[result_name]] <- est5
        
        avglist[[result_name]] <- avg
        exlist[[result_name]] <- ex
        difflist[[result_name]] <- diff
        
      }, error = function(e) {
        message(sprintf("Error in %s: %s", result_name, e$message))
        # 오류가 나면 해당 반복만 건너뜀
      })
    }
  }
}
results_list <-list()
bandwidths <- c("h")
bandwidths <- c("h")

# 현재 퍼센타일에 대한 결과를 저장할 데이터 프레임 초기화
estimate_matrix_df <- data.frame(
  "Data_Name" = rep(c("FULL", "Q1Q2", "Q2Q3","Q3Q4","Q1","Q2","Q3","Q4"), each = 2),  # "Full", "Q1Q2", "Q3Q4"에 대해 t, f 순서 반복
  "Ex_super" = rep(c("t", "f"), 8),  # Covariate 열 추가
  "N" = numeric(16),  # h/2에 해당하는 pv 열
  stringsAsFactors = FALSE
)
# results_list 초기화
results_list <- list()

for (j in 1:5) {
  list_name <- paste0('results', j)  # "results1"이 됩니다.
  
  # 각 Data_Name에 대해 Estimate 값과 Covariate 값을 채우기
  for (i in 1:nrow(estimate_matrix_df)) {
    data_name <- estimate_matrix_df$Data_Name[i]
    Ex_super <- estimate_matrix_df$Ex_super[i]  # t, f를 구분
    
    for (bw in bandwidths) {
      # bandwidth별로 결과 key 생성
      result_key <- paste('0.05', Ex_super, data_name, sep = "_") # Bandwidth와 type 포함된 key 형성
      
      # get()을 사용하여 해당 리스트를 가져옴
      result_data <- get(list_name)[[result_key]]
      
      # result_key가 존재하면 Estimate 값을 채움
      if (!is.null(result_data)) {
        estimate_matrix_df[i, paste0("coef_", bw, "_conv")] <- result_data[["Estimate"]][1]
        estimate_matrix_df[i, paste0("coef_", bw, "_bc")] <- result_data[["Estimate"]][2]
        estimate_matrix_df[i, paste0("se_", bw, "_conv")] <- result_data[["se"]][1]
        estimate_matrix_df[i, paste0("se_", bw, "_bc")] <- result_data[["se"]][2]
        estimate_matrix_df[i, paste0("pv_", bw, "_conv")] <- result_data[["pv"]][1]
        estimate_matrix_df[i, paste0("pv_", bw, "_bc")] <- result_data[["pv"]][2]
        estimate_matrix_df[i, paste0("beta_Y_p_l_", bw)] <- result_data[["beta_Y_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_Y_p_r_", bw)] <- result_data[["beta_Y_p_r"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_l_", bw)] <- result_data[["beta_T_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_r_", bw)] <- result_data[["beta_T_p_r"]][1]
        
        # N_h, N_b, h, b 값 형성
        estimate_matrix_df$N_h[i] <- paste("[", paste(result_data[["N_h"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$N_b[i] <- paste("[", paste(result_data[["N_b"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$h[i] <- paste("[", paste(round(result_data[["bws"]][1,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$b[i] <- paste("[", paste(round(result_data[["bws"]][2,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$obs_h[i] <- result_data[["N_h"]][1]+result_data[["N_h"]][2]
        estimate_matrix_df$obs_b[i] <- result_data[["N_b"]][1]+result_data[["N_b"]][2]
        
        # N 값 합산
        n_values <- result_data[["N"]]
        estimate_matrix_df$N[i] <- sum(n_values)
        
        # 결과 저장
        results_list[[as.character(j)]] <- estimate_matrix_df
      }
    }
  }
}

percentiles <- c( "0.05")

for (j in c(1:5)) {
  
  results_list[[j]][["coef_h_conv2"]] <- paste(
    round(results_list[[j]][["coef_h_conv"]], 3),ifelse(results_list[[j]][["pv_h_conv"]] < 0.01, "***",
                                                        ifelse(results_list[[j]][["pv_h_conv"]] < 0.05, "**",
                                                               ifelse(results_list[[j]][["pv_h_conv"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_conv"]], 3), ")",
    
    sep = ""
  )
  results_list[[j]][["coef_h_bc2"]] <- paste(
    round(results_list[[j]][["coef_h_bc"]], 3),ifelse(results_list[[j]][["pv_h_bc"]] < 0.01, "***",
                                                      ifelse(results_list[[j]][["pv_h_bc"]] < 0.05, "**",
                                                             ifelse(results_list[[j]][["pv_h_bc"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_bc"]], 3), ")",
    
    sep = ""
  )
  
}
combined_results_list <- list()

# 각 percentile에 대해 h_combined, 2h_combined, h/2_combined, covariate_combined만 추출
for (percentile in names(results_list)) {
  # 각 리스트에서 필요한 열만 추출
  combined_results_list[[percentile]] <- results_list[[percentile]][, c("Data_Name","Ex_super","N","N_h","N_b",
                                                                        "coef_h_conv2","coef_h_bc2", "h","b","beta_Y_p_l_h","beta_Y_p_r_h",
                                                                        "beta_T_p_l_h","beta_T_p_r_h"
  )]
}
quarterly[["2023_3Q"]] <- results_list

# 23 3Q detail ------------------------------------------------------------------

tex_data <- data.frame(
  "Condition" = rep(c("FULL", "Q1Q2", "Q2Q3", "Q3Q4", "Q1", "Q2", "Q3", "Q4"), each = 2),
  "Ex_super" = rep(c("t", "f"), 8),
  "coef_h_conv" = round(cbind(results_list[[1]]$coef_h_conv,
                              results_list[[2]]$coef_h_conv,
                              results_list[[3]]$coef_h_conv,
                              results_list[[4]]$coef_h_conv,
                              results_list[[5]]$coef_h_conv),3),
  "se_h_conv" = round(cbind(results_list[[1]]$se_h_conv, 
                            results_list[[2]]$se_h_conv,
                            results_list[[3]]$se_h_conv,
                            results_list[[4]]$se_h_conv,
                            results_list[[5]]$se_h_conv),3),
  "pv_h_conv" = cbind(results_list[[1]]$pv_h_conv,
                      results_list[[2]]$pv_h_conv,
                      results_list[[3]]$pv_h_conv,
                      results_list[[4]]$pv_h_conv,
                      results_list[[5]]$pv_h_conv
  ),
  "bws_h" = cbind(results_list[[1]]$h),
  "N_h" = cbind(results_list[[1]]$N_h,
                results_list[[2]]$N_h,
                results_list[[3]]$N_h,
                results_list[[4]]$N_h,
                results_list[[5]]$N_h
  ))
tex_data <- tex_data %>%
  mutate(Ex_super = factor(Ex_super, levels = c("t", "f"))) %>%
  arrange(Ex_super)
tex_table_detail <- "\\begin{table}[]\n \\TABLE \n\ {2023 Q3 \\label{2023_Q3_t}} {\\begin{tabular}{lccccc}\n"
tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) & (5) \\\\ \\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==1}}\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")


# 각 조건에 대해 행 추가
for (i in 1:8) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", bws_h_conv, "&", "&","&","&", "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
  
}


tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\begin{table}[]\n \\TABLE \n\ {2023 Q3 \\label{2023_Q3_f}} {\\begin{tabular}{lccccc}\n", sep="")
tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) &(5) \\\\ \\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==0}} \\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
for (i in 9:16) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", bws_h_conv, "&", "&","&","&", "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
  
}

# 테이블 끝 마무리
tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
writeLines(tex_table_detail, "5-1. 2023_Q3_act.tex")
# TeX 코드 출력
cat(tex_table_detail)


# 23 4Q regression --------------------------------------------------------
z <- data.frame()  # 결과를 저장할 빈 데이터 프레임
z=rbind(Q423)
#summary(z$first_month_number_of_reviews)
avglist = list()
exlist= list()
difflist = list()
results1 <- list()
results2 <- list()
results3 <- list()
results4 <- list()
results5 <- list()

conditions <- list(
  "FULL" = (z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1Q2" = (z$ex_q1 == 1 | z$ex_q2 == 1),
  "Q2Q3" = (z$ex_q3 == 1 | z$ex_q2 == 1),
  "Q3Q4" = (z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1" = (z$ex_q1 == 1),
  "Q2" = (z$ex_q2 == 1),
  "Q3" = (z$ex_q3 == 1),
  "Q4" = (z$ex_q4 == 1)
)
summary(z$ex_q4)
#z=z%>%filter(first_month_ltm>=5)
for (super_type in c('f', 't')) {
  for (condition_name in names(conditions)) {
    condition <- conditions[[condition_name]]
    
    filtered_data <- z %>%
      filter(ex_super == super_type & condition)
    
    if (nrow(filtered_data) > 0) {
      margin <- filtered_data$running_scr - 4.75
      
      result_name <- paste(pct, super_type, condition_name, sep = "_")
      
      tryCatch({
        est1 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est2 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(2*est1[['bws']][1,1], 2*est1[['bws']][1,2]),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est3 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.2, 0.1),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est4 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.3, 0.15),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est5 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.4, 0.2),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        avg <- summary(filtered_data$avg_price)
        ex <- summary(filtered_data$ex_avg)
        diff <- summary((filtered_data$avg_price - filtered_data$ex_avg) / filtered_data$ex_avg)
        
        results1[[result_name]] <- est1
        results2[[result_name]] <- est2
        results3[[result_name]] <- est3
        results4[[result_name]] <- est4
        results5[[result_name]] <- est5
        
        avglist[[result_name]] <- avg
        exlist[[result_name]] <- ex
        difflist[[result_name]] <- diff
        
      }, error = function(e) {
        message(sprintf("Error in %s: %s", result_name, e$message))
        # 오류가 나면 해당 반복만 건너뜀
      })
    }
  }
}
results_list <-list()
bandwidths <- c("h")
bandwidths <- c("h")

# 현재 퍼센타일에 대한 결과를 저장할 데이터 프레임 초기화
estimate_matrix_df <- data.frame(
  "Data_Name" = rep(c("FULL", "Q1Q2", "Q2Q3","Q3Q4","Q1","Q2","Q3","Q4"), each = 2),  # "Full", "Q1Q2", "Q3Q4"에 대해 t, f 순서 반복
  "Ex_super" = rep(c("t", "f"), 8),  # Covariate 열 추가
  "N" = numeric(16),  # h/2에 해당하는 pv 열
  stringsAsFactors = FALSE
)
# results_list 초기화
results_list <- list()

for (j in 1:5) {
  list_name <- paste0('results', j)  # "results1"이 됩니다.
  
  # 각 Data_Name에 대해 Estimate 값과 Covariate 값을 채우기
  for (i in 1:nrow(estimate_matrix_df)) {
    data_name <- estimate_matrix_df$Data_Name[i]
    Ex_super <- estimate_matrix_df$Ex_super[i]  # t, f를 구분
    
    for (bw in bandwidths) {
      # bandwidth별로 결과 key 생성
      result_key <- paste('0.05', Ex_super, data_name, sep = "_") # Bandwidth와 type 포함된 key 형성
      
      # get()을 사용하여 해당 리스트를 가져옴
      result_data <- get(list_name)[[result_key]]
      
      # result_key가 존재하면 Estimate 값을 채움
      if (!is.null(result_data)) {
        estimate_matrix_df[i, paste0("coef_", bw, "_conv")] <- result_data[["Estimate"]][1]
        estimate_matrix_df[i, paste0("coef_", bw, "_bc")] <- result_data[["Estimate"]][2]
        estimate_matrix_df[i, paste0("se_", bw, "_conv")] <- result_data[["se"]][1]
        estimate_matrix_df[i, paste0("se_", bw, "_bc")] <- result_data[["se"]][2]
        estimate_matrix_df[i, paste0("pv_", bw, "_conv")] <- result_data[["pv"]][1]
        estimate_matrix_df[i, paste0("pv_", bw, "_bc")] <- result_data[["pv"]][2]
        estimate_matrix_df[i, paste0("beta_Y_p_l_", bw)] <- result_data[["beta_Y_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_Y_p_r_", bw)] <- result_data[["beta_Y_p_r"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_l_", bw)] <- result_data[["beta_T_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_r_", bw)] <- result_data[["beta_T_p_r"]][1]
        
        # N_h, N_b, h, b 값 형성
        estimate_matrix_df$N_h[i] <- paste("[", paste(result_data[["N_h"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$N_b[i] <- paste("[", paste(result_data[["N_b"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$h[i] <- paste("[", paste(round(result_data[["bws"]][1,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$b[i] <- paste("[", paste(round(result_data[["bws"]][2,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$obs_h[i] <- result_data[["N_h"]][1]+result_data[["N_h"]][2]
        estimate_matrix_df$obs_b[i] <- result_data[["N_b"]][1]+result_data[["N_b"]][2]
        
        # N 값 합산
        n_values <- result_data[["N"]]
        estimate_matrix_df$N[i] <- sum(n_values)
        
        # 결과 저장
        results_list[[as.character(j)]] <- estimate_matrix_df
      }
    }
  }
}

percentiles <- c( "0.05")

for (j in c(1:5)) {
  
  results_list[[j]][["coef_h_conv2"]] <- paste(
    round(results_list[[j]][["coef_h_conv"]], 3),ifelse(results_list[[j]][["pv_h_conv"]] < 0.01, "***",
                                                        ifelse(results_list[[j]][["pv_h_conv"]] < 0.05, "**",
                                                               ifelse(results_list[[j]][["pv_h_conv"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_conv"]], 3), ")",
    
    sep = ""
  )
  results_list[[j]][["coef_h_bc2"]] <- paste(
    round(results_list[[j]][["coef_h_bc"]], 3),ifelse(results_list[[j]][["pv_h_bc"]] < 0.01, "***",
                                                      ifelse(results_list[[j]][["pv_h_bc"]] < 0.05, "**",
                                                             ifelse(results_list[[j]][["pv_h_bc"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_bc"]], 3), ")",
    
    sep = ""
  )
  
}
combined_results_list <- list()

# 각 percentile에 대해 h_combined, 2h_combined, h/2_combined, covariate_combined만 추출
for (percentile in names(results_list)) {
  # 각 리스트에서 필요한 열만 추출
  combined_results_list[[percentile]] <- results_list[[percentile]][, c("Data_Name","Ex_super","N","N_h","N_b",
                                                                        "coef_h_conv2","coef_h_bc2", "h","b","beta_Y_p_l_h","beta_Y_p_r_h",
                                                                        "beta_T_p_l_h","beta_T_p_r_h"
  )]
}
quarterly[["2023_4Q"]] <- results_list

# 23 4Q detail ------------------------------------------------------------------

tex_data <- data.frame(
  "Condition" = rep(c("FULL", "Q1Q2", "Q2Q3", "Q3Q4", "Q1", "Q2", "Q3", "Q4"), each = 2),
  "Ex_super" = rep(c("t", "f"), 8),
  "coef_h_conv" = round(cbind(results_list[[1]]$coef_h_conv,
                              results_list[[2]]$coef_h_conv,
                              results_list[[3]]$coef_h_conv,
                              results_list[[4]]$coef_h_conv,
                              results_list[[5]]$coef_h_conv),3),
  "se_h_conv" = round(cbind(results_list[[1]]$se_h_conv, 
                            results_list[[2]]$se_h_conv,
                            results_list[[3]]$se_h_conv,
                            results_list[[4]]$se_h_conv,
                            results_list[[5]]$se_h_conv),3),
  "pv_h_conv" = cbind(results_list[[1]]$pv_h_conv,
                      results_list[[2]]$pv_h_conv,
                      results_list[[3]]$pv_h_conv,
                      results_list[[4]]$pv_h_conv,
                      results_list[[5]]$pv_h_conv
  ),
  "bws_h" = cbind(results_list[[1]]$h),
  "N_h" = cbind(results_list[[1]]$N_h,
                results_list[[2]]$N_h,
                results_list[[3]]$N_h,
                results_list[[4]]$N_h,
                results_list[[5]]$N_h
  ))
tex_data <- tex_data %>%
  mutate(Ex_super = factor(Ex_super, levels = c("t", "f"))) %>%
  arrange(Ex_super)
tex_table_detail <- "\\begin{table}[]\n \\TABLE \n\ {2023 Q4 \\label{2023_Q4_t}} {\\begin{tabular}{lccccc}\n"
tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) & (5) \\\\ \\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==TRUE}}\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")


# 각 조건에 대해 행 추가
for (i in 1:8) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", bws_h_conv, "&", "&","&","&", "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
  
}

tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")

tex_table_detail <- paste(tex_table_detail, "\\begin{table}[]\n \\TABLE \n\ {2023 Q4 \\label{2023_Q4_f}} {\\begin{tabular}{lccccc}\n", sep="")
tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) &(5) \\\\ \\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==0}} \\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
for (i in 9:16) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", bws_h_conv, "&", "&","&","&", "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
  
}

# 테이블 끝 마무리
tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
writeLines(tex_table_detail, "5-2. 2023_Q4_act.tex")
# TeX 코드 출력
cat(tex_table_detail)


# 24 1Q regression --------------------------------------------------------
z <- data.frame()  # 결과를 저장할 빈 데이터 프레임
z=rbind(Q124)
#summary(z$first_month_number_of_reviews)
avglist = list()
exlist= list()
difflist = list()
results1 <- list()
results2 <- list()
results3 <- list()
results4 <- list()
results5 <- list()

conditions <- list(
  "FULL" = (z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1Q2" = (z$ex_q1 == 1 | z$ex_q2 == 1),
  "Q2Q3" = (z$ex_q3 == 1 | z$ex_q2 == 1),
  "Q3Q4" = (z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1" = (z$ex_q1 == 1),
  "Q2" = (z$ex_q2 == 1),
  "Q3" = (z$ex_q3 == 1),
  "Q4" = (z$ex_q4 == 1)
)
summary(z$ex_q4)
#z=z%>%filter(first_month_ltm>=5)
for (super_type in c('f', 't')) {
  for (condition_name in names(conditions)) {
    condition <- conditions[[condition_name]]
    
    filtered_data <- z %>%
      filter(ex_super == super_type & condition)
    
    if (nrow(filtered_data) > 0) {
      margin <- filtered_data$running_scr - 4.75
      
      result_name <- paste(pct, super_type, condition_name, sep = "_")
      
      tryCatch({
        est1 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est2 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(2*est1[['bws']][1,1], 2*est1[['bws']][1,2]),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est3 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.2, 0.1),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est4 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.3, 0.15),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est5 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.4, 0.2),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        avg <- summary(filtered_data$avg_price)
        ex <- summary(filtered_data$ex_avg)
        diff <- summary((filtered_data$avg_price - filtered_data$ex_avg) / filtered_data$ex_avg)
        
        results1[[result_name]] <- est1
        results2[[result_name]] <- est2
        results3[[result_name]] <- est3
        results4[[result_name]] <- est4
        results5[[result_name]] <- est5
        
        avglist[[result_name]] <- avg
        exlist[[result_name]] <- ex
        difflist[[result_name]] <- diff
        
      }, error = function(e) {
        message(sprintf("Error in %s: %s", result_name, e$message))
        # 오류가 나면 해당 반복만 건너뜀
      })
    }
  }
}
results_list <-list()
bandwidths <- c("h")
bandwidths <- c("h")

# 현재 퍼센타일에 대한 결과를 저장할 데이터 프레임 초기화
estimate_matrix_df <- data.frame(
  "Data_Name" = rep(c("FULL", "Q1Q2", "Q2Q3","Q3Q4","Q1","Q2","Q3","Q4"), each = 2),  # "Full", "Q1Q2", "Q3Q4"에 대해 t, f 순서 반복
  "Ex_super" = rep(c("t", "f"), 8),  # Covariate 열 추가
  "N" = numeric(16),  # h/2에 해당하는 pv 열
  stringsAsFactors = FALSE
)
# results_list 초기화
results_list <- list()

for (j in 1:5) {
  list_name <- paste0('results', j)  # "results1"이 됩니다.
  
  # 각 Data_Name에 대해 Estimate 값과 Covariate 값을 채우기
  for (i in 1:nrow(estimate_matrix_df)) {
    data_name <- estimate_matrix_df$Data_Name[i]
    Ex_super <- estimate_matrix_df$Ex_super[i]  # t, f를 구분
    
    for (bw in bandwidths) {
      # bandwidth별로 결과 key 생성
      result_key <- paste('0.05', Ex_super, data_name, sep = "_") # Bandwidth와 type 포함된 key 형성
      
      # get()을 사용하여 해당 리스트를 가져옴
      result_data <- get(list_name)[[result_key]]
      
      # result_key가 존재하면 Estimate 값을 채움
      if (!is.null(result_data)) {
        estimate_matrix_df[i, paste0("coef_", bw, "_conv")] <- result_data[["Estimate"]][1]
        estimate_matrix_df[i, paste0("coef_", bw, "_bc")] <- result_data[["Estimate"]][2]
        estimate_matrix_df[i, paste0("se_", bw, "_conv")] <- result_data[["se"]][1]
        estimate_matrix_df[i, paste0("se_", bw, "_bc")] <- result_data[["se"]][2]
        estimate_matrix_df[i, paste0("pv_", bw, "_conv")] <- result_data[["pv"]][1]
        estimate_matrix_df[i, paste0("pv_", bw, "_bc")] <- result_data[["pv"]][2]
        estimate_matrix_df[i, paste0("beta_Y_p_l_", bw)] <- result_data[["beta_Y_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_Y_p_r_", bw)] <- result_data[["beta_Y_p_r"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_l_", bw)] <- result_data[["beta_T_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_r_", bw)] <- result_data[["beta_T_p_r"]][1]
        
        # N_h, N_b, h, b 값 형성
        estimate_matrix_df$N_h[i] <- paste("[", paste(result_data[["N_h"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$N_b[i] <- paste("[", paste(result_data[["N_b"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$h[i] <- paste("[", paste(round(result_data[["bws"]][1,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$b[i] <- paste("[", paste(round(result_data[["bws"]][2,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$obs_h[i] <- result_data[["N_h"]][1]+result_data[["N_h"]][2]
        estimate_matrix_df$obs_b[i] <- result_data[["N_b"]][1]+result_data[["N_b"]][2]
        
        # N 값 합산
        n_values <- result_data[["N"]]
        estimate_matrix_df$N[i] <- sum(n_values)
        
        # 결과 저장
        results_list[[as.character(j)]] <- estimate_matrix_df
      }
    }
  }
}

percentiles <- c( "0.05")

for (j in c(1:5)) {
  
  results_list[[j]][["coef_h_conv2"]] <- paste(
    round(results_list[[j]][["coef_h_conv"]], 3),ifelse(results_list[[j]][["pv_h_conv"]] < 0.01, "***",
                                                        ifelse(results_list[[j]][["pv_h_conv"]] < 0.05, "**",
                                                               ifelse(results_list[[j]][["pv_h_conv"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_conv"]], 3), ")",
    
    sep = ""
  )
  results_list[[j]][["coef_h_bc2"]] <- paste(
    round(results_list[[j]][["coef_h_bc"]], 3),ifelse(results_list[[j]][["pv_h_bc"]] < 0.01, "***",
                                                      ifelse(results_list[[j]][["pv_h_bc"]] < 0.05, "**",
                                                             ifelse(results_list[[j]][["pv_h_bc"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_bc"]], 3), ")",
    
    sep = ""
  )
  
}
combined_results_list <- list()

# 각 percentile에 대해 h_combined, 2h_combined, h/2_combined, covariate_combined만 추출
for (percentile in names(results_list)) {
  # 각 리스트에서 필요한 열만 추출
  combined_results_list[[percentile]] <- results_list[[percentile]][, c("Data_Name","Ex_super","N","N_h","N_b",
                                                                        "coef_h_conv2","coef_h_bc2", "h","b","beta_Y_p_l_h","beta_Y_p_r_h",
                                                                        "beta_T_p_l_h","beta_T_p_r_h"
  )]
}
quarterly[["2024_1Q"]] <- results_list

# 24 1Q detail ------------------------------------------------------------------

tex_data <- data.frame(
  "Condition" = rep(c("FULL", "Q1Q2", "Q2Q3", "Q3Q4", "Q1", "Q2", "Q3", "Q4"), each = 2),
  "Ex_super" = rep(c("t", "f"), 8),
  "coef_h_conv" = round(cbind(results_list[[1]]$coef_h_conv,
                              results_list[[2]]$coef_h_conv,
                              results_list[[3]]$coef_h_conv,
                              results_list[[4]]$coef_h_conv,
                              results_list[[5]]$coef_h_conv),3),
  "se_h_conv" = round(cbind(results_list[[1]]$se_h_conv, 
                            results_list[[2]]$se_h_conv,
                            results_list[[3]]$se_h_conv,
                            results_list[[4]]$se_h_conv,
                            results_list[[5]]$se_h_conv),3),
  "pv_h_conv" = cbind(results_list[[1]]$pv_h_conv,
                      results_list[[2]]$pv_h_conv,
                      results_list[[3]]$pv_h_conv,
                      results_list[[4]]$pv_h_conv,
                      results_list[[5]]$pv_h_conv
  ),
  "bws_h" = cbind(results_list[[1]]$h),
  "N_h" = cbind(results_list[[1]]$N_h,
                results_list[[2]]$N_h,
                results_list[[3]]$N_h,
                results_list[[4]]$N_h,
                results_list[[5]]$N_h
  ))
tex_data <- tex_data %>%
  mutate(Ex_super = factor(Ex_super, levels = c("t", "f"))) %>%
  arrange(Ex_super)
tex_table_detail <- "\\begin{table}[]\n \\TABLE \n\ {2024 Q1 \\label{2024_Q1_t}} {\\begin{tabular}{lccccc}\n"
tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) &(5) \\\\ \\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==1}}\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")


# 각 조건에 대해 행 추가
for (i in 1:8) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", bws_h_conv, "&", "&","&","&", "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
  
}

tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")

tex_table_detail <- paste(tex_table_detail, "\\begin{table}[]\n \\TABLE \n\ {2024 Q1 \\label{2024_Q1_f}} {\\begin{tabular}{lccccc}\n", sep="")
tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) &(5) \\\\ \\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==0}} \\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
for (i in 9:16) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", bws_h_conv, "&", "&","&","&", "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
}

# 테이블 끝 마무리
tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
writeLines(tex_table_detail, "5-3. 2024_Q1_act.tex")
# TeX 코드 출력
cat(tex_table_detail)


# 24 2Q regression --------------------------------------------------------
z <- data.frame()  # 결과를 저장할 빈 데이터 프레임
z=rbind(Q224)
#summary(z$first_month_number_of_reviews)
avglist = list()
exlist= list()
difflist = list()
results1 <- list()
results2 <- list()
results3 <- list()
results4 <- list()
results5 <- list()

conditions <- list(
  "FULL" = (z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1Q2" = (z$ex_q1 == 1 | z$ex_q2 == 1),
  "Q2Q3" = (z$ex_q3 == 1 | z$ex_q2 == 1),
  "Q3Q4" = (z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1" = (z$ex_q1 == 1),
  "Q2" = (z$ex_q2 == 1),
  "Q3" = (z$ex_q3 == 1),
  "Q4" = (z$ex_q4 == 1)
)
summary(z$ex_q4)
#z=z%>%filter(first_month_ltm>=5)
for (super_type in c('f', 't')) {
  for (condition_name in names(conditions)) {
    condition <- conditions[[condition_name]]
    
    filtered_data <- z %>%
      filter(ex_super == super_type & condition)
    
    if (nrow(filtered_data) > 0) {
      margin <- filtered_data$running_scr - 4.75
      
      result_name <- paste(pct, super_type, condition_name, sep = "_")
      
      tryCatch({
        est1 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est2 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(2*est1[['bws']][1,1], 2*est1[['bws']][1,2]),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est3 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.2, 0.1),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est4 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.3, 0.15),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est5 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.4, 0.2),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        avg <- summary(filtered_data$avg_price)
        ex <- summary(filtered_data$ex_avg)
        diff <- summary((filtered_data$avg_price - filtered_data$ex_avg) / filtered_data$ex_avg)
        
        results1[[result_name]] <- est1
        results2[[result_name]] <- est2
        results3[[result_name]] <- est3
        results4[[result_name]] <- est4
        results5[[result_name]] <- est5
        
        avglist[[result_name]] <- avg
        exlist[[result_name]] <- ex
        difflist[[result_name]] <- diff
        
      }, error = function(e) {
        message(sprintf("Error in %s: %s", result_name, e$message))
        # 오류가 나면 해당 반복만 건너뜀
      })
    }
  }
}
results_list <-list()
bandwidths <- c("h")
bandwidths <- c("h")

# 현재 퍼센타일에 대한 결과를 저장할 데이터 프레임 초기화
estimate_matrix_df <- data.frame(
  "Data_Name" = rep(c("FULL", "Q1Q2", "Q2Q3","Q3Q4","Q1","Q2","Q3","Q4"), each = 2),  # "Full", "Q1Q2", "Q3Q4"에 대해 t, f 순서 반복
  "Ex_super" = rep(c("t", "f"), 8),  # Covariate 열 추가
  "N" = numeric(16),  # h/2에 해당하는 pv 열
  stringsAsFactors = FALSE
)
# results_list 초기화
results_list <- list()

for (j in 1:5) {
  list_name <- paste0('results', j)  # "results1"이 됩니다.
  
  # 각 Data_Name에 대해 Estimate 값과 Covariate 값을 채우기
  for (i in 1:nrow(estimate_matrix_df)) {
    data_name <- estimate_matrix_df$Data_Name[i]
    Ex_super <- estimate_matrix_df$Ex_super[i]  # t, f를 구분
    
    for (bw in bandwidths) {
      # bandwidth별로 결과 key 생성
      result_key <- paste('0.05', Ex_super, data_name, sep = "_") # Bandwidth와 type 포함된 key 형성
      
      # get()을 사용하여 해당 리스트를 가져옴
      result_data <- get(list_name)[[result_key]]
      
      # result_key가 존재하면 Estimate 값을 채움
      if (!is.null(result_data)) {
        estimate_matrix_df[i, paste0("coef_", bw, "_conv")] <- result_data[["Estimate"]][1]
        estimate_matrix_df[i, paste0("coef_", bw, "_bc")] <- result_data[["Estimate"]][2]
        estimate_matrix_df[i, paste0("se_", bw, "_conv")] <- result_data[["se"]][1]
        estimate_matrix_df[i, paste0("se_", bw, "_bc")] <- result_data[["se"]][2]
        estimate_matrix_df[i, paste0("pv_", bw, "_conv")] <- result_data[["pv"]][1]
        estimate_matrix_df[i, paste0("pv_", bw, "_bc")] <- result_data[["pv"]][2]
        estimate_matrix_df[i, paste0("beta_Y_p_l_", bw)] <- result_data[["beta_Y_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_Y_p_r_", bw)] <- result_data[["beta_Y_p_r"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_l_", bw)] <- result_data[["beta_T_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_r_", bw)] <- result_data[["beta_T_p_r"]][1]
        
        # N_h, N_b, h, b 값 형성
        estimate_matrix_df$N_h[i] <- paste("[", paste(result_data[["N_h"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$N_b[i] <- paste("[", paste(result_data[["N_b"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$h[i] <- paste("[", paste(round(result_data[["bws"]][1,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$b[i] <- paste("[", paste(round(result_data[["bws"]][2,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$obs_h[i] <- result_data[["N_h"]][1]+result_data[["N_h"]][2]
        estimate_matrix_df$obs_b[i] <- result_data[["N_b"]][1]+result_data[["N_b"]][2]
        
        # N 값 합산
        n_values <- result_data[["N"]]
        estimate_matrix_df$N[i] <- sum(n_values)
        
        # 결과 저장
        results_list[[as.character(j)]] <- estimate_matrix_df
      }
    }
  }
}

percentiles <- c( "0.05")

for (j in c(1:5)) {
  
  results_list[[j]][["coef_h_conv2"]] <- paste(
    round(results_list[[j]][["coef_h_conv"]], 3),ifelse(results_list[[j]][["pv_h_conv"]] < 0.01, "***",
                                                        ifelse(results_list[[j]][["pv_h_conv"]] < 0.05, "**",
                                                               ifelse(results_list[[j]][["pv_h_conv"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_conv"]], 3), ")",
    
    sep = ""
  )
  results_list[[j]][["coef_h_bc2"]] <- paste(
    round(results_list[[j]][["coef_h_bc"]], 3),ifelse(results_list[[j]][["pv_h_bc"]] < 0.01, "***",
                                                      ifelse(results_list[[j]][["pv_h_bc"]] < 0.05, "**",
                                                             ifelse(results_list[[j]][["pv_h_bc"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_bc"]], 3), ")",
    
    sep = ""
  )
  
}
combined_results_list <- list()

# 각 percentile에 대해 h_combined, 2h_combined, h/2_combined, covariate_combined만 추출
for (percentile in names(results_list)) {
  # 각 리스트에서 필요한 열만 추출
  combined_results_list[[percentile]] <- results_list[[percentile]][, c("Data_Name","Ex_super","N","N_h","N_b",
                                                                        "coef_h_conv2","coef_h_bc2", "h","b","beta_Y_p_l_h","beta_Y_p_r_h",
                                                                        "beta_T_p_l_h","beta_T_p_r_h"
  )]
}
quarterly[["2024_2Q"]] <- results_list

# 24 2Q detail ------------------------------------------------------------------

tex_data <- data.frame(
  "Condition" = rep(c("FULL", "Q1Q2", "Q2Q3", "Q3Q4", "Q1", "Q2", "Q3", "Q4"), each = 2),
  "Ex_super" = rep(c("t", "f"), 8),
  "coef_h_conv" = round(cbind(results_list[[1]]$coef_h_conv,
                              results_list[[2]]$coef_h_conv,
                              results_list[[3]]$coef_h_conv,
                              results_list[[4]]$coef_h_conv,
                              results_list[[5]]$coef_h_conv),3),
  "se_h_conv" = round(cbind(results_list[[1]]$se_h_conv, 
                            results_list[[2]]$se_h_conv,
                            results_list[[3]]$se_h_conv,
                            results_list[[4]]$se_h_conv,
                            results_list[[5]]$se_h_conv),3),
  "pv_h_conv" = cbind(results_list[[1]]$pv_h_conv,
                      results_list[[2]]$pv_h_conv,
                      results_list[[3]]$pv_h_conv,
                      results_list[[4]]$pv_h_conv,
                      results_list[[5]]$pv_h_conv
  ),
  "bws_h" = cbind(results_list[[1]]$h),
  "N_h" = cbind(results_list[[1]]$N_h,
                results_list[[2]]$N_h,
                results_list[[3]]$N_h,
                results_list[[4]]$N_h,
                results_list[[5]]$N_h
  ))
tex_data <- tex_data %>%
  mutate(Ex_super = factor(Ex_super, levels = c("t", "f"))) %>%
  arrange(Ex_super)
tex_table_detail <- "\\begin{table}[]\n \\TABLE \n\ {2024 Q2 \\label{2024_Q2_t}} {\\begin{tabular}{lccccc}\n"
tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) & (5)\\\\ \\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==1}}\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")


# 각 조건에 대해 행 추가
for (i in 1:8) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", bws_h_conv, "&", "&","&","&", "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
}

tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")

tex_table_detail <- paste(tex_table_detail, "\\begin{table}[]\n \\TABLE \n\ {2024 Q2 \\label{2024_Q2_f}} {\\begin{tabular}{lccccc}\n", sep="")
tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) &(5) \\\\ \\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==0}} \\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
for (i in 9:16) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", bws_h_conv, "&", "&","&","&", "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
}

# 테이블 끝 마무리
tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
writeLines(tex_table_detail, "5-4. 2024_Q2_act.tex")
# TeX 코드 출력
cat(tex_table_detail)


# 24 3Q regression --------------------------------------------------------
z <- data.frame()  # 결과를 저장할 빈 데이터 프레임
z=rbind(Q324)
#summary(z$first_month_number_of_reviews)
avglist = list()
exlist= list()
difflist = list()
results1 <- list()
results2 <- list()
results3 <- list()
results4 <- list()
results5 <- list()

conditions <- list(
  "FULL" = (z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1Q2" = (z$ex_q1 == 1 | z$ex_q2 == 1),
  "Q2Q3" = (z$ex_q3 == 1 | z$ex_q2 == 1),
  "Q3Q4" = (z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1" = (z$ex_q1 == 1),
  "Q2" = (z$ex_q2 == 1),
  "Q3" = (z$ex_q3 == 1),
  "Q4" = (z$ex_q4 == 1)
)
summary(z$ex_q4)
#z=z%>%filter(first_month_ltm>=5)
for (super_type in c('f', 't')) {
  for (condition_name in names(conditions)) {
    condition <- conditions[[condition_name]]
    
    filtered_data <- z %>%
      filter(ex_super == super_type & condition)
    
    if (nrow(filtered_data) > 0) {
      margin <- filtered_data$running_scr - 4.75
      
      result_name <- paste(pct, super_type, condition_name, sep = "_")
      
      tryCatch({
        est1 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est2 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(2*est1[['bws']][1,1], 2*est1[['bws']][1,2]),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est3 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.2, 0.1),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est4 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.3, 0.15),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est5 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.4, 0.2),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        avg <- summary(filtered_data$avg_price)
        ex <- summary(filtered_data$ex_avg)
        diff <- summary((filtered_data$avg_price - filtered_data$ex_avg) / filtered_data$ex_avg)
        
        results1[[result_name]] <- est1
        results2[[result_name]] <- est2
        results3[[result_name]] <- est3
        results4[[result_name]] <- est4
        results5[[result_name]] <- est5
        
        avglist[[result_name]] <- avg
        exlist[[result_name]] <- ex
        difflist[[result_name]] <- diff
        
      }, error = function(e) {
        message(sprintf("Error in %s: %s", result_name, e$message))
        # 오류가 나면 해당 반복만 건너뜀
      })
    }
  }
}
results_list <-list()
bandwidths <- c("h")
bandwidths <- c("h")

# 현재 퍼센타일에 대한 결과를 저장할 데이터 프레임 초기화
estimate_matrix_df <- data.frame(
  "Data_Name" = rep(c("FULL", "Q1Q2", "Q2Q3","Q3Q4","Q1","Q2","Q3","Q4"), each = 2),  # "Full", "Q1Q2", "Q3Q4"에 대해 t, f 순서 반복
  "Ex_super" = rep(c("t", "f"), 8),  # Covariate 열 추가
  "N" = numeric(16),  # h/2에 해당하는 pv 열
  stringsAsFactors = FALSE
)
# results_list 초기화
results_list <- list()

for (j in 1:5) {
  list_name <- paste0('results', j)  # "results1"이 됩니다.
  
  # 각 Data_Name에 대해 Estimate 값과 Covariate 값을 채우기
  for (i in 1:nrow(estimate_matrix_df)) {
    data_name <- estimate_matrix_df$Data_Name[i]
    Ex_super <- estimate_matrix_df$Ex_super[i]  # t, f를 구분
    
    for (bw in bandwidths) {
      # bandwidth별로 결과 key 생성
      result_key <- paste('0.05', Ex_super, data_name, sep = "_") # Bandwidth와 type 포함된 key 형성
      
      # get()을 사용하여 해당 리스트를 가져옴
      result_data <- get(list_name)[[result_key]]
      
      # result_key가 존재하면 Estimate 값을 채움
      if (!is.null(result_data)) {
        estimate_matrix_df[i, paste0("coef_", bw, "_conv")] <- result_data[["Estimate"]][1]
        estimate_matrix_df[i, paste0("coef_", bw, "_bc")] <- result_data[["Estimate"]][2]
        estimate_matrix_df[i, paste0("se_", bw, "_conv")] <- result_data[["se"]][1]
        estimate_matrix_df[i, paste0("se_", bw, "_bc")] <- result_data[["se"]][2]
        estimate_matrix_df[i, paste0("pv_", bw, "_conv")] <- result_data[["pv"]][1]
        estimate_matrix_df[i, paste0("pv_", bw, "_bc")] <- result_data[["pv"]][2]
        estimate_matrix_df[i, paste0("beta_Y_p_l_", bw)] <- result_data[["beta_Y_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_Y_p_r_", bw)] <- result_data[["beta_Y_p_r"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_l_", bw)] <- result_data[["beta_T_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_r_", bw)] <- result_data[["beta_T_p_r"]][1]
        
        # N_h, N_b, h, b 값 형성
        estimate_matrix_df$N_h[i] <- paste("[", paste(result_data[["N_h"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$N_b[i] <- paste("[", paste(result_data[["N_b"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$h[i] <- paste("[", paste(round(result_data[["bws"]][1,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$b[i] <- paste("[", paste(round(result_data[["bws"]][2,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$obs_h[i] <- result_data[["N_h"]][1]+result_data[["N_h"]][2]
        estimate_matrix_df$obs_b[i] <- result_data[["N_b"]][1]+result_data[["N_b"]][2]
        
        # N 값 합산
        n_values <- result_data[["N"]]
        estimate_matrix_df$N[i] <- sum(n_values)
        
        # 결과 저장
        results_list[[as.character(j)]] <- estimate_matrix_df
      }
    }
  }
}

percentiles <- c( "0.05")

for (j in c(1:5)) {
  
  results_list[[j]][["coef_h_conv2"]] <- paste(
    round(results_list[[j]][["coef_h_conv"]], 3),ifelse(results_list[[j]][["pv_h_conv"]] < 0.01, "***",
                                                        ifelse(results_list[[j]][["pv_h_conv"]] < 0.05, "**",
                                                               ifelse(results_list[[j]][["pv_h_conv"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_conv"]], 3), ")",
    
    sep = ""
  )
  results_list[[j]][["coef_h_bc2"]] <- paste(
    round(results_list[[j]][["coef_h_bc"]], 3),ifelse(results_list[[j]][["pv_h_bc"]] < 0.01, "***",
                                                      ifelse(results_list[[j]][["pv_h_bc"]] < 0.05, "**",
                                                             ifelse(results_list[[j]][["pv_h_bc"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_bc"]], 3), ")",
    
    sep = ""
  )
  
}
combined_results_list <- list()

# 각 percentile에 대해 h_combined, 2h_combined, h/2_combined, covariate_combined만 추출
for (percentile in names(results_list)) {
  # 각 리스트에서 필요한 열만 추출
  combined_results_list[[percentile]] <- results_list[[percentile]][, c("Data_Name","Ex_super","N","N_h","N_b",
                                                                        "coef_h_conv2","coef_h_bc2", "h","b","beta_Y_p_l_h","beta_Y_p_r_h",
                                                                        "beta_T_p_l_h","beta_T_p_r_h"
  )]
}
quarterly[["2024_3Q"]] <- results_list

# 24 3Q detail ------------------------------------------------------------------

tex_data <- data.frame(
  "Condition" = rep(c("FULL", "Q1Q2", "Q2Q3", "Q3Q4", "Q1", "Q2", "Q3", "Q4"), each = 2),
  "Ex_super" = rep(c("t", "f"), 8),
  "coef_h_conv" = round(cbind(results_list[[1]]$coef_h_conv,
                              results_list[[2]]$coef_h_conv,
                              results_list[[3]]$coef_h_conv,
                              results_list[[4]]$coef_h_conv,
                              results_list[[5]]$coef_h_conv),3),
  "se_h_conv" = round(cbind(results_list[[1]]$se_h_conv, 
                            results_list[[2]]$se_h_conv,
                            results_list[[3]]$se_h_conv,
                            results_list[[4]]$se_h_conv,
                            results_list[[5]]$se_h_conv),3),
  "pv_h_conv" = cbind(results_list[[1]]$pv_h_conv,
                      results_list[[2]]$pv_h_conv,
                      results_list[[3]]$pv_h_conv,
                      results_list[[4]]$pv_h_conv,
                      results_list[[5]]$pv_h_conv
  ),
  "bws_h" = cbind(results_list[[1]]$h),
  "N_h" = cbind(results_list[[1]]$N_h,
                results_list[[2]]$N_h,
                results_list[[3]]$N_h,
                results_list[[4]]$N_h,
                results_list[[5]]$N_h
  ))
tex_data <- tex_data %>%
  mutate(Ex_super = factor(Ex_super, levels = c("t", "f"))) %>%
  arrange(Ex_super)
tex_table_detail <- "\\begin{table}[]\n \\TABLE \n\ {2024 Q3 \\label{2024_Q3_t}} {\\begin{tabular}{lccccc}\n"
tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) &(5) \\\\ \\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==1}}\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")


# 각 조건에 대해 행 추가
for (i in 1:8) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", bws_h_conv, "&", "&","&","&", "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
}

tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")

tex_table_detail <- paste(tex_table_detail, "\\begin{table}[]\n \\TABLE \n\ {2024 Q3 \\label{2024_Q3_f}} {\\begin{tabular}{lccccc}\n", sep="")
tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) &(5) \\\\ \\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==0}} \\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
for (i in 9:16) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", bws_h_conv, "&", "&","&","&", "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
}

# 테이블 끝 마무리
tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
writeLines(tex_table_detail, "5-5. 2024_Q3_act.tex")
# TeX 코드 출력
cat(tex_table_detail)


# 24 4Q regression --------------------------------------------------------
z <- data.frame()  # 결과를 저장할 빈 데이터 프레임
z=rbind(Q424)
#summary(z$first_month_number_of_reviews)
avglist = list()
exlist= list()
difflist = list()
results1 <- list()
results2 <- list()
results3 <- list()
results4 <- list()
results5 <- list()

conditions <- list(
  "FULL" = (z$ex_q1 == 1 | z$ex_q2 == 1 | z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1Q2" = (z$ex_q1 == 1 | z$ex_q2 == 1),
  "Q2Q3" = (z$ex_q3 == 1 | z$ex_q2 == 1),
  "Q3Q4" = (z$ex_q3 == 1 | z$ex_q4 == 1),
  "Q1" = (z$ex_q1 == 1),
  "Q2" = (z$ex_q2 == 1),
  "Q3" = (z$ex_q3 == 1),
  "Q4" = (z$ex_q4 == 1)
)
summary(z$ex_q4)
#z=z%>%filter(first_month_ltm>=5)
for (super_type in c('f', 't')) {
  for (condition_name in names(conditions)) {
    condition <- conditions[[condition_name]]
    
    filtered_data <- z %>%
      filter(ex_super == super_type & condition)
    
    if (nrow(filtered_data) > 0) {
      margin <- filtered_data$running_scr - 4.75
      
      result_name <- paste(pct, super_type, condition_name, sep = "_")
      
      tryCatch({
        est1 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est2 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(2*est1[['bws']][1,1], 2*est1[['bws']][1,2]),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est3 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.2, 0.1),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est4 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.3, 0.15),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        est5 <- rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          h = c(0.4, 0.2),
          all = TRUE,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
        
        avg <- summary(filtered_data$avg_price)
        ex <- summary(filtered_data$ex_avg)
        diff <- summary((filtered_data$avg_price - filtered_data$ex_avg) / filtered_data$ex_avg)
        
        results1[[result_name]] <- est1
        results2[[result_name]] <- est2
        results3[[result_name]] <- est3
        results4[[result_name]] <- est4
        results5[[result_name]] <- est5
        
        avglist[[result_name]] <- avg
        exlist[[result_name]] <- ex
        difflist[[result_name]] <- diff
        
      }, error = function(e) {
        message(sprintf("Error in %s: %s", result_name, e$message))
        # 오류가 나면 해당 반복만 건너뜀
      })
    }
  }
}
results_list <-list()
bandwidths <- c("h")
bandwidths <- c("h")

# 현재 퍼센타일에 대한 결과를 저장할 데이터 프레임 초기화
estimate_matrix_df <- data.frame(
  "Data_Name" = rep(c("FULL", "Q1Q2", "Q2Q3","Q3Q4","Q1","Q2","Q3","Q4"), each = 2),  # "Full", "Q1Q2", "Q3Q4"에 대해 t, f 순서 반복
  "Ex_super" = rep(c("t", "f"), 8),  # Covariate 열 추가
  "N" = numeric(16),  # h/2에 해당하는 pv 열
  stringsAsFactors = FALSE
)
# results_list 초기화
results_list <- list()

for (j in 1:5) {
  list_name <- paste0('results', j)  # "results1"이 됩니다.
  
  # 각 Data_Name에 대해 Estimate 값과 Covariate 값을 채우기
  for (i in 1:nrow(estimate_matrix_df)) {
    data_name <- estimate_matrix_df$Data_Name[i]
    Ex_super <- estimate_matrix_df$Ex_super[i]  # t, f를 구분
    
    for (bw in bandwidths) {
      # bandwidth별로 결과 key 생성
      result_key <- paste('0.05', Ex_super, data_name, sep = "_") # Bandwidth와 type 포함된 key 형성
      
      # get()을 사용하여 해당 리스트를 가져옴
      result_data <- get(list_name)[[result_key]]
      
      # result_key가 존재하면 Estimate 값을 채움
      if (!is.null(result_data)) {
        estimate_matrix_df[i, paste0("coef_", bw, "_conv")] <- result_data[["Estimate"]][1]
        estimate_matrix_df[i, paste0("coef_", bw, "_bc")] <- result_data[["Estimate"]][2]
        estimate_matrix_df[i, paste0("se_", bw, "_conv")] <- result_data[["se"]][1]
        estimate_matrix_df[i, paste0("se_", bw, "_bc")] <- result_data[["se"]][2]
        estimate_matrix_df[i, paste0("pv_", bw, "_conv")] <- result_data[["pv"]][1]
        estimate_matrix_df[i, paste0("pv_", bw, "_bc")] <- result_data[["pv"]][2]
        estimate_matrix_df[i, paste0("beta_Y_p_l_", bw)] <- result_data[["beta_Y_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_Y_p_r_", bw)] <- result_data[["beta_Y_p_r"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_l_", bw)] <- result_data[["beta_T_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_r_", bw)] <- result_data[["beta_T_p_r"]][1]
        
        # N_h, N_b, h, b 값 형성
        estimate_matrix_df$N_h[i] <- paste("[", paste(result_data[["N_h"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$N_b[i] <- paste("[", paste(result_data[["N_b"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$h[i] <- paste("[", paste(round(result_data[["bws"]][1,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$b[i] <- paste("[", paste(round(result_data[["bws"]][2,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$obs_h[i] <- result_data[["N_h"]][1]+result_data[["N_h"]][2]
        estimate_matrix_df$obs_b[i] <- result_data[["N_b"]][1]+result_data[["N_b"]][2]
        
        # N 값 합산
        n_values <- result_data[["N"]]
        estimate_matrix_df$N[i] <- sum(n_values)
        
        # 결과 저장
        results_list[[as.character(j)]] <- estimate_matrix_df
      }
    }
  }
}

percentiles <- c( "0.05")

for (j in c(1:5)) {
  
  results_list[[j]][["coef_h_conv2"]] <- paste(
    round(results_list[[j]][["coef_h_conv"]], 3),ifelse(results_list[[j]][["pv_h_conv"]] < 0.01, "***",
                                                        ifelse(results_list[[j]][["pv_h_conv"]] < 0.05, "**",
                                                               ifelse(results_list[[j]][["pv_h_conv"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_conv"]], 3), ")",
    
    sep = ""
  )
  results_list[[j]][["coef_h_bc2"]] <- paste(
    round(results_list[[j]][["coef_h_bc"]], 3),ifelse(results_list[[j]][["pv_h_bc"]] < 0.01, "***",
                                                      ifelse(results_list[[j]][["pv_h_bc"]] < 0.05, "**",
                                                             ifelse(results_list[[j]][["pv_h_bc"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_h_bc"]], 3), ")",
    
    sep = ""
  )
  
}
combined_results_list <- list()

# 각 percentile에 대해 h_combined, 2h_combined, h/2_combined, covariate_combined만 추출
for (percentile in names(results_list)) {
  # 각 리스트에서 필요한 열만 추출
  combined_results_list[[percentile]] <- results_list[[percentile]][, c("Data_Name","Ex_super","N","N_h","N_b",
                                                                        "coef_h_conv2","coef_h_bc2", "h","b","beta_Y_p_l_h","beta_Y_p_r_h",
                                                                        "beta_T_p_l_h","beta_T_p_r_h"
  )]
}
quarterly[["2024_4Q"]] <- results_list

# 24 4Q detail ------------------------------------------------------------------

tex_data <- data.frame(
  "Condition" = rep(c("FULL", "Q1Q2", "Q2Q3", "Q3Q4", "Q1", "Q2", "Q3", "Q4"), each = 2),
  "Ex_super" = rep(c("t", "f"), 8),
  "coef_h_conv" = round(cbind(results_list[[1]]$coef_h_conv,
                              results_list[[2]]$coef_h_conv,
                              results_list[[3]]$coef_h_conv,
                              results_list[[4]]$coef_h_conv,
                              results_list[[5]]$coef_h_conv),3),
  "se_h_conv" = round(cbind(results_list[[1]]$se_h_conv, 
                            results_list[[2]]$se_h_conv,
                            results_list[[3]]$se_h_conv,
                            results_list[[4]]$se_h_conv,
                            results_list[[5]]$se_h_conv),3),
  "pv_h_conv" = cbind(results_list[[1]]$pv_h_conv,
                      results_list[[2]]$pv_h_conv,
                      results_list[[3]]$pv_h_conv,
                      results_list[[4]]$pv_h_conv,
                      results_list[[5]]$pv_h_conv
  ),
  "bws_h" = cbind(results_list[[1]]$h),
  "N_h" = cbind(results_list[[1]]$N_h,
                results_list[[2]]$N_h,
                results_list[[3]]$N_h,
                results_list[[4]]$N_h,
                results_list[[5]]$N_h
  ))
tex_data <- tex_data %>%
  mutate(Ex_super = factor(Ex_super, levels = c("t", "f"))) %>%
  arrange(Ex_super)
tex_table_detail <- "\\begin{table}[]\n \\TABLE \n\ {2024 Q4 \\label{2024_Q4_t}} {\\begin{tabular}{lccccc}\n"
tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) &(5) \\\\ \\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==1}}\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")


# 각 조건에 대해 행 추가
for (i in 1:8) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", bws_h_conv, "&", "&","&","&", "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("",  "&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("\\\\ \n"), sep = "")
  
}
tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")

tex_table_detail <- paste(tex_table_detail, "\\begin{table}[]\n \\TABLE \n\ {2024 Q4 \\label{2024_Q4_f}} {\\begin{tabular}{lccccc}\n", sep="")
tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) &(5) \\\\ \\hline\n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==0}} \\\\ \n", sep = "")
tex_table_detail <- paste(tex_table_detail, "\\\\ \n", sep = "")

for (i in 9:16) {
  condition <- tex_data$Condition[i]
  ex_super <- tex_data$Ex_super[i]
  
  # coef와 se를 숫자 형식으로 처리
  coef_h_conv <- tex_data$coef_h_conv.1[i]
  se_h_conv <- tex_data$se_h_conv.1[i]
  pv_h_conv <- tex_data$pv_h_conv.1[i]
  bws_h_conv <- tex_data$bws_h[i]
  N_h <- tex_data$N_h.1[i]
  
  coef_h_conv2 <- tex_data$coef_h_conv.2[i]
  se_h_conv2 <- tex_data$se_h_conv.2[i]
  pv_h_conv2 <- tex_data$pv_h_conv.2[i]
  N_h2 <- tex_data$N_h.2[i]
  
  coef_h_conv3 <- tex_data$coef_h_conv.3[i]
  se_h_conv3 <- tex_data$se_h_conv.3[i]
  pv_h_conv3 <- tex_data$pv_h_conv.3[i]
  N_h3 <- tex_data$N_h.3[i]
  
  coef_h_conv4 <- tex_data$coef_h_conv.4[i]
  se_h_conv4 <- tex_data$se_h_conv.4[i]
  pv_h_conv4 <- tex_data$pv_h_conv.4[i]
  N_h4 <- tex_data$N_h.4[i]
  
  coef_h_conv5 <- tex_data$coef_h_conv.5[i]
  se_h_conv5 <- tex_data$se_h_conv.5[i]
  pv_h_conv5 <- tex_data$pv_h_conv.5[i]
  N_h5 <- tex_data$N_h.5[i]
  
  
  
  # p-value에 따라 별을 추가
  coef_h_conv_star <- paste(coef_h_conv, ifelse(pv_h_conv < 0.01, "***", ifelse(pv_h_conv < 0.05, "**", ifelse(pv_h_conv < 0.1, "*", ""))), sep = "")
  coef_h_conv_star2 <- paste(coef_h_conv2, ifelse(pv_h_conv2 < 0.01, "***", ifelse(pv_h_conv2 < 0.05, "**", ifelse(pv_h_conv2 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star3 <- paste(coef_h_conv3, ifelse(pv_h_conv3 < 0.01, "***", ifelse(pv_h_conv3 < 0.05, "**", ifelse(pv_h_conv3 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star4 <- paste(coef_h_conv4, ifelse(pv_h_conv4 < 0.01, "***", ifelse(pv_h_conv4 < 0.05, "**", ifelse(pv_h_conv4 < 0.1, "*", ""))), sep = "")
  coef_h_conv_star5 <- paste(coef_h_conv5, ifelse(pv_h_conv5 < 0.01, "***", ifelse(pv_h_conv5 < 0.05, "**", ifelse(pv_h_conv5 < 0.1, "*", ""))), sep = "")
  
  # 행 추가
  tex_table_detail <- paste(tex_table_detail, paste(condition,  "&", coef_h_conv_star, "&", coef_h_conv_star2, "&",coef_h_conv_star3,"&",coef_h_conv_star4,"&",coef_h_conv_star5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("&","(",se_h_conv,")&(", se_h_conv2, ") & (",se_h_conv3,")&(",se_h_conv4,")&(",se_h_conv5, ")\\\\ \n",sep=""), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste(  "&", bws_h_conv, "&", "&","&","&", "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste(  "&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5, "\\\\ \n"), sep = "")
  tex_table_detail <- paste(tex_table_detail, paste("\\\\ \n"), sep = "")
  
}

# 테이블 끝 마무리
tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
writeLines(tex_table_detail, "5-6. 2024_Q4_act.tex")
# TeX 코드 출력
cat(tex_table_detail)



save(quarterly, file='quarterly_act.RData')
save.image(file='active.RData')
