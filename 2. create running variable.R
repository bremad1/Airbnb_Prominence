# running variable 만들기
library(dplyr)
library(stringr) #str_extract
library(lubridate)
library(rlang)
library(openxlsx)


#load hosts' rating data out of NYC
foreign =read.csv('scrapped data/foreign.csv',colClasses = c("new_id" = "character") )
foreign = foreign%>%rename(id=new_id)
#load hosts' rating data of NYC

suc_tmp = read.csv('scrapped data/suc_tmp.csv',colClasses = c("new_id" = "character"))
suc_tmp = suc_tmp%>%rename(id=new_id)

host_suc_tmp=read.csv('scrapped data/host_suc_foreign_listing.csv', colClasses = c("new_host_id" = "character","new_id" = 'character'))
host_suc_tmp=host_suc_tmp%>%rename(id=new_id,host_id=new_host_id)
cohost = read.csv('scrapped data/cohost1.csv', colClasses = c("new_host_id" = "character","new_id" = 'character'))
cohost = cohost%>%filter(!is.na(new_host_id))
cohost=cohost%>%rename(id=new_id,host_id=new_host_id)

suc = rbind(suc_tmp,foreign)
################remove cohost (since it does not affect real running variable) & add foreign room##################
load('2. combined_data.RData')
distinct_combinations = combined_data %>%group_by(id,host_id)%>%summarize(count = n(), .groups='drop')%>%
  group_by(id)%>%slice_max(count,n=1,with_ties = F)%>%select(-count)

# Step 1: find repeated combination
multiple_matches <- host_suc_tmp %>%
  group_by(id) %>%
  mutate(unique_hosts = n_distinct(host_id)) %>%
  filter(unique_hosts > 1) %>%
  ungroup()

# Step 2: cohost 데이터에 있는 조합 우선 선택
selected_combinations <- multiple_matches %>%
  inner_join(cohost, by = c("id", "host_id"))

# Step 3: selected_combinations에 포함된 new_id 제외
remaining_data <- host_suc_tmp %>%
  filter(!id %in% selected_combinations$id)

# Step 4: 중복 중 첫 번째 조합 선택 (selected_combinations에 없는 경우만) -->measurement error
remaining_combinations <- multiple_matches %>%
  filter(!id %in% selected_combinations$id) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

# Step 5: create final combination of (id, host_id)
z <- bind_rows(
  selected_combinations,
  remaining_data %>%
    anti_join(multiple_matches, by = "id"), # 중복이 아닌 조합 추가
  remaining_combinations
) %>%
  distinct(id,host_id)  # 중복 제거

# Step 6 : combine it with original dataset
distinct_combinations <- rbind(distinct_combinations, z) %>%
  group_by(id) %>%
  slice(1) %>%  # 그룹 내에서 첫 번째 행만 남김
  ungroup()
# Step 7 : Check repeated combination
distinct_combinations %>%
  group_by(id) %>%
  filter(n_distinct(host_id) > 1)
distinct_combinations %>%
  count(id) %>%
  filter(n > 1)
############################# Change date form of the review data############################ 
reviews = suc
reviews$date <- ifelse(
  reviews$review_num >= 1 & is.na(reviews$date_raw) | reviews$date_raw=='1 day ago', 
  "November 2024",  # 2024-11-01로 설정
  reviews$date_raw  # 기존 값도 Date 형식으로 변환
)
month_map <- setNames(month.name, month.abb)

# 데이터 변환 함수
format_date <- function(date_raw) {
  sapply(date_raw, function(date) {
    # 이미 올바른 형식 (예: "January 2024")인 경우 그대로 반환
    if (grepl("^[A-Za-z]+ \\d{4}$", date)) {
      return(date)
    }
    # 21-Nov 또는 Nov-21 형태를 처리
    date_standardized <- gsub("(\\d+)-(\\w+)", "\\2 \\1", date) # 숫자-문자 -> 문자-숫자
    date_standardized <- gsub("(\\w+)-(\\d+)", "\\1 \\2", date_standardized) # 문자-숫자 그대로 유지
    
    # 월 풀네임으로 변환
    parts <- strsplit(date_standardized, " ")[[1]]
    if (length(parts) == 2) {
      month_full <- month_map[parts[1]] # 약칭을 풀네임으로
      year <- ifelse(as.numeric(parts[2]) < 100, paste0("20", parts[2]), parts[2]) # 연도 형식 통일
      return(paste(month_full, year))
    }
    return(NA) # 처리할 수 없는 형식은 NA 반환
  })
}

# 변환 적용
reviews$date2 <- format_date(reviews$date)
reviews$date_column <- str_extract(
  reviews$date2, 
  "(?i)(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec|January|February|March|April|May|June|July|August|September|October|November|December)\\s*\\d{4}"
)

# 2. 추출된 날짜 문자열을 "2014-01-01" 형식으로 변환
reviews$date_column <- parse_date_time(reviews$date_column, orders = c("b Y", "B Y"))

#check matching
distinct_values <- reviews %>%
  distinct(date_column,date_raw) %>%
  arrange(date_column) 
reviews_with_host = reviews%>%left_join(distinct_combinations,by="id")
###############keep necessary dataset#############################
#j = reviews_with_host%>%filter(!is.na(date_raw) & is.na(date_value))
reviews_with_host = reviews_with_host%>%filter(year(date_column)>=2020)
reviews$date_column <- as.Date(reviews$date_column)
str(reviews$date_column)

reviews_with_host <- reviews_with_host %>%
  mutate(
    # 날짜 기준 변수
    running_23jul = if_else(date_column >= as.Date("2022-07-01") & date_column <= as.Date("2023-06-30"), rating, NA_real_),
    running_23oct = if_else(date_column >= as.Date("2022-10-01") & date_column <= as.Date("2023-09-30"), rating, NA_real_),
    running_24jan = if_else(date_column >= as.Date("2023-01-01") & date_column <= as.Date("2023-12-31"), rating, NA_real_),
    running_24apr = if_else(date_column >= as.Date("2023-04-01") & date_column <= as.Date("2024-03-30"), rating, NA_real_),
    running_24jul = if_else(date_column >= as.Date("2023-07-01") & date_column <= as.Date("2024-06-30"), rating, NA_real_),
    running_24oct = if_else(date_column >= as.Date("2023-10-01") & date_column <= as.Date("2024-09-30"), rating, NA_real_)
  )

reviews_with_host %>%
  summarise(distinct_count = n_distinct(id))
# 각 new_id별로 요약
summary_result <- reviews_with_host %>%
  group_by(id) %>%
  summarize(
    running_23jul_sum = sum(running_23jul, na.rm = TRUE),
    running_23jul_count = sum(!is.na(running_23jul), na.rm = TRUE),
    running_23oct_sum = sum(running_23oct, na.rm = TRUE),
    running_23oct_count = sum(!is.na(running_23oct), na.rm = TRUE),
    running_24jan_sum = sum(running_24jan, na.rm = TRUE),
    running_24jan_count = sum(!is.na(running_24jan), na.rm = TRUE),
    running_24apr_sum = sum(running_24apr, na.rm = TRUE),
    running_24apr_count = sum(!is.na(running_24apr), na.rm = TRUE),
    running_24jul_sum = sum(running_24jul, na.rm = TRUE),
    running_24jul_count = sum(!is.na(running_24jul), na.rm = TRUE),
    running_24oct_sum = sum(running_24oct, na.rm = TRUE),
    running_24oct_count = sum(!is.na(running_24oct), na.rm = TRUE)
  )
summary_result_with_host <- reviews_with_host %>%
  select(id, host_id) %>%
  distinct() %>%
  right_join(summary_result, by = "id")

# 2. host_id별로 다시 요약
final_result <- summary_result_with_host %>%
  group_by(host_id) %>%
  summarize(
    total_running_23jul_sum = sum(running_23jul_sum, na.rm = TRUE),
    total_running_23jul_count = sum(running_23jul_count, na.rm = TRUE),
    total_running_23jul_avg = total_running_23jul_sum / total_running_23jul_count,
    total_running_23oct_sum = sum(running_23oct_sum, na.rm = TRUE),
    total_running_23oct_count = sum(running_23oct_count, na.rm = TRUE),
    total_running_23oct_avg = total_running_23oct_sum / total_running_23oct_count,
    total_running_24jan_sum = sum(running_24jan_sum, na.rm = TRUE),
    total_running_24jan_count = sum(running_24jan_count, na.rm = TRUE),
    total_running_24jan_avg = total_running_24jan_sum / total_running_24jan_count,
    total_running_24apr_sum = sum(running_24apr_sum, na.rm = TRUE),
    total_running_24apr_count = sum(running_24apr_count, na.rm = TRUE),
    total_running_24apr_avg = total_running_24apr_sum / total_running_24apr_count,
    total_running_24jul_sum = sum(running_24jul_sum, na.rm = TRUE),
    total_running_24jul_count = sum(running_24jul_count, na.rm = TRUE),
    total_running_24jul_avg = total_running_24jul_sum / total_running_24jul_count,
    total_running_24oct_sum = sum(running_24oct_sum, na.rm = TRUE),
    total_running_24oct_count = sum(running_24oct_count, na.rm = TRUE),
    total_running_24oct_avg = total_running_24oct_sum / total_running_24oct_count,
  )
#create running variable
final_data2 <- combined_data %>%
  left_join(final_result, by = "host_id") %>%
  mutate(
    running_scr = case_when(
      year(Date) == 2023 & month(Date) %in% c(7, 8, 9) ~ total_running_23jul_avg,
      year(Date) == 2023 & month(Date) %in% c(10, 11, 12) ~ total_running_23oct_avg,
      year(Date) == 2024 & month(Date) %in% c(1, 2, 3) ~ total_running_24jan_avg,
      year(Date) == 2024 & month(Date) %in% c(4, 5, 6) ~ total_running_24apr_avg,
      year(Date) == 2024 & month(Date) %in% c(7, 8, 9) ~ total_running_24jul_avg,
      year(Date) == 2024 & month(Date) %in% c(10, 11, 12) ~ total_running_24oct_avg,
      TRUE ~ NA_real_
    ),
    ltm_scr = case_when(
      year(Date) == 2023 & month(Date) %in% c(7, 8, 9) ~ total_running_23jul_count,
      year(Date) == 2023 & month(Date) %in% c(10, 11, 12) ~ total_running_23oct_count,
      year(Date) == 2024 & month(Date) %in% c(1, 2, 3) ~ total_running_24jan_count,
      year(Date) == 2024 & month(Date) %in% c(4, 5, 6) ~ total_running_24apr_count,
      year(Date) == 2024 & month(Date) %in% c(7, 8, 9) ~ total_running_24jul_count,
      year(Date) == 2024 & month(Date) %in% c(10, 11, 12) ~ total_running_24oct_count,
      TRUE ~ NA_real_
    )
  )
summary(final_data2$running_scr)

ex_quarter_ltm <- final_data2 %>%
  select(Date, id, number_of_reviews_ltm) %>%
  distinct(Date, id, .keep_all = TRUE) %>%  # 중복된 Date와 host_id 조합 제거
  arrange(id, Date) %>%
  group_by(id) %>%
  mutate(
    ex_quarter_ltm = case_when(
      # 2023년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 3 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 3 & year(Date) == 2023])), 
               NA_real_),
      
      # 2023년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 6 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 6 & year(Date) == 2023])), 
               NA_real_),
      
      # 2023년 10, 11, 12월에는 같은 해 8월의 host_is_superhost 값을 사용
      month(Date) %in% c(10, 11, 12) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 9 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 9 & year(Date) == 2023])), 
               NA_real_),
      
      # 2024년 1, 2, 3월에는 2023년 11월의 host_is_superhost 값을 사용
      month(Date) %in% c(1, 2, 3) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 12 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 12 & year(Date) == 2023])), 
               NA_real_),
      
      # 2024년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 3 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 3 & year(Date) == 2024])), 
               NA_real_),
      
      # 2024년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 6 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 6 & year(Date) == 2024])), 
               NA_real_),
      
      month(Date) %in% c(10, 11, 12) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 9 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 9 & year(Date) == 2024])), 
               NA_real_),
      
      
      # 나머지는 NA로 설정
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

ex_quarter_rating <- final_data2 %>%
  select(Date, id, review_scores_rating) %>%
  distinct(Date, id, .keep_all = TRUE) %>%  # 중복된 Date와 host_id 조합 제거
  arrange(id, Date) %>%
  group_by(id) %>%
  mutate(
    ex_quarter_rating = case_when(
      # 2023년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 3 & year(Date) == 2023), 
               as.numeric(first(review_scores_rating[month(Date) == 3 & year(Date) == 2023])), 
               NA_real_),
      
      # 2023년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 6 & year(Date) == 2023), 
               as.numeric(first(review_scores_rating[month(Date) == 6 & year(Date) == 2023])), 
               NA_real_),
      
      # 2023년 10, 11, 12월에는 같은 해 8월의 host_is_superhost 값을 사용
      month(Date) %in% c(10, 11, 12) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 9 & year(Date) == 2023), 
               as.numeric(first(review_scores_rating[month(Date) == 9 & year(Date) == 2023])), 
               NA_real_),
      
      # 2024년 1, 2, 3월에는 2023년 11월의 host_is_superhost 값을 사용
      month(Date) %in% c(1, 2, 3) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 12 & year(Date) == 2023), 
               as.numeric(first(review_scores_rating[month(Date) == 12 & year(Date) == 2023])), 
               NA_real_),
      
      # 2024년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 3 & year(Date) == 2024), 
               as.numeric(first(review_scores_rating[month(Date) == 3 & year(Date) == 2024])), 
               NA_real_),
      
      # 2024년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 6 & year(Date) == 2024), 
               as.numeric(first(review_scores_rating[month(Date) == 6 & year(Date) == 2024])), 
               NA_real_),
      
      month(Date) %in% c(10, 11, 12) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 9 & year(Date) == 2024), 
               as.numeric(first(review_scores_rating[month(Date) == 9 & year(Date) == 2024])), 
               NA_real_),
      
      
      # 나머지는 NA로 설정
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

ex_quarter_number_of_reviews <- final_data2 %>%
  select(Date, id, number_of_reviews) %>%
  distinct(Date, id, .keep_all = TRUE) %>%  # 중복된 Date와 host_id 조합 제거
  arrange(id, Date) %>%
  group_by(id) %>%
  mutate(
    ex_quarter_number_of_reviews = case_when(
      # 2023년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 3 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews[month(Date) == 3 & year(Date) == 2023])), 
               NA_real_),
      
      # 2023년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 6 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews[month(Date) == 6 & year(Date) == 2023])), 
               NA_real_),
      
      # 2023년 10, 11, 12월에는 같은 해 8월의 host_is_superhost 값을 사용
      month(Date) %in% c(10, 11, 12) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 9 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews[month(Date) == 9 & year(Date) == 2023])), 
               NA_real_),
      
      # 2024년 1, 2, 3월에는 2023년 11월의 host_is_superhost 값을 사용
      month(Date) %in% c(1, 2, 3) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 12 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews[month(Date) == 12 & year(Date) == 2023])), 
               NA_real_),
      
      # 2024년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 3 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews[month(Date) == 3 & year(Date) == 2024])), 
               NA_real_),
      
      # 2024년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 6 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews[month(Date) == 6 & year(Date) == 2024])), 
               NA_real_),
      
      month(Date) %in% c(10, 11, 12) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 9 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews[month(Date) == 9 & year(Date) == 2024])), 
               NA_real_),
      
      
      # 나머지는 NA로 설정
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

first_month_number_of_reviews <- final_data2 %>%
  select(Date, id, number_of_reviews) %>%
  distinct(Date, id, .keep_all = TRUE) %>%  # 중복된 Date와 host_id 조합 제거
  arrange(id, Date) %>%
  group_by(id) %>%
  mutate(
    first_month_number_of_reviews = case_when(
      # 2023년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 4 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews[month(Date) == 4 & year(Date) == 2023])), 
               NA_real_),
      
      # 2023년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 7 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews[month(Date) == 7 & year(Date) == 2023])), 
               NA_real_),
      
      # 2023년 10, 11, 12월에는 같은 해 8월의 host_is_superhost 값을 사용
      month(Date) %in% c(10, 11, 12) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 10 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews[month(Date) == 10 & year(Date) == 2023])), 
               NA_real_),
      
      # 2024년 1, 2, 3월에는 2023년 11월의 host_is_superhost 값을 사용
      month(Date) %in% c(1, 2, 3) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 1 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews[month(Date) == 1 & year(Date) == 2024])), 
               NA_real_),
      
      # 2024년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 4 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews[month(Date) == 4 & year(Date) == 2024])), 
               NA_real_),
      
      # 2024년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 7 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews[month(Date) == 7 & year(Date) == 2024])), 
               NA_real_),
      
      month(Date) %in% c(10, 11, 12) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 10 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews[month(Date) == 10 & year(Date) == 2024])), 
               NA_real_),
      
      
      # 나머지는 NA로 설정
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()


first_month_ltm <- final_data2 %>%
  select(Date, id, number_of_reviews_ltm) %>%
  distinct(Date, id, .keep_all = TRUE) %>%  # 중복된 Date와 host_id 조합 제거
  arrange(id, Date) %>%
  group_by(id) %>%
  mutate(
    first_month_ltm = case_when(
      # 2023년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 4 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 4 & year(Date) == 2023])), 
               NA_real_),
      
      # 2023년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 7 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 7 & year(Date) == 2023])), 
               NA_real_),
      
      # 2023년 10, 11, 12월에는 같은 해 8월의 host_is_superhost 값을 사용
      month(Date) %in% c(10, 11, 12) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 10 & year(Date) == 2023), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 10 & year(Date) == 2023])), 
               NA_real_),
      
      # 2024년 1, 2, 3월에는 2023년 11월의 host_is_superhost 값을 사용
      month(Date) %in% c(1, 2, 3) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 1 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 1 & year(Date) == 2024])), 
               NA_real_),
      
      # 2024년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 4 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 4 & year(Date) == 2024])), 
               NA_real_),
      
      # 2024년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 7 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 7 & year(Date) == 2024])), 
               NA_real_),
      
      month(Date) %in% c(10, 11, 12) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 10 & year(Date) == 2024), 
               as.numeric(first(number_of_reviews_ltm[month(Date) == 10 & year(Date) == 2024])), 
               NA_real_),
      
      
      # 나머지는 NA로 설정
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

first_month_rating <- final_data2 %>%
  select(Date, id, review_scores_rating) %>%
  distinct(Date, id, .keep_all = TRUE) %>%  # 중복된 Date와 host_id 조합 제거
  arrange(id, Date) %>%
  group_by(id) %>%
  mutate(
    first_month_rating = case_when(
      # 2023년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 4 & year(Date) == 2023), 
               as.numeric(first(review_scores_rating[month(Date) == 4 & year(Date) == 2023])), 
               NA_real_),
      
      # 2023년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 7 & year(Date) == 2023), 
               as.numeric(first(review_scores_rating[month(Date) == 7 & year(Date) == 2023])), 
               NA_real_),
      
      # 2023년 10, 11, 12월에는 같은 해 8월의 host_is_superhost 값을 사용
      month(Date) %in% c(10, 11, 12) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 10 & year(Date) == 2023), 
               as.numeric(first(review_scores_rating[month(Date) == 10 & year(Date) == 2023])), 
               NA_real_),
      
      # 2024년 1, 2, 3월에는 2023년 11월의 host_is_superhost 값을 사용
      month(Date) %in% c(1, 2, 3) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 1 & year(Date) == 2024), 
               as.numeric(first(review_scores_rating[month(Date) == 1 & year(Date) == 2024])), 
               NA_real_),
      
      # 2024년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 4 & year(Date) == 2024), 
               as.numeric(first(review_scores_rating[month(Date) == 4 & year(Date) == 2024])), 
               NA_real_),
      
      # 2024년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 7 & year(Date) == 2024), 
               as.numeric(first(review_scores_rating[month(Date) == 7 & year(Date) == 2024])), 
               NA_real_),
      
      month(Date) %in% c(10, 11, 12) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 10 & year(Date) == 2024), 
               as.numeric(first(review_scores_rating[month(Date) == 10 & year(Date) == 2024])), 
               NA_real_),
      
      
      # 나머지는 NA로 설정
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()
final_data2 = final_data2%>%left_join(ex_quarter_ltm%>%select(Date,id,ex_quarter_ltm), by=c('Date','id'))
final_data2 = final_data2%>%left_join(ex_quarter_rating%>%select(Date,id,ex_quarter_rating), by=c('Date','id'))
final_data2 = final_data2%>%left_join(ex_quarter_number_of_reviews%>%select(Date,id,ex_quarter_number_of_reviews), by=c('Date','id'))
final_data2 = final_data2%>%left_join(first_month_number_of_reviews%>%select(Date,id,first_month_number_of_reviews), by=c('Date','id'))
final_data2 = final_data2%>%left_join(first_month_ltm%>%select(Date,id,first_month_ltm), by=c('Date','id'))
final_data2 = final_data2%>%left_join(first_month_rating%>%select(Date,id,first_month_rating), by=c('Date','id'))


summary(final_data2$first_month_rating)


save(final_data2, file='final_data2.RData')

#####################
load('final_data2.RData')
#check duplicates
# final_data2 %>%
#   mutate(year_month = paste(year(Date), month(Date), sep = "-")) %>% # year-month 조합 생성
#   group_by(id, year_month) %>%
#   filter(n() > 1) %>% 
#   arrange(id, year_month)


#remove extreme value
Entire_0.05 <- final_data2 %>% 
  group_by(year(Date),month(Date))%>%
  filter(room_type =='Entire home/apt')%>%filter(price > quantile(price, probs = 0.05, na.rm = T) & price <quantile(price,probs = 0.95,na.rm =T))%>%ungroup()%>%
  arrange(id, Date) %>% 
  group_by(id) %>%       # id별로 그룹화
  mutate(
    lag_date = dplyr::lag(Date),
    date_diff = 12 * (year(Date) - year(lag_date)) + (month(Date) - month(lag_date)),
    ex_price1 = if_else(date_diff == 1, dplyr::lag(price, 1), NA_real_),
    ex_price2 = if_else(date_diff == 2, dplyr::lag(price, 1), 
                        if_else(date_diff == 1, dplyr::lag(price, 2), NA_real_)),
    ex_price3 = if_else(date_diff == 3, dplyr::lag(price, 1), 
                        if_else(date_diff == 2, dplyr::lag(price, 2), 
                                if_else(date_diff == 1, dplyr::lag(price, 3), NA_real_))),
    ex_price4 = if_else(date_diff ==4, dplyr::lag(price,1),
                        if_else(date_diff == 3, dplyr::lag(price, 2), 
                                if_else(date_diff == 2, dplyr::lag(price, 3), 
                                        if_else(date_diff == 1, dplyr::lag(price, 4), NA_real_)))),
    ex_price5 = if_else(date_diff ==5, dplyr::lag(price,1),
                        if_else(date_diff == 4, dplyr::lag(price, 2), 
                                if_else(date_diff == 3, dplyr::lag(price, 3), 
                                        if_else(date_diff == 2, dplyr::lag(price, 4),
                                                if_else(date_diff == 1, dplyr::lag(price, 5), NA_real_)))))  
  ) %>%
  ungroup()
str(final_data2$Date)
Entire <- final_data2 %>% 
  filter(room_type =='Entire home/apt')%>%
  arrange(id, Date) %>% 
  group_by(id) %>%       # id별로 그룹화
  mutate(
    lag_date = dplyr::lag(Date),
    date_diff = 12 * (year(Date) - year(lag_date)) + (month(Date) - month(lag_date)),
    ex_price1 = if_else(date_diff == 1, dplyr::lag(price, 1), NA_real_),
    ex_price2 = if_else(date_diff == 2, dplyr::lag(price, 1), 
                        if_else(date_diff == 1, dplyr::lag(price, 2), NA_real_)),
    ex_price3 = if_else(date_diff == 3, dplyr::lag(price, 1), 
                        if_else(date_diff == 2, dplyr::lag(price, 2), 
                                if_else(date_diff == 1, dplyr::lag(price, 3), NA_real_))),
    ex_price4 = if_else(date_diff ==4, dplyr::lag(price,1),
                        if_else(date_diff == 3, dplyr::lag(price, 2), 
                                if_else(date_diff == 2, dplyr::lag(price, 3), 
                                        if_else(date_diff == 1, dplyr::lag(price, 4), NA_real_)))),
    ex_price5 = if_else(date_diff ==5, dplyr::lag(price,1),
                        if_else(date_diff == 4, dplyr::lag(price, 2), 
                                if_else(date_diff == 3, dplyr::lag(price, 3), 
                                        if_else(date_diff == 2, dplyr::lag(price, 4),
                                                if_else(date_diff == 1, dplyr::lag(price, 5), NA_real_)))))  
  ) %>%
  ungroup()
#lag는 dplyr과 stats 패키지 두 가지에 둘어있는데, 충돌을 막기 위해 dplyr::lag로 명시해주는 것이 좋음. date_diff=0으로 나오는 것 방지 가능

Private_0.05 <- final_data2 %>% 
  group_by(year(Date),month(Date))%>%
  filter(room_type =='Private room')%>%filter(price > quantile(price, probs = 0.05, na.rm = T) & price <quantile(price,probs = 0.95,na.rm =T))%>%ungroup()%>%
  arrange(id, Date) %>% 
  group_by(id) %>%       # id별로 그룹화
  mutate(
    lag_date = dplyr::lag(Date),
    date_diff = 12 * (year(Date) - year(lag_date)) + (month(Date) - month(lag_date)),
    ex_price1 = if_else(date_diff == 1, dplyr::lag(price, 1), NA_real_),
    ex_price2 = if_else(date_diff == 2, dplyr::lag(price, 1), 
                        if_else(date_diff == 1, dplyr::lag(price, 2), NA_real_)),
    ex_price3 = if_else(date_diff == 3, dplyr::lag(price, 1), 
                        if_else(date_diff == 2, dplyr::lag(price, 2), 
                                if_else(date_diff == 1, dplyr::lag(price, 3), NA_real_))),
    ex_price4 = if_else(date_diff ==4, dplyr::lag(price,1),
                        if_else(date_diff == 3, dplyr::lag(price, 2), 
                                if_else(date_diff == 2, dplyr::lag(price, 3), 
                                        if_else(date_diff == 1, dplyr::lag(price, 4), NA_real_)))),
    ex_price5 = if_else(date_diff ==5, dplyr::lag(price,1),
                        if_else(date_diff == 4, dplyr::lag(price, 2), 
                                if_else(date_diff == 3, dplyr::lag(price, 3), 
                                        if_else(date_diff == 2, dplyr::lag(price, 4),
                                                if_else(date_diff == 1, dplyr::lag(price, 5), NA_real_)))))  
  ) %>%
  ungroup()

Private <- final_data2 %>% 
  filter(room_type =='Private room')%>%ungroup()%>%
  arrange(id, Date) %>% 
  group_by(id) %>%       # id별로 그룹화
  mutate(
    lag_date = dplyr::lag(Date),
    date_diff = 12 * (year(Date) - year(lag_date)) + (month(Date) - month(lag_date)),
    ex_price1 = if_else(date_diff == 1, dplyr::lag(price, 1), NA_real_),
    ex_price2 = if_else(date_diff == 2, dplyr::lag(price, 1), 
                        if_else(date_diff == 1, dplyr::lag(price, 2), NA_real_)),
    ex_price3 = if_else(date_diff == 3, dplyr::lag(price, 1), 
                        if_else(date_diff == 2, dplyr::lag(price, 2), 
                                if_else(date_diff == 1, dplyr::lag(price, 3), NA_real_))),
    ex_price4 = if_else(date_diff ==4, dplyr::lag(price,1),
                        if_else(date_diff == 3, dplyr::lag(price, 2), 
                                if_else(date_diff == 2, dplyr::lag(price, 3), 
                                        if_else(date_diff == 1, dplyr::lag(price, 4), NA_real_)))),
    ex_price5 = if_else(date_diff ==5, dplyr::lag(price,1),
                        if_else(date_diff == 4, dplyr::lag(price, 2), 
                                if_else(date_diff == 3, dplyr::lag(price, 3), 
                                        if_else(date_diff == 2, dplyr::lag(price, 4),
                                                if_else(date_diff == 1, dplyr::lag(price, 5), NA_real_)))))  
  ) %>%
  ungroup()
Total <- final_data2 %>% 
  group_by(year(Date),month(Date))%>%
  arrange(id, Date) %>% 
  group_by(id) %>%       # id별로 그룹화
  mutate(
    lag_date = dplyr::lag(Date),
    date_diff = 12 * (year(Date) - year(lag_date)) + (month(Date) - month(lag_date)),
    ex_price1 = if_else(date_diff == 1, dplyr::lag(price, 1), NA_real_),
    ex_price2 = if_else(date_diff == 2, dplyr::lag(price, 1), 
                        if_else(date_diff == 1, dplyr::lag(price, 2), NA_real_)),
    ex_price3 = if_else(date_diff == 3, dplyr::lag(price, 1), 
                        if_else(date_diff == 2, dplyr::lag(price, 2), 
                                if_else(date_diff == 1, dplyr::lag(price, 3), NA_real_))),
    ex_price4 = if_else(date_diff ==4, dplyr::lag(price,1),
                        if_else(date_diff == 3, dplyr::lag(price, 2), 
                                if_else(date_diff == 2, dplyr::lag(price, 3), 
                                        if_else(date_diff == 1, dplyr::lag(price, 4), NA_real_)))),
    ex_price5 = if_else(date_diff ==5, dplyr::lag(price,1),
                        if_else(date_diff == 4, dplyr::lag(price, 2), 
                                if_else(date_diff == 3, dplyr::lag(price, 3), 
                                        if_else(date_diff == 2, dplyr::lag(price, 4),
                                                if_else(date_diff == 1, dplyr::lag(price, 5), NA_real_)))))  
  ) %>%
  ungroup()


#Entire = Entire%>%left_join(ex_running%>%select(Date,host_id,ex_running), by=c('Date','host_id'))
#Private = Private%>%left_join(ex_running%>%select(Date,host_id,ex_running), by=c('Date','host_id'))


ex2 <- Entire %>%
  select(Date, host_id, host_is_superhost) %>% #keep necessary variable only
  distinct(Date, host_id, .keep_all = TRUE) %>%  # remove repeated (Date, host_id) combination
  arrange(host_id, Date) %>%
  group_by(host_id) %>%
  mutate( #create variable that indicates superhost status of last period
    ex_super2 = case_when( 
      
      
      # 2023년 10, 11, 12월에는 같은 해 8월의 host_is_superhost 값을 사용
      month(Date) %in% c(10, 11, 12) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 5 & year(Date) == 2023), 
               as.character(first(host_is_superhost[month(Date) == 5 & year(Date) == 2023])), 
               NA_character_),
      
      # 2024년 1, 2, 3월에는 2023년 11월의 host_is_superhost 값을 사용
      month(Date) %in% c(1, 2, 3) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 8 & year(Date) == 2023), 
               as.character(first(host_is_superhost[month(Date) == 8 & year(Date) == 2023])), 
               NA_character_),
      
      # 2024년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 11 & year(Date) == 2023), 
               as.character(first(host_is_superhost[month(Date) == 11 & year(Date) == 2023])), 
               NA_character_),
      
      # 2024년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 2 & year(Date) == 2024), 
               as.character(first(host_is_superhost[month(Date) == 2 & year(Date) == 2024])), 
               NA_character_),
      
      month(Date) %in% c(10, 11, 12) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 5 & year(Date) == 2024), 
               as.character(first(host_is_superhost[month(Date) ==5 & year(Date) == 2024])), 
               NA_character_),
      
      
      # 나머지는 NA로 설정
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

Entire = Entire%>%left_join(ex2%>%select(Date,host_id,ex_super2), by=c('Date','host_id'))
Entire$ex_super2

save(Entire_0.05, file='Entire_0.05.RData')
save(Private_0.05, file='Private_0.05.RData')
save(Entire, file='Entire.RData')
save(Private, file ='Private.RData')
save(Total, file ='Total.RData')

