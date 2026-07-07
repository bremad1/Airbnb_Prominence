generate_quarterly_dataset <- function(data, valid_months, analysis_months, target_year, target_months, cross_year = FALSE, pct = 0.05) {
  # Step 1: no_dup
  no_dup <- data %>%
    group_by(id) %>%
    filter(n_distinct(host_id) == 1) %>%
    ungroup()
  
  # Step 2: valid_listings
  if (cross_year==F){
    valid_listings <- no_dup %>%
      filter(
        ((year(Date) == target_year & month(Date) %in% valid_months))
      ) %>%
      filter(!is.na(price) & host_is_superhost != '' & !is.na(host_is_superhost)) %>%
      group_by(host_id) %>%
      filter(n_distinct(host_is_superhost) == 1) %>%
      distinct(id) %>%
      pull(id)
    
    # Step 3: temp_data (analysis months 모두 포함 + target months 필터)
    temp_data <- no_dup %>%
      filter(
        ( (year(Date) == target_year & month(Date) %in% analysis_months)
        )
      ) %>%
      filter(host_is_superhost != '' & !is.na(host_is_superhost) & !is.na(price)) %>%
      group_by(id) %>%
      filter(all(analysis_months %in% month(Date))) %>%
      ungroup() %>%
      filter(
        ( (year(Date) == target_year & month(Date) %in% target_months)
        )
      ) %>%
      filter(id %in% valid_listings) %>%
      group_by(host_id) %>%
      filter(n_distinct(host_is_superhost) == 1) %>%
      ungroup()
  } else {
    valid_listings <-no_dup %>% filter(
      ((year(Date) == target_year -1 & month(Date) %in% valid_months) & !is.na(price) & host_is_superhost != '' & !is.na(host_is_superhost))) %>%
      group_by(host_id) %>%
      filter(n_distinct(host_is_superhost) == 1) %>%
      distinct(id) %>%
      pull(id)
    
    temp_data <- no_dup %>%
      filter(
        ( (year(Date) == target_year-1 & month(Date) %in% analysis_months[c(1,2,3)]) | ((year(Date) == target_year & month(Date) %in% analysis_months[c(4,5,6)])) &
            host_is_superhost != '' & !is.na(host_is_superhost) & !is.na(price))) %>%
      group_by(id) %>%
      filter(all(analysis_months %in% month(Date))) %>%
      ungroup() %>%
      filter(
        ((year(Date) == target_year & month(Date) %in% target_months))
      ) %>%
      filter(id %in% valid_listings) %>%
      group_by(host_id) %>%
      filter(n_distinct(host_is_superhost) == 1) %>%
      ungroup()
  }
  
  
  # Step 4: id별 하나 남기기 + avg_price
  temp_data <- temp_data %>%
    group_by(id) %>%
    mutate(
      avg_price = ifelse(
        sum(!is.na(price)) > 1,
        mean(price, na.rm = TRUE),
        price[!is.na(price)])
    ) %>%
    slice(1) %>%
    ungroup()
  
  # Step 5: ex_avg 계산
  temp_data$ex_avg <- ifelse(
    month(temp_data$Date) %in% c(2,5,8,11),
    rowMeans(cbind(temp_data$ex_price2, temp_data$ex_price3,temp_data$ex_price4), na.rm = TRUE),
    ifelse(
      month(temp_data$Date) %in% c(1,4,7,10),
      rowMeans(cbind(temp_data$ex_price1, temp_data$ex_price2, temp_data$ex_price3), na.rm = TRUE),
      rowMeans(cbind(temp_data$ex_price3, temp_data$ex_price4,temp_data$ex_price2), na.rm = TRUE)
    )
  )
  
  # Step 6: 가격 변화율 필터
  temp_data <- temp_data %>%
    filter(
      ((avg_price - ex_avg) / ex_avg) > quantile((avg_price - ex_avg) / ex_avg, probs = pct, na.rm = TRUE) &
        ((avg_price - ex_avg) / ex_avg) < quantile((avg_price - ex_avg) / ex_avg, probs = 1 - pct, na.rm = TRUE)
    ) %>%
    filter(first_month_ltm >= 1)
  
  # Step 7: ex_q1 ~ ex_q4 정의
  temp_data <- temp_data %>%
    mutate(
      ex_q1 = ifelse(ex_avg < quantile(ex_avg, probs = 0.25), 1, 0),
      ex_q2 = ifelse(ex_avg >= quantile(ex_avg, probs = 0.25) & ex_avg < quantile(ex_avg, probs = 0.5), 1, 0),
      ex_q3 = ifelse(ex_avg >= quantile(ex_avg, probs = 0.5) & ex_avg < quantile(ex_avg, probs = 0.75), 1, 0),
      ex_q4 = ifelse(ex_avg >= quantile(ex_avg, probs = 0.75), 1, 0)
    )
  
  temp_data$price_diff = log(temp_data$avg_price) - log(temp_data$ex_avg)
  
  # Step 8: Superhost ratio by quartile
  superhost_ratio_by_quartile <- temp_data %>%
    mutate(ex_q = case_when(
      ex_q1 == 1 ~ "ex_q1",
      ex_q2 == 1 ~ "ex_q2",
      ex_q3 == 1 ~ "ex_q3",
      ex_q4 == 1 ~ "ex_q4"
    )) %>%
    group_by(ex_q, host_is_superhost) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = host_is_superhost, values_from = count, values_fill = 0) %>%
    rename(Non_superhost = "f", Superhost = "t") %>%
    mutate(obs = Superhost + Non_superhost,
           Superhost_ratio = Superhost / (Superhost + Non_superhost))
  
  # Step 9: Summary statistics
  summary_by_quartile <- temp_data %>%
    mutate(ex_q = case_when(
      ex_q1 == 1 ~ "ex_q1",
      ex_q2 == 1 ~ "ex_q2",
      ex_q3 == 1 ~ "ex_q3",
      ex_q4 == 1 ~ "ex_q4"
    )) %>%
    group_by(ex_q) %>%
    summarise(
      mean_ex_avg = mean(ex_avg, na.rm = TRUE),
      min_ex_avg = min(ex_avg, na.rm = TRUE),
      max_ex_avg = max(ex_avg, na.rm = TRUE),
      sd_ex_avg = sd(ex_avg, na.rm = TRUE),
      mean_price = mean(avg_price, na.rm = TRUE),
      min_price = min(avg_price, na.rm = TRUE),
      max_price = max(avg_price, na.rm = TRUE),
      sd_price = sd(avg_price, na.rm = TRUE),
      mean_price_diff = mean(price_diff, na.rm = TRUE),
      min_price_diff = min(price_diff, na.rm = TRUE),
      max_price_diff = max(price_diff, na.rm = TRUE),
      sd_price_diff = sd(price_diff, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(
      mean__ex = mean_ex_avg, min__ex = min_ex_avg, max__ex = max_ex_avg, sd__ex = sd_ex_avg,
      mean__price = mean_price, min__price = min_price, max__price = max_price, sd__price = sd_price,
      mean__price_diff = mean_price_diff, min__price_diff = min_price_diff, max__price_diff = max_price_diff, sd__price_diff = sd_price_diff
    ) %>%
    pivot_longer(cols = mean__ex:sd__price_diff, 
                 names_to = c(".value", "stat"),
                 names_sep = "__")
  
  return(list(
    temp_data = temp_data,
    superhost_ratio_by_quartile = superhost_ratio_by_quartile,
    summary_by_quartile = summary_by_quartile
  ))
}
