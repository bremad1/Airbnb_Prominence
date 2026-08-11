#ex_super2
library(dplyr)
library(lubridate)
library(purrr)
library(openxlsx)
library(writexl)
library(tidyr)
library(ggplot2)
library(rdrobust)
library(plm)
library(grid)
library(gridExtra)
load('RData/Entire.RData')
load('RData/Quarterly_dataset.RData')
load('RData/Estimation_Results.RData')

# Ex_super2 ---------------------------------------------------------------

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

Entire_with_ex2 = Entire%>%left_join(ex2%>%select(Date,host_id,ex_super2), by=c('Date','host_id'))
Entire_with_ex2$ex_super2
save(Entire_with_ex2,file='Entire_with_ex2.RData')
# 2. Generate Quarterly Dataset -------------------------------------------

source('func/1. generate_quarterly_dataset.R')

# 2024 1Q (跨연도 cross_year = TRUE)
result_Q124 <- generate_quarterly_dataset(
  data = Entire_with_ex2,
  valid_months = c(8,9),
  analysis_months = c(7,8,9,1,2,3),
  target_year = 2024,
  target_months = c(2,3),
  cross_year = TRUE
)
Q124 <- result_Q124$temp_data
Q124_superhost_ratio_by_quartile <- result_Q124$superhost_ratio_by_quartile
Q124_summary_by_quartile <- result_Q124$summary_by_quartile

# 2024 2Q
result_Q224 <- generate_quarterly_dataset(
  data = Entire_with_ex2,
  valid_months = c(11,12),
  analysis_months = c(10,11,12,4,5,6),
  target_year = 2024,
  target_months = c(5,6),
  cross_year = TRUE
)
Q224 <- result_Q224$temp_data
Q224_superhost_ratio_by_quartile <- result_Q224$superhost_ratio_by_quartile
Q224_summary_by_quartile <- result_Q224$summary_by_quartile

# 2024 3Q
result_Q324 <- generate_quarterly_dataset(
  data = Entire_with_ex2,
  valid_months = c(2,3),
  analysis_months = c(1,2,3,7,8,9),
  target_year = 2024,
  target_months = c(8,9),
  cross_year = FALSE
)
Q324 <- result_Q324$temp_data
Q324_superhost_ratio_by_quartile <- result_Q324$superhost_ratio_by_quartile
Q324_summary_by_quartile <- result_Q324$summary_by_quartile

# 2024 4Q
result_Q424 <- generate_quarterly_dataset(
  data = Entire_with_ex2,
  valid_months = c(5,6),
  analysis_months = c(4,5,6,10,11,12),
  target_year = 2024,
  target_months = c(11,12),
  cross_year = FALSE
)
Q424 <- result_Q424$temp_data
Q424_superhost_ratio_by_quartile <- result_Q424$superhost_ratio_by_quartile
Q424_summary_by_quartile <- result_Q424$summary_by_quartile

rm(result_Q124,result_Q224,result_Q324,result_Q424)
#ctrl+shift+c
# save(Q323,Q323_summary_by_quartile,Q323_superhost_ratio_by_quartile,
#      Q423,Q423_summary_by_quartile,Q423_superhost_ratio_by_quartile,
#      Q124,Q124_summary_by_quartile,Q124_superhost_ratio_by_quartile,
#      Q224,Q224_summary_by_quartile,Q224_superhost_ratio_by_quartile,
#      Q324,Q324_summary_by_quartile,Q324_superhost_ratio_by_quartile,
#      Q424,Q424_summary_by_quartile,Q424_superhost_ratio_by_quartile,
#      file = 'Quarterly_dataset.RData'
#      )

z <- data.frame()  # 결과를 저장할 빈 데이터 프레임
z=rbind(Q124,Q224,Q324,Q424)
z$date3 <- factor(z$Date)
z$date3_ym <- format(as.Date(z$date3), "%Y-%m")
z$date3_ym
pct=0.05
results1 <- list()
results2 <- list()
results3 <- list()
results4 <- list()
results5 <- list()
results6 <- list()
source('func/2-1. RunRD_ex2.R')
runrd(z,F)

results_list <-list()
bandwidths <- c("h")
# 현재 퍼센타일에 대한 결과를 저장할 데이터 프레임 초기화
estimate_matrix_df <- data.frame(
  "Data_Name" = rep(c("FULL", "Q1Q2", "Q2Q3","Q3Q4","Q1","Q2","Q3","Q4"), each = 2),  # "Full", "Q1Q2", "Q3Q4"에 대해 t, f 순서 반복
  "Ex_super" = rep(c("t", "f"), 8),  # Covariate 열 추가
  "N" = numeric(16),  # h/2에 해당하는 pv 열
  stringsAsFactors = FALSE
)


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
        estimate_matrix_df[i, paste0("coef_conv")] <- result_data[["Estimate"]][1]
        estimate_matrix_df[i, paste0("coef_bc")] <- result_data[["Estimate"]][2]
        estimate_matrix_df[i, paste0("se_conv")] <- result_data[["se"]][1]
        estimate_matrix_df[i, paste0("se_bc")] <- result_data[["se"]][2]
        estimate_matrix_df[i, paste0("se_robust")] <- result_data[["se"]][3]
        estimate_matrix_df[i, paste0("pv_conv")] <- result_data[["pv"]][1]
        estimate_matrix_df[i, paste0("pv_bc")] <- result_data[["pv"]][2]
        estimate_matrix_df[i, paste0("pv_robust")] <- result_data[["pv"]][3]
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


for (j in c(1:6)) {
  
  results_list[[j]][["coef_h_conv2"]] <- paste(
    round(results_list[[j]][["coef_conv"]], 3),ifelse(results_list[[j]][["pv_conv"]] < 0.01, "***",
                                                      ifelse(results_list[[j]][["pv_conv"]] < 0.05, "**",
                                                             ifelse(results_list[[j]][["pv_conv"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_conv"]], 3), ")",
    
    sep = ""
  )
  results_list[[j]][["coef_bc2"]] <- paste(
    round(results_list[[j]][["coef_bc"]], 3),ifelse(results_list[[j]][["pv_bc"]] < 0.01, "***",
                                                    ifelse(results_list[[j]][["pv_bc"]] < 0.05, "**",
                                                           ifelse(results_list[[j]][["pv_bc"]] < 0.1, "*", ""))),
    " (", round(results_list[[j]][["se_bc"]], 3), ")",
    
    sep = ""
  )
  
}
save_table(detail=F,robust=F)
save_table(detail=T,robust=F)
save_table(detail=F,robust=T)
save_table(detail=T,robust=T)
#save(results1,results2,results3,results4,results5,results6,results_list, file = 'RData/results_list.RData')
#save_table(T)
cat(readLines('tex/Ent.tex'), sep="\n")

