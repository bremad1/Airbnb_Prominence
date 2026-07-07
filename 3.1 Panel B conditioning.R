# Statistics
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
library(ggtext)
library(rddensity)
library(rdd)

setwd('C:/Users/sim/Desktop/iCloudDrive/iCloudDrive/4-1/Airbnb/Test')
#load('quarterly.RData')
load('RData/Entire.RData')
load('Quarterly_dataset1.RData')
# load('RData/results_list.RData')

# 1. ex_super2==1 ---------------------------------------------------------------
z <- data.frame()  # 결과를 저장할 빈 데이터 프레임
z=rbind(Q323,Q423,Q124,Q224,Q324,Q424)
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
z=z%>%filter(ex_quarter_ltm>=5)
nrow(z%>%filter(ex_super =='f'))
nrow(z%>%filter(ex_super =='f' & ex_super2=='t'))
# z=z%>%filter(ex_super2=='t')
source('func/2-1. RunRD_Ex2.R') #For ex_ssuper2=='t
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


save_table2(detail=F,robust=F)
save_table2(detail=T,robust=F)
save_table2(detail=F,robust=T)
save_table2(detail=T,robust=T)




# 2. (Fail) Ex_running -----------------------------------------------------------

ex <- Entire %>%
  select(Date, host_id, running_scr) %>% #keep necessary variable only
  distinct(Date, host_id, .keep_all = TRUE) %>%  # remove repeated (Date, host_id) combination
  arrange(host_id, Date) %>%
  group_by(host_id) %>%
  mutate( #create variable that indicates superhost status of last period
    ex_running = case_when( 
      # 2023년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 2 & year(Date) == 2023), 
               as.character(first(running_scr[month(Date) == 2 & year(Date) == 2023])), 
               NA_character_),
      
      # 2023년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 5 & year(Date) == 2023), 
               as.character(first(running_scr[month(Date) == 5 & year(Date) == 2023])), 
               NA_character_),
      
      # 2023년 10, 11, 12월에는 같은 해 8월의 host_is_superhost 값을 사용
      month(Date) %in% c(10, 11, 12) & year(Date) == 2023 ~ 
        ifelse(any(month(Date) == 8 & year(Date) == 2023), 
               as.character(first(running_scr[month(Date) == 8 & year(Date) == 2023])), 
               NA_character_),
      
      # 2024년 1, 2, 3월에는 2023년 11월의 host_is_superhost 값을 사용
      month(Date) %in% c(1, 2, 3) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 11 & year(Date) == 2023), 
               as.character(first(running_scr[month(Date) == 11 & year(Date) == 2023])), 
               NA_character_),
      
      # 2024년 4, 5, 6월에는 같은 해 2월의 host_is_superhost 값을 사용
      month(Date) %in% c(4, 5, 6) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 2 & year(Date) == 2024), 
               as.character(first(running_scr[month(Date) == 2 & year(Date) == 2024])), 
               NA_character_),
      
      # 2024년 7, 8, 9월에는 같은 해 5월의 host_is_superhost 값을 사용
      month(Date) %in% c(7, 8, 9) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 5 & year(Date) == 2024), 
               as.character(first(running_scr[month(Date) == 5 & year(Date) == 2024])), 
               NA_character_),
      
      month(Date) %in% c(10, 11, 12) & year(Date) == 2024 ~ 
        ifelse(any(month(Date) == 8 & year(Date) == 2024), 
               as.character(first(running_scr[month(Date) == 8 & year(Date) == 2024])), 
               NA_character_),
      
      
      # 나머지는 NA로 설정
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

Entire = Entire%>%left_join(ex%>%select(Date,host_id,ex_running), by=c('Date','host_id'))

test= Entire%>%select(Date,host_id,ex_super,ex_super2,running_scr,ex_running)


# 2-1. Generate Quarterly Dataset1 -------------------------------------------

source('func/1. generate_quarterly_dataset.R')

#valid_months =filtering id who had consistent superhost status in the previous quarter.

# 2023 3Q
result_Q323 <- generate_quarterly_dataset(
  data = Entire,
  valid_months = c(5,6),
  analysis_months = c(4,5,6,7,8,9),
  target_year = 2023,
  target_months = c(8,9),
  cross_year = FALSE
)
Q323 <- result_Q323$temp_data
Q323_superhost_ratio_by_quartile <- result_Q323$superhost_ratio_by_quartile
Q323_summary_by_quartile <- result_Q323$summary_by_quartile


# 2023 4Q
result_Q423 <- generate_quarterly_dataset(
  data = Entire,
  valid_months = c(8,9),
  analysis_months = c(7,8,9,10,11,12),
  target_year = 2023,
  target_months = c(11,12),
  cross_year = FALSE
)
Q423 <- result_Q423$temp_data
Q423_superhost_ratio_by_quartile <- result_Q423$superhost_ratio_by_quartile
Q423_summary_by_quartile <- result_Q423$summary_by_quartile

# 2024 1Q (跨연도 cross_year = TRUE)
result_Q124 <- generate_quarterly_dataset(
  data = Entire,
  valid_months = c(11,12),
  analysis_months = c(10,11,12,1,2,3),
  target_year = 2024,
  target_months = c(2,3),
  cross_year = TRUE
)
Q124 <- result_Q124$temp_data
Q124_superhost_ratio_by_quartile <- result_Q124$superhost_ratio_by_quartile
Q124_summary_by_quartile <- result_Q124$summary_by_quartile

# 2024 2Q
result_Q224 <- generate_quarterly_dataset(
  data = Entire,
  valid_months = c(2,3),
  analysis_months = c(1,2,3,4,5,6),
  target_year = 2024,
  target_months = c(5,6),
  cross_year = FALSE
)
Q224 <- result_Q224$temp_data
Q224_superhost_ratio_by_quartile <- result_Q224$superhost_ratio_by_quartile
Q224_summary_by_quartile <- result_Q224$summary_by_quartile

# 2024 3Q
result_Q324 <- generate_quarterly_dataset(
  data = Entire,
  valid_months = c(5,6),
  analysis_months = c(4,5,6,7,8,9),
  target_year = 2024,
  target_months = c(8,9),
  cross_year = FALSE
)
Q324 <- result_Q324$temp_data
Q324_superhost_ratio_by_quartile <- result_Q324$superhost_ratio_by_quartile
Q324_summary_by_quartile <- result_Q324$summary_by_quartile

# 2024 4Q
result_Q424 <- generate_quarterly_dataset(
  data = Entire,
  valid_months = c(8,9),
  analysis_months = c(7,8,9,10,11,12),
  target_year = 2024,
  target_months = c(11,12),
  cross_year = FALSE
)
Q424 <- result_Q424$temp_data
Q424_superhost_ratio_by_quartile <- result_Q424$superhost_ratio_by_quartile
Q424_summary_by_quartile <- result_Q424$summary_by_quartile

# 3. ltm>=5 + others ---------------------------------------------------------------
z <- data.frame()  # 결과를 저장할 빈 데이터 프레임
z=rbind(Q323,Q423,Q124,Q224,Q324,Q424)
z$date3 <- factor(z$Date)
z$date3_ym <- format(as.Date(z$date3), "%Y-%m")
z$date3_ym
#z=z%>%filter(review_scores_rating>=4.5)
pct=0.05
results1 <- list()
results2 <- list()
results3 <- list()
results4 <- list()
results5 <- list()
results6 <- list()

z=z%>%filter(ex_quarter_ltm>=4 & ex_quarter_rating>=4)

source('func/2. RunRD.R')
runrd(z,F)
nrow(z%>%filter(ex_quarter_ltm>=5 & (ex_q1==1 | ex_q2 ==1) & ex_super=='f'))
nrow(z%>%filter(ex_quarter_ltm>=5 & (ex_q2==1 | ex_q3 ==1) & ex_super=='f'))
nrow(z%>%filter(ex_quarter_ltm>=5 & (ex_q3==1 | ex_q4 ==1) & ex_super=='f'))
nrow(z%>%filter(ex_super=='f'))
nrow(z%>%filter(ex_super=='f' &ex_super2=='t'))
nrow(z%>%filter(ex_super=='f' & ex_quarter_ltm >=5 ))
#test= Entire%>%select(Date,id,host_id,ex_quarter_ltm, calculated_host_listings_count)%>%filter(host_id==465866457)

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
save_table2(detail=F,robust=F, level=F)


# RDplot -------------------------------------------------------------
for (superhost in c('t','f')){
  if (superhost=='t'){
    results = results1[[9]] }
  else { results = results1[[1]]}
  data = z%>%filter(ex_super== superhost &!is.na(host_is_superhost) & !is.na(id) &
                      host_is_superhost!='' &!is.na(ex_avg) & ex_avg!='' &ex_super2=='t')
  data$margin = data$running_scr-4.75
  data= data%>%filter(-results$bws[1,1]<= margin & margin <= results$bws[1,2])
  dummy_vars <- as.data.frame(model.matrix(~ date3_ym - 1, data = data))
  dummy_vars <- dummy_vars[, -1]
  rd_plot = rdplot(y=data$price_diff,
                   x = data$margin ,subset=-results$bws[1,1]<= data$margin & data$margin <= results$bws[1,2], 
                   kernel="tri", p=1, scale=1,covs = cbind(dummy_vars),
                   title="RD Plot: Airbnb", 
                   y.label="",
                   x.label="Rating for Host",masspoints = 'off')
  rd_plot_first = rdplot(y=data$host_is_superhost2,x = data$margin , subset=-results$bws[1,1]<= data$margin & data$margin <= results$bws[1,2],
                         kernel="tri", p=1, scale=1,
                         title="", 
                         y.label="",
                         x.label="Rating for Host",masspoints = 'off')
  rd_plot
  # 왼쪽, 오른쪽 데이터 나누기
  left_data <- data %>% filter(margin >= -results$bws[1,1], margin < 0)
  right_data <- data %>% filter(margin >= 0, margin <= results$bws[1,2])
  
  # 왼쪽, 오른쪽 따로 선형회귀
  left_fit <- lm(price_diff ~ margin, data = left_data)
  right_fit <- lm(price_diff ~ margin, data = right_data)
  
  left_fit_first <- lm(host_is_superhost2~margin,data = left_data )
  right_fit_first <-lm(host_is_superhost2~margin,data = right_data )
  # 예측값과 표준오차 얻기
  
  left_pred <- predict(left_fit, newdata = left_data, se.fit = TRUE)
  right_pred <- predict(right_fit, newdata = right_data, se.fit = TRUE)
  
  left_pred_first <- predict(left_fit_first, newdata = left_data, se.fit = TRUE)
  right_pred_first <- predict(right_fit_first, newdata = right_data, se.fit = TRUE)
  
  cutoff_point <- data.frame(margin = 0)
  left_cutoff <- predict(left_fit, newdata = cutoff_point, se.fit = TRUE)
  left_cutoff_first <- predict(left_fit_first, newdata = cutoff_point, se.fit = TRUE)
  
  # 기존 예측값에 margin == 0 값 추가
  left_pred$fit     <- c(left_pred$fit, left_cutoff$fit)
  left_pred$se.fit  <- c(left_pred$se.fit, left_cutoff$se.fit)
  
  left_pred_first$fit     <- c(left_pred_first$fit, left_cutoff_first$fit)
  left_pred_first$se.fit  <- c(left_pred_first$se.fit, left_cutoff_first$se.fit)
  # Scatter points (rdplot의 bin center와 평균 y값)
  scatter_data <- data.frame(
    x = rd_plot$vars_bins$rdplot_mean_bin,
    y = rd_plot$vars_bins$rdplot_mean_y
  )
  scatter_data_first <- data.frame(
    x = rd_plot_first$vars_bins$rdplot_mean_bin,
    y = rd_plot_first$vars_bins$rdplot_mean_y
  )
  # Fitted line points
  cutoff_row <- data.frame(margin = 0)
  for (col in setdiff(names(left_data), "margin")) {
    cutoff_row[[col]] <- NA
  }
  left_data <- bind_rows(left_data, cutoff_row)
  
  fitted_data <- data.frame(
    x = c(left_data$margin, right_data$margin),
    y = c(left_pred$fit, right_pred$fit),
    group = c(rep("Left Fit", length(left_pred$fit)), rep("Right Fit", length(right_pred$fit)))
  )
  
  
  fitted_data_first <- data.frame(
    x = c(left_data$margin, right_data$margin),
    y = c(left_pred_first$fit, right_pred_first$fit),
    group = c(rep("Left Fit", length(left_pred_first$fit)), rep("Right Fit", length(right_pred_first$fit)))
  )
  # Confidence interval points
  confidence_data <- data.frame(
    x = c(left_data$margin, right_data$margin),
    ymin = c(left_pred$fit - 1.96 * left_pred$se.fit, right_pred$fit - 1.96 * right_pred$se.fit),
    ymax = c(left_pred$fit + 1.96 * left_pred$se.fit, right_pred$fit + 1.96 * right_pred$se.fit)
  )
  
  confidence_data_first <- data.frame(
    x = c(left_data$margin, right_data$margin),
    ymin = c(left_pred_first$fit - 1.96 * left_pred_first$se.fit, right_pred_first$fit - 1.96 * right_pred_first$se.fit),
    ymax = c(left_pred_first$fit + 1.96 * left_pred_first$se.fit, right_pred_first$fit + 1.96 * right_pred_first$se.fit)
  )
  
  idx_right_cutoff <- which.min(right_data$margin)
  
  right_cutoff_ymax <- right_pred$fit[idx_right_cutoff] +
    1.96 * right_pred$se.fit[idx_right_cutoff]
  
  right_cutoff_ymax
  
  
  # 1.1 Plot 1
  plot_final <- ggplot() +
    geom_point(data = scatter_data, aes(x = x, y = y), color = "black") +
    geom_line(data = fitted_data, aes(x = x, y = y, color = group), linewidth = 1) +
    geom_ribbon(data = confidence_data, aes(x = x, ymin = ymin, ymax = ymax),
                alpha = 0.5, fill = "grey30") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    labs(
      x = "Distance to Rating cutoff",
      y = NULL,
      #subtitle = if(superhost == 't') "Panel A" else "Panel B"
    ) +
    scale_color_manual(values = c("black", "black")) +
    #coord_cartesian(xlim = range(scatter_data$x),ylim = c(-0.1, 0.1)) +
    coord_cartesian(xlim = c(-0.1,0.1),ylim = c(-0.1,0.1)) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 20),      # 눈금 숫자 굵게 + 크기
      axis.title.x = element_text(size = 30)
      #,   # x축 라벨 굵게 + 크기
      #plot.subtitle = element_text(hjust = 0, size = 15,                   # Panel A/B 굵게 + 크기 ↑
      #  margin = margin(t = 0, b = 10))
      ,
      legend.position = "none"
    )
  plot_final
  plot_first <- ggplot() +
    geom_point(data = scatter_data_first, aes(x = x, y = y), color = "black") +
    geom_line(data = fitted_data_first, aes(x = x, y = y, color = group), size = 1) +
    geom_ribbon(data = confidence_data_first, aes(x = x, ymin = ymin, ymax = ymax),
                alpha = 0.5, fill = "grey30") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    coord_cartesian(xlim = c(-0.1,0.1),ylim = c(-0,1)) +
    labs(
      x = "Distance to Rating cutoff",
      y= NULL,
      subtitle = if(superhost=='t') "Panel A" else "Panel B"
    ) +
    scale_color_manual(values = c("black", "black")) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 20),      # 눈금 숫자 굵게 + 크기
      axis.title.x = element_text(size = 30)
      #,   # x축 라벨 굵게 + 크기
      #plot.subtitle = element_text(hjust = 0, size = 15,                   # Panel A/B 굵게 + 크기 ↑
      #  margin = margin(t = 0, b = 10))
      ,
      legend.position = "none"
    )
  
  if (superhost == 't') {
    plot_t <- plot_final
    plot_t_first <- plot_first
  } else {
    plot_f <- plot_final
    plot_f_first <- plot_first
    
  }
  
}



grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(row, col) viewport(layout.pos.row = row, layout.pos.col = col)
plot_f <- plot_f + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
plot_f_first <- plot_f_first + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
print(plot_f_first + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 1))
print(plot_f + geom_hline(yintercept = 0, linetype = "dashed", color = "black") + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 2))

F1_core <- arrangeGrob(plot_t_first + labs(title = NULL, subtitle = NULL),
                       plot_t + labs(title = NULL, subtitle = NULL),
                       ncol = 2)

F1_full <- grobTree(
  F1_core,
  
  # 1행 왼쪽 (First Stage)
  textGrob("First Stage", x = unit(0.02, "npc"), y = unit(0.97, "npc"),
           just = "left", gp = gpar(fontsize = 30)),
  
  # 1행 오른쪽 (Second Stage)
  textGrob("Second Stage", x = unit(0.52, "npc"), y = unit(0.97, "npc"),
           just = "left", gp = gpar(fontsize = 30))
  
  
)
ggsave("Figure/F2_Estimation_B.eps", plot = F1_full, device = cairo_ps, width = 16, height = 8)


# test --------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(rdrobust)

for (superhost in c("t", "f")) {
  
  if (superhost == "t") {
    results <- results1[[9]]
  } else {
    results <- results1[[1]]
  }
  
  h_l <- results$bws[1, 1]
  h_r <- results$bws[1, 2]
  
  data <- z %>%
    filter(
      ex_super == superhost,
      !is.na(host_is_superhost),
      !is.na(id),
      host_is_superhost != "",
      !is.na(ex_avg),
      ex_avg != "",
      ex_super2 == "t"
    )
  
  data$margin <- data$running_scr - 4.75
  
  data <- data %>%
    filter(
      margin >= -h_l,
      margin <=  h_r
    )
  
  dummy_vars <- as.data.frame(model.matrix(~ date3_ym - 1, data = data))
  dummy_vars <- dummy_vars[, -1, drop = FALSE]
  
  # ------------------------------------------------------------
  # 1. rdplot에서 bin point만 가져오기: outcome
  # ------------------------------------------------------------
  
  rd_plot <- rdplot(
    y = data$price_diff,
    x = data$margin,
    h = c(h_l, h_r),
    x.lim = c(-h_l, h_r),
    kernel = "tri",
    p = 1,
    scale = 1,
    title = "RD Plot: Airbnb",
    y.label = "",
    x.label = "Rating for Host",
    masspoints = "off",
    hide = TRUE
  )
  
  scatter_data <- data.frame(
    x = rd_plot$vars_bins$rdplot_mean_bin,
    y = rd_plot$vars_bins$rdplot_mean_y
  )
  
  # ------------------------------------------------------------
  # 2. rdplot에서 bin point만 가져오기: first stage
  # ------------------------------------------------------------
  
  rd_plot_first <- rdplot(
    y = data$host_is_superhost2,
    x = data$margin,
    h = c(h_l, h_r),
    x.lim = c(-h_l, h_r),
    kernel = "tri",
    p = 1,
    scale = 1,
    title = "",
    y.label = "",
    x.label = "Rating for Host",
    masspoints = "off",
    hide = TRUE
  )
  
  scatter_data_first <- data.frame(
    x = rd_plot_first$vars_bins$rdplot_mean_bin,
    y = rd_plot_first$vars_bins$rdplot_mean_y
  )
  
  # ------------------------------------------------------------
  # 3. rdrobust estimates로 outcome fitted line 직접 만들기
  # ------------------------------------------------------------
  
  a_l <- results$beta_Y_p_l[1, 1]
  b_l <- results$beta_Y_p_l[1, 2]
  
  a_r <- results$beta_Y_p_r[1, 1]
  b_r <- results$beta_Y_p_r[1, 2]
  
  line_left <- data.frame(
    margin = seq(-h_l, 0, length.out = 200)
  ) %>%
    mutate(yhat = a_l + b_l * margin)
  
  line_right <- data.frame(
    margin = seq(0, h_r, length.out = 200)
  ) %>%
    mutate(yhat = a_r + b_r * margin)
  
  fitted_data <- bind_rows(
    line_left %>%
      transmute(
        x = margin,
        y = yhat,
        group = "Left Fit"
      ),
    line_right %>%
      transmute(
        x = margin,
        y = yhat,
        group = "Right Fit"
      )
  )
  
  # ------------------------------------------------------------
  # 4. rdrobust estimates로 first-stage fitted line 직접 만들기
  # ------------------------------------------------------------
  
  a_l_first <- results$beta_T_p_l[1, 1]
  b_l_first <- results$beta_T_p_l[1, 2]
  
  a_r_first <- results$beta_T_p_r[1, 1]
  b_r_first <- results$beta_T_p_r[1, 2]
  
  line_left_first <- data.frame(
    margin = seq(-h_l, 0, length.out = 200)
  ) %>%
    mutate(yhat = a_l_first + b_l_first * margin)
  
  line_right_first <- data.frame(
    margin = seq(0, h_r, length.out = 200)
  ) %>%
    mutate(yhat = a_r_first + b_r_first * margin)
  
  fitted_data_first <- bind_rows(
    line_left_first %>%
      transmute(
        x = margin,
        y = yhat,
        group = "Left Fit"
      ),
    line_right_first %>%
      transmute(
        x = margin,
        y = yhat,
        group = "Right Fit"
      )
  )
  
  # ------------------------------------------------------------
  # 5. Outcome plot
  # ------------------------------------------------------------
  
  plot_final <- ggplot() +
    geom_point(
      data = scatter_data,
      aes(x = x, y = y),
      color = "black",
      size = 2
    ) +
    geom_line(
      data = fitted_data,
      aes(x = x, y = y, color = group),
      linewidth = 1
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "black"
    ) +
    labs(
      x = "Distance to Rating cutoff",
      y = NULL
    ) +
    scale_color_manual(values = c("black", "black")) +
    coord_cartesian(
      xlim = c(-0.1, 0.1),
      ylim = c(-0.1, 0.1)
    ) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30),
      legend.position = "none"
    )
  
  plot_final
  
  # ------------------------------------------------------------
  # 6. First-stage plot
  # ------------------------------------------------------------
  
  plot_first <- ggplot() +
    geom_point(
      data = scatter_data_first,
      aes(x = x, y = y),
      color = "black",
      size = 2
    ) +
    geom_line(
      data = fitted_data_first,
      aes(x = x, y = y, color = group),
      linewidth = 1
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "black"
    ) +
    coord_cartesian(
      xlim = c(-0.1, 0.1),
      ylim = c(0, 1)
    ) +
    labs(
      x = "Distance to Rating cutoff",
      y = NULL,
      subtitle = if (superhost == "t") "Panel A" else "Panel B"
    ) +
    scale_color_manual(values = c("black", "black")) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30),
      legend.position = "none"
    )
  
  plot_first
  
  # ------------------------------------------------------------
  # 7. Save plots
  # ------------------------------------------------------------
  
  if (superhost == "t") {
    plot_t <- plot_final
    plot_t_first <- plot_first
  } else {
    plot_f <- plot_final
    plot_f_first <- plot_first
  }
}
