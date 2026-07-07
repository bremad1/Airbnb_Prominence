# 필요한 패키지 로드
library(dplyr)
library(plm)
library(rdrobust)

# run did -----------------------------------------------------------------
run_did_analysis <- function(data1, data2, quarter, direction, local = FALSE) {
  
  # Direction에 따라 슈퍼호스트 여부 설정
  is_superhost <- ifelse(direction == "f->t", TRUE, FALSE)
  
  # Treatment ID 설정
  treatment_ids <- data2 %>%
    filter(host_is_superhost2 == TRUE) %>%
    pull(id) %>%
    unique()
  
  # 공통 ID 추출
  common_ids <- intersect(data1$id, data2$id)
  
  # 공통 ID를 사용하여 조건에 맞는 행 결합
  if (is_superhost) {
    z <- rbind(
      data1 %>% filter(id %in% common_ids, host_is_superhost == 'f'),
      data2 %>% filter(id %in% common_ids, ex_super == 'f')
    )
  } else {
    z <- rbind(
      data1 %>% filter(id %in% common_ids, host_is_superhost == 't'),
      data2 %>% filter(id %in% common_ids, ex_super == 't')
    )
  }
  
  # 로컬 분석 부분
  if (local) {
    # direction에 따라 로컬 분석 대상 설정
    rd_data <- if (direction == "f->t") {
      data2 %>% filter(ex_super == 'f')
    } else {
      data2 %>% filter(ex_super == 't')
    }
    
    margin <- rd_data$running_scr - 4.75
    est1 <- rdrobust(
      y = log(rd_data$avg_price) - log(rd_data$ex_avg),
      x = margin,
      fuzzy = rd_data$host_is_superhost2,
      all = TRUE,
      kernel = "tri",
      cluster= rd_data$id,
      bwselect = 'msetwo',
      p = 1,
      masspoints = 'off',
      bwrestrict = TRUE
    )
    data_name <- deparse(substitute(data2))
    
    nQ <- rd_data %>%
      filter(running_scr >= 4.75 - est1[["bws"]][1, 1] & 
               running_scr <= 4.75 + est1[["bws"]][1, 2])
    
    # 전역 리스트에 저장 (10개 만들기 위해 고유 key 사용)
    local_data_key <- paste0(data_name, "_", direction, "_", quarter, ifelse(local, "_local", "_total"))
    local_data[[local_data_key]] <<- nQ
    
    common_ids <- intersect(data1$id, nQ$id)
    
    # 로컬 데이터 결합
    if (is_superhost) {
      z <- rbind(
        data1 %>% filter(id %in% common_ids, host_is_superhost == 'f'),
        nQ %>% filter(id %in% common_ids, ex_super == 'f')
      )
    } else {
      z <- rbind(
        data1 %>% filter(id %in% common_ids, host_is_superhost == 't'),
        nQ %>% filter(id %in% common_ids, ex_super == 't')
      )
    }
  }
  
  # 데이터 전처리
  z$date3 <- factor(z$Date)
  z$date3_ym <- format(as.Date(z$date3), "%Y-%m")
  z$month <- month(ym(z$date3_ym))
  
  # Post 변수 생성 규칙
  if (quarter == "Q124") {
    z$post <- ifelse(z$month <= 3, 1, 0)
  } else if (quarter == "Q224") {
    z$post <- ifelse(z$month >= 4, 1, 0)
  } else if (quarter == "Q324") {
    z$post <- ifelse(z$month >= 7, 1, 0)
  } else if (quarter == "Q423" || quarter == "Q424") {
    z$post <- ifelse(z$month >= 10, 1, 0)
  }
  
  # Treat 변수 생성
  z$treat <- ifelse(z$id %in% treatment_ids, 1, 0)
  z$id <- as.factor(z$id)
  z$month <- as.factor(z$month)
  z <- z %>% arrange(id, month)
  
  # 데이터 정리 및 패널 데이터 변환
  z <- z %>%
    select(id, host_id, price, price_diff, treat, post, first_month_number_of_reviews, first_month_rating,
            first_month_ltm,  month )
  z <- na.omit(z)
  z <- pdata.frame(z, index = c('id', 'month'))
  
  # 모델 적합
  model_pool <- plm(log(price) ~ treat * post + first_month_rating + first_month_number_of_reviews + 
                      first_month_ltm , data = z, model = "pooling")
  model_within <- plm(log(price) ~ treat * post + first_month_rating + first_month_number_of_reviews + 
                        first_month_ltm , data = z, model = "within", effect = "individual")
  
  return(list(pooling = model_pool, within = model_within))
}

#### Same Results regression###
#z$id_fe <- as.factor(z$id)
#mod_did <- lm(log(price) ~ treat * post + review_scores_rating + number_of_reviews + 
#                first_month_ltm + number_of_reviews_l30d + id_fe,
#              data = z)
#summary_mod <- summary(mod_did)
#coef_treat_post <- summary_mod$coefficients["treat:post", "Estimate"]
#se_treat_post <- summary_mod$coefficients["treat:post", "Std. Error"]
#pval_treat_post <- summary_mod$coefficients["treat:post", "Pr(>|t|)"]

# results -----------------------------------------------------------------



add_stars <- function(coef, pvalue) {
  if (pvalue <= 0.01) {
    return(paste0(formatC(coef, format = "f", digits = 3), "***"))
  } else if (pvalue <= 0.05) {
    return(paste0(formatC(coef, format = "f", digits = 3), "**"))
  } else if (pvalue <= 0.1) {
    return(paste0(formatC(coef, format = "f", digits = 3), "*"))
  } else {
    return(formatC(coef, format = "f", digits = 3))
  }
}

# 결과 정리 함수 (계수, 표준오차, p값 추출)
extract_treat_post <- function(model, direction) {
  summary_mod <- summary(model)
  
  # 모델 결과에서 "treat:post" 계수 추출 시 구조 확인
  if ("treat:post" %in% rownames(summary_mod$coefficients)) {
    coef_treat_post <- summary_mod$coefficients["treat:post", "Estimate"]
    se_treat_post <- summary_mod$coefficients["treat:post", "Std. Error"]
    pval_treat_post <- summary_mod$coefficients["treat:post", "Pr(>|t|)"]
    
    # t->f인 경우 부호 반전
    if (direction == "t->f") {
      coef_treat_post <- coef_treat_post
    }
    
    # 유의성 별표 추가
    coef_with_stars <- add_stars(coef_treat_post, pval_treat_post)
    se_display <- formatC(se_treat_post, format = "f", digits = 3)
    
    return(list(coef = coef_with_stars, se = se_display))
  } else {
    return(list(coef = "NA", se = "NA"))
  }
}

# 테이블 생성 함수
create_result_table <- function(direction, local) {
  # 테이블 초기화 (4x5 구조)
  table <- matrix(NA, nrow = 4, ncol = 5)
  rownames(table) <- c("pooling", "se_pooling", "within", "se_within")
  colnames(table) <- c("Q423", "Q124", "Q224", "Q324", "Q424")
  
  # 5개 분기에 대해 결과 채우기
  quarters <- c("Q423", "Q124", "Q224", "Q324", "Q424")
  for (i in seq_along(quarters)) {
    analysis_name <- paste0(quarters[i], "_", direction, ifelse(local, "_local", "_total"))
    
    # pooling 결과 추출
    pooling_model <- did_results[[analysis_name]][["pooling"]]
    pooling_result <- extract_treat_post(pooling_model, direction)
    table["pooling", i] <- pooling_result$coef
    table["se_pooling", i] <- paste0("(", pooling_result$se, ")")
    
    # within 결과 추출
    within_model <- did_results[[analysis_name]][["within"]]
    within_result <- extract_treat_post(within_model, direction)
    table["within", i] <- within_result$coef
    table["se_within", i] <- paste0("(", within_result$se, ")")
  }
  
  return(table)
}

generate_latex_table <- function(tables) {
  # LaTeX 테이블 헤더
  latex_code <- "\\begin{table}[]\n\\TABLE\n{DID Estimation Results \\label{DID}}\n"
  latex_code <- paste0(latex_code, "{\\begin{tabular}{lccccc}\n")
  latex_code <- paste0(latex_code, "\\hline\n")
  latex_code <- paste0(latex_code,"&", "Q423", "&" ,"Q124" ,"&", "Q224" ,"&", "Q324" ,"&", "Q424", "\\\\")

  # 이스케이프 함수 (특수 문자 처리)
  escape_latex <- function(text) {
    text <- gsub("_", "\\\\_", text)            # _를 \_로 변환
    return(text)
  }
  
  # 테이블 그룹을 Ex_super == 0과 1로 구분하여 LaTeX 코드 생성
  for (ex_super in c(1, 0)) {
    # 그룹 제목
    latex_code <- paste0(latex_code, "\\hline\n")
    latex_code <- paste0(latex_code, "\\\\ \n")
    
    latex_code <- paste0(latex_code, "\\textbf{Ex\\_super == ", ex_super, "} \\\\\n")
    latex_code <- paste0(latex_code, "\\\\ \n")
    
    # Total과 Local 테이블 구분
    for (scope in c("Total", "Local")) {
      latex_code <- paste0(latex_code, "\\textbf{", scope, "} \\\\\n")
      
      # f->t 또는 t->f 테이블을 구분하여 추가
      for (direction in if (ex_super == 0) c("f->t") else c("t->f")) {
        table_name_total <- paste0(direction, ifelse(scope == "Total", "_total", "_local"))
        if (table_name_total %in% names(tables)) {
          table_data <- tables[[table_name_total]]
          
          # 열 제목

          # 행 추가
          for (row_name in rownames(table_data)) {
            escaped_row_name <- escape_latex(row_name)
            row_values <- paste(table_data[row_name, ], collapse = " & ")
            latex_code <- paste0(latex_code, escaped_row_name, " & ", row_values, " \\\\\n")
          }
        }
      }
      latex_code <- paste0(latex_code, "\\\\ \n")
      
    }
  }
  
  # 테이블 끝
  latex_code <- paste0(latex_code, "\\hline\n")
  latex_code <- paste0(latex_code, "\\end{tabular}}{}\n\\end{table}\n")
  return(latex_code)
}

# LaTeX 코드 생성


# plot --------------------------------------------------------------------

make_treatment_plot_quarter <- function(prev_quarter, curr_quarter, prev_label, curr_label, post_months) {
  
  # 1. Treatment ID 정의
  treatment_ids <- curr_quarter %>%
    filter(host_is_superhost2 == TRUE) %>%
    pull(id) %>%
    unique()
  
  # 2. 공통 ID 추출
  common_ids <- intersect(prev_quarter$id, curr_quarter$id)
  
  # 3. 데이터 결합 및 post 처리
  ex_plot_data_f <- bind_rows(
    prev_quarter %>%
      filter(id %in% common_ids, host_is_superhost == 'f') %>%
      mutate(post = 0),
    
    curr_quarter %>%
      filter(id %in% common_ids, ex_super == 'f') %>%
      mutate(month = month(as.Date(Date)),
             post = ifelse(month %in% post_months, 1, 0))
  )
  
  ex_plot_data_t <- bind_rows(
    prev_quarter %>%
      filter(id %in% common_ids, host_is_superhost == 't') %>%
      mutate(post = 0),
    
    curr_quarter %>%
      filter(id %in% common_ids, ex_super == 't') %>%
      mutate(month = month(as.Date(Date)),
             post = ifelse(month %in% post_months, 1, 0))
  )
  
  # 4. treat 변수 생성
  ex_plot_data_f <- ex_plot_data_f %>%
    mutate(treat = ifelse(id %in% treatment_ids, 1, 0))
  
  ex_plot_data_t <- ex_plot_data_t %>%
    mutate(treat = ifelse(id %in% treatment_ids, 1, 0))
  
  # 5. 평균 로그 가격 계산
  mean_price_data_f <- ex_plot_data_f %>%
    group_by(post, treat) %>%
    summarise(mean_price = mean(log(price), na.rm = TRUE), .groups = "drop")
  
  mean_price_data_t <- ex_plot_data_t %>%
    group_by(post, treat) %>%
    summarise(mean_price = mean(log(price), na.rm = TRUE), .groups = "drop")
  
  # 6. 핵심 값 계산
  start_point_f <- mean_price_data_f %>% filter(post == 0, treat == 1) %>% pull(mean_price)
  start_point_t <- mean_price_data_t %>% filter(post == 0, treat == 1) %>% pull(mean_price)
  
  control_slope_f <- mean_price_data_f %>% filter(treat == 0) %>% pull(mean_price)
  control_slope_t <- mean_price_data_t %>% filter(treat == 0) %>% pull(mean_price)
  
  actual_point_f <- mean_price_data_f %>% filter(post == 1, treat == 1) %>% pull(mean_price)
  actual_point_t <- mean_price_data_t %>% filter(post == 1, treat == 1) %>% pull(mean_price)
  
  # 7. 값 체크: 빠지면 NULL 반환
  if (length(start_point_f) == 0 || length(control_slope_f) != 2 || length(actual_point_f) == 0 ||
      length(start_point_t) == 0 || length(control_slope_t) != 2 || length(actual_point_t) == 0) {
    warning("값이 부족하여 plot 생성을 건너뜁니다.")
    return(NULL)
  }
  
  slope_f <- diff(control_slope_f)
  slope_t <- diff(control_slope_t)
  
  end_point_f <- start_point_f + slope_f
  end_point_t <- start_point_t + slope_t
  
  # 8. 텍스트 색상 결정
  effect_color_f <- if (actual_point_f > end_point_f) "#e41a1c" else "#377eb8"
  effect_color_t <- if (actual_point_t > end_point_t) "#e41a1c" else "#377eb8"
  
  # 9. 플롯 생성 (f)
  plot_f <- ggplot(mean_price_data_f, aes(x = as.factor(post), y = mean_price, color = as.factor(treat), group = as.factor(treat))) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    annotate("segment", x = 1, xend = 2, y = start_point_f, yend = end_point_f,
             linetype = "dashed", color = "#ff7f0e", size = 0.8) +
    annotate("segment", x = 2, xend = 2, y = end_point_f, yend = actual_point_f,
             linetype = "solid", color = "black", size = 1,
             arrow = arrow(length = unit(0.2, "cm"), ends = "both")) +
    annotate("text", x = 2.1, y = (end_point_f + actual_point_f) / 2,
             label = "Treatment\nEffect", color = effect_color_f,
             hjust = 0, vjust = 0.5, size = 3, fontface = "bold") +
    scale_x_discrete(labels = c("0" = prev_label, "1" = curr_label)) +
    scale_color_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                       labels = c("0" = "Control", "1" = "Treat")) +
    labs(x = "Time", y = "Mean Log Price", color = "Group") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  # 10. 플롯 생성 (t)
  plot_t <- ggplot(mean_price_data_t, aes(x = as.factor(post), y = mean_price, color = as.factor(treat), group = as.factor(treat))) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    annotate("segment", x = 1, xend = 2, y = start_point_t, yend = end_point_t,
             linetype = "dashed", color = "#ff7f0e", size = 0.8) +
    annotate("segment", x = 2, xend = 2, y = end_point_t, yend = actual_point_t,
             linetype = "solid", color = "black", size = 1,
             arrow = arrow(length = unit(0.2, "cm"), ends = "both")) +
    annotate("text", x = 2.1, y = (end_point_t + actual_point_t) / 2,
             label = "Treatment\nEffect", color = effect_color_t,
             hjust = 0, vjust = 0.5, size = 3, fontface = "bold") +
    scale_x_discrete(labels = c("0" = prev_label, "1" = curr_label)) +
    scale_color_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                       labels = c("0" = "Control", "1" = "Treat")) +
    labs(x = "Time", y = "Mean Log Price", color = "Group") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  return(list(f_plot = plot_f, t_plot = plot_t))
}
