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
#load('Quarterly_dataset1.RData')
#load('RData/Estimation_Results.RData')
# load('RData/results_list.RData')

###### (Not necessary) Panel regression #########

z <- data.frame()
z=rbind(Q323,Q423,Q124,Q224,Q324,Q424)
z$date3 <- factor(z$Date)
z$date3_ym <- format(as.Date(z$date3), "%Y-%m")
z$date3_ym
library(plm)
z$first_month_ltm
z$first_month_number_of_reviews
z$first_month_rating
try=pdata.frame(z%>%filter(ex_super=='t'), index=c('id','date3_ym'))
reg1 = plm(price_diff~host_is_superhost2+first_month_ltm+first_month_number_of_reviews+first_month_rating,data=try) ;summary(reg1)
reg1 = plm(log(price) ~ host_is_superhost2 + first_month_ltm + first_month_number_of_reviews + first_month_rating,
           data = try,
           index = c("id","date3_ym"),           # 개체 식별자 (예: id, host_id 등)
           model = "within")  ;summary(reg1)
reg2 = plm(price_diff ~ host_is_superhost2 + first_month_ltm + first_month_number_of_reviews + first_month_rating,
           data = try,
           index = c("id", "date"),
           model = "within",
           effect = "time") ;summary(reg2)
reg3 = plm(price_diff ~ host_is_superhost2 + first_month_ltm + first_month_number_of_reviews + first_month_rating,
           data = try,
           index = c("id", "date"),
           model = "within",
           effect = "twoways") ;summary(reg3)
#load('Statistics.RData')

# 1. monthly summary table ---------------------------------------------------

# 1.1 monthly_superhost_ratio_by_listings 

start_date <- as.Date("2023-04-01")
end_date <- as.Date("2024-12-31")

monthly_stats <- map_df(seq(start_date, end_date, by = "month"), function(d) {
  Entire %>%
    filter(year(Date) == year(d), month(Date) == month(d), !is.na(price)) %>%
    mutate(host_is_superhost = ifelse(host_is_superhost == "", "NA", ifelse(host_is_superhost=='t',"Superhost", ifelse(host_is_superhost=='f',"Non_superhost",NA))))%>%
    count(host_is_superhost) %>%
    mutate(Date = floor_date(d, "month"))
}) %>%
  pivot_wider(names_from = host_is_superhost, values_from = n, values_fill = 0)

monthly_stats$obs=monthly_stats$'Non_superhost'+monthly_stats$'Superhost'+monthly_stats$'NA'
monthly_stats$superhost_ratio_except_NA = monthly_stats$'Superhost'/(monthly_stats$'Non_superhost'+monthly_stats$'Superhost')


# 1.2 monthly_superhost_ratio_by_hosts 

start_date <- as.Date("2023-04-01")
end_date <- as.Date("2024-12-31")

monthly_host_stats <- map_df(seq(start_date, end_date, by = "month"), function(d) {
  Entire %>%
    filter(year(Date) == year(d), month(Date) == month(d), !is.na(price)) %>%
    mutate(host_is_superhost = ifelse(host_is_superhost == "", "NA", ifelse(host_is_superhost=='t',"Superhost", ifelse(host_is_superhost=='f',"Non_superhost",NA))))%>%
    group_by(host_id) %>%
    summarise(host_is_superhost = first(host_is_superhost), .groups = "drop") %>%
    count(host_is_superhost) %>%
    mutate(Date = floor_date(d, "month"))
})%>%
  pivot_wider(names_from = host_is_superhost, values_from = n, values_fill = 0)

monthly_host_stats$number_of_hosts=monthly_host_stats$'Non_superhost'+monthly_host_stats$'Superhost'+monthly_host_stats$'NA'
monthly_host_stats$superhost_ratio_except_NA = monthly_host_stats$'Superhost'/(monthly_host_stats$'Non_superhost'+monthly_host_stats$'Superhost')
#write_xlsx(list("Listings" = monthly_stats, "Host" = monthly_host_stats), "monthly_superhost_ratio.xlsx")


monthly_table <- "\\begin{table}[]\n \\TABLE \n\ {Raw data monthly superhost listing ratio for Entire home/apt} {\\begin{tabular}{lccccc}\n"
monthly_table <- paste(monthly_table, "\\hline\n", sep = "")
monthly_table <- paste(monthly_table, " Date & Non\\_superhost & Superhost & NA &obs & ratio (\\%) \\\\ \\hline\n", sep = "")
monthly_table <- paste(monthly_table, "\\\\ \n", sep = "")


for (i in 1:21){
  Date = monthly_stats$Date[i]
  Non = monthly_stats$'Non_superhost'[i]
  Sup = monthly_stats$'Superhost'[i]
  `NA` = monthly_stats$'NA'[i]
  obs = monthly_stats$obs[i]
  ratio = round(monthly_stats$superhost_ratio_except_NA[i]*100,3)
  monthly_table <- paste(monthly_table, paste(Date,  "&", Non, "&", Sup, "&",`NA`,"&",obs,"&", ratio, "\\\\ \n", sep = ""))
}
monthly_table <- paste(monthly_table, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
cat(monthly_table)
# TeX 코드 출력

monthly_table <- paste(monthly_table,"\\begin{table}[]\n \\TABLE \n\ {Raw data monthly superhost host ratio for Entire home/apt} {\\begin{tabular}{lccccc}\n",sep="")
monthly_table <- paste(monthly_table, "\\hline\n", sep = "")
monthly_table <- paste(monthly_table, " Date & Non\\_superhost & Superhost & NA & \\# of hosts & ratio (\\%) \\\\ \\hline\n", sep = "")
monthly_table <- paste(monthly_table, "\\\\ \n", sep = "")


for (i in 1:21){
  Date = monthly_host_stats$Date[i]
  Non = monthly_host_stats$'Non_superhost'[i]
  Sup = monthly_host_stats$'Superhost'[i]
  `NA` = monthly_host_stats$'NA'[i]
  obs = monthly_host_stats$number_of_hosts[i]
  ratio = round(monthly_host_stats$superhost_ratio_except_NA[i]*100,3)
  monthly_table <- paste(monthly_table, paste(Date,  "&", Non, "&", Sup, "&",`NA`,"&",obs,"&", ratio, "\\\\ \n", sep = ""))
}
monthly_table <- paste(monthly_table, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")
writeLines(monthly_table, "tex/1. Monthly Statstics.tex")
cat(monthly_table)


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

rm(result_Q323,result_Q423,result_Q124,result_Q224,result_Q324,result_Q424)
#ctrl+shift+c
save(Q323,Q323_summary_by_quartile,Q323_superhost_ratio_by_quartile,
     Q423,Q423_summary_by_quartile,Q423_superhost_ratio_by_quartile,
     Q124,Q124_summary_by_quartile,Q124_superhost_ratio_by_quartile,
     Q224,Q224_summary_by_quartile,Q224_superhost_ratio_by_quartile,
     Q324,Q324_summary_by_quartile,Q324_superhost_ratio_by_quartile,
     Q424,Q424_summary_by_quartile,Q424_superhost_ratio_by_quartile,
     file = 'Quarterly_dataset1.RData'
     )

# 2-2. (Unnecessary) Generate Quarterly Dataset2 -------------------------------------------

source('func/1. generate_quarterly_dataset.R')

#valid_months = filtering id who had consistent superhost status in the previous quarter.

# 2023 3Q
result_Q323 <- generate_quarterly_dataset2(
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
result_Q423 <- generate_quarterly_dataset2(
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
result_Q124 <- generate_quarterly_dataset2(
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
result_Q224 <- generate_quarterly_dataset2(
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
result_Q324 <- generate_quarterly_dataset2(
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
result_Q424 <- generate_quarterly_dataset2(
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

rm(result_Q323,result_Q423,result_Q124,result_Q224,result_Q324,result_Q424)
#ctrl+shift+c
save(Q323,Q323_summary_by_quartile,Q323_superhost_ratio_by_quartile,
     Q423,Q423_summary_by_quartile,Q423_superhost_ratio_by_quartile,
     Q124,Q124_summary_by_quartile,Q124_superhost_ratio_by_quartile,
     Q224,Q224_summary_by_quartile,Q224_superhost_ratio_by_quartile,
     Q324,Q324_summary_by_quartile,Q324_superhost_ratio_by_quartile,
     Q424,Q424_summary_by_quartile,Q424_superhost_ratio_by_quartile,
     file = 'Quarterly_dataset2.RData'
)

# 2-3. (Unnecessary) Generate Quarterly Dataset3 -------------------------------------------

source('func/1. generate_quarterly_dataset.R')

#valid_months =filtering id who had consistent superhost status in the previous quarter.

# 2023 3Q
result_Q323 <- generate_quarterly_dataset3(
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
result_Q423 <- generate_quarterly_dataset3(
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
result_Q124 <- generate_quarterly_dataset3(
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
result_Q224 <- generate_quarterly_dataset3(
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
result_Q324 <- generate_quarterly_dataset3(
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
result_Q424 <- generate_quarterly_dataset3(
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

rm(result_Q323,result_Q423,result_Q124,result_Q224,result_Q324,result_Q424)
#ctrl+shift+c
save(Q323,Q323_summary_by_quartile,Q323_superhost_ratio_by_quartile,
     Q423,Q423_summary_by_quartile,Q423_superhost_ratio_by_quartile,
     Q124,Q124_summary_by_quartile,Q124_superhost_ratio_by_quartile,
     Q224,Q224_summary_by_quartile,Q224_superhost_ratio_by_quartile,
     Q324,Q324_summary_by_quartile,Q324_superhost_ratio_by_quartile,
     Q424,Q424_summary_by_quartile,Q424_superhost_ratio_by_quartile,
     file = 'Quarterly_dataset3.RData'
)

# 2-4. (Unnecessary) Generate Quarterly Dataset4 -------------------------------------------

source('func/1. generate_quarterly_dataset.R')

#valid_months =filtering id who had consistent superhost status in the previous quarter.

# 2023 3Q
result_Q323 <- generate_quarterly_dataset4(
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
result_Q423 <- generate_quarterly_dataset4(
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
result_Q124 <- generate_quarterly_dataset4(
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
result_Q224 <- generate_quarterly_dataset4(
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
result_Q324 <- generate_quarterly_dataset4(
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
result_Q424 <- generate_quarterly_dataset4(
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

rm(result_Q323,result_Q423,result_Q124,result_Q224,result_Q324,result_Q424)
#ctrl+shift+c
save(Q323,Q323_summary_by_quartile,Q323_superhost_ratio_by_quartile,
     Q423,Q423_summary_by_quartile,Q423_superhost_ratio_by_quartile,
     Q124,Q124_summary_by_quartile,Q124_superhost_ratio_by_quartile,
     Q224,Q224_summary_by_quartile,Q224_superhost_ratio_by_quartile,
     Q324,Q324_summary_by_quartile,Q324_superhost_ratio_by_quartile,
     Q424,Q424_summary_by_quartile,Q424_superhost_ratio_by_quartile,
     file = 'Quarterly_dataset4.RData'
)

# Table 1. Summary Statistics by superhost status  ---------------------------------
load('RData/Quarterly_dataset1.RData')

z <- data.frame()  
z=rbind(Q323,Q423,Q124,Q224,Q324,Q424)
z$date3 <- factor(z$Date)
z$date3_ym <- format(as.Date(z$date3), "%Y-%m")
z$date3_ym
z$host_response_rate_clean <- ifelse(
  z$host_response_rate == "N/A" | is.na(z$host_response_rate),
  NA,
  as.numeric(gsub("%", "", z$host_response_rate))
)

# ANOVA test
sup=(z%>%filter(ex_super=='t',host_is_superhost2==T))$price
nonsup=(z%>%filter(ex_super=='t',host_is_superhost2==F))$price
sup_diff=(z%>%filter(ex_super=='t',host_is_superhost2==T))$price_diff
nonsup_diff=(z%>%filter(ex_super=='t',host_is_superhost2==F))$price_diff

sup=(z%>%filter(host_is_superhost2==T))$price
nonsup=(z%>%filter(host_is_superhost2==F))$price

sup_diff=(z%>%filter(host_is_superhost2==T))$price_diff
nonsup_diff=(z%>%filter(host_is_superhost2==F))$price_diff


value_diff = c(sup_diff,nonsup_diff)

summary(sup)
summary(nonsup)
value <- c(sup,nonsup)
group <- c(rep("t", length(sup)),
           rep("f", length(nonsup)))
data = data.frame(group,value)
data2 = data.frame(group,value_diff)

model <- aov(value ~ group, data = data)
model2 <- aov(value_diff ~ group, data = data2)

summary(model)
summary(model2)
t.test(sup, nonsup, var.equal = FALSE)
wilcox.test(sup,nonsup)
t.test(sup_diff, nonsup_diff, var.equal = FALSE)

mean(nonsup_diff)
mean(sup_diff)
#
summarize_vars <- function(df, vars, group_var = "host_is_superhost", file = "summary_table.tex") {
  
  # 내부 요약 함수
  summary_stats <- function(df, vars) {
    stats <- lapply(vars, function(v) {
      data.frame(
        variable = v,
        mean = mean(df[[v]], na.rm = TRUE),
        median = median(df[[v]], na.rm = TRUE),
        sd = sd(df[[v]], na.rm = TRUE)
      )
    })
    result <- do.call(rbind, stats)
    result
  }
  
  # 그룹별 요약
  superhost_df <- df %>% filter(!!sym(group_var) == 't')
  nonsuperhost_df <- df %>% filter(!!sym(group_var) == 'f')
  
  super_summary <- summary_stats(superhost_df, vars)
  non_summary <- summary_stats(nonsuperhost_df, vars)
  
  super_summary$group <- "Superhost"
  non_summary$group <- "Non-Superhost"
  
  # obs 수 추가
  obs_super <- nrow(superhost_df)
  obs_non <- nrow(nonsuperhost_df)
  
  combined <- merge(super_summary, non_summary, by = "variable", suffixes = c("_super", "_non"))
  combined$variable <- factor(combined$variable, levels = vars)
  combined <- combined[order(combined$variable), ]
  #levels(combined$variable) <- vars
  
  # LaTeX 테이블 생성
  cat("\\begin{table}[]\n", file = file)
  cat("\\centering \n", file = file,append=T)
  cat("\\caption{Statistics by Superhost Status with listings more than one review in last 12 months\\label{tab:desc_stats}} \n", file = file,append=T)
  cat("\\vspace{0.5em} \n", file=file,append = TRUE)
  cat("\\begin{tabular}{lccc|ccc} \n", file = file, append = TRUE)
  cat("\\hline \n", file = file, append = TRUE)
  cat(paste0(" & \\multicolumn{3}{c|}{Superhost (N = ", obs_super, ")} & \\multicolumn{3}{c}{Non-Superhost (N = ", obs_non, ")} \\\\\n"), file = file, append = TRUE)
  cat("Variable & Mean & Median & SD & Mean & Median & SD \\\\\n", file = file, append = TRUE)
  cat("\\hline\n", file = file, append = TRUE)
  
  for (i in 1:nrow(combined)) {
    varname <- gsub("_", "\\\\_", as.character(combined$variable[i]))
    cat(varname, "&",
        sprintf("%.2f", combined$mean_super[i]), "&",
        sprintf("%.2f", combined$median_super[i]), "&",
        sprintf("%.2f", combined$sd_super[i]), "&",
        sprintf("%.2f", combined$mean_non[i]), "&",
        sprintf("%.2f", combined$median_non[i]), "&",
        sprintf("%.2f", combined$sd_non[i]), "\\\\\n",
        file = file, append = TRUE)
  }
  
  cat("\\hline\n", file = file, append = TRUE)
  cat("\\end{tabular} \n", file = file, append = TRUE)
  cat("\\vspace{0.5em} \n", file = file, append = TRUE)
  cat("\\parbox{0.95\\textwidth}{\\footnotesize \\textit{Note.} price increment is the log price difference between the current and previous quarter. 
\\textit{rating} is the cumulative average rating for each listing. 
\\textit{number of reviews} is the total number of reviews accumulated over time. 
\\textit{number of reviews (ltm)} refers to reviews received in the last twelve months. 
\\textit{response rate} is the host's cumulative response rate, expressed as a percentage.
} \n", file = file, append = TRUE)
  cat("\\end{table} \n \n", file = file, append = TRUE)
  
}
vars <- c("price","price_diff", "first_month_rating", "first_month_number_of_reviews", "first_month_ltm", "host_response_rate_clean")
summarize_vars(z, vars, file = "tex/superhost_summary.tex")
latex_code <- readLines("tex/superhost_summary.tex")
cat(latex_code, sep = "\n")
# Table 2. Descriptive Statistics by Superhost Status Change  -------------------------------------------------------------------

write_superhost_table <- function(df, vars, file = "tex/superhost_panel_table.tex") {
  
  summary_stats <- function(data, vars) {
    stats <- lapply(vars, function(v) {
      data.frame(
        variable = v,
        mean = mean(data[[v]], na.rm = TRUE),
        median = median(data[[v]], na.rm = TRUE),
        sd = sd(data[[v]], na.rm = TRUE)
      )
    })
    do.call(rbind, stats)
  }
  
  escape_var <- function(x) gsub("_", "\\\\_", x)
  
  # 패널 A: ex_super == 1
  df_A_super <- df %>% filter(ex_super == 't', host_is_superhost == 't')
  df_A_non   <- df %>% filter(ex_super == 't', host_is_superhost == 'f')
  
  df_A <- data.frame(
    variable = vars,
    mean_super_A   = sapply(vars, function(v) mean(df_A_super[[v]], na.rm = TRUE)),
    median_super_A = sapply(vars, function(v) median(df_A_super[[v]], na.rm = TRUE)),
    sd_super_A     = sapply(vars, function(v) sd(df_A_super[[v]], na.rm = TRUE)),
    mean_non_A     = sapply(vars, function(v) mean(df_A_non[[v]], na.rm = TRUE)),
    median_non_A   = sapply(vars, function(v) median(df_A_non[[v]], na.rm = TRUE)),
    sd_non_A       = sapply(vars, function(v) sd(df_A_non[[v]], na.rm = TRUE))
  )
  
  # 패널 B: ex_super == 0
  df_B_super <- df %>% filter(ex_super == 'f', host_is_superhost == 't')
  df_B_non   <- df %>% filter(ex_super == 'f', host_is_superhost == 'f')
  
  df_B <- data.frame(
    variable = vars,
    mean_super_B   = sapply(vars, function(v) mean(df_B_super[[v]], na.rm = TRUE)),
    median_super_B = sapply(vars, function(v) median(df_B_super[[v]], na.rm = TRUE)),
    sd_super_B     = sapply(vars, function(v) sd(df_B_super[[v]], na.rm = TRUE)),
    mean_non_B     = sapply(vars, function(v) mean(df_B_non[[v]], na.rm = TRUE)),
    median_non_B   = sapply(vars, function(v) median(df_B_non[[v]], na.rm = TRUE)),
    sd_non_B       = sapply(vars, function(v) sd(df_B_non[[v]], na.rm = TRUE))
  )
  
  # 관측치 수
  N_A_super <- nrow(df_A_super)
  N_A_non   <- nrow(df_A_non)
  N_B_super <- nrow(df_B_super)
  N_B_non   <- nrow(df_B_non)
  
  # LaTeX 테이블 출력
  cat("\\begin{table}[]\n\\renewcommand{\\arraystretch}{1.1}\n", file = file)
  cat("\\TABLE {Descriptive Statistics by Superhost Status \\label{tab:desc_stats2}} {\n", file = file, append = TRUE)
  cat("\\makebox[\\textwidth][c]{%\n", file = file, append = TRUE)
  cat("\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lccc|ccc}\n\\hline\n", file = file, append = TRUE)
  
  ### Panel A
  cat("\\textbf{Panel A: Ex\\_super = 1} \\\\\n", file = file, append = TRUE)
  cat(paste0("& \\multicolumn{3}{c}{Superhost (N = ", N_A_super, ")} & \\multicolumn{3}{c}{Non-superhost (N = ", N_A_non, ")} \\\\\n"), file = file, append = TRUE)
  cat("Variable & Mean & Median & SD & Mean & Median & SD \\\\\n\\hline\n", file = file, append = TRUE)
  
  for (i in 1:nrow(df_A)) {
    var <- escape_var(df_A$variable[i])
    cat(sprintf("%s & %.2f & %.2f & %.2f & %.2f & %.2f & %.2f \\\\\n",
                var,
                df_A$mean_super_A[i], df_A$median_super_A[i], df_A$sd_super_A[i],
                df_A$mean_non_A[i], df_A$median_non_A[i], df_A$sd_non_A[i]),
        file = file, append = TRUE)
  }
  
  ### Panel B
  cat("\\hline\n\\textbf{Panel B: Ex\\_super = 0} \\\\\n", file = file, append = TRUE)
  cat(paste0("& \\multicolumn{3}{c}{Superhost (N = ", N_B_super, ")} & \\multicolumn{3}{c}{Non-superhost (N = ", N_B_non, ")} \\\\\n"), file = file, append = TRUE)
  cat("Variable & Mean & Median & SD & Mean & Median & SD \\\\\n\\hline\n", file = file, append = TRUE)
  
  for (i in 1:nrow(df_B)) {
    var <- escape_var(df_B$variable[i])
    cat(sprintf("%s & %.2f & %.2f & %.2f & %.2f & %.2f & %.2f \\\\\n",
                var,
                df_B$mean_super_B[i], df_B$median_super_B[i], df_B$sd_super_B[i],
                df_B$mean_non_B[i], df_B$median_non_B[i], df_B$sd_non_B[i]),
        file = file, append = TRUE)
  }
  
  cat("\\hline\n\\end{tabular*}}}\n", file = file, append = TRUE)
  cat("{\\textit{Note.} Each panel shows summary statistics for the specified transition group.}\n\\end{table}\n", file = file, append = TRUE)
}

vars <- c("price", "price_diff", "review_scores_rating", "number_of_reviews", 
          "number_of_reviews_ltm", "host_response_rate_clean", "calculated_host_listings_count","reviews_per_month", "first_month_ltm")

write_superhost_table(df = z, vars = vars, file = "tex/superhost_panel_table.tex")
cat(readLines("tex/superhost_panel_table.tex"), sep = "\n")





# (Unnecessary) Aggregating superhost ratio & Superhost ratio calculation-------------------------------------------------------------
  Q323_superhost_ratio_by_quartile2 <- Q323_superhost_ratio_by_quartile %>%
    summarise(ex_q = "Total", Non_superhost = sum(Non_superhost), Superhost = sum(Superhost), obs = sum(obs), Superhost_ratio = Superhost/obs) %>%
    bind_rows(Q323_superhost_ratio_by_quartile)
  Q423_superhost_ratio_by_quartile2 <- Q423_superhost_ratio_by_quartile %>%
    summarise(ex_q = "Total", Non_superhost = sum(Non_superhost), Superhost = sum(Superhost), obs = sum(obs), Superhost_ratio = Superhost/obs) %>%
    bind_rows(Q423_superhost_ratio_by_quartile)
  Q124_superhost_ratio_by_quartile2 <- Q124_superhost_ratio_by_quartile %>%
    summarise(ex_q = "Total", Non_superhost = sum(Non_superhost), Superhost = sum(Superhost), obs = sum(obs), Superhost_ratio = Superhost/obs) %>%
    bind_rows(Q124_superhost_ratio_by_quartile)
  Q224_superhost_ratio_by_quartile2 <- Q224_superhost_ratio_by_quartile %>%
    summarise(ex_q = "Total", Non_superhost = sum(Non_superhost), Superhost = sum(Superhost), obs = sum(obs), Superhost_ratio = Superhost/obs) %>%
    bind_rows(Q224_superhost_ratio_by_quartile)
  Q324_superhost_ratio_by_quartile2 <- Q324_superhost_ratio_by_quartile %>%
    summarise(ex_q = "Total", Non_superhost = sum(Non_superhost), Superhost = sum(Superhost), obs = sum(obs), Superhost_ratio = Superhost/obs) %>%
    bind_rows(Q324_superhost_ratio_by_quartile)
  Q424_superhost_ratio_by_quartile2 <- Q424_superhost_ratio_by_quartile %>%
    summarise(ex_q = "Total", Non_superhost = sum(Non_superhost), Superhost = sum(Superhost), obs = sum(obs), Superhost_ratio = Superhost/obs) %>%
    bind_rows(Q424_superhost_ratio_by_quartile)
total_superhost_ratio = rbind(Q323_superhost_ratio_by_quartile2,
                              Q423_superhost_ratio_by_quartile2,
                              Q124_superhost_ratio_by_quartile2,
                              Q224_superhost_ratio_by_quartile2,
                              Q324_superhost_ratio_by_quartile2,
                              Q424_superhost_ratio_by_quartile2
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
writeLines(Q_sup_ratio, "tex/2. Quarterly Superhost Ratio.tex")
cat(Q_sup_ratio)

# (Unnecessary) Aggregating price summary statistics ------------------------------------
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
Q_price_summary <- "\\begin{table}[]\n \\TABLE{Price Summary Statistics \\label{Price_sum}}{\\begin{tabular}{lcccc|lcccc}\n"
Q_price_summary <- paste(Q_price_summary, "\\hline\n", sep = "")
Q_price_summary <- paste(Q_price_summary, " Quartile & Variable & Mean & Min & Max & Quartile & Variable & Mean & Min & Max \\\\ \\hline\n", sep = "")

for (i in seq(1, 36, by = 3)){  # 3줄씩 묶어야 하니까 step=3
  # 그룹별로 제목 추가
  if (i == 1) {
    Q_price_summary <- paste(Q_price_summary, " \\\\ \n \\textbf{2023 Q3} & & & & & \\textbf{2024 Q2} & & & & \\\\ \n", sep = "")
  }
  if (i == 13) {
    Q_price_summary <- paste(Q_price_summary, " \\\\ \n \\textbf{2023 Q4} & & & & & \\textbf{2024 Q3} & & & & \\\\ \n", sep = "")
  }
  if (i == 25) {
    Q_price_summary <- paste(Q_price_summary, " \\\\ \n \\textbf{2024 Q1} & & & & & \\textbf{2024 Q4} & & & & \\\\ \n", sep = "")
  }
  
  # 왼쪽 (i, i+1, i+2)
  Ex_q_left = gsub("_", "\\\\_", total_price_summary$ex_q[i])
  Ex_q_right = gsub("_", "\\\\_", total_price_summary$ex_q[i + 36])
  
  for (j in 0:2) {  # 세 줄 (ex, price, price_diff)
    Variable_left = gsub("_", "\\\\_", total_price_summary$stat[i + j])
    Mean_left = round(total_price_summary$mean[i + j], 3)
    Min_left = round(total_price_summary$min[i + j], 3)
    Max_left = round(total_price_summary$max[i + j], 3)
    
    Variable_right = gsub("_", "\\\\_", total_price_summary$stat[i + 36 + j])
    Mean_right = round(total_price_summary$mean[i + 36 + j], 3)
    Min_right = round(total_price_summary$min[i + 36 + j], 3)
    Max_right = round(total_price_summary$max[i + 36 + j], 3)
    
    # Quartile은 첫 줄에만 넣고, 다음은 공백
    Quartile_left = ifelse(j == 0, Ex_q_left, "")
    Quartile_right = ifelse(j == 0, Ex_q_right, "")
    
    Q_price_summary <- paste(
      Q_price_summary,
      paste(Quartile_left, "&", Variable_left, "&", Mean_left, "&", Min_left, "&", Max_left, "&",
            Quartile_right, "&", Variable_right, "&", Mean_right, "&", Min_right, "&", Max_right, "\\\\ \n", sep = " "),
      sep = ""
    )
  }
}

Q_price_summary <- paste(Q_price_summary, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")

# 저장
writeLines(Q_price_summary, "tex/3. price summary.tex")
cat(Q_price_summary)


# Table A7. Change of the superhost -------------------------------------------------
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

Sup_change <- "\\begin{table}[]\n \\TABLE \n {Superhost Status Change \\label{Sup_change}} {\\begin{tabular}{lcccc|lcccc}\n"
Sup_change <- paste(Sup_change, "\\hline\n", sep = "")
Sup_change <- paste(Sup_change, " Date &Ex\\_super &Superhost & Obs & Ratio & Date & Ex\\_super & Superhost & Obs & Ratio \\\\ \\hline\n", sep = "")
Sup_change <- paste(Sup_change, " \\\\ \n", sep = "")

for (i in 1:12) { # 총 24개를 12개씩 두 칼럼으로 나누니까 1~12만 loop
  
  # 왼쪽 칼럼
  if (i %% 4 == 1) {  # 각 분기의 첫 번째 줄마다 Quarter 제목 추가
    if (i == 1) left_quarter <- "\\textbf{2023 Q3}"
    if (i == 5) left_quarter <- "\\textbf{2023 Q4}"
    if (i == 9) left_quarter <- "\\textbf{2024 Q1}"
  } else {
    left_quarter <- ""
  }
  
  left_ex_super <- change$ex_super[i]
  left_superhost <- change$host_is_superhost[i]
  left_obs <- change$obs[i]
  left_ratio <- round(change$ratio[i]*100, 3)
  
  # 오른쪽 칼럼 (i + 12번째)
  j <- i + 12
  
  if (i %% 4 == 1) {  # 각 분기의 첫 번째 줄마다 Quarter 제목 추가
    if (j == 13) right_quarter <- "\\multirow{1}{*}{\\textbf{2024 Q2}}"
    if (j == 17) right_quarter <- "\\multirow{1}{*}{\\textbf{2024 Q3}}"
    if (j == 21) right_quarter <- "\\multirow{1}{*}{\\textbf{2024 Q4}}"
  } else {
    right_quarter <- ""
  }
  
  right_ex_super <- change$ex_super[j]
  right_superhost <- change$host_is_superhost[j]
  right_obs <- change$obs[j]
  right_ratio <- round(change$ratio[j]*100, 3)
  
  # 한 줄에 왼쪽과 오른쪽을 붙여서
  Sup_change <- paste(Sup_change, 
                      paste(left_quarter, "&", left_ex_super, "&", left_superhost, "&", left_obs, "&", left_ratio, 
                            "&", right_quarter, "&", right_ex_super, "&", right_superhost, "&", right_obs, "&", right_ratio, "\\\\ \n", sep = " "), 
                      sep = "")
  
  if (i %% 4 == 0) {  # 4줄 끝날 때마다 줄바꿈
    Sup_change <- paste(Sup_change, " \\\\ \n", sep = "")
  }
}

Sup_change <- paste(Sup_change, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}", sep = "")

writeLines(Sup_change, "tex/4. Sup Change.tex")
cat(Sup_change)


# (Unnecessary) At least once superhost -------------------------------------------------
z <- data.frame()  # 결과를 저장할 빈 데이터 프레임
z=rbind(Q323,Q423,Q124,Q224,Q324,Q424)
z$date3 <- factor(z$Date)
z$date3_ym <- format(as.Date(z$date3), "%Y-%m")
z$date3_ym
superhost_ids <- unique(z$host_id[z$host_is_superhost == 't'|z$ex_super=='t'])
z$date3_ym <- format(as.Date(z$date3), "%Y-%m")

# 해당 host_id를 가진 행들만 필터링
result <- z[z$host_id %in% superhost_ids, ]
result <- result %>%
  filter(ex_super == 'f')
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
    "FULL" = (result$ex_q1 == 1 | result$ex_q2 == 1 | result$ex_q3 == 1 | result$ex_q4 == 1),
    "Q1Q2" = (result$ex_q1 == 1 | result$ex_q2 == 1),
    "Q2Q3" = (result$ex_q3 == 1 | result$ex_q2 == 1),
    "Q3Q4" = (result$ex_q3 == 1 | result$ex_q4 == 1),
    "Q1" = (result$ex_q1 == 1),
    "Q2" = (result$ex_q2 == 1),
    "Q3" = (result$ex_q3 == 1),
    "Q4" = (result$ex_q4 == 1)
  )

  
  for (super_type in c('f')) {
    for (condition_name in names(conditions)) {
      condition <- conditions[[condition_name]]
      
      filtered_data <- result %>%
        filter(ex_super == super_type & condition)
      
      if (nrow(filtered_data) > 0) {
        dummy_vars <- as.data.frame(model.matrix(~ date3_ym - 1, data = filtered_data))
        dummy_vars <- dummy_vars[, -1]
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
            cluster = filtered_data$id,
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
            cluster = filtered_data$id,
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
            cluster = filtered_data$id,
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
            cluster = filtered_data$id,
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
            cluster = filtered_data$id,
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
  results_list <-list()
  bandwidths <- c("h")

# (Unnecessary) save regression ---------------------------------------------------------

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
  
  tex_table_detail <- "\\begin{table}[]\n \\TABLE \n\ {At Least Once Superhost Between 23Q2 - 24Q4  \\label{At_Least_Once}} {\\begin{tabular}{lcccccc}\n"
  
  tex_table_detail <- paste(tex_table_detail, "\\hline\n", sep = "")
  tex_table_detail <- paste(tex_table_detail, "Condition & (1) & (2) & (3) & (4) & (5) &(6) \\\\ \\hline\n", sep = "")
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
  tex_table_detail <- paste(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{Note.} The first column reports the estimates obtained using the MSE-optimal bandwidth calculated separately for each side of the cutoff using time fixed effect. The second column displays results based on twice the MSE-optimal bandwidth. The third to fifth columns report estimates using asymmetric bandwidth choices: 0.2 (left) and 0.1 (right), 0.3 (left) and 0.15 (right), and 0.4 (left) and 0.2 (right), respectively. Last column shows the esimation results using MSE-optimal bandwidth calculated seperately for each side of the cutoff without using time fixed effect.", sep = "")
  writeLines(tex_table_detail, "tex/6. At_Least_Once.tex")
  # TeX 코드 출력
  cat(tex_table_detail)
  quarterly[["At_Least_Once"]] <- results_list
  

# Regression --------------------------------------------------------------
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

source('func/2. RunRD.R')
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
# save(results1,results2,results3,results4,results5,results6,results_list, file = 'RData/results_list.RData')
#save_table(T)
cat(readLines('tex/Ent.tex'), sep="\n")



# ex_super2 ---------------------------------------------------------------
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

source('func/2-1. RunRD_Ex2.R') #For ex_super2=='t
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



# RDplot -------------------------------------------------------------
# load('RData/results_list.RData')
quarterly[["Total"]]<-results_list

for (superhost in c('t','f')){
  if (superhost=='t'){
    results = results1[[9]] }
  else { results = results1[[1]]}
  data = z%>%filter(ex_super== superhost &!is.na(host_is_superhost) & !is.na(id) &
                      host_is_superhost!='' &!is.na(ex_avg) & ex_avg!='')
  data$margin = data$running_scr-4.75
  data= data%>%filter(-results$bws[1,1]<= margin & margin <= results$bws[1,2])
  rd_plot = rdplot(y=data$price_diff,x = data$margin , subset=-results$bws[1,1]<= data$margin & data$margin <= results$bws[1,2], 
                   kernel="tri", p=1, scale=1,
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
plot_t <- plot_t + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
plot_t_first <- plot_t_first + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
print(plot_t_first + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 1))
print(plot_t + geom_hline(yintercept = 0, linetype = "dashed", color = "black") + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 2))

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
ggsave("Figure/F2_Estimation22.eps", plot = F1_full, device = cairo_ps, width = 16, height = 8)
ggsave("Figure/F2_Estimation.pdf", F1_full, width = 16, height = 8)

# Ex_super == "F"
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))

vplayout <- function(row, col) viewport(layout.pos.row = row, layout.pos.col = col)
plot_f <- plot_f + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
plot_f_first <- plot_f_first + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
print(plot_f_first + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 1))
print(plot_f + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 2))


F1_core <- arrangeGrob(plot_f_first + labs(title = NULL, subtitle = NULL),
                       plot_f + labs(title = NULL, subtitle = NULL),
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
ggsave("Figure/F2_Estimation2.eps", plot = F1_full, device = cairo_ps, width = 16, height = 8)
ggsave("Figure/F2_Estimation2.pdf", F1_full, width = 16, height = 8)


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
    coord_cartesian(xlim = range(scatter_data$x),ylim = c(-0.1, 0.1)) +
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
    #coord_cartesian(xlim = c(-0.1,0.1),ylim = c(-0,1)) +
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


grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(row, col) viewport(layout.pos.row = row, layout.pos.col = col)
plot_t <- plot_t + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
plot_t_first <- plot_t_first + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
print(plot_t_first + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 1))
print(plot_t + geom_hline(yintercept = 0, linetype = "dashed", color = "black") + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 2))

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
ggsave("Figure/F2_Estimation.eps", plot = F1_full, device = cairo_ps, width = 16, height = 8)
ggsave("Figure/F2_Estimation.pdf", F1_full, width = 16, height = 8)


# 4 figures together
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))

vplayout <- function(row, col) viewport(layout.pos.row = row, layout.pos.col = col)
plot_t <- plot_t + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
plot_t_first <- plot_t_first + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))

plot_f <- plot_f + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
plot_f_first <- plot_f_first + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
# 1행: superhost == 't'
print(plot_t_first + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 1))
print(plot_f_first + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 2))

# 2행: superhost == 'f'
print(plot_f + labs(title = NULL, subtitle = NULL), vp = vplayout(2, 2))
print(plot_t + labs(title = NULL, subtitle = NULL), vp = vplayout(2, 1))


F1_core <- arrangeGrob(plot_t_first + labs(title = NULL, subtitle = NULL),
                       plot_f_first + labs(title = NULL, subtitle = NULL),
                       plot_t + labs(title = NULL, subtitle = NULL),
                       plot_f + labs(title = NULL, subtitle = NULL),
                       ncol = 2)

F1_full <- grobTree(
  F1_core,
  
  # 1행 왼쪽 (First Stage)
  textGrob("First Stage", x = unit(0.0, "npc"), y = unit(0.97, "npc"),
           just = "left", gp = gpar(fontface = "bold", fontsize = 20)),
  textGrob("Panel A", x = unit(0.02, "npc"), y = unit(0.93, "npc"),
           just = "left", gp = gpar(fontsize = 13)),
  
  # 1행 오른쪽 (Second Stage)
  textGrob("Panel B", x = unit(0.52, "npc"), y = unit(0.93, "npc"),
           just = "left", gp = gpar(fontsize = 13)),
  
  # 2행 왼쪽
  textGrob("Second Stage", x = unit(0.0, "npc"), y = unit(0.47, "npc"),
           just = "left", gp = gpar(fontface = "bold", fontsize = 20)),
  textGrob("Panel A", x = unit(0.02, "npc"), y = unit(0.43, "npc"),
           just = "left", gp = gpar(fontsize = 13)),
  
  # 2행 오른쪽
  textGrob("Panel B", x = unit(0.52, "npc"), y = unit(0.43, "npc"),
           just = "left", gp = gpar(fontsize = 13))
)

ggsave("Figure/F2_Estimation.eps", plot = F1_full, device = cairo_ps, width = 10, height = 10)
ggsave("Figure/F2_Estimation.pdf", F1_full, width = 10, height = 10)

# 1.2 bws = 0.1
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
plot_first
if (superhost == 't') {
  plot_t <- plot_final
  plot_t_first <- plot_first
} else {
  plot_f <- plot_final
  plot_f_first <- plot_first
  
}


grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(row, col) viewport(layout.pos.row = row, layout.pos.col = col)
plot_t <- plot_t + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
plot_t_first <- plot_t_first + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
print(plot_t_first + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 1))
print(plot_t + geom_hline(yintercept = 0, linetype = "dashed", color = "black") + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 2))

plot_t <- plot_t +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
  ) +
  labs(title = NULL, subtitle = NULL)


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
ggsave("Figure/F2_Estimation_v3.eps", plot = F1_full, device = cairo_ps, width = 16, height = 8)
ggsave("Figure/F2_Estimation_p2.pdf", F1_full, width = 16, height = 8)


# Plot 3
print(plot_t + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 2))

# (must be fixed) Regression for panel C (combined) -------------------------------------------


# filtered_data <- z %>% filter((ex_super == "f" & host_is_superhost2 ==1)| (ex_super == "t" & host_is_superhost2 ==0) )
filtered_data <- z
dummy_vars <- as.data.frame(model.matrix(~ date3_ym - 1, data = filtered_data))
dummy_vars <- dummy_vars[, -1]
margin <- filtered_data$running_scr - 4.75
y = log(filtered_data$avg_price) - log(filtered_data$ex_avg)

est1 <- rdrobust(
  y = y,
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
  y = y,
  x = margin,
  fuzzy = filtered_data$host_is_superhost2,
  covs = cbind(dummy_vars),
  h = c(2*est1[['bws']][1,1], 2*est1[['bws']][1,2]),
  all = TRUE,
  cluster = filtered_data$id,
  kernel = "tri",
  bwselect = 'msetwo',
  p = 1,
  masspoints = 'off',
  bwrestrict = TRUE
)

est3 <- rdrobust(
  y = y,
  x = margin,
  fuzzy = filtered_data$host_is_superhost2,
  covs = cbind(dummy_vars),
  h = c(0.2, 0.1),
  all = TRUE,
  cluster = filtered_data$id,
  kernel = "tri",
  bwselect = 'msetwo',
  p = 1,
  masspoints = 'off',
  bwrestrict = TRUE
)

est4 <- rdrobust(
  y = y,
  x = margin,
  fuzzy = filtered_data$host_is_superhost2,
  covs = cbind(dummy_vars),
  h = c(0.3, 0.15),
  cluster = filtered_data$id,
  all = TRUE,
  kernel = "tri",
  bwselect = 'msetwo',
  p = 1,
  masspoints = 'off',
  bwrestrict = TRUE
)

est5 <- rdrobust(
  y = y,
  x = margin,
  fuzzy = filtered_data$host_is_superhost2,
  covs = cbind(dummy_vars),
  h = c(0.4, 0.2),
  all = TRUE,
  cluster = filtered_data$id,
  kernel = "tri",
  bwselect = 'msetwo',
  p = 1,
  masspoints = 'off',
  bwrestrict = TRUE
)

est6 <- rdrobust(
  y = y,
  x = margin,
  fuzzy = filtered_data$host_is_superhost2,
  all = TRUE,
  kernel = "tri",
  cluster = filtered_data$id,
  bwselect = 'msetwo',
  p = 1,
  masspoints = 'off',
  bwrestrict = TRUE
)
print(results_list[["6"]][["beta_Y_p_r_h"]][1] - results_list[["6"]][["beta_Y_p_l_h"]][1])
print(results_list[["6"]][["beta_T_p_r_h"]][1] - results_list[["6"]][["beta_T_p_l_h"]][1])

print(results_list[["6"]][["beta_Y_p_r_h"]][2] - results_list[["6"]][["beta_Y_p_l_h"]][2])
print(results_list[["6"]][["beta_T_p_r_h"]][2] - results_list[["6"]][["beta_T_p_l_h"]][2])

# RDplot for different bws -------------------------------------------------------------
# load('RData/results_list.RData')
quarterly[["Total"]]<-results_list

for (superhost in c('t','f')){
  if (superhost=='t'){
    results = results6[[9]] }
  else { results = results6[[1]]}
  data = z%>%filter(ex_super== superhost &!is.na(host_is_superhost) & !is.na(id) &
                      host_is_superhost!='' &!is.na(ex_avg) & ex_avg!='')
  data$margin = data$running_scr-4.75
  data= data%>%filter(-results$bws[1,1]<= margin & margin <= results$bws[1,2])
  rd_plot = rdplot(y=data$price_diff,x = data$margin , subset=-results$bws[1,1]<= data$margin & data$margin <= results$bws[1,2], 
                   kernel="tri", p=1, scale=1,
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
plot_t <- plot_t + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
plot_t_first <- plot_t_first + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
print(plot_t_first + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 1))
print(plot_t + geom_hline(yintercept = 0, linetype = "dashed", color = "black") + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 2))

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
ggsave("Figure/F2_Estimation_bws6.eps", plot = F1_full, device = cairo_ps, width = 16, height = 8)
ggsave("Figure/F2_Estimation.pdf", F1_full, width = 16, height = 8)

# 1.2 bws = 0.1
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
plot_first
if (superhost == 't') {
  plot_t <- plot_final
  plot_t_first <- plot_first
} else {
  plot_f <- plot_final
  plot_f_first <- plot_first
  
}


grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(row, col) viewport(layout.pos.row = row, layout.pos.col = col)
plot_t <- plot_t + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
plot_t_first <- plot_t_first + theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10))
print(plot_t_first + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 1))
print(plot_t + geom_hline(yintercept = 0, linetype = "dashed", color = "black") + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 2))

plot_t <- plot_t +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
  ) +
  labs(title = NULL, subtitle = NULL)


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
ggsave("Figure/F2_Estimation_v3.eps", plot = F1_full, device = cairo_ps, width = 16, height = 8)
ggsave("Figure/F2_Estimation_p2.pdf", F1_full, width = 16, height = 8)


# Plot 3
print(plot_t + labs(title = NULL, subtitle = NULL), vp = vplayout(1, 2))

# Manipulation Test ------------------------------------------------------------



x=na.omit(z$running_scr)
x=x-4.75

x_t=na.omit((z%>%filter(ex_super=='t'))$running_scr)
x_t=x_t-4.75

x_f=na.omit((z%>%filter(ex_super=='f'))$running_scr)
x_f=x_f-4.75

ggplot(z%>%filter(ex_super=='t'), aes(x = running_scr-4.75)) +
  geom_density(color = "#4D4D4D", size = 1) +  # 밀도 곡선을 어두운 회색 선으로 그리기
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +  # 컷오프 라인 회색 점선
  xlim(-0.1, 0.1) + theme_classic() +
  labs(x = "Distance to Rating Cutoff", y = "Density") +
  scale_color_manual(values = c("black", "black")) +
  theme(legend.position = "none") # 기본적인 미니멀한 테마

ggsave("Figure/Density.eps", width = 6, height = 4, device = "eps")

# 1. McCrary Test
library(rdd)

#Good
Mc1=DCdensity(runvar = x,   
          cutpoint = 0,       
          plot = TRUE)

Mc1

#Bad

rdtest <- DCdensity(runvar = x_t, # or x
                    cutpoint = 0,
                    bw = 0.25,  # from 0.05, everything failed
                    plot = TRUE) ; print(rdtest)


#Bad
Mc2=DCdensity(runvar = x_t,   
              cutpoint = 0,       
              plot = TRUE)

Mc2

# 2. RDDensity
test_result = rddensity(x)
test_result_t = rddensity(x_t)
test_result_t = rddensity(x_t,c=0, all=T,bwselect='each', h=c(results1[["0.05_t_FULL"]][["bws"]][1,1],results1[["0.05_t_FULL"]][["bws"]][1,2])) # good
test_result_f = rddensity(x_f,c=0, all=T,bwselect='each', h=c(results1[["0.05_f_FULL"]][["bws"]][1,1],results1[["0.05_f_FULL"]][["bws"]][1,2])) # good

test_result_t$h 

summary(test_result_t)
summary(test_result_f)

# For this, we need to see p-value of Robust Method
test_result = rddensity(x,c=0, all=T,bwselect='each', h=c(0.15,0.15)) #good
test_result = rddensity(x,c=0, all=T,bwselect='each', h=c(0.3,0.15)) # good
test_result = rddensity(x,c=0, all=T,bwselect='each', h=c(0.4,0.2))
test_result = rddensity(x,c=0, all=T,bwselect='each', h=c(0.2,0.1))

test_result = rddensity(x_t,c=0, all=T,bwselect='each', h=c(0.15,0.15)) #good
test_result = rddensity(x_t,c=0, all=T,bwselect='each', h=c(0.3,0.15)) 
test_result = rddensity(x_t,c=0, all=T,bwselect='each', h=c(0.4,0.2))
test_result = rddensity(x_t,c=0, all=T,bwselect='each', h=c(0.2,0.1))


summary(test_result)


#McCrary Test
rdtest=DCdensity(runvar = x, cutpoint = 0,
                 plot = TRUE,ext.out = TRUE)
rdtest=DCdensity(runvar = x_t, cutpoint = 0, bw=c(test_result[["h"]][["left"]],test_result[["h"]][["right"]]),
                 plot = TRUE,ext.out = TRUE) 




summary(z$running_scr)
plot_density_test <- rdplotdensity(rdd = test_result, type = 'both',
                                   X = z$running_scr-4.75#,plotRange = c(4.2,4.85)
)

plot_density_test
print(plot_density_test)
?DCdensity
rdplotdensity(rdd = test_result, type = 'both', X = z$running_scr,,plotRange = c(4.2,4.85))
?rddensity
cairo_pdf("Figure/Density_rdplot.pdf", width = 6, height = 4)
dev.off()
# Placebo Test ------------------------------------------------------------
placebo_test <- list()
cutoffs <- c(4.875,4.85,4.825,4.675, 4.65,4.625)

bw_list <- list(
  "bw1" = NULL,
  "bw2" = function(bw) 2 * bw,
  "bw3" = function(dummy) c(0.2, 0.1),
  "bw4" = function(dummy) c(0.3, 0.15),
  "bw5" = function(dummy) c(0.4, 0.2),
  "bw6" = NULL  # no covariates version
)

for (super_type in c('t', 'f')) {
  for (cutoff in cutoffs) {
    
    filtered_data <- z %>% filter(ex_super == super_type)
    if (nrow(filtered_data) > 0) {
      dummy_vars <- as.data.frame(model.matrix(~ date3_ym - 1, data = filtered_data))[, -1, drop = FALSE]
      margin <- filtered_data$running_scr - cutoff
      result_key <- paste0("cutoff_", cutoff, "_type_", super_type)
      placebo_test[[result_key]] <- list()
      
      base_fit <- tryCatch({
        rdrobust(
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
          x = margin,
          fuzzy = filtered_data$host_is_superhost2,
          covs = dummy_vars,
          all = TRUE,
          cluster = filtered_data$id,
          kernel = "tri",
          bwselect = 'msetwo',
          p = 1,
          masspoints = 'off',
          bwrestrict = TRUE
        )
      }, error = function(e) {
        message("Base RD error at ", result_key, ": ", e$message)
        return(NULL)
      })
      
      if (!is.null(base_fit)) {
        bws <- base_fit[["bws"]][1, ]
        
        for (bw_name in names(bw_list)) {
          bw_fun <- bw_list[[bw_name]]
          h_val <- if (is.null(bw_fun)) bws else bw_fun(bws)
          use_covs <- if (bw_name == "bw6") NULL else dummy_vars
          
          tryCatch({
            # First stage: fuzzy RD
            
            # Second stage: sharp RD
            sharp_fit <- rdrobust(
              y = log(filtered_data$avg_price) - log(filtered_data$ex_avg),
              x = margin,
              fuzzy = NULL,
              covs = use_covs,
              h = h_val,
              all = TRUE,
              cluster = filtered_data$id,
              kernel = "tri",
              bwselect = 'msetwo',
              p = 1,
              masspoints = 'off',
              bwrestrict = TRUE
            )
            
            # Save both results under each bandwidth
            placebo_test[[result_key]][[bw_name]] <- list(
              first_coef  = base_fit[["tau_T"]][1],
              first_se    = base_fit[["se_T"]][1],
              first_pv    = base_fit[["pv_T"]][1],
              second_coef = sharp_fit[["coef"]][1],
              second_se   = sharp_fit[["se"]][1],
              second_pv   = sharp_fit[["pv"]][1]
            )
            
          }, error = function(e) {
            message(sprintf("Error in %s - %s: %s", result_key, bw_name, e$message))
          })
        }
      }
    }
  }
}
#
star_format <- function(est, pval) {
  if (is.na(est) || is.na(pval)) return("")
  stars <- ifelse(pval < 0.01, "***",
                  ifelse(pval < 0.05, "**",
                         ifelse(pval < 0.1, "*", "")))
  sprintf("%.3f%s", est, stars)
}

make_labeled_rows <- function(test, cutoff, super_type, stage_type, label) {
  coef_row <- c(label)
  se_row   <- c(" ")
  
  for (bw in paste0("bw", 1:6)) {
    key <- paste0("cutoff_", cutoff, "_type_", super_type)
    if (!is.null(test[[key]][[bw]])) {
      coef <- test[[key]][[bw]][[paste0(stage_type, "_coef")]]
      pv   <- test[[key]][[bw]][[paste0(stage_type, "_pv")]]
      se   <- test[[key]][[bw]][[paste0(stage_type, "_se")]]
      
      coef_row <- c(coef_row, star_format(coef, pv))
      se_row   <- c(se_row, sprintf("(%.3f)", se))
    } else {
      coef_row <- c(coef_row, "")
      se_row   <- c(se_row, "")
    }
  }
  return(c(
    paste(coef_row, collapse = " & ") %>% paste0(" \\\\"),
    paste(se_row, collapse = " & ") %>% paste0(" \\\\")
  ))
}

make_cutoff_block <- function(test, cutoff) {
  lines <- c(
    paste0("Cutoff = ", cutoff, " \\\\"),
    "\\textbf{Panel A} &&&&&& \\\\",
    make_labeled_rows(test, cutoff, "t", "first", "\\textit{First Stage}"),
    make_labeled_rows(test, cutoff, "t", "second", "\\textit{Second Stage}"),
    "\\textbf{Panel B} &&&&&& \\\\",
    make_labeled_rows(test, cutoff, "f", "first", "\\textit{First Stage}"),
    make_labeled_rows(test, cutoff, "f", "second", "\\textit{Second Stage}"),
    "\\hline"
  )
  return(lines)
}

make_final_placebo_table <- function(test,cutoffs) {
  lines <- c(
    "\\begin{table}",
    "\\TABLE",
    "{Placebo RD Estimates Across Bandwidths}",
    "{\\begin{tabular}{lcccccc}",
    " & (1) & (2) & (3) & (4) & (5) & (6) \\\\"
  )
  for (cutoff in cutoffs) {
    lines <- c(lines, make_cutoff_block(test, cutoff))
  }
  lines <- c(lines, "\\end{tabular}}{}", "\\end{table}")
  cat(paste(lines, collapse = "\n"))
}

make_final_placebo_table(placebo_test,cutoffs=cutoffs)

# Quarterly RD ------------------------------------------------------------

source('func/3. Quarterly RD.R')
quarter_data_list <- list(
  "2023_3Q" = Q323,
  "2023_4Q" = Q423,
  "2024_1Q" = Q124,
  "2024_2Q" = Q224,
  "2024_3Q" = Q324,
  "2024_4Q" = Q424
)

output_file_list <- list(
  "2023_3Q" = "5-1. 2023_Q3.tex",
  "2023_4Q" = "5-2. 2023_Q4.tex",
  "2024_1Q" = "5-3. 2024_Q1.tex",
  "2024_2Q" = "5-4. 2024_Q2.tex",
  "2024_3Q" = "5-5. 2024_Q3.tex",
  "2024_4Q" = "5-6. 2024_Q4.tex"
)

# 2. 모든 쿼터 데이터에 대해 반복

for (quarter_name in names(quarter_data_list)) {
  
  # 데이터 꺼내기
  data_input <- quarter_data_list[[quarter_name]]
  output_filename <- output_file_list[[quarter_name]]
  
  # 회귀 분석 실행
  run_quarter_regression(data_input, pct = "0.05", quarter_label = quarter_name)
  
  # TeX 테이블 생성 및 저장
  make_tex_table_detail(quarter_label = quarter_name, output_filename = output_filename)
  
  # tex/ 붙은 파일 경로
  output_filename_full <- file.path("tex", output_filename)
  
  # 파일 내용 출력
  cat("\n=====", output_filename_full, "=====\n")
  cat(readLines(output_filename_full), sep = "\n")
  cat("\n\n")
}

cat(readLines("tex/5-1. 2023_Q3.tex"), sep = "\n")
cat(readLines("tex/5-2. 2023_Q4.tex"), sep = "\n")
cat(readLines("tex/5-3. 2024_Q1.tex"), sep = "\n")
cat(readLines("tex/5-4. 2024_Q2.tex"), sep = "\n")
cat(readLines("tex/5-5. 2024_Q3.tex"), sep = "\n")
cat(readLines("tex/5-6. 2024_Q4.tex"), sep = "\n")
cat(readLines("tex/Entire_detail.tex"),sep="\n")

# Placebo Test ------------------------------------------------------------

filtered_data <- z %>%
  filter(ex_super == 't' )
dummy_vars <- as.data.frame(model.matrix(~ date3_ym - 1, data = filtered_data))
dummy_vars <- dummy_vars[, -1]
filtered_data$margin <- filtered_data$running_scr - 4.75
test1 = rdrobust(
  y = log(filtered_data$avg_price)-log(filtered_data$ex_avg),
  x = filtered_data$margin,
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
#first stage
test2 = rdrobust(
  y = filtered_data$host_is_superhost2,
  x = filtered_data$margin,
  h= c(test1$bws[1,1],test1$bws[1,2]),
  covs = cbind(dummy_vars),
  all = TRUE,
  cluster = filtered_data$id,
  kernel = "tri",
  bwselect = 'msetwo',
  p = 1,
  masspoints = 'off',
  bwrestrict = TRUE
)
rdplot(y=log(filtered_data$avg_price)-log(filtered_data$ex_avg),x = filtered_data$margin, subset=-test1$bws[1,1]<= filtered_data$margin & filtered_data$margin <= test1$bws[1,2], 
                 kernel="tri", p=1, scale=1,
                 title="RD Plot: Airbnb", 
                 y.label="Occupancy Rate",
                 x.label="Rating for Host",masspoints = 'off')
rd_plot
# DID ---------------------------------------------------------------------


source('func/DID_func.R')
local_data <- list()

# 분석을 수행할 분기와 방향 설정
quarters <- c("Q423","Q124", "Q224", "Q324",  "Q424")
directions <- c("f->t", "t->f")
local_options <- c(TRUE, FALSE)

# 이전 분기 맵 설정
previous_quarter <- list(
  Q423 = "Q323",
  Q124 = "Q423",
  Q224 = "Q124",
  Q324 = "Q224",
  Q424 = "Q324"
)

# 반복문을 이용하여 분석 실행
did_results <- list()
for (quarter in quarters) {
  for (direction in directions) {
    for (local in local_options) {
      analysis_name <- paste0(quarter, "_", direction, ifelse(local, "_local", "_total"))
      # 이전 분기를 맵에서 가져옴
      previous_q <- previous_quarter[[quarter]]
      data1 <- get(previous_q)  # 이전 분기 데이터
      data2 <- get(quarter)     # 현재 분기 데이터
      did_results[[analysis_name]] <- run_did_analysis(data1, data2, quarter, direction, local)
      
    }
  }
}

# 4개의 테이블을 만들기 위한 반복문
tables <- list()
directions <- c("f->t", "t->f")
locals <- c(FALSE, TRUE)

for (direction in directions) {
  for (local in locals) {
    table_name <- paste0(direction, ifelse(local, "_local", "_total"))
    tables[[table_name]] <- create_result_table(direction, local)
  }
}
tables<- tables[c(3, 4, 1, 2)]

latex_output <- generate_latex_table(tables)

# LaTeX 코드 파일로 저장
cat(latex_output, file = "DID_results_combined.tex")

# LaTeX 코드 미리보기
cat(latex_output)

#plot observation test
rd_data=Q224%>%filter(ex_super=='f')
est1 <- rdrobust(
  y = log(rd_data$avg_price) - log(rd_data$ex_avg),
  x = rd_data$host_response_rate2-90,
  #x=rd_data$ltm_scr-3,
  fuzzy = rd_data$host_is_superhost2,
  all = TRUE,
  kernel = "tri",
  cluster = rd_data$id,
  bwselect = 'msetwo',
  p = 1,
  masspoints = 'off',
  bwrestrict = TRUE
)
rd_data %>%
  filter(running_scr >= 4.75 - est1[["bws"]][1, 1] & 
           running_scr <= 4.75 + est1[["bws"]][1, 2] & ex_super=='t')
Q224$host_response_rate2 <- as.numeric(
  gsub("%", "", Q224$host_response_rate)
)
test=Q224%>%filter(host_response_rate2<90 & host_is_superhost=='t')%>%select(Date,host_id,id,host_response_rate,host_response_rate2,host_is_superhost,host_is_superhost2)
test2= Entire%>%select(Date,host_id,id,host_response_rate,host_is_superhost,host_is_superhost2)

summary(rd_data$host_response_rate2)
result_Q423 <- make_treatment_plot_quarter(Q323, Q423, "Q323", "Q423", post_months = c(10, 11, 12))
result_Q124 <- make_treatment_plot_quarter(Q423, Q124, "Q423", "Q124", post_months = c(1, 2, 3))
result_Q224 <- make_treatment_plot_quarter(Q124, Q224, "Q124", "Q224", post_months = c(4, 5, 6))
result_Q324 <- make_treatment_plot_quarter(Q224, Q324, "Q224", "Q324", post_months = c(7, 8, 9))
result_Q424 <- make_treatment_plot_quarter(Q324, Q424, "Q324", "Q424", post_months = c(10, 11, 12))
plot_list <- list(
  result_Q423$t_plot, result_Q423$f_plot,
  result_Q124$t_plot, result_Q124$f_plot,
  result_Q224$t_plot, result_Q224$f_plot,
  result_Q324$t_plot, result_Q324$f_plot,
  result_Q424$t_plot, result_Q424$f_plot
)

lQ423 = local_data[[1]]
lQ124 = local_data[[3]]
lQ224 = local_data[[5]]
lQ324 = local_data[[7]]
lQ424 = local_data[[9]]

local_result_Q423 <- make_treatment_plot_quarter(Q323, lQ423, "Q323", "Q423", post_months = c(10, 11, 12))
local_result_Q124 <- make_treatment_plot_quarter(Q423, lQ124, "Q423", "Q124", post_months = c(1, 2, 3))
local_result_Q224 <- make_treatment_plot_quarter(Q124, lQ224, "Q124", "Q224", post_months = c(4, 5, 6))
local_result_Q324 <- make_treatment_plot_quarter(Q224, lQ324, "Q224", "Q324", post_months = c(7, 8, 9))
local_result_Q424 <- make_treatment_plot_quarter(Q324, lQ424, "Q324", "Q424", post_months = c(10, 11, 12))

local_plot_list <- list(
  local_result_Q423$t_plot, local_result_Q423$f_plot,
  local_result_Q124$t_plot, local_result_Q124$f_plot,
  local_result_Q224$t_plot, local_result_Q224$f_plot,
  local_result_Q324$t_plot, local_result_Q324$f_plot,
  local_result_Q424$t_plot, local_result_Q424$f_plot
)


# 1. 제목 텍스트 grob 만들기
title_left <- textGrob("Ex_super == 1", gp = gpar(fontsize = 14, fontface = "bold"))
title_right <- textGrob("Ex_super == 0", gp = gpar(fontsize = 14, fontface = "bold"))

# 2. 5행 2열 그래프 배열 만들기
plots_grob <- arrangeGrob(grobs = plot_list, ncol = 2)
local_plots_grob <- arrangeGrob(grobs = local_plot_list, ncol = 2)

# 3. column title 포함한 최종 그리드 생성
final_plot <- arrangeGrob(
  arrangeGrob(title_left, title_right, ncol = 2),
  plots_grob,
  ncol = 1,
  heights = c(0.05, 1)
)

local_final_plot <- arrangeGrob(
  arrangeGrob(title_left, title_right, ncol = 2),
  local_plots_grob,
  ncol = 1,
  heights = c(0.05, 1)
)
# 4. 보기
grid.newpage()
grid.draw(final_plot)
grid.draw(local_final_plot)
# 5. 저장도 가능
ggsave("treatment_effect_grid_plot.eps", final_plot,
       width = 12, height = 20, units = "in", device = cairo_ps)
ggsave("local_treatment_effect_grid_plot.eps", local_final_plot,
       width = 12, height = 20, units = "in", device = cairo_ps)

