## rd_table_generator.R
# Two simple functions reflecting your original code: one for "at least once" sample, one for entire sample.

# runrd ------------------------------------------------------------------


runrd <-function(data,level) {
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
  z$date3_ym <- format(as.Date(z$date3), "%Y-%m")

  # 더미 변수 생성
  
  for (super_type in c('f', 't')) {
    for (condition_name in names(conditions)) {
      condition <- conditions[[condition_name]]
      
      filtered_data <- z %>%
       filter(ex_super == super_type & condition)
      
      
      if (nrow(filtered_data) > 0) {
        dummy_vars <- as.data.frame(model.matrix(~ date3_ym - 1, data = filtered_data))
        dummy_vars <- dummy_vars[, -1]
        margin <- filtered_data$running_scr - 4.75
        result_name <- paste(pct, super_type, condition_name, sep = "_")
        if(level ==T){
          y= log(filtered_data$avg_price)
        } else {
          y = log(filtered_data$avg_price) - log(filtered_data$ex_avg)
        }
        
        tryCatch({
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
          if (level==T){
            results1_level[[result_name]] <<- est1
            results2_level[[result_name]] <<- est2
            results3_level[[result_name]] <<- est3
            results4_level[[result_name]] <<- est4
            results5_level[[result_name]] <<- est5
            results6_level[[result_name]] <<- est6
          } else {
            results1[[result_name]] <<- est1
            results2[[result_name]] <<- est2
            results3[[result_name]] <<- est3
            results4[[result_name]] <<- est4
            results5[[result_name]] <<- est5
            results6[[result_name]] <<- est6
          }

          
        
          
        }, error = function(e) {
          message(sprintf("Error in %s: %s", result_name, e$message))
          # 오류가 나면 해당 반복만 건너뜀
        })
      }
    }
  }

}

# save_table --------------------------------------------------------------


save_table = function(detail,robust) {
  tex_data <-data.frame()
  tex_data <- data.frame(
    "Condition" = rep(c("FULL", "Q1Q2", "Q2Q3", "Q3Q4", "Q1", "Q2", "Q3", "Q4"), each = 2),
    "Ex_super" = rep(c("t", "f"), 8),


    if (robust==T){
      list(
        
      "coef" = round(cbind(results_list[[1]]$coef_bc,
                           results_list[[2]]$coef_bc,
                           results_list[[3]]$coef_bc,
                           results_list[[4]]$coef_bc,
                           results_list[[5]]$coef_bc,
                           results_list[[6]]$coef_bc),3),  
      "pv" = cbind(results_list[[1]]$pv_robust,
                   results_list[[2]]$pv_robust,
                   results_list[[3]]$pv_robust,
                   results_list[[4]]$pv_robust,
                   results_list[[5]]$pv_robust,
                   results_list[[6]]$pv_robust
      ),
      "se" = round(cbind(results_list[[1]]$se_robust, 
                         results_list[[2]]$se_robust,
                         results_list[[3]]$se_robust,
                         results_list[[4]]$se_robust,
                         results_list[[5]]$se_robust,
                         results_list[[6]]$se_robust),3),
      "bws" = cbind(results_list[[1]]$b),
      
      "obs" = cbind(results_list[[1]]$obs_b,
                    results_list[[2]]$obs_b,
                    results_list[[3]]$obs_b,
                    results_list[[4]]$obs_b,
                    results_list[[5]]$obs_b,
                    results_list[[6]]$obs_b))
    } else{
      list(
      "coef" = round(cbind(results_list[[1]]$coef_conv,
                             results_list[[2]]$coef_conv,
                             results_list[[3]]$coef_conv,
                             results_list[[4]]$coef_conv,
                             results_list[[5]]$coef_conv,
                             results_list[[6]]$coef_conv),3),  
      "pv" = cbind(results_list[[1]]$pv_conv,
                   results_list[[2]]$pv_conv,
                   results_list[[3]]$pv_conv,
                   results_list[[4]]$pv_conv,
                   results_list[[5]]$pv_conv,
                   results_list[[6]]$pv_conv
      ),
      "se" = round(cbind(results_list[[1]]$se_conv, 
                         results_list[[2]]$se_conv,
                         results_list[[3]]$se_conv,
                         results_list[[4]]$se_conv,
                         results_list[[5]]$se_conv,
                         results_list[[6]]$se_conv),3),
      "bws" = cbind(results_list[[1]]$h),
      
      "obs" = cbind(results_list[[1]]$obs_h,
                    results_list[[2]]$obs_h,
                    results_list[[3]]$obs_h,
                    results_list[[4]]$obs_h,
                    results_list[[5]]$obs_h,
                    results_list[[6]]$obs_h))
    }

      # "N" = cbind(results_list[[1]]$N_h,
      #               results_list[[2]]$N_h,
      #               results_list[[3]]$N_h,
      #               results_list[[4]]$N_h,
      #               results_list[[5]]$N_h,
      #               results_list[[6]]$N_h

                    

    )
  tex_data <- tex_data %>%
    mutate(Ex_super = factor(Ex_super, levels = c("t", "f"))) %>%
    arrange(Ex_super)
  table_name <- "\\begin{table}[]\n \\TABLE \n\ {Entire Room Regression Results(Ex==1) \\label{result_detail}} {\\begin{tabular}{lcccccc}\n"
  table_name <- paste(table_name, "\\hline\n", sep = "")
  table_name <- paste(table_name, "Condition & (1) & (2) & (3) & (4) & (5) &(6) \\\\ \\hline\n", sep = "")
  table_name <- paste(table_name, "\\\\ \n", sep = "")
  table_name <- paste(table_name, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==1}}\\\\ \n", sep = "")
  table_name <- paste(table_name, "\\\\ \n", sep = "")
  
  
  # 각 조건에 대해 행 추가
  for (i in 1:16) {
    if (i == 1) {
      table_name <- ""
      table_name <- paste0(table_name,
                           "\\begin{table}[]\n\\TABLE\n{Entire Room Regression Results (Ex==1) \\label{result_detail1}}{\\begin{tabular}{lcccccc}\n",
                           "\\hline\n",
                           "Condition & (1) & (2) & (3) & (4) & (5) & (6) \\\\ \\hline\n",
                           "\\\\ \n",
                           "\\multicolumn{5}{l}{\\textbf{EX\\_Super==1}} \\\\ \n\\\\ \n"
      )
    }
    if (i == 9) {
      table_name <- paste0(table_name,
                           "\\hline \n\\end{tabular}}{\\textit{Note.} The first column reports the estimates obtained using the MSE-optimal bandwidth calculated separately for each side of the cutoff using time fixed effect. The second column displays results based on twice the MSE-optimal bandwidth. The third to fifth columns report estimates using asymmetric bandwidth choices: 0.2 (left) and 0.1 (right), 0.3 (left) and 0.15 (right), and 0.4 (left) and 0.2 (right), respectively. Last column shows the estimation results using MSE-optimal bandwidth calculated separately for each side of the cutoff without using time fixed effect.}\n\\end{table}\n\n"
      )
      table_name <- paste0(table_name,
                           "\\begin{table}[]\n\\TABLE\n{Entire Room Regression Results (Ex==0) \\label{result_detail2}}{\\begin{tabular}{lcccccc}\n",
                           "\\hline\n",
                           "Condition & (1) & (2) & (3) & (4) & (5) & (6) \\\\ \\hline\n",
                           "\\\\ \n",
                           "\\multicolumn{5}{l}{\\textbf{EX\\_Super==0}} \\\\ \n\\\\ \n"
      )
    }
    condition <- tex_data$Condition[i]
    ex_super <- tex_data$Ex_super[i]
    
    # coef와 se를 숫자 형식으로 처리
    coef1 <- tex_data[i,3]
    se1 <- tex_data[i,15]
    pv1 <- tex_data[i,9]
    bws <- tex_data$bws[i]
    N_h <- tex_data[i,22]
    
    coef2 <- tex_data[i,4]
    se2 <- tex_data[i,16]
    pv2 <- tex_data[i,10]
    N_h2 <- tex_data[i,23]
    
    coef3 <- tex_data[i,5]
    se3 <- tex_data[i,17]
    pv3 <- tex_data[i,11]
    N_h3 <- tex_data[i,24]
    
    coef4 <- tex_data[i,6]
    se4 <- tex_data[i,18]
    pv4 <- tex_data[i,12]
    N_h4 <- tex_data[i,25]
    
    coef5 <- tex_data[i,7]
    se5 <- tex_data[i,19]
    pv5 <- tex_data[i,13]
    N_h5 <-tex_data[i,26]
    
    coef6 <- tex_data[i,8]
    se6 <- tex_data[i,20]
    pv6 <- tex_data[i,14]
    N_h6 <- tex_data[i,27]
    
    # p-value에 따라 별을 추가
    coef_star <- paste(coef1, ifelse(pv1 < 0.01, "***", ifelse(pv1 < 0.05, "**", ifelse(pv1 < 0.1, "*", ""))), sep = "")
    coef_star2 <- paste(coef2, ifelse(pv2 < 0.01, "***", ifelse(pv2 < 0.05, "**", ifelse(pv2 < 0.1, "*", ""))), sep = "")
    coef_star3 <- paste(coef3, ifelse(pv3 < 0.01, "***", ifelse(pv3 < 0.05, "**", ifelse(pv3 < 0.1, "*", ""))), sep = "")
    coef_star4 <- paste(coef4, ifelse(pv4 < 0.01, "***", ifelse(pv4 < 0.05, "**", ifelse(pv4 < 0.1, "*", ""))), sep = "")
    coef_star5 <- paste(coef5, ifelse(pv5 < 0.01, "***", ifelse(pv5 < 0.05, "**", ifelse(pv5 < 0.1, "*", ""))), sep = "")
    coef_star6 <- paste(coef6, ifelse(pv6 < 0.01, "***", ifelse(pv6 < 0.05, "**", ifelse(pv6 < 0.1, "*", ""))), sep = "")
    # 행 추가
    table_name <- paste(table_name, paste(condition,  "&", coef_star, "&", coef_star2, "&",coef_star3,"&",coef_star4,"&",coef_star5,"&",coef_star6, "\\\\ \n"), sep = "")
    table_name <- paste(table_name, paste("&","(",se1,")&(", se2, ") & (",se3,")&(",se4,")&(",se5,")&(",se6, ")\\\\ \n",sep=""), sep = "")
    if (detail==T){
    table_name <- paste(table_name, paste("&", bws, "&", "&","&","&", "&","\\\\ \n"), sep = "")
    table_name <- paste(table_name, paste("\\textit{obs}","&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5,"&",N_h6, "\\\\ \n"), sep = "")
    }
  }
  table_name <- paste(table_name, "\\hline \n \\end{tabular}}{\\textit{Note.} The first column reports the estimates obtained using the MSE-optimal bandwidth calculated separately for each side of the cutoff using time fixed effect. The second column displays results based on twice the MSE-optimal bandwidth. The third to fifth columns report estimates using asymmetric bandwidth choices: 0.2 (left) and 0.1 (right), 0.3 (left) and 0.15 (right), and 0.4 (left) and 0.2 (right), respectively. Last column shows the esimation results using MSE-optimal bandwidth calculated seperately for each side of the cutoff without using time fixed effect.}\n\\end{table}", sep = "")
  
  
  if (detail == TRUE & robust == TRUE) {
    table = "tex/6-1. Entire_detail_robust.tex"
  } else if (detail == TRUE & robust == FALSE) {
    table = "tex/6-2. Entire_detail_conv.tex"
  } else if (detail == FALSE & robust == TRUE) {
    table = "tex/Ent_robust.tex"
  } else if (detail == FALSE & robust == FALSE) {
    table = "tex/Ent_conv.tex"
  }
  writeLines(table_name, table)
  cat(table_name)
}



save_table2 = function(detail,robust,level) {
  tex_data <-data.frame()
  tex_data <- data.frame(
    "Condition" = rep(c("FULL", "Q1Q2", "Q2Q3", "Q3Q4", "Q1", "Q2", "Q3", "Q4"), each = 2),
    "Ex_super" = rep(c("t", "f"), 8),
    
    
    if (robust==T){
      list(
        
        "coef" = round(cbind(results_list[[1]]$coef_bc,
                             results_list[[2]]$coef_bc,
                             results_list[[3]]$coef_bc,
                             results_list[[4]]$coef_bc,
                             results_list[[5]]$coef_bc,
                             results_list[[6]]$coef_bc),3),  
        "pv" = cbind(results_list[[1]]$pv_robust,
                     results_list[[2]]$pv_robust,
                     results_list[[3]]$pv_robust,
                     results_list[[4]]$pv_robust,
                     results_list[[5]]$pv_robust,
                     results_list[[6]]$pv_robust
        ),
        "se" = round(cbind(results_list[[1]]$se_robust, 
                           results_list[[2]]$se_robust,
                           results_list[[3]]$se_robust,
                           results_list[[4]]$se_robust,
                           results_list[[5]]$se_robust,
                           results_list[[6]]$se_robust),3),
        "bws" = cbind(results_list[[1]]$b),
        
        "obs" = cbind(results_list[[1]]$obs_b,
                      results_list[[2]]$obs_b,
                      results_list[[3]]$obs_b,
                      results_list[[4]]$obs_b,
                      results_list[[5]]$obs_b,
                      results_list[[6]]$obs_b))
    } else{
      list(
        "coef" = round(cbind(results_list[[1]]$coef_conv,
                             results_list[[2]]$coef_conv,
                             results_list[[3]]$coef_conv,
                             results_list[[4]]$coef_conv,
                             results_list[[5]]$coef_conv,
                             results_list[[6]]$coef_conv),3),  
        "pv" = cbind(results_list[[1]]$pv_conv,
                     results_list[[2]]$pv_conv,
                     results_list[[3]]$pv_conv,
                     results_list[[4]]$pv_conv,
                     results_list[[5]]$pv_conv,
                     results_list[[6]]$pv_conv
        ),
        "se" = round(cbind(results_list[[1]]$se_conv, 
                           results_list[[2]]$se_conv,
                           results_list[[3]]$se_conv,
                           results_list[[4]]$se_conv,
                           results_list[[5]]$se_conv,
                           results_list[[6]]$se_conv),3),
        "bws" = cbind(results_list[[1]]$h),
        
        "obs" = cbind(results_list[[1]]$obs_h,
                      results_list[[2]]$obs_h,
                      results_list[[3]]$obs_h,
                      results_list[[4]]$obs_h,
                      results_list[[5]]$obs_h,
                      results_list[[6]]$obs_h))
    }
    
    # "N" = cbind(results_list[[1]]$N_h,
    #               results_list[[2]]$N_h,
    #               results_list[[3]]$N_h,
    #               results_list[[4]]$N_h,
    #               results_list[[5]]$N_h,
    #               results_list[[6]]$N_h
    
    
    
  )
  tex_data <- tex_data %>%
    mutate(Ex_super = factor(Ex_super, levels = c("t", "f"))) %>%
    arrange(Ex_super)
  table_name <- "\\begin{table}[]\n \\TABLE \n\ {Entire Room Regression Results(Ex==1) \\label{result_detail}} {\\begin{tabular}{lcccccc}\n"
  table_name <- paste(table_name, "\\hline\n", sep = "")
  table_name <- paste(table_name, "Condition & (1) & (2) & (3) & (4) & (5) &(6) \\\\ \\hline\n", sep = "")
  table_name <- paste(table_name, "\\\\ \n", sep = "")
  table_name <- paste(table_name, "\\multicolumn{5}{l}{\\textbf{EX\\_Super==1}}\\\\ \n", sep = "")
  table_name <- paste(table_name, "\\\\ \n", sep = "")
  
  
  # 각 조건에 대해 행 추가
  for (i in 1:16) {
    if (i == 1) {
      table_name <- ""
      table_name <- paste0(table_name,
                           "\\begin{table}[]\n\\TABLE\n{Entire Room Regression Results (Ex==1) \\label{result_detail1}}{\\begin{tabular}{lcccccc}\n",
                           "\\hline\n",
                           "Condition & (1) & (2) & (3) & (4) & (5) & (6) \\\\ \\hline\n",
                           "\\\\ \n",
                           "\\multicolumn{5}{l}{\\textbf{EX\\_Super==1}} \\\\ \n\\\\ \n"
      )
    }
    if (i == 9) {
      table_name <- paste0(table_name,
                           "\\hline \n\\end{tabular}}{\\textit{Note.} The first column reports the estimates obtained using the MSE-optimal bandwidth calculated separately for each side of the cutoff using time fixed effect. The second column displays results based on twice the MSE-optimal bandwidth. The third to fifth columns report estimates using asymmetric bandwidth choices: 0.2 (left) and 0.1 (right), 0.3 (left) and 0.15 (right), and 0.4 (left) and 0.2 (right), respectively. Last column shows the estimation results using MSE-optimal bandwidth calculated separately for each side of the cutoff without using time fixed effect.}\n\\end{table}\n\n"
      )
      table_name <- paste0(table_name,
                           "\\begin{table}[]\n\\TABLE\n{Entire Room Regression Results (Ex==0) \\label{result_detail2}}{\\begin{tabular}{lcccccc}\n",
                           "\\hline\n",
                           "Condition & (1) & (2) & (3) & (4) & (5) & (6) \\\\ \\hline\n",
                           "\\\\ \n",
                           "\\multicolumn{5}{l}{\\textbf{EX\\_Super==0}} \\\\ \n\\\\ \n"
      )
    }
    condition <- tex_data$Condition[i]
    ex_super <- tex_data$Ex_super[i]
    
    # coef와 se를 숫자 형식으로 처리
    coef1 <- tex_data[i,3]
    se1 <- tex_data[i,15]
    pv1 <- tex_data[i,9]
    bws <- tex_data$bws[i]
    N_h <- tex_data[i,22]
    
    coef2 <- tex_data[i,4]
    se2 <- tex_data[i,16]
    pv2 <- tex_data[i,10]
    N_h2 <- tex_data[i,23]
    
    coef3 <- tex_data[i,5]
    se3 <- tex_data[i,17]
    pv3 <- tex_data[i,11]
    N_h3 <- tex_data[i,24]
    
    coef4 <- tex_data[i,6]
    se4 <- tex_data[i,18]
    pv4 <- tex_data[i,12]
    N_h4 <- tex_data[i,25]
    
    coef5 <- tex_data[i,7]
    se5 <- tex_data[i,19]
    pv5 <- tex_data[i,13]
    N_h5 <-tex_data[i,26]
    
    coef6 <- tex_data[i,8]
    se6 <- tex_data[i,20]
    pv6 <- tex_data[i,14]
    N_h6 <- tex_data[i,27]
    
    # p-value에 따라 별을 추가
    coef_star <- paste(coef1, ifelse(pv1 < 0.01, "***", ifelse(pv1 < 0.05, "**", ifelse(pv1 < 0.1, "*", ""))), sep = "")
    coef_star2 <- paste(coef2, ifelse(pv2 < 0.01, "***", ifelse(pv2 < 0.05, "**", ifelse(pv2 < 0.1, "*", ""))), sep = "")
    coef_star3 <- paste(coef3, ifelse(pv3 < 0.01, "***", ifelse(pv3 < 0.05, "**", ifelse(pv3 < 0.1, "*", ""))), sep = "")
    coef_star4 <- paste(coef4, ifelse(pv4 < 0.01, "***", ifelse(pv4 < 0.05, "**", ifelse(pv4 < 0.1, "*", ""))), sep = "")
    coef_star5 <- paste(coef5, ifelse(pv5 < 0.01, "***", ifelse(pv5 < 0.05, "**", ifelse(pv5 < 0.1, "*", ""))), sep = "")
    coef_star6 <- paste(coef6, ifelse(pv6 < 0.01, "***", ifelse(pv6 < 0.05, "**", ifelse(pv6 < 0.1, "*", ""))), sep = "")
    # 행 추가
    table_name <- paste(table_name, paste(condition,  "&", coef_star, "&", coef_star2, "&",coef_star3,"&",coef_star4,"&",coef_star5,"&",coef_star6, "\\\\ \n"), sep = "")
    table_name <- paste(table_name, paste("&","(",se1,")&(", se2, ") & (",se3,")&(",se4,")&(",se5,")&(",se6, ")\\\\ \n",sep=""), sep = "")
    if (detail==T){
      table_name <- paste(table_name, paste("&", bws, "&", "&","&","&", "&","\\\\ \n"), sep = "")
      table_name <- paste(table_name, paste("\\textit{obs}","&", N_h, "&", N_h2, "&",N_h3,"&",N_h4,"&",N_h5,"&",N_h6, "\\\\ \n"), sep = "")
    }
  }
  table_name <- paste(table_name, "\\hline \n \\end{tabular}}{\\textit{Note.} The first column reports the estimates obtained using the MSE-optimal bandwidth calculated separately for each side of the cutoff using time fixed effect. The second column displays results based on twice the MSE-optimal bandwidth. The third to fifth columns report estimates using asymmetric bandwidth choices: 0.2 (left) and 0.1 (right), 0.3 (left) and 0.15 (right), and 0.4 (left) and 0.2 (right), respectively. Last column shows the esimation results using MSE-optimal bandwidth calculated seperately for each side of the cutoff without using time fixed effect.}\n\\end{table}", sep = "")
  
  
  if (detail == TRUE & robust == TRUE & level == TRUE) {
    table = "tex/6-1. Entire_detail_robust_level.tex"
  } else if (detail == TRUE & robust == FALSE & level == TRUE) {
    table = "tex/6-2. Entire_detail_conv_level.tex"
  } else if (detail == FALSE & robust == TRUE& level == TRUE) {
    table = "tex/Ent_robust_level.tex"
  } else if (detail == FALSE & robust == FALSE& level == TRUE) {
    table = "tex/Ent_conv_level.tex"
  }
    else if (detail == TRUE & robust == TRUE & level == F) {
    table = "tex/6-1. Entire_detail_robust.tex"
  } else if (detail == TRUE & robust == FALSE & level == F ) {
    table = "tex/6-2. Entire_detail_conv.tex"
  } else if (detail == FALSE & robust == TRUE & level == F) {
    table = "tex/Ent_robust.tex"
  } else if (detail == FALSE & robust == FALSE & level == F) {
    table = "tex/Ent_conv.tex"
  }
  writeLines(table_name, table)
  cat(table_name)
}
