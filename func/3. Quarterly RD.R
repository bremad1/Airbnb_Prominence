run_quarter_regression <- function(data_input, pct = "0.05", quarter_label = "2023_3Q") {
  
  z <- rbind(data_input)
  
  avglist <- list()
  exlist <- list()
  difflist <- list()
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
            h = c(2 * est1[['bws']][1,1], 2 * est1[['bws']][1,2]),
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
        })
      }
    }
  }
  
  results_list <- list()
  
  estimate_matrix_df <- data.frame(
    "Data_Name" = rep(c("FULL", "Q1Q2", "Q2Q3", "Q3Q4", "Q1", "Q2", "Q3", "Q4"), each = 2),
    "Ex_super" = rep(c("t", "f"), 8),
    "N" = numeric(16),
    stringsAsFactors = FALSE
  )
  
  for (j in 1:5) {
    list_name <- paste0('results', j)
    
    for (i in 1:nrow(estimate_matrix_df)) {
      data_name <- estimate_matrix_df$Data_Name[i]
      Ex_super <- estimate_matrix_df$Ex_super[i]
      result_key <- paste(pct, Ex_super, data_name, sep = "_")
      result_data <- get(list_name)[[result_key]]
      
      if (!is.null(result_data)) {
        estimate_matrix_df[i, paste0("coef_h_conv")] <- result_data[["Estimate"]][1]
        estimate_matrix_df[i, paste0("coef_h_bc")] <- result_data[["Estimate"]][2]
        estimate_matrix_df[i, paste0("se_h_conv")] <- result_data[["se"]][1]
        estimate_matrix_df[i, paste0("se_h_bc")] <- result_data[["se"]][2]
        estimate_matrix_df[i, paste0("pv_h_conv")] <- result_data[["pv"]][1]
        estimate_matrix_df[i, paste0("pv_h_bc")] <- result_data[["pv"]][2]
        estimate_matrix_df[i, paste0("beta_Y_p_l_h")] <- result_data[["beta_Y_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_Y_p_r_h")] <- result_data[["beta_Y_p_r"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_l_h")] <- result_data[["beta_T_p_l"]][1]
        estimate_matrix_df[i, paste0("beta_T_p_r_h")] <- result_data[["beta_T_p_r"]][1]
        
        estimate_matrix_df$N_h[i] <- paste("[", paste(result_data[["N_h"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$N_b[i] <- paste("[", paste(result_data[["N_b"]], collapse = " "), "]", sep = "")
        estimate_matrix_df$h[i] <- paste("[", paste(round(result_data[["bws"]][1,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$b[i] <- paste("[", paste(round(result_data[["bws"]][2,],4), collapse = " "), "]", sep = "")
        estimate_matrix_df$obs_h[i] <- result_data[["N_h"]][1] + result_data[["N_h"]][2]
        estimate_matrix_df$obs_b[i] <- result_data[["N_b"]][1] + result_data[["N_b"]][2]
        estimate_matrix_df$N[i] <- sum(result_data[["N"]])
      }
    }
    results_list[[as.character(j)]] <- estimate_matrix_df
  }
  
  quarterly[[quarter_label]] <<- results_list
}
make_tex_table_detail <- function(quarter_label, output_filename) {
  
  tex_data <- data.frame(
    "Condition" = rep(c("FULL", "Q1Q2", "Q2Q3", "Q3Q4", "Q1", "Q2", "Q3", "Q4"), each = 2),
    "Ex_super" = rep(c("t", "f"), 8),
    "coef_h_conv" = round(cbind(quarterly[[quarter_label]][[1]]$coef_h_conv,
                                quarterly[[quarter_label]][[2]]$coef_h_conv,
                                quarterly[[quarter_label]][[3]]$coef_h_conv,
                                quarterly[[quarter_label]][[4]]$coef_h_conv,
                                quarterly[[quarter_label]][[5]]$coef_h_conv), 3),
    "se_h_conv" = round(cbind(quarterly[[quarter_label]][[1]]$se_h_conv,
                              quarterly[[quarter_label]][[2]]$se_h_conv,
                              quarterly[[quarter_label]][[3]]$se_h_conv,
                              quarterly[[quarter_label]][[4]]$se_h_conv,
                              quarterly[[quarter_label]][[5]]$se_h_conv), 3),
    "pv_h_conv" = cbind(quarterly[[quarter_label]][[1]]$pv_h_conv,
                        quarterly[[quarter_label]][[2]]$pv_h_conv,
                        quarterly[[quarter_label]][[3]]$pv_h_conv,
                        quarterly[[quarter_label]][[4]]$pv_h_conv,
                        quarterly[[quarter_label]][[5]]$pv_h_conv),
    "bws_h" = cbind(quarterly[[quarter_label]][[1]]$h),
    "N_h" = cbind(quarterly[[quarter_label]][[1]]$N_h,
                  quarterly[[quarter_label]][[2]]$N_h,
                  quarterly[[quarter_label]][[3]]$N_h,
                  quarterly[[quarter_label]][[4]]$N_h,
                  quarterly[[quarter_label]][[5]]$N_h)
  )
  
  tex_data <- tex_data %>%
    mutate(Ex_super = factor(Ex_super, levels = c("t", "f"))) %>%
    arrange(Ex_super)
  
  tex_table_detail <- paste0("\\begin{table}[]\n \\TABLE \n\ {", quarter_label, " \\label{", quarter_label, "_t}} {\\begin{tabular}{lccccc}\n")
  tex_table_detail <- paste0(tex_table_detail, "\\hline\n")
  tex_table_detail <- paste0(tex_table_detail, "Condition & (1) & (2) & (3) & (4) & (5) \\\\ \\hline\n")
  tex_table_detail <- paste0(tex_table_detail, "\\\\ \n")
  tex_table_detail <- paste0(tex_table_detail, "\\multicolumn{6}{l}{\\textbf{EX\\_Super==1}}\\\\ \n\\\\ \n")
  
  for (i in 1:16) {
    
    if (i == 9) {
      tex_table_detail <- paste0(tex_table_detail, "\\multicolumn{6}{l}{\\textbf{EX\\_Super==0}}\\\\ \n\\\\ \n")
    }
    
    condition <- tex_data$Condition[i]
    
    coef1 <- tex_data$coef_h_conv.1[i]
    coef2 <- tex_data$coef_h_conv.2[i]
    coef3 <- tex_data$coef_h_conv.3[i]
    coef4 <- tex_data$coef_h_conv.4[i]
    coef5 <- tex_data$coef_h_conv.5[i]
    
    se1 <- tex_data$se_h_conv.1[i]
    se2 <- tex_data$se_h_conv.2[i]
    se3 <- tex_data$se_h_conv.3[i]
    se4 <- tex_data$se_h_conv.4[i]
    se5 <- tex_data$se_h_conv.5[i]
    
    pv1 <- tex_data$pv_h_conv.1[i]
    pv2 <- tex_data$pv_h_conv.2[i]
    pv3 <- tex_data$pv_h_conv.3[i]
    pv4 <- tex_data$pv_h_conv.4[i]
    pv5 <- tex_data$pv_h_conv.5[i]
    
    bws <- tex_data$bws_h[i]
    
    N1 <- tex_data$N_h.1[i]
    N2 <- tex_data$N_h.2[i]
    N3 <- tex_data$N_h.3[i]
    N4 <- tex_data$N_h.4[i]
    N5 <- tex_data$N_h.5[i]
    
    # 별 찍기 함수
    star <- function(pv) {
      if (is.na(pv)) return("")
      else if (pv < 0.01) return("***")
      else if (pv < 0.05) return("**")
      else if (pv < 0.1) return("*")
      else return("")
    }
    
    # 테이블 내용 추가
    tex_table_detail <- paste0(tex_table_detail, paste(condition, "&",
                                                       paste0(coef1, star(pv1)), "&",
                                                       paste0(coef2, star(pv2)), "&",
                                                       paste0(coef3, star(pv3)), "&",
                                                       paste0(coef4, star(pv4)), "&",
                                                       paste0(coef5, star(pv5)),
                                                       "\\\\ \n"))
    
    tex_table_detail <- paste0(tex_table_detail, paste("&",
                                                       paste0("(", se1, ")"), "&",
                                                       paste0("(", se2, ")"), "&",
                                                       paste0("(", se3, ")"), "&",
                                                       paste0("(", se4, ")"), "&",
                                                       paste0("(", se5, ")"),
                                                       "\\\\ \n"))
    
    tex_table_detail <- paste0(tex_table_detail, paste("&", bws, "& & & & \\\\ \n"))
    
    tex_table_detail <- paste0(tex_table_detail, paste("&",
                                                       N1, "&", N2, "&", N3, "&", N4, "&", N5,
                                                       "\\\\ \n\\\\ \n"))
  }
  
  
  tex_table_detail <- paste0(tex_table_detail, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}")
  output_filename <- file.path("tex", output_filename)
  
  writeLines(tex_table_detail, output_filename)
}

make_tex_table <- function(quarter_label, output_filename) {
  
  tex_data <- data.frame(
    "Condition" = rep(c("FULL", "Q1Q2", "Q2Q3", "Q3Q4", "Q1", "Q2", "Q3", "Q4"), each = 2),
    "Ex_super" = rep(c("t", "f"), 8),
    "coef_h_conv" = round(cbind(quarterly[[quarter_label]][[1]]$coef_h_conv,
                                quarterly[[quarter_label]][[2]]$coef_h_conv,
                                quarterly[[quarter_label]][[3]]$coef_h_conv,
                                quarterly[[quarter_label]][[4]]$coef_h_conv,
                                quarterly[[quarter_label]][[5]]$coef_h_conv), 3),
    "se_h_conv" = round(cbind(quarterly[[quarter_label]][[1]]$se_h_conv,
                              quarterly[[quarter_label]][[2]]$se_h_conv,
                              quarterly[[quarter_label]][[3]]$se_h_conv,
                              quarterly[[quarter_label]][[4]]$se_h_conv,
                              quarterly[[quarter_label]][[5]]$se_h_conv), 3),
    "pv_h_conv" = cbind(quarterly[[quarter_label]][[1]]$pv_h_conv,
                        quarterly[[quarter_label]][[2]]$pv_h_conv,
                        quarterly[[quarter_label]][[3]]$pv_h_conv,
                        quarterly[[quarter_label]][[4]]$pv_h_conv,
                        quarterly[[quarter_label]][[5]]$pv_h_conv),
    "bws_h" = cbind(quarterly[[quarter_label]][[1]]$h),
    "N_h" = cbind(quarterly[[quarter_label]][[1]]$N_h,
                  quarterly[[quarter_label]][[2]]$N_h,
                  quarterly[[quarter_label]][[3]]$N_h,
                  quarterly[[quarter_label]][[4]]$N_h,
                  quarterly[[quarter_label]][[5]]$N_h)
  )
  
  tex_data <- tex_data %>%
    mutate(Ex_super = factor(Ex_super, levels = c("t", "f"))) %>%
    arrange(Ex_super)
  
  tex_table <- paste0("\\begin{table}[]\n \\TABLE \n\ {", quarter_label, " \\label{", quarter_label, "_t}} {\\begin{tabular}{lccccc}\n")
  tex_table <- paste0(tex_table, "\\hline\n")
  tex_table <- paste0(tex_table, "Condition & (1) & (2) & (3) & (4) & (5) \\\\ \\hline\n")
  tex_table <- paste0(tex_table, "\\\\ \n")
  tex_table <- paste0(tex_table, "\\multicolumn{6}{l}{\\textbf{EX\\_Super==1}}\\\\ \n\\\\ \n")
  
  for (i in 1:16) {
    
    if (i == 9) {
      tex_table_detail <- paste0(tex_table, "\\multicolumn{6}{l}{\\textbf{EX\\_Super==0}}\\\\ \n\\\\ \n")
    }
    
    condition <- tex_data$Condition[i]
    
    coef1 <- tex_data$coef_h_conv.1[i]
    coef2 <- tex_data$coef_h_conv.2[i]
    coef3 <- tex_data$coef_h_conv.3[i]
    coef4 <- tex_data$coef_h_conv.4[i]
    coef5 <- tex_data$coef_h_conv.5[i]
    
    se1 <- tex_data$se_h_conv.1[i]
    se2 <- tex_data$se_h_conv.2[i]
    se3 <- tex_data$se_h_conv.3[i]
    se4 <- tex_data$se_h_conv.4[i]
    se5 <- tex_data$se_h_conv.5[i]
    
    pv1 <- tex_data$pv_h_conv.1[i]
    pv2 <- tex_data$pv_h_conv.2[i]
    pv3 <- tex_data$pv_h_conv.3[i]
    pv4 <- tex_data$pv_h_conv.4[i]
    pv5 <- tex_data$pv_h_conv.5[i]
    
    bws <- tex_data$bws_h[i]
    
    N1 <- tex_data$N_h.1[i]
    N2 <- tex_data$N_h.2[i]
    N3 <- tex_data$N_h.3[i]
    N4 <- tex_data$N_h.4[i]
    N5 <- tex_data$N_h.5[i]
    
    # 별 찍기 함수
    star <- function(pv) {
      if (is.na(pv)) return("")
      else if (pv < 0.01) return("***")
      else if (pv < 0.05) return("**")
      else if (pv < 0.1) return("*")
      else return("")
    }
    
    # 테이블 내용 추가
    tex_table <- paste0(tex_table, paste(condition, "&",
                                                       paste0(coef1, star(pv1)), "&",
                                                       paste0(coef2, star(pv2)), "&",
                                                       paste0(coef3, star(pv3)), "&",
                                                       paste0(coef4, star(pv4)), "&",
                                                       paste0(coef5, star(pv5)),
                                                       "\\\\ \n"))
    
    tex_table <- paste0(tex_table, paste("&",
                                                       paste0("(", se1, ")"), "&",
                                                       paste0("(", se2, ")"), "&",
                                                       paste0("(", se3, ")"), "&",
                                                       paste0("(", se4, ")"), "&",
                                                       paste0("(", se5, ")"),
                                                       "\\\\ \n"))
    
  }
  
  
  tex_table <- paste0(tex_table, "\\hline \n \\end{tabular}}{\\textit{}}\n\\end{table}")
  
  writeLines(tex_table, output_filename)
  cat(tex_table)
}

