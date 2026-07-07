library(dplyr)
library(tidyr)
library(openxlsx)

library(broom)
library(ggplot2)

load('scrapped data/ratio_flex3.RData')
#load('scrapped data/ratio_flex6.RData')
load('scrapped data/ratio_flex12.RData')

# 계산 함수 (위와 동일)
compute_phi_table <- function(ratio, method) {
  ratio_f <- if (method == "method1") {
    ratio %>% filter(obs_total >= 19)
  } else {
    ratio %>% filter(obs_total >= 73)
  }
  
  exposure_f <- if (method == "method3") {
    ratio_f %>% filter(page_num <= 2)
  } else {
    ratio_f %>% filter(page_num == 1)
  }
  
  supply_df <- ratio_f %>%
    group_by(grid_id) %>%
    summarise(S_g = sum(superhost == 1), N_g = n(), s_g = S_g / N_g, .groups = "drop")
  
  exposure_df <- exposure_f %>%
    group_by(grid_id) %>%
    summarise(S1_g = sum(superhost == 1), N1_g = n(), r_g = S1_g / N1_g, .groups = "drop")
  
  phi_df <- left_join(exposure_df, supply_df, by = "grid_id") %>%
    mutate(phi_g = r_g / s_g)
  
  phi_bar <- mean(phi_df$phi_g, na.rm = TRUE)
  s_bar <- ratio_f %>%
    summarise(s_bar = sum(superhost == 1) / n()) %>%
    pull(s_bar)
  r_hat <- phi_bar * s_bar
  
  reg <- lm(r_g ~ s_g - 1, data = phi_df)
  coef_val <- coef(reg)[1]
  
  return(c(r_hat = r_hat, phi_bar = phi_bar, s_bar = s_bar, coef = coef_val))
}

# 메인 데이터셋과 라벨
datasets <- list(
  ratio_flex3 = ratio_flex3,
  #ratio_flex6 = ratio_flex6,
  ratio_flex12 = ratio_flex12
)

# 라텍스 테이블 생성
methods <- c("method1", "method2", "method3")
latex_lines <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "& Method 1 & Method 2 & Method 3 \\\\",
  "\\midrule"
)
# 방어적으로 행 존재 확인 + 길이 확인 후 sprintf 수행
safe_sprintf <- function(label, values) {
  if (is.null(values) || length(values) < 3 || any(is.na(values))) {
    return(paste0(label, " & NA & NA & NA \\\\"))
  } else {
    return(sprintf(paste0(label, " & %.3f & %.3f & %.3f \\\\"), values[1], values[2], values[3]))
  }
}

for (dname in names(datasets)) {
  latex_lines <- c(latex_lines, paste0("\\texttt{", dname, "} \\\\"))
  
  rows <- lapply(methods, function(m) compute_phi_table(datasets[[dname]], m))
  mat <- do.call(cbind, rows)
  rownames(mat) <- c("r_hat", "phi_bar", "s_bar", "coef")
  
  latex_lines <- c(latex_lines,
                   safe_sprintf("$\\hat{r}_g$", mat["r_hat", ]),
                   safe_sprintf("$\\bar{\\phi}_g$", mat["phi_bar", ]),
                   safe_sprintf("$\\bar{s}_g$", mat["s_bar", ]),
                   safe_sprintf("OLS coef", mat["coef", ]),
                   "\\midrule"
  )
  
}

latex_lines <- c(latex_lines[1:(length(latex_lines) - 1)], "\\bottomrule", "\\end{tabular}")

# 출력
cat(paste(latex_lines, collapse = "\n"))


############### (Unnecessary) Generate dataset #####################
build_G_mat <- function(ratio_df, K = 15) {
  
  library(dplyr)
  library(tidyr)
  
  # --------------------------------------------------
  # 1. grid-level summary
  # --------------------------------------------------
  G_mat <- ratio_df %>%
    group_by(grid_id) %>%
    summarise(
      N_g_tot = max(obs_total, na.rm = TRUE),
      A_g_tot = max(sup_total, na.rm = TRUE),
      max_page = ifelse(
        all(is.na(page_num)),
        0,
        max(page_num, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  
  # --------------------------------------------------
  # 2. page-level counts (NOT cumulative)
  #    N_g_k_tmp, A_g_k_tmp
  # --------------------------------------------------
  tmp <- ratio_df %>%
    filter(!is.na(grid_id), !is.na(page_num)) %>%
    filter(page_num >= 1, page_num <= K) %>%
    mutate(superhost = ifelse(is.na(superhost), 0, superhost)) %>%
    group_by(grid_id, page_num) %>%
    summarise(
      N_g_k_tmp = n(),                # page k의 listing 수
      A_g_k_tmp = sum(superhost),     # page k의 superhost 수
      .groups = "drop"
    )
  
  # --------------------------------------------------
  # 3. wide (tmp)
  # --------------------------------------------------
  tmp_wide <- tmp %>%
    pivot_wider(
      names_from  = page_num,
      values_from = c(N_g_k_tmp, A_g_k_tmp),
      names_glue  = "{.value}_{page_num}",
      values_fill = 0
    )
  
  # --------------------------------------------------
  # 4. merge
  # --------------------------------------------------
  G_mat_ext <- G_mat %>%
    left_join(tmp_wide, by = "grid_id")
  
  # --------------------------------------------------
  # 5. ensure all tmp columns exist (missing pages → 0)
  # --------------------------------------------------
  for (k in 1:K) {
    n_col <- paste0("N_g_k_tmp_", k)
    a_col <- paste0("A_g_k_tmp_", k)
    
    if (!n_col %in% names(G_mat_ext)) G_mat_ext[[n_col]] <- 0
    if (!a_col %in% names(G_mat_ext)) G_mat_ext[[a_col]] <- 0
  }
  
  # --------------------------------------------------
  # 6. cumulative sums
  # --------------------------------------------------
  for (k in 1:K) {
    N_cols <- paste0("N_g_k_tmp_", 1:k)
    A_cols <- paste0("A_g_k_tmp_", 1:k)
    
    G_mat_ext[[paste0("N_g_", k)]] <- rowSums(G_mat_ext[, N_cols, drop = FALSE])
    G_mat_ext[[paste0("A_g_", k)]] <- rowSums(G_mat_ext[, A_cols, drop = FALSE])
  }
  
  # --------------------------------------------------
  # 7. structural NA (k > max_page)
  # --------------------------------------------------
  for (k in 1:K) {
    G_mat_ext[[paste0("N_g_", k)]][G_mat_ext$max_page < k] <- NA
    G_mat_ext[[paste0("A_g_", k)]][G_mat_ext$max_page < k] <- NA
  }
  
  # --------------------------------------------------
  # 8. reorder columns
  # --------------------------------------------------
  front_cols <- c("grid_id", "N_g_tot", "A_g_tot", "max_page")
  page_cols  <- unlist(lapply(1:K, function(k) {
    c(paste0("N_g_", k), paste0("A_g_", k))
  }))
  
  G_mat_ext <- G_mat_ext[, c(front_cols, page_cols)]
  
  return(G_mat_ext)
}




g_mat3  <- build_G_mat(ratio_flex3,  K = 15)
g_mat12 <- build_G_mat(ratio_flex12, K = 15)


#wb <- createWorkbook()

# Sheet 1: g_mat6
#addWorksheet(wb, "ratio12")
#writeData(wb, sheet = "ratio12", g_mat12)

# Sheet 2: g_mat12
#addWorksheet(wb, "ratio3")
#writeData(wb, sheet = "ratio3", g_mat6)

# saveWorkbook(wb, "emp.xlsx", overwrite = TRUE)

##############2.1 Supermartingale NLS ##############
build_G_mat2 <- function(ratio_df, K = 15) {
  
  library(dplyr)
  library(tidyr)
  
  # --------------------------------------------------
  # 1. grid-level summary
  # --------------------------------------------------
  G_mat <- ratio_df %>%
    group_by(grid_id) %>%
    summarise(
      N_g_tot = max(obs_total, na.rm = TRUE),
      A_g_tot = max(sup_total, na.rm = TRUE),
      max_page = ifelse(
        all(is.na(page_num)),
        0,
        max(page_num, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  
  # --------------------------------------------------
  # 2. page-level counts
  # --------------------------------------------------
  tmp <- ratio_df %>%
    filter(!is.na(grid_id), !is.na(page_num)) %>%
    filter(page_num >= 1, page_num <= K) %>%
    mutate(superhost = ifelse(is.na(superhost), 0, superhost)) %>%
    group_by(grid_id, page_num) %>%
    summarise(
      n_A_p_tmp = sum(superhost),          # page p의 superhost 수
      n_B_p_tmp = sum(1 - superhost),      # page p의 non-superhost 수
      .groups = "drop"
    )
  
  # --------------------------------------------------
  # 3. wide
  # --------------------------------------------------
  tmp_wide <- tmp %>%
    pivot_wider(
      names_from  = page_num,
      values_from = c(n_A_p_tmp, n_B_p_tmp),
      names_glue  = "{.value}_{page_num}",
      values_fill = 0
    )
  
  # --------------------------------------------------
  # 4. merge
  # --------------------------------------------------
  G_mat_ext <- G_mat %>%
    left_join(tmp_wide, by = "grid_id")
  
  # --------------------------------------------------
  # 5. ensure all tmp columns exist (missing pages → 0)
  # --------------------------------------------------
  for (p in 1:K) {
    a_col <- paste0("n_A_p_tmp_", p)
    b_col <- paste0("n_B_p_tmp_", p)
    if (!a_col %in% names(G_mat_ext)) G_mat_ext[[a_col]] <- 0
    if (!b_col %in% names(G_mat_ext)) G_mat_ext[[b_col]] <- 0
  }
  
  # --------------------------------------------------
  # 6. n_A_p, n_B_p (page-level counts)
  #    N_A_p, N_B_p (cumulative from page p to end)
  # --------------------------------------------------
  for (p in 1:K) {
    
    # page p의 슈퍼호스트 / 논슈퍼호스트 수
    G_mat_ext[[paste0("n_A_", p)]] <- G_mat_ext[[paste0("n_A_p_tmp_", p)]]
    G_mat_ext[[paste0("n_B_", p)]] <- G_mat_ext[[paste0("n_B_p_tmp_", p)]]
    
    # p페이지 이전까지의 누적합 (1 ~ p-1)
    if (p == 1) {
      cum_A_before <- 0
      cum_B_before <- 0
    } else {
      cum_A_before <- rowSums(G_mat_ext[, paste0("n_A_p_tmp_", 1:(p-1)), drop = FALSE])
      cum_B_before <- rowSums(G_mat_ext[, paste0("n_B_p_tmp_", 1:(p-1)), drop = FALSE])
    }
    
    # p페이지부터 끝까지 = tot - (p 이전 누적)
    G_mat_ext[[paste0("N_A_", p)]] <- G_mat_ext$A_g_tot - cum_A_before
    G_mat_ext[[paste0("N_B_", p)]] <- (G_mat_ext$N_g_tot - G_mat_ext$A_g_tot) - cum_B_before
  }
  
  # --------------------------------------------------
  # 7. structural NA (p > max_page)
  # --------------------------------------------------
  for (p in 1:K) {
    mask <- G_mat_ext$max_page < p
    G_mat_ext[[paste0("n_A_", p)]][mask] <- NA
    G_mat_ext[[paste0("n_B_", p)]][mask] <- NA
    G_mat_ext[[paste0("N_A_", p)]][mask] <- NA
    G_mat_ext[[paste0("N_B_", p)]][mask] <- NA
  }
  
  # --------------------------------------------------
  # 8. reorder columns
  # --------------------------------------------------
  front_cols <- c("grid_id", "N_g_tot", "A_g_tot", "max_page")
  page_cols  <- unlist(lapply(1:K, function(p) {
    c(paste0("n_A_", p), paste0("n_B_", p),
      paste0("N_A_", p), paste0("N_B_", p))
  }))
  
  G_mat_ext <- G_mat_ext[, c(front_cols, page_cols)]
  
  return(G_mat_ext)
}
g_mat12 <- build_G_mat2(ratio_flex12)


df_nls <- g_mat12 %>%
  filter(max_page >= 6) %>%
  pivot_longer(
    cols = matches("^(n_A|n_B|N_A|N_B)_\\d+$"),
    names_to = c(".value", "page_num"),
    names_pattern = "^(.+)_(\\d+)$"
  ) %>%
  rename(n_Ap = n_A, n_Bp = n_B, N_Ap = N_A, N_Bp = N_B) %>%
  mutate(
    page_num = as.integer(page_num),
    page_size = n_Ap + n_Bp,
    y = n_Ap / page_size
  ) %>%
  filter(
    page_num %in% 1:5,
    !is.na(n_Ap), !is.na(n_Bp), !is.na(N_Ap), !is.na(N_Bp),
    page_size > 0,
    N_Ap + N_Bp > 0
  ) %>%
  filter(page_num < max_page | max_page == 15)

fit_common <- nls(
  y ~ (alpha * N_Ap) / (alpha * N_Ap + (1 - alpha) * N_Bp),
  data = df_nls,
  start = list(alpha = 0.5),
  lower = 1e-6,
  upper = 1 - 1e-6,
  algorithm = "port"
)

alpha_common <- coef(fit_common)["alpha"]
rss_common <- sum(resid(fit_common)^2)

summary(fit_common)


#2

results_nls <- g_mat12 %>%
  filter(max_page >= 6) %>%
  pivot_longer(
    cols = matches("^(n_A|n_B|N_A|N_B)_\\d+$"),
    names_to = c(".value", "page_num"),
    names_pattern = "^(.+)_(\\d+)$"
  ) %>%
  rename(n_Ap = n_A, n_Bp = n_B, N_Ap = N_A, N_Bp = N_B) %>%
  mutate(
    page_num = as.integer(page_num),
    page_size = n_Ap + n_Bp
  ) %>%
  filter(
    page_num %in% 1:5,
    !is.na(n_Ap), !is.na(n_Bp), !is.na(N_Ap), !is.na(N_Bp),
    page_size > 0,
    N_Ap + N_Bp > 0
  ) %>%
  filter(page_num < max_page | max_page == 15)%>%
  group_by(page_num) %>%
  group_map(~ {
    fit <- nls(
      n_Ap / page_size ~
        (alpha * N_Ap) / (alpha * N_Ap + (1 - alpha) * N_Bp),
      data = .x,
      start = list(alpha = 0.5),
      lower = 1e-6,
      upper = 1 - 1e-6,
      algorithm = "port"
    )
    
    tibble(
      page_num = .y$page_num,
      alpha = unname(coef(fit)["alpha"]),
      se = summary(fit)$coefficients["alpha", "Std. Error"],
      n_obs = nrow(.x)
    )
  }, .keep = TRUE) %>%
  bind_rows()
p <- results_nls %>%
  mutate(
    ci_lower = alpha - 1.96 * se,
    ci_upper = alpha + 1.96 * se
  ) %>%
  ggplot(aes(x = page_num, y = alpha)) +
  geom_ribbon(
    aes(ymin = ci_lower, ymax = ci_upper),
    fill = "#b3b3b3",   # grey30을 50% 투명도로 섞은 것과 유사한 색상
    color = NA
  ) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "black", size = 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey30") +
  scale_x_continuous(breaks = 1:5) +
  labs(
    x = "Page",
    y = expression(alpha[t]),
    title = expression("NLS estimates of " ~ alpha[t] ~ " by page")
  ) +
  theme_bw()
p
ggsave("Figure/supem.eps", plot = p, width = 6, height = 4, device = "eps")
ggsave("Figure/supem.eps", width = 6, height = 4, device = "eps")

#3 Test

pages <- sort(unique(df_nls$page_num))
P <- length(pages)

page_index <- match(df_nls$page_num, pages)

pred_fun <- function(alpha_vec, data = df_nls, page_index = page_index) {
  a <- alpha_vec[page_index]
  (a * data$N_Ap) / (a * data$N_Ap + (1 - a) * data$N_Bp)
}

obj_page <- function(alpha_vec) {
  pred <- pred_fun(alpha_vec)
  sum((df_nls$y - pred)^2)
}

fit_page <- optim(
  par = rep(0.5, P),
  fn = obj_page,
  method = "L-BFGS-B",
  lower = rep(1e-6, P),
  upper = rep(1 - 1e-6, P)
)

alpha_page <- fit_page$par
rss_page <- fit_page$value

results_page <- tibble(
  page_num = pages,
  alpha = alpha_page
)

results_page


n <- nrow(df_nls)

k_common <- 1
k_page <- P

df_num <- k_page - k_common
df_den <- n - k_page

F_stat <- ((rss_common - rss_page) / df_num) / (rss_page / df_den)
p_val <- 1 - pf(F_stat, df_num, df_den)

test_equal_alpha <- tibble(
  rss_common = rss_common,
  rss_page = rss_page,
  df_num = df_num,
  df_den = df_den,
  F_stat = F_stat,
  p_value = p_val
)

test_equal_alpha

############## (Unnecessary) Supermartingale OLS ##########################


test = g_mat12%>%filter(max_page>=3)
for (p in 1:15) {
  y_col <- paste0("Y_", p)
  x_col <- paste0("X_", p)
  
  n_A <- test[[paste0("n_A_", p)]]
  n_B <- test[[paste0("n_B_", p)]]
  N_A <- test[[paste0("N_A_", p)]]
  N_B <- test[[paste0("N_B_", p)]]
  
  # n_A_p or N_A_p가 0이면 NA
  test[[y_col]] <- ifelse(
    is.na(n_A) | is.na(N_A) | n_A == 0 | N_A == 0,
    NA_real_,
    (n_A + n_B) / n_A - (N_A - N_B) / N_A
  )
  
  test[[x_col]] <- ifelse(
    is.na(N_A) | N_A == 0,
    NA_real_,
    N_B / N_A
  )
}

results <- data.frame(
  page   = 1:15,
  beta_p = NA_real_,
  se_p   = NA_real_,
  d_p    = NA_real_,
  d_lo   = NA_real_,  # CI lower (1/U)
  d_hi   = NA_real_,  # CI upper (1/L)
  n      = NA_integer_
)
for (p in 1:15) {
  y_col <- paste0("Y_", p)
  x_col <- paste0("X_", p)
  
  df_p <- test %>%
    select(all_of(c(y_col, x_col, "max_page"))) %>%
    rename(Y = 1, X = 2) %>%
    filter(!is.na(Y), !is.na(X)) %>%
    filter(max_page > p | max_page == 15)   
  if (nrow(df_p) < 3) next
  
  fit <- lm(Y ~ X - 1, data = df_p)
  s   <- summary(fit)
  
  bp  <- coef(fit)[1]
  se  <- s$coefficients[1, 2]
  L   <- bp - 1.96 * se
  U   <- bp + 1.96 * se
  
  results[p, "beta_p"] <- bp
  results[p, "U"] <- U
  results[p, "L"] <- L
  results[p, "se_p"]   <- se
  results[p, "d_p"]    <- 1 / bp
  results[p, "d_lo"]   <- 1 / U
  results[p, "d_hi"]   <- 1 / L
  results[p, "n"]      <- nrow(df_p)
}

# plot
results_clean <- results %>% filter(!is.na(beta_p))

ggplot(results_clean, aes(x = page, y = beta_p)) +
  geom_ribbon(aes(ymin = L, ymax = U),
              fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 2.5) +
  scale_x_continuous(breaks = results_clean$page) +
  labs(
    x     = "Page (p)",
    y     = expression("(β"[p]*")"),
    title = "Estimated discrimination parameter by page"
  ) +
  theme_bw()

ggplot(results_clean, aes(x = page, y = d_p)) +
  #geom_ribbon(aes(ymin = d_lo, ymax = d_hi),
  #            fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 2.5) +
  scale_x_continuous(breaks = results_clean$page) +
  labs(
    x     = "Page (p)",
    y     = expression(d[p] ~ "(= 1/β"[p]*")"),
    title = "Estimated discrimination parameter by page"
  ) +
  theme_bw()



