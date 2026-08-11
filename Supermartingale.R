
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
    
    # number of superhost/non-superhost listings in page p
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
