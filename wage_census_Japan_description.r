###########################################################
# Prefecture-level wage census of Japan
# Part 2 Description
# 25nd. September 2025
# Yuzuru Utsunomiya, Ph. D.
# 
# 
###########################################################
# 
# ----- read.library -----
library(tidyverse)
library(janitor)
library(forcats)
library(srvyr)        # survey design with tidy verbs
library(gtsummary)
library(gt)
library(arrow)
# 
# ----- read.data -----
# Load & clean 
wc_raw <- 
  tryCatch(
    arrow::read_feather("wage_panel.feather"),
    error = function(e) readr::read_csv("wage_panel.csv", guess_max = 1e6)
  )
# 
# ----- table 1 -----
# Clean names: N_of_employees -> n_of_employees
wc <- 
  wc_raw %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(
    n_of_employees = dplyr::if_else(
      is.na(n_of_employees), 
      0, 
      n_of_employees
    )
  )
# survey design
# 
svy <- 
  wc %>% 
  dplyr::filter(
    !is.na(n_of_employees), 
    n_of_employees > 0
    ) %>% 
  srvyr::as_survey(
    weights = n_of_employees
    )
# provide variables & labels
# For continuous variables to include (existence-checked)
vars_cont <- 
  c(
    "mean_age","length_of_service",
    "actual_number_of_scheduled_hours_worked",
    "actual_number_of_overtime_worked",
    "contractual_cash_earnings",
    "hourly_scheduled_cash_earnings",
    "annual_special_cash_earnings"
    ) %>% 
  intersect(names(wc))
# labels (no intersect here)
labels_cont <- 
  list(
    mean_age                                ~ "Mean age [years]",
    length_of_service                       ~ "Length of service [years]",
    actual_number_of_scheduled_hours_worked ~ "Scheduled hours worked [h/mo]",
    actual_number_of_overtime_worked        ~ "Overtime hours [h/mo]",
    contractual_cash_earnings               ~ "Contractual cash earnings [k JPY/mo]",
    hourly_scheduled_cash_earnings          ~ "Hourly scheduled cash earnings [JPY/h]",
    annual_special_cash_earnings            ~ "Annual special cash earnings [k JPY/yr]"
    )
# For top-k discrete versions (weighted by employees)
topk_age <- c(4)
topk_ind <- c(5)
# 
wc2 <- 
  svy$variables %>% 
  dplyr::as_tibble()  %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(
    age_class_top = forcats::fct_lump_n(age_class, n = topk_age, w = n_of_employees, other_level = "Other"),
    industry_top  = forcats::fct_lump_n(industry,  n = topk_ind,  w = n_of_employees, other_level = "Other"),
    company_size  = forcats::fct_explicit_na(company_size, na_level = "(Missing)"),
    gender        = forcats::fct_explicit_na(gender, na_level = "(Missing)")
  )
# 
svy2 <- 
  wc2 %>%  
  as_survey(weights = n_of_employees)
# Helper: one block (size==sz) with gender columns
make_block_cont <- 
  function(svy_data) {
    gtsummary::tbl_svysummary(
      data      = svy_data,
      by        = gender,
      include   = all_of(vars_cont),
      statistic = list(all_continuous() ~ "{mean} ({sd})"),
      digits    = list(all_continuous() ~ 1),
      missing   = "ifany",
      label     = labels_cont
      )  %>% 
      gtsummary::add_overall(last = TRUE) %>% 
      bold_labels()
}
# Replace the previous make_block_cat() with this version:
make_block_cat <- 
  function(svy_data, var_name, var_label) {
    var_sym <- rlang::sym(var_name)
    gtsummary::tbl_svysummary(
      data      = svy_data,
      by        = gender,
      include   = all_of(var_name),                # <- use string
      percent   = "column",
      type      = list(all_categorical() ~ "categorical"),
      statistic = list(all_categorical() ~ "{p}%"),
      digits    = list(all_categorical() ~ 1),
      missing   = "no",
      label     = list(!!var_sym ~ var_label)      # <- use sym for label mapping
      )  %>% 
    add_overall(last = TRUE)  %>% 
    bold_labels()
}
# Build Table 1A (Main): Continuous + Top discrete
# Tidyverse-style: build `block_list` with %>% and purrr::map
# Size levels
size_levels <- 
  svy2$variables$company_size %>% 
  droplevels() %>% 
  levels()
if (is.null(size_levels)) {
  size_levels <- 
    svy2$variables$company_size %>% unique() %>% 
    sort()
  }
# Build block_list with map() and %>%
block_list <- 
  size_levels %>%
  purrr::map(function(sz) {
    svy_sz <- 
      svy2 %>% 
      dplyr::filter(company_size == sz)
    list(
      make_block_cont(svy_sz),
      make_block_cat(svy_sz, "age_class_top", "Age class (top)"),
      make_block_cat(svy_sz, "industry_top",  "Industry (top)")
    ) %>%
      gtsummary::tbl_stack() %>%
      gtsummary::modify_header(all_stat_cols() ~ "**{level}**")
    }
    )
# Merge horizontally with spanner headers (still tidyverse-friendly)
table1a <- 
  gtsummary::tbl_merge(
    tbls = block_list,
    tab_spanner = paste0("**", size_levels, "**")
    )
table1a_gt <- table1a %>%
  gtsummary::as_gt() %>%
  gt::tab_caption("Table 1A. Weighted means (SD) and selected categorical distributions by company size × gender (2006–2024, prefecture-year panel)") %>%
  gt::tab_source_note(
    gt::md(
      "Notes: Continuous variables shown as mean (SD). Categorical variables shown as weighted column percentages. 'Top' categories are selected using employee-weighted frequency; remaining levels are grouped as 'Other'. All statistics are weighted by the number of employees."
      )
    )
table1a_gt
# Optionally save
gtsave(table1a_gt, "Table1A_main.html")
gtsave(table1a_gt, "Table1A_main.png", vwidth = 1800)
# 
# ----- appendix.table -----
# by industry
# We stack per company size to keep width reasonable.
make_industry_full <- 
  function(svy_data) {
    gtsummary::tbl_svysummary(
      data      = svy_data,
      by        = gender,
      include   = industry,
      percent   = "column",
      type      = list(all_categorical() ~ "categorical"),
      statistic = list(all_categorical() ~ "{p}%"),
      digits    = list(all_categorical() ~ 1),
      missing   = "no",
      label     = list(industry ~ "Industry")
      )  %>% 
    add_overall(last = TRUE) |>
    bold_labels()
  }
# 
a1_blocks <- 
  lapply(size_levels, function(sz) {
    svy2 %>%
      dplyr::filter(
        company_size == !!sz)  %>%  make_industry_full()
    }
    )
table_a1 <- 
  tbl_stack(
    a1_blocks, 
    group_header = paste0(
      "Company size: ", size_levels
      )
    )
table_a1_gt <- 
  table_a1 %>% 
  as_gt() %>% 
  tab_caption("Table A1. Weighted column percentages of Industry (all levels), stacked by company size × gender") |>
  tab_source_note(md("Notes: Weighted column percentages within gender columns. Overall is gender-pooled within each company size."))
# Print
table_a1_gt
# save
gtsave(table_a1_gt, "TableA1_industry_full.html")
# A2 by prefecture
# (Chropleth map is preferrable)
# To control width, we pool company sizes (still stratify by gender), stacked layout.
table_a2 <- 
  tbl_svysummary(
    data      = svy2,
    by        = gender,
    include   = prefecture,
    percent   = "column",
    type      = list(all_categorical() ~ "categorical"),
    statistic = list(all_categorical() ~ "{p}%"),
    digits    = list(all_categorical() ~ 1),
    missing   = "no",
    label     = list(prefecture ~ "Prefecture (all)")
    )  %>% 
  add_overall(last = TRUE) |>
  bold_labels()
# 
table_a2_gt <- table_a2 |>
  as_gt() |>
  tab_caption("Table A2. Weighted column percentages of Prefecture (all 47), pooled across company sizes × gender") |>
  tab_source_note(md("Notes: Weighted column percentages within gender columns. Overall is gender-pooled across all company sizes."))
# Print
table_a2_gt
# save
# gtsave(table_a2_gt, "TableA2_prefecture_full.html")
#     
# ----- correlation.matrices -----
# Correlation Matrices: weighted Pearson (main) + unweighted Spearman
# Variables of interest (continuous)
vars_cont <- 
  c(
    "mean_age","length_of_service",
    "actual_number_of_scheduled_hours_worked",
    "actual_number_of_overtime_worked",
    "contractual_cash_earnings",
    "hourly_scheduled_cash_earnings",
    "annual_special_cash_earnings"
    )  %>% 
  intersect(names(wc))
# Survey design for employee-weighted stats
stopifnot("n_of_employees" %in% names(wc))
svy <- wc |>
  filter(!is.na(n_of_employees), n_of_employees > 0) |>
  as_survey(weights = n_of_employees)
# Weighted Pearson correlation via survey-weighted covariance
# Build a one-sided formula like ~x1 + x2 + ... for svyvar()
form <- 
  as.formula(paste("~", paste(vars_cont, collapse = " + ")))
cov_mat <- survey::svyvar(form, design = svy, na.rm = TRUE)  # survey-weighted covariance
cov_mat <- as.matrix(cov_mat)
cor_weighted_pearson <- cov2cor(cov_mat)
# Round for display
cor_w_disp <- round(cor_weighted_pearson, 3)
# 
# ----- multicolinearity -----
# Choose continuous predictors for collinearity diagnostics
vars_cont <- c(
  "mean_age",
  "length_of_service",
  "actual_number_of_scheduled_hours_worked",
  "actual_number_of_overtime_worked",
  "hourly_scheduled_cash_earnings"
  )  %>%  
  intersect(names(wc))
# 
df <- 
  wc  %>%  
    dplyr::select(
      all_of(
        c(vars_cont, "n_of_employees")
        )
      )  %>% 
    dplyr::filter(
      if_all(all_of(vars_cont),
             ~ 
               !is.na(.)
             )
      ) %>% 
    dplyr::filter(
      !is.na(n_of_employees), 
      n_of_employees > 0
      )
# Threshold screen on weighted Pearson
# Reuse the saved matrix if you want; here we recompute quickly.
w <- df$n_of_employees
X <- df |> select(all_of(vars_cont)) |> as.matrix()
# Weighted covariance and correlation
Xc <- scale(X, center = TRUE, scale = FALSE)           # center only
w_norm <- w / sum(w)
mu <- colSums(w_norm * X)
Xcw <- sweep(X, 2, mu, "-")
cov_w <- t(Xcw * w_norm) %*% Xcw / (1 - sum(w_norm^2)) # finite-pop correction (optional)
cor_w <- cov2cor(cov_w)
# 
screen_pairs <- which(abs(cor_w) >= 0.8 & lower.tri(cor_w), arr.ind = TRUE) %>%
  as_tibble() %>%
  mutate(var1 = colnames(cor_w)[col], var2 = rownames(cor_w)[row],
         r = map2_dbl(row, col, ~ cor_w[.x, .y])) %>%
  select(var1, var2, r) %>%
  arrange(desc(abs(r)))
screen_pairs
# View candidates with |r| >= 0.8
# Clustering by 1 - |r| 
dist_mat <- 
  as.dist(1 - abs(cor_w))
hc <- 
  hclust(dist_mat, method = "average")
# Choose number of clusters (e.g., k = 3–5). Adjust as needed:
k <- 4
grp <- cutree(hc, k = k)
cluster_df <- 
  dplyr::tibble(variable = names(grp), cluster = grp) %>% arrange(cluster, variable)
cluster_df
#  Approximate weighted VIF via weighted R^2 
weighted_vif <- function(data, predictors, weight_col) {
  # Keep only needed columns and drop rows with any NA among them
  df_use <- data %>%
    dplyr::select(dplyr::all_of(c(predictors, weight_col))) %>%
    tidyr::drop_na()
  # 
  purrr::map_dfr(predictors, function(target) {
    others <- setdiff(predictors, target)
    # 
    # If there are no "others", VIF is undefined
    if (length(others) == 0) {
      return(tibble(variable = target, R2_w = NA_real_, VIF = NA_real_))
    }
    # 
    fml <- reformulate(termlabels = others, response = target)
    # 
    # NOTE: don't use .data here; pass explicit vector for weights
    fit <- lm(fml, data = df_use, weights = df_use[[weight_col]])
    # 
    r2 <- suppressWarnings(summary(fit)$r.squared)
    r2 <- ifelse(is.finite(r2), r2, NA_real_)
    # 
    # Guard against r2=1 due to perfect collinearity
    if (is.na(r2) || r2 >= 0.999999) {
      vif_val <- Inf
    } else {
      vif_val <- 1 / (1 - r2)
    }
    # 
    tibble(variable = target, R2_w = r2, VIF = vif_val)
    }
    ) %>%
    arrange(desc(VIF))
}
# vif
vif_tbl <- 
  weighted_vif(df, vars_cont, "n_of_employees")
vif_tbl
# Inspect variables with VIF > 5 or 10

