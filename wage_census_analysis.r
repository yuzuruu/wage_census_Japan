###########################################################
# Prefecture-level wage census of Japan
# Part 3 Analysis
# 
# Made: 25nd. September 2025
# Revised: 1st. October 2025
# Yuzuru Utsunomiya, Ph. D.
# 
###########################################################
# 
# ----- read.library -----
library(tidyverse)
library(janitor)
library(brms)
library(posterior)
library(bayesplot)
library(cmdstanr)
library(tidyverse)
library(brms)
library(broom.mixed)
# Prefer cmdstanr as default backend
options(brms.backend = "cmdstanr")
# Optional: reduce console chatter
options(mc.cores = parallel::detectCores())
# 
# ----- load.data -----
# 
wc <- tryCatch(
  arrow::read_feather("wage_panel.feather"),
  error = function(e) readr::read_csv("wage_panel.csv", guess_max = 1e6)
  ) %>% 
  clean_names()
# Keep only needed variables; drop obvious NAs
vars_use <- c(
  "year","gender","prefecture","division","industry",
  "age_class","company_size","mean_age","length_of_service",
  "hourly_scheduled_cash_earnings","n_of_employees"
  )
# 
df0 <- 
  wc %>%
  dplyr::select(any_of(vars_use)) %>%
  # If 'industry' is actually the division, normalize name:
  dplyr::mutate(
    division = ifelse(
      !is.na(industry) & (is.na(division) | division == ""),
      industry, 
      division
      )
    ) %>%
  dplyr::drop_na(
    n_of_employees, gender, prefecture, division,
    age_class, company_size, mean_age, length_of_service,
    hourly_scheduled_cash_earnings, year
    )
# 
# ----- refine.data -----
# Factor handling & scaling
# Ordered factors -> monotonic effects
# Ensure levels are in ascending order (edit if needed)
df0 <- 
  df0 %>%
  dplyr::mutate(
    gender       = factor(gender),
    prefecture   = factor(prefecture),
    division     = factor(division),
    # age_class and company_size are treated as ordered for mo()
    age_class    = factor(age_class, ordered = TRUE),
    company_size = factor(company_size, ordered = TRUE),
    # scale continuous
    year_sc      = as.numeric(scale(year)),
    mean_age_sc  = as.numeric(scale(mean_age)),
    los_sc       = as.numeric(scale(length_of_service)),
    wage_hour_sc = as.numeric(scale(hourly_scheduled_cash_earnings))
  )
# Split by gender
dlist <- split(
  df0, df0$gender
  )
# Families & priors
fam_nb <- 
  negbinomial(link = "log")
# set prior
priors <- c(
  # fixed effects (scaled predictors) ~ Normal(0, 1)
  prior(normal(0, 1), class = "b"),
  # intercept (on log scale) Student-t
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  # random effects sd
  prior(exponential(1), class = "sd"),
  # residual overdispersion (shape parameter)
  prior(exponential(1), class = "shape"),
  # monotonic simplex weights get default priors; can be adjusted via class = "simo"
  prior(dirichlet(2), class = "simo") # weakly informative over simplex
)
# Formulas 
# NOTE: 
# mo(age_class), mo(company_size) -> monotonic effects for ordered factors
# M0: no interaction, division as random intercept
form_M0 <- bf(
  n_of_employees ~
    year_sc + mean_age_sc + los_sc + wage_hour_sc +
    mo(age_class) + mo(company_size) +
    (1 + year_sc | prefecture) + (1 | division)
)
# M1: wage x size interaction, division random intercept
form_M1 <- bf(
  n_of_employees ~
    year_sc + mean_age_sc + los_sc + wage_hour_sc * mo(company_size) +
    mo(age_class) +
    (1 + year_sc | prefecture) + (1 | division)
)
# 
# ----- analyses.brms ------
# Fit function
# Inspect the exact 'simo' prior names required for your formula
gp_M1 <- get_prior(
  formula = form_M1,
  data    = dlist[[1]],            # e.g., the Female subset
  family  = negbinomial(link = "log")
)
gp_M1 %>% filter(class == "simo")
# Build Dirichlet vectors with length = (#levels - 1) for each monotonic predictor
K_age  <- nlevels(dlist[[1]]$age_class)    - 1
K_size <- nlevels(dlist[[1]]$company_size) - 1
# prior
priors_base <- c(
  prior(normal(0, 1), class = "b"),
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(exponential(1), class = "sd"),
  prior(exponential(1), class = "shape")
)
# Replace 'moage_class1' / 'mocompany_size1' with those shown by get_prior()
priors_simo <- c(
  prior(dirichlet(rep(1, K_age)),  class = "simo", coef = "moage_class1"),
  prior(dirichlet(rep(1, K_size)), class = "simo", coef = "mocompany_size1")
)
priors <- c(
  priors_base, priors_simo
  )
# magic words
options(brms.backend = "cmdstanr")
Sys.setenv(STAN_NUM_THREADS = parallel::detectCores())
# 
fit_one <- 
  function(dat, form, family = negbinomial(link = "log"),
           chains = 4, cores = 4, threads_per_chain = 4, 
           grainsize = 250,iter = 2000, warmup = 800, 
           adapt_delta = 0.9) 
    {
    brms::brm(
      formula = form,
      family  = family,
      data    = dat,
      prior   = priors,                 # ← 既存の priors をそのまま
      backend = "cmdstanr",
      chains  = chains,
      cores   = cores,                  # 並列チェイン
      threads = threading(threads_per_chain, grainsize = grainsize),  # チェイン内並列
      iter    = iter, warmup = warmup, seed = 123,
      control = list(adapt_delta = adapt_delta, max_treedepth = 12),
      inits   = 0,                      # 収束を安定させる簡便策
      save_model = "brms_stancode.stan",
      file_refit = "on_change",         # Stanコード/データが変わらなければ再コンパイル回避
      refresh = 50
    )
    }
# Fit per gender (M0 and M1) ----------------------------------------------
fits <- 
  lapply(dlist, function(d) {
    list(
      M0 = fit_one(d, form_M0),
      M1 = fit_one(d, form_M1)
      )
    }
    )
# 
# ----- model.evaluation -----
# Model comparison with LOO 
loos <- lapply(fits, function(lst) lapply(lst, loo))
# Example access: loos[["Female"]][["M1"]]
compare_tbl <- lapply(loos, function(x) loo_compare(x$M0, x$M1))
compare_tbl
# Diagnosis
ppc_plots <- lapply(fits, function(lst) {
  lapply(lst, function(fm) {
    p1 <- pp_check(fm, type = "dens_overlay")
    p2 <- pp_check(fm, type = "rootogram")
    list(dens = p1, rootogram = p2)
  })
})
# print(ppc_plots[["Female"]][["M1"]]$dens) etc.
# summary
summaries <- lapply(fits, function(lst) lapply(lst, summary))
# Conditional effects (marginal effects)
meffs <- lapply(fits, function(lst) {
  list(
    wage = conditional_effects(lst$M1, effects = "wage_hour_sc:mo(company_size)"),
    year = conditional_effects(lst$M1, effects = "year_sc", re_formula = NA)
  )
})
# Save models -------------------------------------------------------------
saveRDS(fits, file = "brms_nb_gender_models.rds")
# 
# ----- check.postfit -----
# Inputs expected:
# - fits_main: list(Female = list(M0=..., M1=...), Male = list(M0=..., M1=...))
# If you used different object names, adjust below.

# Convergence & sampler diagnostics 
check_convergence <- function(fit) {
  dr <- as_draws_df(fit)
  summ <- fit %>% posterior::summarize_draws()
  list(
    rhat_worst = max(summ$rhat, na.rm = TRUE),
    ess_bulk_min = min(summ$ess_bulk, na.rm = TRUE),
    ess_tail_min = min(summ$ess_tail, na.rm = TRUE),
    n_divergent = sum(attr(dr, "sampler_diagnostics")$divergent__ %||% 0)
  )
}
# diag
diag_table <- purrr::imap_dfr(fits_main, function(models, g) {
  purrr::imap_dfr(models, function(fm, mname) {
    d <- check_convergence(fm)
    tibble(gender = g, model = mname, !!!d)
  })
})
readr::write_csv(diag_table, "brms_diagnostics.csv")
diag_table
# Fixed effects table (exp(beta) with 95% CI)
tidy_fixed <- function(fm) {
  broom.mixed::tidy(fm, effects = "fixed", conf.int = TRUE) %>%
    mutate(across(c(estimate, conf.low, conf.high), exp)) %>%
    mutate(term = as.character(term))
}
# 
fixed_tbl <- purrr::imap_dfr(fits_main, function(models, g) {
  purrr::imap_dfr(models, function(fm, mname) {
    tidy_fixed(fm) %>% mutate(gender = g, model = mname)
  })
}) %>% select(gender, model, term, estimate, conf.low, conf.high, p.value)
readr::write_csv(fixed_tbl, "brms_fixed_effects_exp.csv")
# LOO
loo_tbl <- purrr::imap_dfr(fits_main, function(models, g) {
  l0 <- loo(models$M0); l1 <- loo(models$M1)
  cmp <- loo_compare(l0, l1) %>% as.data.frame() %>% rownames_to_column("model")
  cmp %>% mutate(gender = g)
})
readr::write_csv(loo_tbl, "brms_loo_compare.csv")
loo_tbl
# PPC plots
dir.create("fig_ppc", showWarnings = FALSE)
purrr::iwalk(fits_main, function(models, g) {
  purrr::iwalk(models, function(fm, mname) {
    p1 <- pp_check(fm, type = "dens_overlay")
    p2 <- pp_check(fm, type = "rootogram")
    ggsave(sprintf("fig_ppc/ppc_dens_%s_%s.png", g, mname), p1, width = 7, height = 5, dpi = 300)
    ggsave(sprintf("fig_ppc/ppc_rootogram_%s_%s.png", g, mname), p2, width = 7, height = 5, dpi = 300)
  })
})
# Marginal effects
dir.create("fig_marginal", showWarnings = FALSE)
plot_marginal_wage <- function(fm) {
  ce <- conditional_effects(fm, effects = "wage_hour_sc:mo(company_size)")
  plot(ce)[[1]] + labs(y = "E[n_of_employees]", x = "wage_hour_sc (z)")
}
purrr::iwalk(fits_main, function(models, g) {
  p <- plot_marginal_wage(models$M1) + ggtitle(paste("Marginal effects -", g))
  ggsave(sprintf("fig_marginal/marginal_wage_size_%s.png", g), p, width = 7, height = 5, dpi = 300)
})
# Random effects
# Prefecture: intercept & year_sc slopes (uncorrelated in your spec)
re_tbl <- purrr::imap_dfr(fits_main, function(models, g) {
  re <- ranef(models$M1, summary = TRUE)
  # 'prefecture' and 'division' groups expected
  bind_rows(
    as_tibble(re$prefecture) %>% mutate(group = "prefecture", gender = g),
    as_tibble(re$division)   %>% mutate(group = "division",   gender = g)
  )
})
readr::write_csv(re_tbl, "brms_random_effects.csv")
# Optional quick plot for prefecture intercepts
dir.create("fig_random", showWarnings = FALSE)
plot_re <- function(fm, group = "prefecture", par = "(Intercept)") {
  re <- ranef(fm, summary = TRUE)[[group]] %>% as_tibble() %>% filter(term == par)
  ggplot(re, aes(reorder(level, estimate), estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0) +
    coord_flip() + labs(x = group, y = paste0(par, " (log scale)"))
}
purrr::iwalk(fits_main, function(models, g) {
  p <- plot_re(models$M1, "prefecture", "(Intercept)") + ggtitle(paste("RE Intercepts -", g))
  ggsave(sprintf("fig_random/re_intercepts_pref_%s.png", g), p, width = 7, height = 9, dpi = 300)
})
# Save fitted objects
saveRDS(fits_main, file = "brms_nb_gender_models_main.rds")

