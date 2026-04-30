# Austrian Electoral Polarization — Difference-in-Differences Analysis
#
# Estimates the effect of compulsory voting (CV) on electoral polarization
# using a difference-in-differences design across four analyses:
#   (1) 1986 CV implementation in Carinthia        — raw polarization score
#   (2) 1992 CV repeal in Styria, Vorarlberg, Tyrol — raw polarization score
#   (3) 1986 CV implementation in Carinthia        — standardised score
#   (4) 1992 CV repeal in Styria, Vorarlberg, Tyrol — standardised score
#
# Each analysis includes eight specifications varying the estimation window
# (narrow vs. wide) and fixed-effect structure (two-way, state-only,
# year-only, none). Standard errors are clustered at the state level.
#
# Primary source:
#   - election_polarization.dta: constructed in data-cleaning.R

# Final output: regression outputs printed on console


# 0. Dependencies
library(tidyverse)
library(haven)
library(fixest)
library(skimr)


# 1. Data Ingestion and Sample Construction
PATH_DATA <- "/Users/ryansi/Downloads/Code/R/austria-cv-polarization-did/election_polarization.dta"

UNTREATED <- c(
  "Upper Austria", "Lower Austria", "Burgenland",
  "Salzburg", "Vienna", "Carinthia"
)

CV_REPEAL_STATES <- c("Styria", "Vorarlberg", "Tyrol")

CONTROLS <- "unemployed_perc + l_pop"

df_clean <- read_dta(PATH_DATA) |>
  mutate(state_name = as_factor(state_name))

# Panel excluding Carinthia: used for the 1992 repeal analysis
df_no_carinthia <- df_clean |>
  group_by(state_code) |>
  mutate(ever_treated = max(CV)) |>
  ungroup() |>
  filter(state_name != "Carinthia")

# Carinthia and its donor pool: used for the 1986 implementation analysis
df_carinthia <- df_clean |>
  group_by(state_code) |>
  mutate(ever_treated = max(CV)) |>
  ungroup() |>
  filter(state_name %in% UNTREATED)


# 2. Helper: Fit Four Fixed-Effect Specifications
# Fits the same outcome ~ treatment + controls formula across four FE
# structures: (state + year), (state only), (year only), (none).
# Returns a named list of feols objects.

fit_fe_variants <- function(formula_rhs, data, cluster = ~state_name) {
  list(
    twoway = feols(as.formula(paste(formula_rhs, "| state_name + year")), data = data, cluster = cluster),
    no_tfe = feols(as.formula(paste(formula_rhs, "| state_name")),        data = data, cluster = cluster),
    no_sfe = feols(as.formula(paste(formula_rhs, "| year")),              data = data, cluster = cluster),
    no_fe  = feols(as.formula(formula_rhs),                               data = data, cluster = cluster)
  )
}


# 3. Analysis 1: 1986 CV Implementation in Carinthia
# Treatment: Carinthia × (year >= 1986)
# Donor pool: Carinthia + five never-treated states
# Windows: narrow (1976–1991), wide (1956–1991)
# Outcome: polarization_score

df_reg_impl <- df_carinthia |>
  mutate(
    CV           = as.integer(trimws(state_name) == "Carinthia" & year >= 1986),
    repeal_shock = as.integer(trimws(state_name) == "Carinthia" & year >= 1992)
  )

FORMULA_IMPL <- paste("polarization_score ~ CV +", CONTROLS)

models_impl_sr <- fit_fe_variants(FORMULA_IMPL, data = df_reg_impl |> filter(between(year, 1976, 1991)))
models_impl_lr <- fit_fe_variants(FORMULA_IMPL, data = df_reg_impl |> filter(between(year, 1956, 1991)))

etable(
  models_impl_sr$twoway, models_impl_sr$no_tfe,
  models_impl_sr$no_sfe, models_impl_sr$no_fe,
  models_impl_lr$twoway, models_impl_lr$no_tfe,
  models_impl_lr$no_sfe, models_impl_lr$no_fe,
  headers = c(
    "Impl. SR", "Impl. SR (no TFE)", "Impl. SR (no SFE)", "Impl. SR (no FE)",
    "Impl. LR", "Impl. LR (no TFE)", "Impl. LR (no SFE)", "Impl. LR (no FE)"
  ),
  depvar = TRUE,
  digits = 4
)


# 4. Analysis 2: 1992 CV Repeal in Styria, Vorarlberg, and Tyrol
# Treatment: {Styria, Vorarlberg, Tyrol} × (year >= 1992)
# Donor pool: never-treated states (Carinthia excluded)
# Windows: narrow (1976–), wide (1956–)
# Outcome: polarization_score

df_reg_repeal <- df_no_carinthia |>
  mutate(
    CV_repealed = as.integer(state_name %in% CV_REPEAL_STATES & year >= 1992)
  )

FORMULA_REPEAL <- paste("polarization_score ~ CV_repealed +", CONTROLS)

models_repeal_sr <- fit_fe_variants(FORMULA_REPEAL, data = df_reg_repeal |> filter(year >= 1976))
models_repeal_lr <- fit_fe_variants(FORMULA_REPEAL, data = df_reg_repeal |> filter(year >= 1956))

etable(
  models_repeal_sr$twoway, models_repeal_sr$no_tfe,
  models_repeal_sr$no_sfe, models_repeal_sr$no_fe,
  models_repeal_lr$twoway, models_repeal_lr$no_tfe,
  models_repeal_lr$no_sfe, models_repeal_lr$no_fe,
  headers = c(
    "Repeal SR", "Repeal SR (no TFE)", "Repeal SR (no SFE)", "Repeal SR (no FE)",
    "Repeal LR", "Repeal LR (no TFE)", "Repeal LR (no SFE)", "Repeal LR (no FE)"
  ),
  depvar = TRUE,
  digits = 4
)


# 5. Analysis 3: 1986 CV Implementation — Standardised Outcome
# Identical design to Analysis 1 but with polarization_score standardised
# (mean 0, SD 1) to facilitate effect-size interpretation across specifications.

df_reg_impl_std <- df_reg_impl |>
  mutate(polarization_score_std = as.numeric(scale(polarization_score)))

FORMULA_IMPL_STD <- paste("polarization_score_std ~ CV +", CONTROLS)

models_impl_std_sr <- fit_fe_variants(FORMULA_IMPL_STD, data = df_reg_impl_std |> filter(between(year, 1976, 1991)))
models_impl_std_lr <- fit_fe_variants(FORMULA_IMPL_STD, data = df_reg_impl_std |> filter(between(year, 1956, 1991)))

etable(
  models_impl_std_sr$twoway, models_impl_std_sr$no_tfe,
  models_impl_std_sr$no_sfe, models_impl_std_sr$no_fe,
  models_impl_std_lr$twoway, models_impl_std_lr$no_tfe,
  models_impl_std_lr$no_sfe, models_impl_std_lr$no_fe,
  headers = c(
    "Impl. SR (std)", "Impl. SR (std, no TFE)", "Impl. SR (std, no SFE)", "Impl. SR (std, no FE)",
    "Impl. LR (std)", "Impl. LR (std, no TFE)", "Impl. LR (std, no SFE)", "Impl. LR (std, no FE)"
  ),
  depvar = TRUE,
  digits = 4
)


# 6. Analysis 4: 1992 CV Repeal — Standardised Outcome
# Identical design to Analysis 2 but with polarization_score standardised
# (mean 0, SD 1) to facilitate effect-size interpretation across specifications.

df_reg_repeal_std <- df_reg_repeal |>
  mutate(polarization_score_std = as.numeric(scale(polarization_score)))

FORMULA_REPEAL_STD <- paste("polarization_score_std ~ CV_repealed +", CONTROLS)

models_repeal_std_sr <- fit_fe_variants(FORMULA_REPEAL_STD, data = df_reg_repeal_std |> filter(year >= 1976))
models_repeal_std_lr <- fit_fe_variants(FORMULA_REPEAL_STD, data = df_reg_repeal_std |> filter(year >= 1956))

etable(
  models_repeal_std_sr$twoway, models_repeal_std_sr$no_tfe,
  models_repeal_std_sr$no_sfe, models_repeal_std_sr$no_fe,
  models_repeal_std_lr$twoway, models_repeal_std_lr$no_tfe,
  models_repeal_std_lr$no_sfe, models_repeal_std_lr$no_fe,
  headers = c(
    "Repeal SR (std)", "Repeal SR (std, no TFE)", "Repeal SR (std, no SFE)", "Repeal SR (std, no FE)",
    "Repeal LR (std)", "Repeal LR (std, no TFE)", "Repeal LR (std, no SFE)", "Repeal LR (std, no FE)"
  ),
  depvar = TRUE,
  digits = 4
)