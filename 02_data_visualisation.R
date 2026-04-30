# Austrian Electoral Polarization — Descriptive Plots
#
# Produces all descriptive and diagnostic figures for the compulsory
# voting difference-in-differences analysis. Figures cover ideological
# positions (RILE scores), parallel trends diagnostics, and raw
# outcome comparisons for the 1986 implementation and 1992 repeal.
#
# Primary source:
#   - election_polarization.dta: constructed in data_construction.R
#
# Final output: eight figures printed to console (ggsave calls included)


# 0. Dependencies
library(tidyverse)
library(haven)


# 1. Load Data
PATH_DATA <- "/filepath/election_polarization.dta"

CARINTHIA_DONOR_POOL <- c(
  "Upper Austria", "Lower Austria", "Burgenland",
  "Salzburg", "Vienna", "Carinthia"
)

COVARIATE_LABELS <- c(
  "unemployed_perc" = "Unemployment (%)",
  "turnout"         = "Turnout",
  "l_pop"           = "Log Population",
  "sh_winner"       = "Winner's Vote Share",
  "invalid"         = "Invalid Votes",
  "minor"           = "Minor Party Vote Share"
)

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
  filter(state_name %in% CARINTHIA_DONOR_POOL)


# 2. Shared Plot Theme
# Extends theme_bw() with formatting conventions applied across all figures.

theme_paper <- function() {
  theme_bw(base_size = 13) +
    theme(
      plot.title        = element_text(face = "bold", size = 12),
      plot.subtitle     = element_text(size = 10, color = "gray30"),
      panel.grid.minor  = element_blank(),
      legend.key        = element_blank(),
      axis.text.x       = element_text(angle = 45, hjust = 1)
    )
}


# 3a. Party RILE Scores Over Time
# Plots the Manifesto Project RILE score for each of the five main Austrian
# parties across all election years in the sample. Zeros are recoded to NA
# to exclude election years in which a party did not contest.

# Austrian party colors and print-safe aesthetic mappings
PARTY_COLORS <- c(
  "SPÖ" = "#E3000F",
  "ÖVP" = "#000000",
  "FPÖ" = "#0056A2",
  "KPÖ" = "#8B0000",
  "VdU" = "#808080"
)

PARTY_SHAPES <- c(
  "SPÖ" = 15, "ÖVP" = 16, "FPÖ" = 17,
  "KPÖ" = 0,  "VdU" = 3
)

PARTY_LINES <- c(
  "SPÖ" = "solid",  "ÖVP" = "solid",  "FPÖ" = "solid",
  "KPÖ" = "dashed", "VdU" = "dotted"
)

rile_ts_data <- df_clean |>
  select(year, ends_with("_rile")) |>
  mutate(across(ends_with("_rile"), ~ na_if(., 0))) |>
  group_by(year) |>
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)), .groups = "drop") |>
  pivot_longer(
    cols      = ends_with("_rile"),
    names_to  = "Party",
    values_to = "RILE_Score",
    values_drop_na = TRUE
  ) |>
  mutate(Party = str_remove(Party, "_rile"))

ggplot(rile_ts_data, aes(x = year, y = RILE_Score,
                         color = Party, shape = Party, linetype = Party)) +
  geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(values = PARTY_COLORS) +
  scale_shape_manual(values = PARTY_SHAPES) +
  scale_linetype_manual(values = PARTY_LINES) +
  labs(
    title    = "Ideological Positions of Austrian Parties, 1950–2008",
    subtitle = "Manifesto Project RILE scores; zero-years (party absent) excluded",
    x        = "Election Year",
    y        = "RILE Score (negative = left, positive = right)",
    color = "Party", shape = "Party", linetype = "Party"
  ) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("output/figures/party_rile_over_time.png", width = 8, height = 6, dpi = 300)


# 3b. Average RILE Score by Party
# Bar chart of each party's mean RILE score across all active election years.
# Parties are ordered left-to-right by their average position.

party_avg_data <- df_clean |>
  select(ends_with("_rile")) |>
  mutate(across(everything(), ~ na_if(., 0))) |>
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) |>
  pivot_longer(cols = everything(), names_to = "Party", values_to = "Avg_RILE") |>
  mutate(
    Party = str_remove(Party, "_rile"),
    Party = factor(Party, levels = Party[order(Avg_RILE)])
  ) |>
  arrange(Avg_RILE)

ggplot(party_avg_data, aes(x = Party, y = Avg_RILE)) +
  geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.8) +
  geom_bar(stat = "identity", fill = "gray70", color = "black", width = 0.7) +
  labs(
    title    = "Average Ideological Position of Austrian Parties, 1950–2008",
    subtitle = "Mean RILE score across all active election years",
    x        = "Party",
    y        = "Average RILE Score (negative = left, positive = right)"
  ) +
  theme_paper() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(angle = 0, hjust = 0.5)
  )

# ggsave("output/figures/average_rile_by_party.png", width = 8, height = 6, dpi = 300)


# 3c. System-Average RILE Score Over Time
# Tracks the unweighted mean RILE score across all active parties per year,
# with a LOESS smoother to illustrate the macro-level ideological drift.

system_avg_data <- df_clean |>
  select(year, ends_with("_rile")) |>
  mutate(across(ends_with("_rile"), ~ na_if(., 0))) |>
  rowwise() |>
  mutate(system_avg = mean(c_across(ends_with("_rile")), na.rm = TRUE)) |>
  ungroup() |>
  group_by(year) |>
  summarise(system_avg = mean(system_avg, na.rm = TRUE), .groups = "drop")

ggplot(system_avg_data, aes(x = year, y = system_avg)) +
  geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.8) +
  geom_smooth(
    method = "loess", span = 0.6, se = FALSE,
    color = "gray60", linetype = "dashed"
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title    = "Ideological Drift of the Austrian Party System, 1950–2008",
    subtitle = "Unweighted mean RILE score across all active parties; dashed line = LOESS trend",
    x        = "Election Year",
    y        = "System-Average RILE Score"
  ) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("output/figures/system_avg_rile.png", width = 8, height = 6, dpi = 300)


# 4. Parallel Trends Diagnostics: Covariate Balance
# Helper function plots group-mean time series for six pre-specified
# covariates, faceted by variable. A dashed vertical line marks the
# relevant treatment year for each sample.

plot_covariate_trends <- function(data, treatment_years, plot_title) {
  data |>
    group_by(year, ever_treated) |>
    summarise(
      across(c(turnout, unemployed_perc, l_pop, sh_winner, invalid, minor),
             mean, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols      = c(turnout, unemployed_perc, l_pop, sh_winner, invalid, minor),
      names_to  = "variable",
      values_to = "value"
    ) |>
    mutate(
      group    = if_else(ever_treated == 1, "Treated", "Control"),
      variable = recode(variable, !!!COVARIATE_LABELS)
    ) |>
    ggplot(aes(x = year, y = value, color = group, linetype = group)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    geom_vline(
      xintercept = treatment_years,
      linetype   = "dashed", color = "black"
    ) +
    scale_color_manual(values = c("Treated" = "black", "Control" = "gray50")) +
    facet_wrap(~ variable, scales = "free_y") +
    labs(
      title    = plot_title,
      subtitle = "Dashed line(s) mark treatment year(s)",
      x        = "Year", y        = "Group Mean",
      color    = "Group", linetype = "Group"
    ) +
    theme_paper() +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1, size = 7),
      panel.spacing = unit(1, "lines")
    )
}

# 4a. Repeal sample (excluding Carinthia): treatment year 1992
plot_covariate_trends(
  data            = df_no_carinthia,
  treatment_years = 1992,
  plot_title      = "Parallel Trends: Covariate Balance — 1992 Repeal Sample"
)

# ggsave("output/figures/parallel_trends_covariates_repeal.png", width = 8, height = 6, dpi = 300)

# 4b. Implementation sample (Carinthia + donor pool): treatment years 1986 and 1992
plot_covariate_trends(
  data            = df_carinthia,
  treatment_years = c(1986, 1992),
  plot_title      = "Parallel Trends: Covariate Balance — 1986 Implementation Sample"
)

# ggsave("output/figures/parallel_trends_covariates_implementation.png", width = 8, height = 6, dpi = 300)


# 5a. Turnout Time Series: Carinthia vs. Donor Pool
# Plots Carinthia's raw turnout alongside the unweighted mean of the five
# donor-pool states. Vertical lines mark CV enactment (1986) and repeal (1992).

ggplot(df_carinthia, aes(x = year, y = turnout)) +
  stat_summary(
    data     = filter(df_carinthia, trimws(state_name) != "Carinthia"),
    aes(color = "Donor Pool (Mean)"),
    fun      = mean, geom = "line", linewidth = 1, linetype = "dotted"
  ) +
  geom_line(
    data = filter(df_carinthia, trimws(state_name) == "Carinthia"),
    aes(color = "Carinthia"), linewidth = 1
  ) +
  geom_point(
    data = filter(df_carinthia, trimws(state_name) == "Carinthia"),
    aes(color = "Carinthia"), size = 2
  ) +
  geom_vline(xintercept = c(1986, 1992), linetype = "dashed", color = "gray40") +
  annotate("text", x = 1986,
           y     = min(df_carinthia$turnout, na.rm = TRUE),
           label = "CV Enacted",  angle = 90, vjust = -0.5, hjust = 0, size = 3.5) +
  annotate("text", x = 1992,
           y     = min(df_carinthia$turnout, na.rm = TRUE),
           label = "CV Repealed", angle = 90, vjust = -0.5, hjust = 0, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(
    name   = "Group",
    values = c("Carinthia" = "black", "Donor Pool (Mean)" = "gray60")
  ) +
  labs(
    title = "Voter Turnout: Carinthia vs. Donor Pool States",
    x     = "Year",
    y     = "Turnout"
  ) +
  theme_paper() +
  theme(
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

# ggsave("output/figures/turnout_carinthia_vs_controls.png", width = 8, height = 6, dpi = 300)


# 5b. Difference in Turnout: Treated vs. Control (1992 Repeal)
# Plots the raw treated-minus-control gap in mean turnout by year, centered
# on the 1992 repeal. Values above zero indicate CV states had higher turnout.

df_no_carinthia |>
  group_by(year, ever_treated) |>
  summarise(turnout = mean(turnout, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = ever_treated, values_from = turnout, names_prefix = "group_") |>
  mutate(
    difference         = group_1 - group_0,
    years_to_treatment = year - 1992
  ) |>
  ggplot(aes(x = years_to_treatment, y = difference)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  annotate("text", x = 0, y = Inf, label = "CV Repealed",
           hjust = 1.1, vjust = 1.5, angle = 90, size = 3.5) +
  labs(
    title    = "Difference in Turnout: Treated minus Control (1992 Repeal)",
    subtitle = "Values above zero indicate CV states had higher turnout than control states",
    x        = "Years Relative to Repeal (0 = 1992)",
    y        = "Difference in Mean Turnout"
  ) +
  theme_paper()

# ggsave("output/figures/turnout_diff_repeal.png", width = 8, height = 6, dpi = 300)


# 5c. Difference in Polarization: Treated vs. Control (1992 Repeal)
# Plots the raw treated-minus-control gap in mean polarization by year with
# 95% confidence intervals. A vertical line marks the 1992 repeal.

df_diff_1992 <- df_no_carinthia |>
  group_by(year, ever_treated) |>
  summarise(
    mean_pol = mean(polarization_score,  na.rm = TRUE),
    var_pol  = var(polarization_score,   na.rm = TRUE),
    n        = sum(!is.na(polarization_score)),
    .groups  = "drop"
  ) |>
  pivot_wider(
    id_cols     = year,
    names_from  = ever_treated,
    values_from = c(mean_pol, var_pol, n)
  ) |>
  mutate(
    difference = mean_pol_1 - mean_pol_0,
    se_diff    = sqrt((var_pol_1 / n_1) + (var_pol_0 / n_0)),
    ci_lower   = difference - 1.96 * se_diff,
    ci_upper   = difference + 1.96 * se_diff
  )

ggplot(df_diff_1992, aes(x = year, y = difference)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "gray70", alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "solid", linewidth = 0.5) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1992, linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = 1992.5,
           y     = max(df_diff_1992$ci_upper, na.rm = TRUE),
           label = "1992 Repeal", hjust = 0, size = 4) +
  labs(
    title    = "Difference in Polarization: Treated vs. Control (1992 Repeal)",
    subtitle = "Treated minus control states; shaded band = 95% CI",
    x        = "Year",
    y        = "Difference in Mean Polarization Score"
  ) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("output/figures/polarization_diff_repeal.png", width = 8, height = 6, dpi = 300)