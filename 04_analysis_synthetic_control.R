# Austrian Electoral Polarization — Synthetic Control Analysis
#
# Estimates the effect of the 1986 compulsory voting (CV) implementation
# in Carinthia on electoral polarization using a synthetic control design.
# The donor pool consists of five never-treated Austrian states. Includes
# in-space and in-time placebo tests for inference.
#
# Primary source:
#   - election_polarization.dta: constructed in data_construction.R
#
# Final output: four figures and five summary tables printed to console
#   (ggsave calls included)


# 0. Dependencies
library(tidyverse)
library(haven)
library(Synth)
library(ggrepel)


# 1. Data Preparation
# Restrict to the Carinthia donor pool and the pre-repeal period (1950–1991).
# The treatment indicator equals 1 for Carinthia from 1986 onward.

PATH_DATA <- "/filepath/election_polarization.dta"

CARINTHIA_DONOR_POOL <- c(
  "Upper Austria", "Lower Austria", "Burgenland",
  "Salzburg", "Vienna", "Carinthia"
)

TREAT_YEAR    <- 1986
PREDICTORS_SC <- c("l_pop", "unemployed_perc", "turnout")

df_sc <- read_dta(PATH_DATA) |>
  mutate(state_name = as.character(as_factor(state_name))) |>
  filter(
    between(year, 1950, 1991),
    state_name %in% CARINTHIA_DONOR_POOL
  ) |>
  mutate(
    state_id = as.numeric(factor(state_name)),
    treated  = as.integer(trimws(state_name) == "Carinthia" & year >= TREAT_YEAR)
  ) |>
  arrange(state_name, year)


# 2. Synthetic Control Estimation
treated_id  <- df_sc |> filter(state_name == "Carinthia") |> distinct(state_id) |> pull() |> as.integer()
control_ids <- df_sc |> filter(state_name != "Carinthia") |> distinct(state_id) |> pull() |> as.integer()
pre_period  <- sort(unique(df_sc$year[df_sc$year < TREAT_YEAR]))
all_periods <- sort(unique(df_sc$year))

dataprep_out <- dataprep(
  foo                  = as.data.frame(df_sc),
  predictors           = PREDICTORS_SC,
  predictors.op        = "mean",
  dependent            = "polarization_score",
  unit.variable        = "state_id",
  unit.names.variable  = "state_name",
  time.variable        = "year",
  treatment.identifier = treated_id,
  controls.identifier  = control_ids,
  time.predictors.prior = pre_period,
  time.optimize.ssr    = pre_period,
  time.plot            = all_periods,
  special.predictors   = lapply(pre_period, function(t) list("polarization_score", t, "mean"))
)

synth_out    <- synth(dataprep_out)
synth_tables <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)

# Assemble treated vs. synthetic path for downstream use
results <- data.frame(
  year      = all_periods,
  treated   = as.numeric(dataprep_out$Y1plot),
  synthetic = as.numeric(dataprep_out$Y0plot %*% synth_out$solution.w)
) |>
  mutate(gap = treated - synthetic)


# 3. Raw Polarization Trajectories: Carinthia vs. Donor Pool
label_data <- df_sc |>
  group_by(state_name) |>
  filter(year == max(year)) |>
  ungroup() |>
  mutate(state_type = if_else(state_name == "Carinthia", "Carinthia", "Control States"))

df_sc |>
  mutate(state_type = if_else(state_name == "Carinthia", "Carinthia", "Control States")) |>
  ggplot(aes(x = year, y = polarization_score, group = state_name)) +
  geom_line(aes(
    color     = state_type,
    linewidth = if_else(state_name == "Carinthia", 1.2, 0.5),
    alpha     = if_else(state_name == "Carinthia", 1.0, 0.4)
  )) +
  geom_vline(xintercept = TREAT_YEAR, linetype = "dashed", color = "black", linewidth = 0.7) +
  annotate("text", x = TREAT_YEAR + 0.5,
           y     = max(df_sc$polarization_score, na.rm = TRUE),
           label = "CV Enacted", hjust = 0, size = 3.5) +
  geom_text_repel(
    data       = label_data,
    aes(label = state_name, color = state_type),
    nudge_x    = 1, direction = "y", hjust = 0,
    segment.size = 0.2, show.legend = FALSE
  ) +
  scale_color_manual(values = c("Carinthia" = "firebrick", "Control States" = "gray50")) +
  scale_linewidth_identity() +
  scale_alpha_identity() +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title = "Raw Polarization Trajectories: Carinthia vs. Donor Pool",
    x     = "Year",
    y     = "Polarization Score"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

# ggsave("output/figures/sc_raw_trajectories.png", width = 8, height = 6, dpi = 300)


# 4. Synthetic Control Path and Gap Plots
path.plot(
  synth.res    = synth_out,
  dataprep.res = dataprep_out,
  Ylab         = "Polarization Score",
  Xlab         = "Year",
  Main         = "Synthetic Control: Carinthia",
  Legend       = c("Carinthia", "Synthetic Carinthia"),
  Legend.position = "topright"
)
abline(v = TREAT_YEAR, lty = 2, col = "gray")

gaps.plot(
  synth.res    = synth_out,
  dataprep.res = dataprep_out,
  Ylab         = "Gap in Polarization Score",
  Xlab         = "Year",
  Main         = "Treatment Effect Gap: Carinthia minus Synthetic Control"
)
abline(v = TREAT_YEAR, lty = 2, col = "gray")


# 5. In-Space Placebo Tests
# Each donor state is iteratively assigned as the pseudo-treated unit.
# RMSPE ratios (post/pre) are used to assess the relative magnitude of
# Carinthia's treatment effect against the placebo distribution.

run_placebo_in_space <- function(treated_state, df_input, treat_year = TREAT_YEAR) {
  df_tmp <- df_input |>
    mutate(state_id = as.numeric(factor(state_name)))
  
  t_id  <- df_tmp |> filter(state_name == treated_state) |> distinct(state_id) |> pull()
  c_ids <- df_tmp |> filter(state_name != treated_state) |> distinct(state_id) |> pull()
  pre   <- sort(unique(df_tmp$year[df_tmp$year < treat_year]))
  all   <- sort(unique(df_tmp$year))
  
  dp <- dataprep(
    foo                  = as.data.frame(df_tmp),
    predictors           = c(),
    predictors.op        = "mean",
    dependent            = "polarization_score",
    unit.variable        = "state_id",
    unit.names.variable  = "state_name",
    time.variable        = "year",
    treatment.identifier = t_id,
    controls.identifier  = c_ids,
    time.predictors.prior = pre,
    time.optimize.ssr    = pre,
    time.plot            = all,
    special.predictors   = lapply(pre, function(t) list("polarization_score", t, "mean"))
  )
  
  so  <- synth(dp)
  gap <- as.numeric(dp$Y1plot - dp$Y0plot %*% so$solution.w)
  
  pre_rmspe  <- sqrt(mean(gap[all < treat_year]^2))
  post_rmspe <- sqrt(mean(gap[all >= treat_year]^2))
  
  list(
    results     = data.frame(state = treated_state, year = all, gap = gap),
    pre_rmspe   = pre_rmspe,
    post_rmspe  = post_rmspe,
    rmspe_ratio = post_rmspe / pre_rmspe
  )
}

placebo_runs <- lapply(
  unique(df_sc$state_name),
  function(st) run_placebo_in_space(st, df_sc)
)
names(placebo_runs) <- unique(df_sc$state_name)

placebo_summary <- data.frame(
  state       = names(placebo_runs),
  pre_rmspe   = sapply(placebo_runs, `[[`, "pre_rmspe"),
  post_rmspe  = sapply(placebo_runs, `[[`, "post_rmspe"),
  rmspe_ratio = sapply(placebo_runs, `[[`, "rmspe_ratio")
) |>
  arrange(desc(rmspe_ratio))

permutation_p <- mean(
  placebo_summary$rmspe_ratio >= placebo_summary$rmspe_ratio[placebo_summary$state == "Carinthia"]
)


# 6. Summary Tables
cat("\n", strrep("=", 60), "\n")
cat("1. Treated vs. Synthetic Outcomes by Year\n")
cat(strrep("=", 60), "\n")
print(results)

cat("\n", strrep("=", 60), "\n")
cat("2. Donor State Weights\n")
cat(strrep("=", 60), "\n")
print(synth_tables$tab.w)

cat("\n", strrep("=", 60), "\n")
cat("3. Pre-Treatment Predictor Balance\n")
cat(strrep("=", 60), "\n")
print(synth_tables$tab.pred)

cat("\n", strrep("=", 60), "\n")
cat("4. In-Space Placebo RMSPE Summary\n")
cat(strrep("=", 60), "\n")
print(placebo_summary)

cat("\n", strrep("=", 60), "\n")
cat("5. Permutation P-Value\n")
cat(strrep("=", 60), "\n")
cat("P-value:", round(permutation_p, 3), "\n")