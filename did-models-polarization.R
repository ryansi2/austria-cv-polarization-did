library(tidyverse)
library(haven)
library(skimr)

df <- read_dta('/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Final Project Data/finaldataset (old).dta')
```

```{r}
df_clean <- df |>
  select(year, state_code, state_code_string, turnout, nparties, oevp_perc, spoe_perc, fpoe_perc, kpoe_perc, wdu_perc, grune_perc, lif_perc, bzoe_perc, right, left, minor, sh_winner, margin, st, parl, pres, invalid, CV, unemployed_perc, l_pop, CV_parl, CV_parl_lag, CV_parl_lead, SPÖ_rile, VdU_rile, ÖVP_rile, FPÖ_rile, GRÜNE_rile, LIF_rile, KPÖ_rile, BZÖ_rile, avg_lr, polarization_score) |>
  filter(year > 1950)
```

```{r}
library(tidyverse)
library(fixest)  # for event study
library(ggplot2)

# print(skim(df_clean))

# Easier: create a 'ever_treated' variable
df_clean_noCarinthia <- df_clean %>%
  group_by(state_code) %>%
  mutate(ever_treated = max(CV)) %>%  # 1 if state ever had CV
  ungroup()|>
  filter(state_code_string != "Carinthia")

# print(skim(df_clean_noCarinthia))

df_clean_onlyCarinthia <- df_clean |>
  group_by(state_code) %>%
  mutate(ever_treated = max(CV)) %>%  # 1 if state ever had CV
  ungroup()|>
  filter(state_code_string == "Upper Austria" |
           state_code_string == "Lower Austria" |
           state_code_string == "Burgenland" |
           state_code_string == "Salzburg" |
           state_code_string == "Vienna"| 
           state_code_string == "Carinthia"
  )
```

```{r}
df_clean_noCarinthia |>
  group_by(year, ever_treated) |>
  summarise(
    across(c(turnout, unemployed_perc, l_pop, sh_winner, invalid, minor), 
           mean, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(cols = c(turnout, unemployed_perc, l_pop, sh_winner, invalid, minor),
               names_to = "variable", values_to = "value") %>%
  mutate(group = ifelse(ever_treated == 1, "Had CV", "Never had CV")) %>%
  mutate(variable = recode(variable,
                           "unemployed_perc" = "Unemployment (%)",
                           "turnout"         = "Turnout",
                           "l_pop"           = "Log Population",
                           "sh_winner"       = "Winner's Vote Share",
                           "invalid"         = "Invalid Votes",
                           "minor"           = "Minor Party Vote Share"
  )) |>
  ggplot(aes(x = year, y = value, color = group, linetype = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1992, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Had CV" = "black", "Never had CV" = "gray50")) +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Parallel Trends: Covariates (Excluding Carinthia)",
    x = "Year", y = "Mean Value", color = "Group", linetype = "Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    panel.spacing = unit(1, "lines")
  )

ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Final Plots/covariates_parallel_trends_NC.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

```

```{r}
df_clean_onlyCarinthia %>%
  group_by(year, ever_treated) %>%
  summarise(
    across(c(turnout, unemployed_perc, l_pop, sh_winner, invalid, minor), 
           mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(turnout, unemployed_perc, l_pop, sh_winner, invalid, minor),
               names_to = "variable", values_to = "value") %>%
  mutate(group = ifelse(ever_treated == 1, "Had CV", "Never had CV")) %>%
  mutate(variable = recode(variable,
                           "unemployed_perc" = "Unemployment (%)",
                           "turnout"         = "Turnout",
                           "l_pop"           = "Log Population",
                           "sh_winner"       = "Winner's Vote Share",
                           "invalid"         = "Invalid Votes",
                           "minor"           = "Minor Party Vote Share"
  )) |>
  ggplot(aes(x = year, y = value, color = group, linetype = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1986, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 1992, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Had CV" = "black", "Never had CV" = "gray50")) +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Parallel Trends: Covariates (Carinthia and Controls)",
    x = "Year", y = "Mean Value", color = "Group", linetype = "Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    panel.spacing = unit(1, "lines")
  )

ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Final Plots/covariates_parallel_trends_OC.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
```

```{r}
df_clean_onlyCarinthia %>%
  group_by(year, ever_treated) %>%
  summarise(
    polarization_score = mean(polarization_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = ever_treated, values_from = polarization_score,
              names_prefix = "group_") %>%
  mutate(
    difference  = group_1 - group_0,
    years_to_treatment = year - 1986        # 1986 becomes 0
  ) %>%
  ggplot(aes(x = years_to_treatment, y = difference)) +
  geom_line(size = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 0,  linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 6,  linetype = "dashed", color = "gray40") +  # 1992 - 1986 = 6
  annotate("text", x = 0, y = Inf, label = "CV Enacted",
           hjust = 1.1, vjust = 1.5, angle = 90, size = 3.5, color = "black") +
  annotate("text", x = 6, y = Inf, label = "CV Repealed",
           hjust = 1.1, vjust = 1.5, angle = 90, size = 3.5, color = "black") +
  labs(
    title = "Difference in Polarization: Carinthia minus Controls",
    subtitle = "Values above zero indicate Carinthia is more polarized than controls",
    x = "Years Relative to CV Enactment (1986 = 0)",
    y = "Difference in Mean Polarization Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Final Plots/outcome_parallel_trends_OC.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
```

```{r}
df_clean_onlyCarinthia %>%
  group_by(year, ever_treated) %>%
  summarise(
    turnout = mean(turnout, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = ever_treated, values_from = turnout,
              names_prefix = "group_") %>%
  mutate(
    difference         = group_1 - group_0,
    years_to_treatment = year - 1986
  ) %>%
  ggplot(aes(x = years_to_treatment, y = difference)) +
  geom_line(size = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "gray40") +
  annotate("text", x = 0, y = Inf, label = "CV Enacted",
           hjust = 1.1, vjust = 1.5, angle = 90, size = 3.5, color = "black") +
  annotate("text", x = 6, y = Inf, label = "CV Repealed",
           hjust = 1.1, vjust = 1.5, angle = 90, size = 3.5, color = "black") +
  labs(
    title = "Difference in Turnout: Carinthia minus Controls",
    subtitle = "Values above zero indicate Carinthia has higher turnout than controls",
    x = "Years Relative to CV Enactment (1986 = 0)",
    y = "Difference in Mean Turnout"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Final Plots/turnout_timeseries_comparison_OC.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
```

```{r}
df_clean_onlyCarinthia |>
  ggplot(aes(x = year, y = turnout)) +
  # Line for the Control Group Average
  stat_summary(data = filter(df_clean_onlyCarinthia, trimws(state_code_string) != "Carinthia"),
               aes(color = "Other States (Mean)"), 
               fun = mean, geom = "line", size = 1, linetype = "dotted") +
  # Line and points for Carinthia
  geom_line(data = filter(df_clean_onlyCarinthia, trimws(state_code_string) == "Carinthia"),
            aes(color = "Carinthia"), size = 1) +
  geom_point(data = filter(df_clean_onlyCarinthia, trimws(state_code_string) == "Carinthia"),
             aes(color = "Carinthia"), size = 2) +
  geom_vline(xintercept = 1986, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 1992, linetype = "dashed", color = "gray40") +
  annotate("text", x = 1986, 
           y = min(df_clean_onlyCarinthia$turnout, na.rm = TRUE),
           label = "CV Enacted", angle = 90, vjust = -0.5, hjust = 0, size = 3.5) +
  annotate("text", x = 1992, 
           y = min(df_clean_onlyCarinthia$turnout, na.rm = TRUE),
           label = "CV Repealed", angle = 90, vjust = -0.5, hjust = 0, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(name = "Group", 
                     values = c("Carinthia" = "black", "Other States (Mean)" = "gray60")) +
  labs(
    title = "Voter Turnout: Carinthia vs. Control States",
    x = "Year",
    y = "Turnout"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Final Plots/turnout_timeseries_raw_OC.png", width = 8, height = 6, dpi = 300)
```

```{r}
df_clean_noCarinthia %>%
  group_by(year, ever_treated) %>%
  summarise(
    turnout = mean(turnout, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = ever_treated, values_from = turnout,
              names_prefix = "group_") %>%
  mutate(
    difference         = group_1 - group_0,
    years_to_treatment = year - 1992
  ) %>%
  ggplot(aes(x = years_to_treatment, y = difference)) +
  geom_line(size = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  annotate("text", x = 0, y = Inf, label = "CV Repealed",
           hjust = 1.1, vjust = 1.5, angle = 90, size = 3.5, color = "black") +
  labs(
    title = "Difference in Turnout: Treated minus Controls",
    subtitle = "Values above zero indicate CV states had higher turnout than control states",
    x = "Years Relative to CV Repeal (1992 = 0)",
    y = "Difference in Mean Turnout"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Final Plots/turnout_timeseries_comparison_NC.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
```

```{r}
# Define groups for clarity
repeal_states <- c("Styria", "Vorarlberg", "Tyrol")

# Create a temporary dataframe with the relative time variable
df_plot <- df_clean_noCarinthia |>
  mutate(time_to_repeal = year - 1992)

df_plot |>
  ggplot(aes(x = time_to_repeal, y = turnout)) +
  # 1. Line for the Control Group Average (the 5 states that never had CV)
  stat_summary(data = filter(df_plot, !(state_code_string %in% repeal_states)),
               aes(color = "Control States (Mean)"), 
               fun = mean, geom = "line", size = 1, linetype = "dotted") +
  # 2. Line for the Repeal Group Average (Styria, Vorarlberg, Tyrol)
  stat_summary(data = filter(df_plot, state_code_string %in% repeal_states),
               aes(color = "1992 Repeal States (Mean)"), 
               fun = mean, geom = "line", size = 1) +
  # 3. Reference line for the Repeal Year (Now at 0 instead of 1992)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  annotate("text", x = 0, 
           y = min(df_plot$turnout, na.rm = TRUE),
           label = "CV Repealed", angle = 90, vjust = -0.5, hjust = 0, size = 3.5) +
  # Formatting
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(name = "Group", 
                     values = c("1992 Repeal States (Mean)" = "black", 
                                "Control States (Mean)" = "gray60")) +
  labs(
    title = "Impact of 1992 CV Repeal on Voter Turnout",
    subtitle = "Excludes Carinthia; Treatment group includes Styria, Vorarlberg, and Tyrol",
    x = "Years Since CV Repeal (0 = 1992)",
    y = "Turnout"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# Updated ggsave path for your new plot
# ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Final Plots/turnout_timeseries_event_study_NC.png", width = 8, height = 6, dpi = 300)
```

```{r}
library(fixest)

# --- Data Prep ---

df_reg <- df_clean_onlyCarinthia %>%
  mutate(
    # CV indicator for implementation (1986 onward for Carinthia)
    CV = as.integer(trimws(state_code_string) == "Carinthia" & year >= 1986),
    
    # Repeal shock: Carinthia after 1992 (when CV was removed)
    repeal_shock = as.integer(trimws(state_code_string) == "Carinthia" & year >= 1992)
  )

# --- Model 1: 1986 Implementation ---
# Narrow window around 1986 (24 obs suggests ~4 elections x 6 states)
model_implementation_sr <- feols(
  polarization_score ~ CV + unemployed_perc + l_pop | state_code_string + year,
  data    = df_reg %>% filter(year >= 1976 & year <= 1991),
  cluster = ~state_code_string
)

model_implementation_sr_no_tfe <- feols(
  polarization_score ~ CV + unemployed_perc + l_pop | state_code_string,
  data    = df_reg %>% filter(year >= 1976 & year <= 1991),
  cluster = ~state_code_string
)

model_implementation_sr_no_sfe <- feols(
  polarization_score ~ CV + unemployed_perc + l_pop | year,
  data    = df_reg %>% filter(year >= 1976 & year <= 1991),
  cluster = ~state_code_string
)

model_implementation_sr_no_fe <- feols(
  polarization_score ~ CV + unemployed_perc + l_pop,
  data    = df_reg %>% filter(year >= 1976 & year <= 1991),
  cluster = ~state_code_string
)

# --- Model 2: 1986 Implementation ---
# Long window around 1986
model_implementation_lr <- feols(
  polarization_score ~ CV + unemployed_perc + l_pop | state_code_string + year,
  data    = df_reg %>% filter(year >= 1956 & year <= 1991),
  cluster = ~state_code_string
)

model_implementation_lr_no_tfe <- feols(
  polarization_score ~ CV + unemployed_perc + l_pop | state_code_string,
  data    = df_reg %>% filter(year >= 1956 & year <= 1991),
  cluster = ~state_code_string
)

model_implementation_lr_no_sfe <- feols(
  polarization_score ~ CV + unemployed_perc + l_pop | year,
  data    = df_reg %>% filter(year >= 1956 & year <= 1991),
  cluster = ~state_code_string
)

model_implementation_lr_no_fe <- feols(
  polarization_score ~ CV + unemployed_perc + l_pop,
  data    = df_reg %>% filter(year >= 1956 & year <= 1991),
  cluster = ~state_code_string
)


# --- Display Table ---
etable(
  model_implementation_sr,
  model_implementation_sr_no_tfe, 
  model_implementation_sr_no_sfe, 
  model_implementation_sr_no_fe, 
  model_implementation_lr,
  model_implementation_lr_no_tfe, 
  model_implementation_lr_no_sfe, 
  model_implementation_lr_no_fe, 
  headers = c("1986 Implementation SR", "1986 Implementation SR no TFE", "1986 Implementation SR no SFE", "1986 Implementation SR no FE", "1986 Implementation LR", "1986 Implementation LR no TFE", "1986 Implementation LR no SFE", "1986 Implementation LR no FE"),
  depvar  = TRUE,
  digits  = 4
)
```

```{r}
library(fixest)

# --- Data Prep ---

df_reg <- df_clean_onlyCarinthia %>%
  mutate(
    # CV indicator for implementation (1986 onward for Carinthia)
    CV = as.integer(trimws(state_code_string) == "Carinthia" & year >= 1986),
    
    # Repeal shock: Carinthia after 1992 (when CV was removed)
    repeal_shock = as.integer(trimws(state_code_string) == "Carinthia" & year >= 1992), 
    
    polarization_score_std = as.numeric(scale(polarization_score))
    
  )

# --- Model 1: 1986 Implementation ---
# Narrow window around 1986 (24 obs suggests ~4 elections x 6 states)
model_implementation_sr <- feols(
  polarization_score_std ~ CV + unemployed_perc + l_pop | state_code_string + year,
  data    = df_reg %>% filter(year >= 1976 & year <= 1991),
  cluster = ~state_code_string
)

model_implementation_sr_no_tfe <- feols(
  polarization_score_std ~ CV + unemployed_perc + l_pop | state_code_string,
  data    = df_reg %>% filter(year >= 1976 & year <= 1991),
  cluster = ~state_code_string
)

model_implementation_sr_no_sfe <- feols(
  polarization_score_std ~ CV + unemployed_perc + l_pop | year,
  data    = df_reg %>% filter(year >= 1976 & year <= 1991),
  cluster = ~state_code_string
)

model_implementation_sr_no_fe <- feols(
  polarization_score_std ~ CV + unemployed_perc + l_pop,
  data    = df_reg %>% filter(year >= 1976 & year <= 1991),
  cluster = ~state_code_string
)

# --- Model 2: 1986 Implementation ---
# Long window around 1986
model_implementation_lr <- feols(
  polarization_score_std ~ CV + unemployed_perc + l_pop | state_code_string + year,
  data    = df_reg %>% filter(year >= 1956 & year <= 1991),
  cluster = ~state_code_string
)

model_implementation_lr_no_tfe <- feols(
  polarization_score_std ~ CV + unemployed_perc + l_pop | state_code_string,
  data    = df_reg %>% filter(year >= 1956 & year <= 1991),
  cluster = ~state_code_string
)

model_implementation_lr_no_sfe <- feols(
  polarization_score_std ~ CV + unemployed_perc + l_pop | year,
  data    = df_reg %>% filter(year >= 1956 & year <= 1991),
  cluster = ~state_code_string
)

model_implementation_lr_no_fe <- feols(
  polarization_score_std ~ CV + unemployed_perc + l_pop,
  data    = df_reg %>% filter(year >= 1956 & year <= 1991),
  cluster = ~state_code_string
)


# --- Display Table ---
etable(
  model_implementation_sr,
  model_implementation_sr_no_tfe, 
  model_implementation_sr_no_sfe, 
  model_implementation_sr_no_fe, 
  model_implementation_lr,
  model_implementation_lr_no_tfe, 
  model_implementation_lr_no_sfe, 
  model_implementation_lr_no_fe, 
  headers = c("1986 Implementation SR", "1986 Implementation SR no TFE", "1986 Implementation SR no SFE", "1986 Implementation SR no FE", "1986 Implementation LR", "1986 Implementation LR no TFE", "1986 Implementation LR no SFE", "1986 Implementation LR no FE"),
  depvar  = TRUE,
  digits  = 4
)
```

```{r}
library(fixest)

# --- Data Prep ---

df_reg <- df_clean_noCarinthia %>%
  mutate(
    CV_repealed = if_else(
      state_code_string %in% c("Styria", "Vorarlberg", "Tyrol") & year >= 1992, 1, 0), 
  )

# --- Model 1: 1992 Repeal SR ---
model_implementation_sr <- feols(
  polarization_score ~ CV_repealed + unemployed_perc + l_pop | state_code_string + year,
  data    = df_reg %>% filter(year >= 1976),
  cluster = ~state_code_string
)

model_implementation_sr_no_tfe <- feols(
  polarization_score ~ CV_repealed + unemployed_perc + l_pop | state_code_string,
  data    = df_reg %>% filter(year >= 1976),
  cluster = ~state_code_string
)

model_implementation_sr_no_sfe <- feols(
  polarization_score ~ CV_repealed + unemployed_perc + l_pop | year,
  data    = df_reg %>% filter(year >= 1976),
  cluster = ~state_code_string
)

model_implementation_sr_no_fe <- feols(
  polarization_score ~ CV_repealed + unemployed_perc + l_pop,
  data    = df_reg %>% filter(year >= 1976),
  cluster = ~state_code_string
)

# --- Model 2: 1992 Repeal LR ---
model_implementation_lr <- feols(
  polarization_score ~ CV_repealed + unemployed_perc + l_pop | state_code_string + year,
  data    = df_reg %>% filter(year >= 1956),
  cluster = ~state_code_string
)

model_implementation_lr_no_tfe <- feols(
  polarization_score ~ CV_repealed + unemployed_perc + l_pop | state_code_string,
  data    = df_reg %>% filter(year >= 1956),
  cluster = ~state_code_string
)

model_implementation_lr_no_sfe <- feols(
  polarization_score ~ CV_repealed + unemployed_perc + l_pop | year,
  data    = df_reg %>% filter(year >= 1956),
  cluster = ~state_code_string
)

model_implementation_lr_no_fe <- feols(
  polarization_score ~ CV_repealed + unemployed_perc + l_pop,
  data    = df_reg %>% filter(year >= 1956),
  cluster = ~state_code_string
)


# --- Display Table ---
etable(
  model_implementation_sr,
  model_implementation_sr_no_tfe, 
  model_implementation_sr_no_sfe, 
  model_implementation_sr_no_fe, 
  model_implementation_lr,
  model_implementation_lr_no_tfe, 
  model_implementation_lr_no_sfe, 
  model_implementation_lr_no_fe, 
  headers = c("1992 Repeal SR", "1992 Repeal SR no TFE", "1992 Repeal SR no SFE", "1992 Repeal SR no FE", "1992 Repeal LR", "1992 Repeal LR no TFE", "1992 Repeal LR no SFE", "1992 Repeal LR no FE"),
  depvar  = TRUE,
  digits  = 4
)
```

```{r}
library(fixest)

# --- Data Prep ---

df_reg <- df_clean_noCarinthia %>%
  mutate(
    CV_repealed = if_else(
      state_code_string %in% c("Styria", "Vorarlberg", "Tyrol") & year >= 1992, 1, 0), 
    polarization_score_std = as.numeric(scale(polarization_score))
    
  )

# --- Model 1: 1992 Repeal SR ---
model_implementation_sr <- feols(
  polarization_score_std ~ CV_repealed + unemployed_perc + l_pop | state_code_string + year,
  data    = df_reg %>% filter(year >= 1976),
  cluster = ~state_code_string
)

model_implementation_sr_no_tfe <- feols(
  polarization_score_std ~ CV_repealed + unemployed_perc + l_pop | state_code_string,
  data    = df_reg %>% filter(year >= 1976),
  cluster = ~state_code_string
)

model_implementation_sr_no_sfe <- feols(
  polarization_score_std ~ CV_repealed + unemployed_perc + l_pop | year,
  data    = df_reg %>% filter(year >= 1976),
  cluster = ~state_code_string
)

model_implementation_sr_no_fe <- feols(
  polarization_score_std ~ CV_repealed + unemployed_perc + l_pop,
  data    = df_reg %>% filter(year >= 1976),
  cluster = ~state_code_string
)

# --- Model 2: 1992 Repeal LR ---
model_implementation_lr <- feols(
  polarization_score_std ~ CV_repealed + unemployed_perc + l_pop | state_code_string + year,
  data    = df_reg %>% filter(year >= 1956),
  cluster = ~state_code_string
)

model_implementation_lr_no_tfe <- feols(
  polarization_score_std ~ CV_repealed + unemployed_perc + l_pop | state_code_string,
  data    = df_reg %>% filter(year >= 1956),
  cluster = ~state_code_string
)

model_implementation_lr_no_sfe <- feols(
  polarization_score_std ~ CV_repealed + unemployed_perc + l_pop | year,
  data    = df_reg %>% filter(year >= 1956),
  cluster = ~state_code_string
)

model_implementation_lr_no_fe <- feols(
  polarization_score_std ~ CV_repealed + unemployed_perc + l_pop,
  data    = df_reg %>% filter(year >= 1956),
  cluster = ~state_code_string
)


# --- Display Table ---
etable(
  model_implementation_sr,
  model_implementation_sr_no_tfe, 
  model_implementation_sr_no_sfe, 
  model_implementation_sr_no_fe, 
  model_implementation_lr,
  model_implementation_lr_no_tfe, 
  model_implementation_lr_no_sfe, 
  model_implementation_lr_no_fe, 
  headers = c("1992 Repeal SR", "1992 Repeal SR no TFE", "1992 Repeal SR no SFE", "1992 Repeal SR no FE", "1992 Repeal LR", "1992 Repeal LR no TFE", "1992 Repeal LR no SFE", "1992 Repeal LR no FE"),
  depvar  = TRUE,
  digits  = 4
)
```

```{r}
# Load required libraries
library(tidyverse)
library(fixest)

# ==============================================================================
# PREPARATION
# ==============================================================================
# Define the steady control group (States that NEVER had Compulsory Voting)
never_cv <- c("Burgenland", "Lower Austria", "Upper Austria", "Salzburg", "Vienna")

# ==============================================================================
# ISOLATE THE 1986 IMPLEMENTATION (Carinthia vs. Never CV)
# ==============================================================================

did_1986_data <- df_clean %>%
  filter(
    # Keep only Carinthia and the pure control group
    state_code_string %in% c("Carinthia", never_cv),
    # CRITICAL: Stop the data before 1992. 
    # If we include 1992+, the repeals in other states will mess up the national TWFE baseline.
    year < 1992 
  ) %>%
  mutate(
    # Create an explicit dummy for the act of implementing the law in 1986
    implementation_shock = ifelse(state_code_string == "Carinthia" & year >= 1986, 1, 0),
    
    # Event study helper: Define the treated group for interactions
    treated_group = ifelse(state_code_string == "Carinthia", 1, 0),
    
    # Ensure log polarization is calculated (if not already done globally)
    log_polarization = log(polarization_score)
  )

# ==============================================================================
# RUN THE TWFE MODELS
# ==============================================================================

# Model 1: Using your existing binary CV variable 
model_implement_standard <- feols(log_polarization ~ CV + unemployed_perc + l_pop | state_code_string + year, 
                                  data = did_1986_data, 
                                  cluster = ~state_code_string)

# Model 2: Using the explicit implementation shock dummy 
model_implement_shock <- feols(log_polarization ~ implementation_shock + unemployed_perc + l_pop | state_code_string + year, 
                               data = did_1986_data, 
                               cluster = ~state_code_string)

# ==============================================================================
# RESULTS & DIAGNOSTICS
# ==============================================================================

# Compare the final models side-by-side
etable(model_implement_standard, model_implement_shock,
       headers = c("1986 Implementation (Standard)", "1986 Implementation (Shock)"))

# Run an Event Study with safe controls to verify parallel pre-trends
# NOTE: Replace '1984' with the exact year of the LAST election in Carinthia before 1986
event_study_1986 <- feols(log_polarization ~ i(year, treated_group, ref = 1984) + unemployed_perc + l_pop | state_code_string + year, 
                          data = did_1986_data, 
                          cluster = ~state_code_string)

# Plot the event study coefficients
iplot(event_study_1986, main = "Event Study: 1986 Implementation of Compulsory Voting (Carinthia)")

# Save the plot for your paper
ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Plots/implement_cv.png", 
       plot = last_plot(), width = 8, height = 6, dpi = 300)
```

```{r}
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# 1. Prepare and reshape the data 
rile_data <- df_clean %>%
  select(year, ends_with("_rile")) %>%
  mutate(across(ends_with("_rile"), ~na_if(., 0))) %>%
  group_by(year) %>%
  summarize(across(everything(), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(
    cols = ends_with("_rile"),
    names_to = "Party",
    values_to = "RILE_Score",
    values_drop_na = TRUE 
  ) %>%
  mutate(Party = str_remove(Party, "_rile"))

# ==============================================================================
# 🎨 ACADEMIC STYLING CONFIGURATION
# ==============================================================================

# Define traditional Austrian party colors (Hex codes)
party_colors <- c(
  "SPÖ"   = "#E3000F",  # Social Democrats: Red
  "ÖVP"   = "#000000",  # People's Party: Black (Historical standard)
  "FPÖ"   = "#0056A2",  # Freedom Party: Blue
  "GRÜNE" = "#60A500",  # Greens: Green
  "KPÖ"   = "#8B0000",  # Communists: Dark Red
  "BZÖ"   = "#F68A00",  # BZÖ: Orange
  "LIF"   = "#E3C200",  # Liberal Forum: Gold/Yellow
  "VdU"   = "#808080"   # VdU: Grey
)

# Define distinct shapes for B&W printing compatibility
party_shapes <- c(
  "SPÖ" = 15, "ÖVP" = 16, "FPÖ" = 17, "GRÜNE" = 18, # Solid shapes for major parties
  "KPÖ" = 0,  "BZÖ" = 1,  "LIF" = 2,  "VdU" = 3     # Hollow/cross shapes for minor parties
)

# Define distinct line types
party_lines <- c(
  "SPÖ" = "solid", "ÖVP" = "solid", "FPÖ" = "solid", "GRÜNE" = "solid",
  "KPÖ" = "dashed", "BZÖ" = "dashed", "LIF" = "dashed", "VdU" = "dotted"
)

# ==============================================================================
# 📊 GENERATE PUBLICATION PLOT
# ==============================================================================

rile_plot <- ggplot(rile_data, aes(x = year, y = RILE_Score, 
                                   color = Party, shape = Party, linetype = Party)) +
  # Use a solid thin line for the center axis instead of a thick dashed one
  geom_hline(yintercept = 0, linetype = "solid", color = "darkgray", linewidth = 0.5) +
  
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  
  # Apply the custom scales
  scale_color_manual(values = party_colors) +
  scale_shape_manual(values = party_shapes) +
  scale_linetype_manual(values = party_lines) +
  
  # Formal academic labels
  labs(
    title = "Ideological Positions of Austrian Parties (1986–2008)",
    subtitle = "Manifesto Project RILE Scores",
    x = "Election Year",
    y = "RILE Score (Negative = Left, Positive = Right)",
    # Keeping all legend titles identical forces ggplot to merge them into one legend box
    color = "Party", shape = "Party", linetype = "Party" 
  ) +
  
  # theme_bw() is the gold standard for journal submissions
  theme_bw() + 
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank(),
    legend.key = element_blank() # Removes the grey boxes behind the legend icons
  )

print(rile_plot)

ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Plots/party_rile_over_time.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
```

```{r}
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# 1. Prepare data: Average each party's score across the entire time period
party_avg_data <- df_clean %>%
  select(ends_with("_rile")) %>%
  # Convert 0s to NA so missing years don't drag the average toward the center
  mutate(across(everything(), ~na_if(., 0))) %>%
  # Calculate the mean for each column
  summarize(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Party",
    values_to = "Avg_RILE"
  ) %>%
  mutate(Party = str_remove(Party, "_rile")) %>%
  # Exclude the VdU party
  filter(Party != "VdU") %>%
  # Sort from most negative (Left) to most positive (Right)
  arrange(Avg_RILE) %>% 
  # Lock in the factor levels so ggplot respects our sorting
  mutate(Party = factor(Party, levels = Party))

# 2. Generate Plot
party_avg_plot <- ggplot(party_avg_data, aes(x = Party, y = Avg_RILE)) +
  # Draw a solid center line for the ideological zero-point
  geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.8) +
  # Use a uniform gray fill for all bars
  geom_bar(stat = "identity", fill = "gray70", color = "black", width = 0.7) +
  
  labs(
    title = "Average Ideological Position of Austrian Parties (1949–2008)",
    subtitle = "Mean RILE Score across all active election years",
    x = "Political Party",
    y = "Average RILE Score (Negative = Left, Positive = Right)"
  ) +
  
  theme_bw() +
  theme(
    legend.position = "none", # Hide legend since X-axis is already labeled
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    panel.grid.major.x = element_blank() # Removes vertical grid lines for cleaner bars
  )

print(party_avg_plot)

ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Plots/average_rile_scores_per_party.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
```

```{r}
# 1. Prepare data: Calculate the average RILE score of ALL parties per year
system_avg_data <- df %>%
  select(year, ends_with("_rile")) %>%
  mutate(across(ends_with("_rile"), ~na_if(., 0))) %>%
  # Rowwise allows us to take the mean across the columns for each specific year
  rowwise() %>%
  mutate(System_Avg = mean(c_across(ends_with("_rile")), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(System_Avg = mean(System_Avg, na.rm = TRUE), .groups = "drop")

# 2. Generate Plot
system_avg_plot <- ggplot(system_avg_data, aes(x = year, y = System_Avg)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "darkgray", linewidth = 0.8) +
  
  # A single, thick, neutral-colored line (e.g., dark grey/black) for the system average
  geom_line(color = "black", linewidth = 1.2) +
  geom_point(color = "black", size = 3) +
  
  # Optional: Add a smooth trendline behind it to show the macro-drift
  geom_smooth(method = "loess", span = 0.6, se = FALSE, color = "gray", linetype = "dashed", alpha = 0.5) +
  
  labs(
    title = "Ideological Drift of the Austrian Party System Over Time",
    subtitle = "Yearly Average of All Active Parties' RILE Scores",
    x = "Election Year",
    y = "System Average RILE Score"
  ) +
  
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank()
  )

print(system_avg_plot)

ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Plots/rile_score_all_states.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
```

```{r}
library(dplyr)
library(tidyr)
library(knitr)

# ==============================================================================
# GENERATE SUMMARY STATISTICS TABLE
# ==============================================================================

summary_table <- df_clean %>%
  # 1. Select only the variables where mean/sd/min/max make mathematical sense
  select(
    turnout, invalid, nparties,
    spoe_perc, oevp_perc, fpoe_perc, grune_perc, 
    kpoe_perc, lif_perc, bzoe_perc,
    left, right,
    sh_winner, margin,
    unemployed_perc, l_pop, polarization_score
  ) %>%
  mutate(
    spoe_perc = spoe_perc/100, 
    oevp_perc = oevp_perc/100, 
    fpoe_perc = fpoe_perc/100, 
    grune_perc = grune_perc/100, 
    kpoe_perc = kpoe_perc/100, 
    lif_perc = lif_perc/100, 
    bzoe_perc = bzoe_perc/100, 
    unemployed_perc = unemployed_perc/100
  ) |>
  
  # 2. Pivot the data to calculate stats for all variables at once
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  
  # 3. Calculate Mean, SD, Min, Max
  group_by(Variable) %>%
  summarize(
    Mean = mean(Value, na.rm = TRUE),
    Std_Dev = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # 4. Rename variables into professional labels for the paper
  mutate(Variable = case_match(
    Variable,
    "turnout" ~ "Voter Turnout",
    "invalid" ~ "Invalid Votes",
    "nparties" ~ "Effective Number of Parties",
    "spoe_perc" ~ "SPÖ Vote Share",
    "oevp_perc" ~ "ÖVP Vote Share",
    "fpoe_perc" ~ "FPÖ Vote Share",
    "grune_perc" ~ "Greens Vote Share",
    "kpoe_perc" ~ "KPÖ Vote Share",
    "lif_perc" ~ "LIF Vote Share",
    "bzoe_perc" ~ "BZÖ Vote Share",
    "left" ~ "Total Left Bloc Share",
    "right" ~ "Total Right Bloc Share",
    "sh_winner" ~ "Winning Party's Vote Share",
    "margin" ~ "Margin of Victory",
    "unemployed_perc" ~ "Unemployment Rate",
    "l_pop" ~ "Log Population",
    .default = Variable
  )) %>%
  
  # 5. Order the table logically (Electoral -> Covariates -> Blocs -> Parties at the bottom)
  arrange(factor(Variable, levels = c(
    "Voter Turnout", 
    "Invalid Votes", 
    "Effective Number of Parties",
    "Margin of Victory", 
    "Winning Party's Vote Share", 
    "Unemployment Rate", 
    "Log Population",
    "Total Left Bloc Share", 
    "Total Right Bloc Share", 
    # Individual Party Percentages at the bottom:
    "SPÖ Vote Share", 
    "ÖVP Vote Share", 
    "FPÖ Vote Share", 
    "Greens Vote Share",
    "KPÖ Vote Share", 
    "LIF Vote Share", 
    "BZÖ Vote Share"
  )))

# Print the table formatted to 2 decimal places
kable(summary_table, digits = 2, format = "markdown", 
      caption = "Table 1: Summary Statistics of State Elections")
```

```{r}
# Load required libraries
library(tidyverse)
library(fixest)

# Define the steady control group (Never had CV)
never_cv <- c("Burgenland", "Lower Austria", "Upper Austria", "Salzburg", "Vienna")

# ==============================================================================
# PREPARATION: Generate Log Polarization
# ==============================================================================
# We do this on the main dataset so it flows down into all subsets
df_clean <- df_clean %>%
  mutate(log_polarization = log(polarization_score))

# ==============================================================================
# PART 1: 1986 Implementation (Carinthia vs. Never CV)
# ==============================================================================

# Subset the data to isolate the 1986 shock
did_1986_data <- df_clean %>%
  filter(
    state_code_string %in% c("Carinthia", never_cv),
    year < 1992 # Stop before 1992 to prevent the repeal from biasing the estimate
  )

# Run the TWFE Model with SAFE controls (Demographics only)
model_implementation <- feols(log_polarization ~ CV + unemployed_perc + l_pop | state_code_string + year, 
                              data = did_1986_data, 
                              cluster = ~state_code_string)


# ==============================================================================
# PART 2: 1992 Repeal (Styria, Tyrol, Vorarlberg vs. Never CV)
# ==============================================================================

# Subset the data to isolate the 1992 repeal shock (excluding Carinthia entirely)
did_1992_data <- df_clean %>%
  filter(
    state_code_string %in% c("Styria", "Tyrol", "Vorarlberg", never_cv)
  ) %>%
  mutate(
    # Create an explicit dummy for the act of repealing the law in 1992
    repeal_shock = ifelse(state_code_string %in% c("Styria", "Tyrol", "Vorarlberg") & year >= 1992, 1, 0),
    
    # Event study helper: Define the treated group for interactions
    treated_group = ifelse(state_code_string %in% c("Styria", "Tyrol", "Vorarlberg"), 1, 0)
  )

# Run the TWFE Models with SAFE controls
# 1. Using your existing CV variable 
model_repeal_standard <- feols(log_polarization ~ CV + unemployed_perc + l_pop | state_code_string + year, 
                               data = did_1992_data, 
                               cluster = ~state_code_string)

# 2. Using the repeal_shock 
model_repeal_shock <- feols(log_polarization ~ repeal_shock + unemployed_perc + l_pop | state_code_string + year, 
                            data = did_1992_data, 
                            cluster = ~state_code_string)

# ==============================================================================
# RESULTS & DIAGNOSTICS
# ==============================================================================

# Compare the final models side-by-side
etable(model_implementation, model_repeal_standard, model_repeal_shock,
       headers = c("1986 Implementation", "1992 Repeal (Standard)", "1992 Repeal (Shock)"))

# Run an Event Study with safe controls to verify parallel pre-trends
# Adjust 'ref = 1990' to whatever your exact pre-treatment election year is
event_study_1992 <- feols(log_polarization ~ i(year, treated_group, ref = 1990) + unemployed_perc + l_pop | state_code_string + year, 
                          data = did_1992_data, 
                          cluster = ~state_code_string)

# Plot the event study coefficients
iplot(event_study_1992, main = "Event Study: 1992 Repeal of Compulsory Voting")

# ==============================================================================
# EVENT STUDY GRAPH: 1992 REPEAL
# ==============================================================================

# 1. Run the Event Study (ref = 1990 sets the baseline to the election before repeal)
event_study_1992 <- feols(polarization_score ~ i(year, treated_group, ref = 1990) + 
                            unemployed_perc + l_pop | state_code_string + year, 
                          data = did_1992_data, 
                          cluster = ~state_code_string)

# 2. Plot the base event study WITHOUT the default 1990 reference line
iplot(event_study_1992, 
      main = "Event Study: 1992 Repeal of Compulsory Voting",
      xlab = "Year",
      ylab = "Polarization Score",
      ref.line = FALSE) # <--- This removes the 1990 line

# 3. Add the custom vertical dotted line at 1992
abline(v = 1992, lty = 2, col = "gray", lwd = 2)

# ==============================================================================
# EVENT STUDY GRAPH: 1986 IMPLEMENTATION
# ==============================================================================

# 1. Run the Event Study (ref = 1990 sets the baseline to the election before repeal)
did_1986_data <- did_1986_data |>
  mutate(
    Carinthia = ifelse(state_code_string == "Carinthia", 1, 0)
  )
event_study_1986 <- feols(polarization_score ~ i(year, Carinthia, ref = 1983) + 
                            unemployed_perc + l_pop | state_code_string + year, 
                          data = did_1986_data, 
                          cluster = ~state_code_string)

# 2. Plot the base event study WITHOUT the default 1990 reference line
iplot(event_study_1986, 
      main = "Event Study: 1986 Implementation of Compulsory Voting",
      xlab = "Year",
      ylab = "Polarization Score",
      ref.line = FALSE) # <--- This removes the 1983 line

# 3. Add the custom vertical dotted line at 1992
abline(v = 1986, lty = 2, col = "gray", lwd = 2)
```

```{r}
df_clean_noCarinthia %>%
  group_by(year, ever_treated) %>%
  summarise(
    across(c(polarization_score), 
           mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(polarization_score),
               names_to = "variable", values_to = "value") %>%
  mutate(group = ifelse(ever_treated == 1, "Had CV", "Never had CV")) %>%
  ggplot(aes(x = year, y = value, color = group, linetype = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1986, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Had CV" = "black", "Never had CV" = "gray50")) +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Parallel Trends Check: Outcome and Covariates (No Carinthia)",
    x = "Year", y = "Mean Value", color = "Group", linetype = "Group"
  ) +
  theme_minimal()

# ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Final Plots/parallel_trends_polarization_NC.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
```

```{r}
df_clean_onlyCarinthia %>%
  group_by(year, ever_treated) %>%
  summarise(
    across(c(polarization_score), 
           mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(polarization_score),
               names_to = "variable", values_to = "value") %>%
  mutate(group = ifelse(ever_treated == 1, "Carinthia", "Control")) |>
  ggplot(aes(x = year, y = value, color = group, linetype = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1986, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Carinthia" = "black", "Control" = "gray50")) +
  geom_vline(xintercept = 1992, linetype = "dashed", color = "black") +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Parallel Trends Check: Outcome and Covariates (Carinthia)",
    x = "Year", y = "Mean Value", color = "Group", linetype = "Group"
  ) +
  theme_minimal()

# ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Final Plots/parallel_trends_polarization_OC.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
```

```{r}
print(skim(df_clean_onlyCarinthia))
```

```{r}
df |>
  filter(trimws(state_code_string) == "Carinthia") |>
  ggplot(aes(x = year, y = turnout)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_vline(xintercept = 1986, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 1992, linetype = "dashed", color = "darkred") +
  annotate("text", x = 1986, y = max(df$turnout[trimws(df$state_code_string) == "Carinthia"], na.rm = TRUE),
           label = "CV", hjust = -0.1, size = 3.5, color = "black") +
  annotate("text", x = 1992, y = max(df$turnout[trimws(df$state_code_string) == "Carinthia"], na.rm = TRUE),
           label = "No CV", hjust = -0.1, size = 3.5, color = "darkred") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Voter Turnout in Carinthia Over Time",
    subtitle = "Dashed lines indicate compulsory voting enactment (1986) and repeal (1992)",
    x = "Year",
    y = "Turnout"
  ) +
  theme_minimal(base_size = 13)
```

```{r}
library(tidyverse)

# 1. Calculate the raw difference and standard errors
df_diff_1992 <- df_clean_noCarinthia %>%
  group_by(year, ever_treated) %>%
  summarise(
    mean_pol = mean(polarization_score, na.rm = TRUE),
    var_pol = var(polarization_score, na.rm = TRUE),
    n = sum(!is.na(polarization_score)),
    .groups = "drop"
  ) %>%
  # Pivot wider so Treated (1) and Control (0) are side-by-side
  pivot_wider(
    id_cols = year,
    names_from = ever_treated,
    values_from = c(mean_pol, var_pol, n)
  ) %>%
  mutate(
    # Calculate difference (Treated - Control)
    difference = mean_pol_1 - mean_pol_0,
    
    # Calculate Standard Error of the Difference
    # Formula: sqrt( (Var_T / N_T) + (Var_C / N_C) )
    se_diff = sqrt((var_pol_1 / n_1) + (var_pol_0 / n_0)),
    
    # Calculate 95% Confidence Intervals (1.96 * SE)
    ci_lower = difference - (1.96 * se_diff),
    ci_upper = difference + (1.96 * se_diff)
  )

# 2. Plot the Difference with the CI Ribbon
ggplot(df_diff_1992, aes(x = year, y = difference)) +
  
  # Add the CI Ribbon first so it stays in the background
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "gray70", alpha = 0.4) +
  
  # Add the baseline zero line (Perfect Parallel Trends)
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  
  # Add the actual difference line and points
  geom_line(size = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  
  # Add the vertical intervention line
  geom_vline(xintercept = 1992, linetype = "dashed", color = "black", size = 0.8) +
  annotate("text", x = 1992.5, y = max(df_diff_1992$ci_upper, na.rm = TRUE), 
           label = "1992 Repeal", hjust = 0, size = 4) +
  
  # Formatting
  labs(
    title = "Difference in Polarization: Treated vs. Control (1992 Repeal)",
    subtitle = "Treated States minus Control States (with 95% CI)",
    x = "Year", 
    y = "Difference in Mean Polarization"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("/Users/ryansi/Library/CloudStorage/OneDrive-DukeUniversity/3.2/Econ 468/Final Paper/Final Plots/parallel_trends_polarization_NC.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
```

```{r}
library(tidyverse)

# 1. Calculate the raw difference and standard errors
df_diff_1986 <- did_1986_data %>%
  group_by(year, Carinthia) %>%
  summarise(
    mean_pol = mean(polarization_score, na.rm = TRUE),
    var_pol = var(polarization_score, na.rm = TRUE),
    n = sum(!is.na(polarization_score)),
    .groups = "drop"
  ) %>%
  # Pivot wider so Carinthia (1) and Control (0) are side-by-side
  pivot_wider(
    id_cols = year,
    names_from = Carinthia,
    values_from = c(mean_pol, var_pol, n)
  ) %>%
  mutate(
    # Calculate absolute difference (Carinthia - Control)
    difference = mean_pol_1 - mean_pol_0,
    
    # Calculate Standard Error of the Difference
    # Because Carinthia is N=1, its variance is 0. 
    # We only use the variance of the Control group (var_pol_0 / n_0)
    se_diff = sqrt(var_pol_0 / n_0),
    
    # Calculate 95% Confidence Intervals
    ci_lower = difference - (1.96 * se_diff),
    ci_upper = difference + (1.96 * se_diff)
  )

# 2. Plot the Difference with the CI Ribbon
ggplot(df_diff_1986, aes(x = year, y = difference)) +
  
  # Add the CI Ribbon first so it stays in the background
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "gray70", alpha = 0.4) +
  
  # Add the baseline zero line (Identical Polarization)
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  
  # Add the actual difference line and points
  geom_line(size = 1, color = "dodgerblue4") +
  geom_point(size = 2, color = "dodgerblue4") +
  
  # Add the vertical intervention line
  geom_vline(xintercept = 1986, linetype = "dashed", color = "black", size = 0.8) +
  annotate("text", x = 1986.5, y = max(df_diff_1986$ci_upper, na.rm = TRUE), 
           label = "1986 Enactment", hjust = 0, size = 4) +
  
  # Formatting
  labs(
    title = "Difference in Polarization: Carinthia vs. Control (1986 Enactment)",
    subtitle = "Carinthia minus Control States (with 95% Confidence Intervals)",
    x = "Year", 
    y = "Difference in Mean Polarization"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Optional: ggsave
# ggsave("parallel_trends_raw_diff_ci_1986.png", width = 8, height = 6, dpi = 300)
```
