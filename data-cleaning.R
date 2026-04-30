# ============================================================
# Austrian Election Polarization Dataset
#
# Constructs a panel dataset of Austrian parliamentary elections
# (1950–2010) augmented with ideological polarization scores
# derived from the Manifesto Project Database (MPDS2025a).
#
# Primary sources:
#   - Hoffmann et al. (2017): Austrian state-level election results
#   - Manifesto Project Database (MPDS2025a): party-level RILE scores
#
# Final output: `election_polarization`
# ============================================================


# 0. Dependencies
library(tidyverse)
library(haven)


# 1. Load Raw Data

# Austrian state-level election results
# Source: Hoffmann et al. (2017), Compulsory voting, turnout, and government spending: Evidence from Austria
elections_raw <- read_dta(
  "Elections.dta"
)

# Manifesto Project Database: party positions & vote shares
# Source: Volkens et al. (2025), MPDS2025a
mpd_raw <- read_dta(
  "MPDataset_MPDS2025a_stata14.dta"
)


# 2. Clean Election Results
# Keep parliamentary elections between 1950 and 2010.
# Convert vote-share variables from proportions to percentages.

PARTIES_ABBREV <- c("FPÖ", "KPÖ", "ÖVP", "SPÖ", "VdU")
VOTE_SHARE_COLS <- c("oevp_perc", "spoe_perc", "fpoe_perc", "kpoe_perc", "wdu_perc")

elections_clean <- elections_raw |>
  filter(parl == 1, between(year, 1950, 2009)) |>
  arrange(year, state_code) |>
  mutate(
    state_name = as_factor(state_code),
    across(all_of(VOTE_SHARE_COLS), ~ round(. * 100, 1))
  )


# 3. Clean Manifesto Project Data
# Retain Austrian entries for the five main parties in scope.
# Add a calendar year column for merging and drop all-NA columns.

mpd_clean <- mpd_raw |>
  filter(
    countryname == "Austria",
    between(date, 195001, 200999),
    partyabbrev %in% PARTIES_ABBREV
  ) |>
  mutate(year = year(as.Date(edate))) |>
  select(where(~ !all(is.na(.)))) |>
  relocate(countryname, edate, partyname, partyabbrev, rile)


# 4. Compute Election-Level Polarization Scores
# For each election date:
#   (a) Weighted-mean RILE score (vote shares as weights)
#   (b) Party-level squared deviation from the weighted mean

NORM_CONSTANT <- 100   # RILE scale normalisation factor

mpd_polarization <- mpd_clean |>
  select(countryname, edate, year, partyname, partyabbrev, rile, pervote) |>
  group_by(edate) |>
  mutate(
    rile_mean = weighted.mean(rile, w = pervote, na.rm = TRUE),
    rile_difference = rile - rile_mean,
    polarization_score_partyyear = (rile_difference / NORM_CONSTANT)^2
  ) |>
  ungroup()


# 5. Merge RILE Scores into Election Panel
# Pivot to one row per year × party, then join onto the
# election-results panel on year.

party_rile_wide <- mpd_polarization |>
  select(year, partyabbrev, rile) |>
  distinct() |>
  pivot_wider(
    names_from  = partyabbrev,
    values_from = rile,
    values_fill = NA
  ) |>
  rename_with(~ paste0(., "_rile"), -year) |>
  arrange(year) |>
  mutate(across(ends_with("_rile"), ~ zoo::na.approx(., na.rm = FALSE)))

elections_merged <- elections_clean |>
  left_join(party_rile_wide, by = "year")


# 6. Compute System-Level Polarization Index
# Polarization index (Dalton 2008):
#   sqrt( Σ_i  s_i * ((pos_i - avg_lr) / 100)^2 )
# where s_i is party i's vote share (0–1) and pos_i its RILE score.

election_polarization <- elections_merged |>
  rowwise() |>
  mutate(
    avg_lr = weighted.mean(
      x = c(`SPÖ_rile`, `ÖVP_rile`, `FPÖ_rile`, `KPÖ_rile`, `VdU_rile`),
      w = c(spoe_perc, oevp_perc, fpoe_perc, kpoe_perc, wdu_perc),
      na.rm = TRUE
    ),
    polarization_score = sqrt(sum(
      (c(spoe_perc, oevp_perc, fpoe_perc, kpoe_perc, wdu_perc) / 100) *
        ((c(`SPÖ_rile`, `ÖVP_rile`, `FPÖ_rile`, `KPÖ_rile`, `VdU_rile`) - avg_lr) / 100)^2,
      na.rm = TRUE
    )),
    ln_polarization_score = log(polarization_score)
  ) |>
  ungroup()