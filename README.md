# The Causal Effect of Compulsory Voting on Electoral Polarization

## Overview

This project estimates the causal effect of compulsory voting (CV) on electoral
polarization in Austria using sub-national policy variation across Austrian states
(*Länder*). The analysis exploits the staggered implementation (1986) and repeal
(1992) of CV laws to identify whether mandatory participation requirements shift
party systems toward the ideological centre.

Polarization is measured using the **Dalton (2008) Index**, constructed from
party-level left-right scores in the **Manifesto Project Database (MPDS2025a)**,
weighted by state-level vote shares from Hoffmann et al. (2017).

---

## Research Questions

- Does compulsory voting pull parties toward the median voter?
- Is the moderating effect of CV reversed upon repeal?
- Are results robust to alternative estimation strategies (DiD vs. synthetic control)?

---

## Data Sources

Raw data are not hosted in this repository due to licensing restrictions.

| Source | Description |
|---|---|
| Hoffmann et al. (2017), *J. Public Econ.* | State-level Austrian election returns, 1949–2010 |
| Manifesto Project Database (MPDS2025a) | Party-level RILE scores and vote shares |

---

## Repository Structure

```text
├── 01_data_construction.R           # Constructs election_polarization.dta from raw sources
├── 02_data_visualisation.R          # Descriptive figures and parallel trends diagnostics
├── 03_analysis_did.R                # Difference-in-differences estimation
├── 04_analysis_synthetic_control.R  # Synthetic control estimation and placebo tests
├── election_polarization.dta        # Final analysis dataset (output of 01_data_construction.R)
├── austria-cv-polarization-did.Rproj
└── .gitignore
```

---

## Methodology

**Difference-in-Differences** (`03_analysis_did.R`)  
Estimates the effect of the 1986 CV implementation in Carinthia and the 1992
repeal in Styria, Vorarlberg, and Tyrol against never-treated donor states.
Models vary across narrow and wide estimation windows and four fixed-effect
structures (two-way, state-only, year-only, none). Standard errors are clustered
at the state level. Robustness checks use a standardised polarization outcome.

**Synthetic Control** (`04_analysis_synthetic_control.R`)  
Constructs a weighted synthetic Carinthia from the donor pool to estimate the
1986 treatment effect. Inference is based on in-space placebo tests (all donor
states as pseudo-treated units) and in-time placebo tests (fake treatment years
for Carinthia). Permutation p-values are reported.

---

## Dependencies

```r
install.packages(c("tidyverse", "haven", "fixest", "Synth", "ggrepel", "zoo"))
```

---

## Citation

Hoffman, Mitchell, Gianmarco Le´on, and Mar´ıa Lombardi. 2017. “Compulsory Voting,
Turnout, and Government Spending: Evidence from Austria.” Journal of Public Economics
145 (January): 103–15. https://doi.org/10.1016/j.jpubeco.2016.10.002.

Volkens, Andrea, et al. (2025). The Manifesto Data Collection. Manifesto Project (MRG/CMP/MARPOR). Version 2025a. Berlin: Wissenschaftszentrum Berlin für Sozialforschung (WZB).
