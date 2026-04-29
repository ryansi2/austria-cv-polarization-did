# The Causal Impact of Compulsory Voting on Political Polarization

## Project Overview
This research investigates the relationship between mandatory electoral participation and ideological polarization. Utilizing a quasi-experimental design, the project analyzes the staggered implementation and repeal of compulsory voting (CV) across Austrian states (*Länder*) to identify whether institutional participation requirements influence party rhetoric.

The study combines high-resolution electoral data with the **Manifesto Project Database (MARPOR)** and uses the **Dalton Polarization Index** to quantify shifts in the political landscape from 1945 to the present.

## Key Research Questions
* Does compulsory voting effectively "pull" parties toward the median voter?
* Do political shocks (e.g., the 1994 EU Referendum) outweigh the moderating effects of voting laws?
* Are the effects of CV repeal statistically significant regarding ideological dispersion?

---

## Methodology & Tech Stack
* **Causal Inference:** Difference-in-Differences (DiD) framework leveraging sub-national policy variation.
* **Data Wrangling:** Merging administrative election results with the Manifesto Project Database using `tidyverse` and `haven`.
* **Polarization Metrics:** Calculation of the **Dalton Index** to measure party system dispersion.
* **Tools:** * **R:** Data cleaning, statistical modeling, and visualization (`ggplot2`, `fixest`).
  * **Stata:** Robustness checks and handling legacy `.dta` formats.

---

## Data Sources
> **Note on Data Access:** Due to licensing restrictions, raw datasets are not hosted in this repository. 

1. **Manifesto Project Database (MPDS2025a):** Party manifesto coding for Austrian national and regional elections. [Access here](https://manifesto-project.wzb.eu/).
2. **Hoffman et al. (2017):** Turnout and government spending data for Austrian municipalities and states.
3. **Austrian Interior Ministry:** Official historical election results at the state level.

---

## Repository Structure
```text
├── data/               # (Empty/Gitignored) Directory for .dta and .csv files
├── scripts/
│   ├── 01_cleaning.R   # Merging MARPOR data with state election results
│   ├── 02_analysis.R   # DiD models and Dalton Index calculations
│   └── 03_viz.R        # Generating plots for polarization trends
├── results/            # Regression tables and exported figures
├── .gitignore          # Configured to ignore large data files (*.dta)
└── README.md
