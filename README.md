# Replication: Costs of Patronage
This is a replication study of Guo, X. 2018: The Costs of Patronage: Evidence from the British Empire, published in American Economic Review, 108(11), p. 3170 - 3198. The paper examines how patronage appointments of colonial officers in the British Empire affected governance outcomes. This replication project reproduces the main econometric results using R.

## How to run
Run `00_master.R` to reproduce everything from scratch (tables, figures, the final PDF).

## Project structure
- `analysis.dta` - the data set in STATA format provided by the author
- `01_tables.R` - produces descriptive and regression tables, saves them in the results folder
- `02_figures.R` - produces plots, saves them in the figures folder
- `replication_chmielowski.Rnw` compiles a final report, sources from the results and figures folder

The final report is `replication_chmielowski.pdf`

## Requirements
- R version 4.4.2
- TinyTeX or a full LaTeX distribution
- R packages: tidyverse, haven, lfe, plm, multicomp, Hmisc, did, mediation, tinytex

