# Project: Party System Polarization and Growth Volatility

## What this project is
Research paper on the nonlinear (U-shaped) relationship between party system
polarization and economic growth volatility in Latin American and global democracies.
Target journal: World Development.

## Authors
- Sergio Béjar (CIDE, corresponding)
- Federico Acosta y Lara (WashU)
- Juan Andrés Moraes (Universidad de la República)

## Key files
- `paper/pola_growthvol_v6.tex` — manuscript (LaTeX)
- `paper/references.bib` — bibliography
- `paper/figures/` — all manuscript figures
- `Code/` — R scripts for data construction and analysis
- `Data/` — panel datasets

## Core findings
- DV: vol_growth = |Δ GDP growth rate| (WDI)
- Main IV: Dalton vote-share polarization index (PELA surveys)
- U-shaped relationship, turning point ≈ 3.21
- Both terms significant at 1% (TWFE, entity-clustered SE)
- Signal strengthens at L1 lag, fades by L3
- Attenuates post-2007 (external shock scope condition)
- 9 mechanism channels tested, all null as sequential mediators
- Global replication: 68 democracies pre-2007, V-Dem v2cacamps

## Coding conventions
- R for all data management and analysis
- Python / linearmodels.PanelOLS for panel econometrics when needed
- LaTeX for manuscript
- Figures saved as PNG to paper/figures/

## Current status
- Manuscript near submission quality (v6)
- Addressing reviewer and colleague feedback
- Next tasks: uncertainty proxy (EPU or forecast dispersion),
  election-timing interactions, possible commodity dependence heterogeneity

## Style guide for manuscript edits
- Avoid AI-isms: no em dashes (use commas, parens, or periods), no "robust/comprehensive/delve/leverage"
- Max 1 em dash per 1000 words
- Tone: direct, concrete, no hedging ("perhaps", "it is worth noting")
- Framing: development-first (WD audience), not polisci-first
- Theory language: "implies under plausible conditions", not "theorem"
- Contributions framed as complementing existing work, not correcting it
