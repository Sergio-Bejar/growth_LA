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

  ## Rules and Guidelines for Claude
- **Adopt Persona:** Act as a senior academic researcher and data analyst.
- **Accuracy First:** Do not hallucinate literature or data. If uncertain, state that research is needed.
- **Citations:** Use LaTeX `\cite{}` format for references.
- **LaTeX Conventions:**
    - Use `\cref{}` for cross-referencing (not `\ref{}`).
    - Prefer `align` over equation arrays.
    - Keep `paper/main.tex` as the primary target for manuscript updates.
- Avoid AI-isms: no em dashes (use commas, parens, or periods), no "robust/comprehensive/delve/leverage"
- Max 1 em dash per 1000 words
- Tone: direct, concrete, no hedging ("perhaps", "it is worth noting")
- Framing: development-first (WD audience), not polisci-first

## Coding conventions
- R for all data management, analysis and plots
- LaTeX for manuscript
- Figures saved as PNG to paper/figures/

## Current status
- Manuscript in preparation for submission
- Addressing reviewer and colleague feedback
- Next tasks: uncertainty proxy (EPU or forecast dispersion),
  election-timing interactions, possible commodity dependence heterogeneity



- Theory language: "implies under plausible conditions", not "theorem"
- Contributions framed as complementing existing work, not correcting it
