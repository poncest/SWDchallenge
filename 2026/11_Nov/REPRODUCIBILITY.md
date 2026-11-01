# November 2026 SWD — Reproducibility

The root `README.md` in this folder deliberately contains only the published image link. This document is the technical companion.

## Sources and raw data

Download both **ATUS 2003–2025 Activity summary file** and **ATUS 2003–2025 Replicate weights file** from the [BLS multi-year file page](https://www.bls.gov/tus/data/datafiles-0325.htm). The extracted files required are `atussum_0325.dat` and `atuswgts_0325.dat`. Put them in `2026/11_Nov/` in the cloned repository, or adapt the path in the scripts. Neither raw file is distributed or committed to Git due to size. The site also provides the 2003–2025 lexicon and user documentation.

## What can be reproduced

1. `11_Nov.R` calculates the eight weighted sex × partnership × period averages, calculates the unadjusted difference in changes, and renders the figure using the repository's `R/` helpers. Its confidence-interval bounds are **historical externally validated constants**, not recomputed by the plotting script.
2. `atus_unadjusted_replication.R` computes the unadjusted change, difference in changes, components and the 95% confidence intervals using 160 BLS successive-difference replicate weights. It requires the correct replicate-weight family prefix to be explicitly selected using `ATUS_REPLICATE_PREFIX`, after examining the header and BLS documentation. It deliberately stops rather than choose among possibly different weight families.
3. The previous **composition-adjusted** contrast, its exact model specification, and the original `atus_final_gate.py` were **not included in the supplied files**. Thus, the historical adjusted estimate and entire predeclared statistical gate cannot yet be independently recreated with this package. Include the original tested script, its variable encoding and decision record before claiming full inferential reproducibility.

## Steps

1. Clone `poncest/SWDchallenge`, including the `R/utils` and `R/themes` helpers. Place these files in `2026/11_Nov` except `README.md` which remains in that directory and displays `img/11_Nov.png`.
2. Install R packages used by `11_Nov.R` (`tidyverse`, `ggtext`, `showtext`, `janitor`, `scales`, `glue`, `ggview`, `here`, `sessioninfo`, and `pacman`) and any dependencies from the custom repository helpers. The recorded R version was 4.6.1. Installed fonts must satisfy `R/utils/fonts.R`.
3. Download and extract the two BLS datasets. Confirm file names exactly (case and spelling).
4. In a clean R session rooted at the repo, run `source('2026/11_Nov/11_Nov.R')` to regenerate the descriptive chart. Check saved PNG and helper-export parameters.
5. Inspect the replicate data header. Specify the **BLS-documented replicate family associated with `TUFNWGTP`**: `Sys.setenv(ATUS_REPLICATE_PREFIX='VERIFIED_PREFIX')`; replace the placeholder after verification. Run `source('2026/11_Nov/atus_unadjusted_replication.R')`. Inspect output `atus_unadjusted_replication_results.csv` and compare to the historical 13.6 [2.8, 24.3] unadjusted result. No successful replication is claimed until run on the raw data.

## Definitions and scope

- Adults ages 25–64, periods 2003–2007 and 2021–2025; 2020 excluded.
- Household activity minutes: sum of harmonized `t02` summary columns, including zero-minute diaries.
- Married: `TRSPPRES == 1`; no spouse/partner: `TRSPPRES == 3`; cohabiting excluded from figure. Sex: `TESEX == 1` men, `TESEX == 2` women.
- Final weights: `TUFNWGTP`, not the pandemic-specific `TU20FWGT`.
- Gender gap: weighted mean for women minus weighted mean for men (individual respondents, **not** matched couples). Difference in changes = change in no-partner gap minus change in married gap.
- Raw estimate/uncertainty refers to descriptive group contrasts; no causal claims.
- ATUS variance method for 160 replicates: `(4 / 160) * sum((replicate_estimate - full_sample_estimate)^2)`. 95% Wald interval: estimate ±1.96 SE.

## Validation limits

The raw 2003–2025 summary and replicate files were **not supplied with this package**, so execution with data and agreement with the original output have **not been verified**. The plotting script contains a hardcoded CI with provenance stated in its source. For complete reproducibility of the previously reported adjusted result and ADVANCE decision, commit `atus_final_gate.py` (or its exact equivalent) and document its Python dependencies and the original data/version checks.
