# Quarterly US tariff data

![status-badge](https://img.shields.io/badge/status-beta-orange.svg)

*Beta version:* This dataset, code and documentation are still being tested, edited and validated. Please report inconsistencies or unexpected results via [GitHub issues](../../issues).

## Download the data

The latest **quarterly HS6–2012 tariff dataset (2015 Q1–2025 Q2)** is available from the\
[**Releases page**](https://github.com/sjhardwick/us_tariff_data/releases).

Each release archive (`.zip`) contains a single CSV file of the full dataset.

## Overview

Primary sources:

-   USITC annual tariff tables (Chapters 1–97)

-   Global Trade Alert (GTA) policy interventions (Chapter 99 and special measures)

-   Trade data from the US Census Bureau [compiled by Peter K. Schott](https://sompks4.github.io/sub_data.html) (for weights and unit values)

## How tariff rates were obtained

### Core coverage (Chapters 1–97)

-   Base data are drawn from USITC annual tariff tables for Chapters 1–97.
-   These provide official applied and scheduled tariff rates for all HTS8 codes.

The dataset extends the USITC tables with relevant Chapter 99 measures obtained from Global Trade Alert. This includes Section 232 and 301 tariffs, for example.

### Retrieving special measures from Global Trade Alert

1.  Extract all GTA measures implemented since 1 January 2015.

2.  Filter (\`code/compile_gta_data.R\`) to import tariffs that are:

    -   Implemented at the national level (exclude subnational or firm-specific).
    -   Not simple reclassifications or court rulings on product classification.
    -   Not preference scheme changes (e.g. GSP updates) already captured in USITC lists.
    -   Not other overlapping “tariff changes” captured in USITC Chapter 1–97 tables.
    -   Not cancelled before they took effect (specifically, the time between implementation and removal must exceed one day).

3.  After filtering, 132 GTA interventions remain, each linked to affected jurisdictions and HS-6 sectors (`data/temp/gta.xlsx`). Additional manual filtering is undertaken to consolidate or remove duplicates, remove measures based solely on originating content (e.g. aluminium originating in Russia) or that remove *de minimis* thresholds.

    -   Section 232 Canada/Mexico tariffs (announced 4 March 2018) were removed, because after 7 March, it was announced that USMCA compliant goods, which depend on rules of origin, would be exempt.

4.  This further filtering reduces the number of interventions to 89 (`data/temp/measures.csv`).

5.  Each intervention is manually coded to indicate whether it adds to or overrides an existing tariff rate.

    -   Default GTA coverage is HS6-2022; where possible, this is refined to HTS-8 using documents linked to by GTA or on the Federal Register.
    -   Section 301 product lists were extracted from Federal Register PDFs.

6.  Exemptions (e.g. Section 301 product exclusions) are flagged so that tariff changes associated with that product/program combination are set to zero during the exemption period. Other tariffs on those products, if unaffected by the exemption, remain in the data.

### Weighting and aggregation

-   HTS8 tariffs are aggregated to HS6 using trade weights based on US import data from the US Census Bureau compiled by Peter K. Schott. An average of 2012, 2013 and 2014 imports is used to construct weights.
-   Tariffs are then mapped to HS6-2012 using concordance tables drawn from Liao et al. (2020). In the case of many-to-one mappings, each constituent code is given an equal weighting.
-   Simple averages, as well as the trade-weighted averages, are reported for each HS6 good. The results are similar (correlation of 0.93).

## Contents

-   `code/`: R scripts to extract and compile tariff rate data.
-   `data/temp/`: correlation tables, trade weights, unit values, lists of measures retrieved from GTA and list of country codes in import data.

## References

-   Fajgelbaum, Pablo D., Pinelopi K. Goldberg, Patrick J. Kennedy, and Amit Khandelwal (2019). The Return to Protectionism, *The Quarterly Journal of Economics*, 135(1), pp. 1–55, <https://doi.org/10.1093/qje/qjz036>
-   Global Trade Alert (2025). Dataset. <https://globaltradealert.org/>
-   Liao, Steven, In Song Kim, Sayumi Miyano, Hao Zhang (2020). concordance: Product Concordance. R package version 2.0.0. <https://CRAN.R-project.org/package=concordance>
-   Schott, Peter K. (2008). The Relative Sophistication of Chinese Exports, *Economic Policy*, 23(53), pp. 6–49, <https://doi.org/10.1111/j.1468-0327.2007.00195.x>
-   United States International Trade Commission (USITC) (2025). Annual Data. <https://dataweb.usitc.gov/tariff/annual>
