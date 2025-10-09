# Quarterly US tariff data

![status-badge](https://img.shields.io/badge/status-beta-orange.svg)

*Beta version:* This dataset, code and documentation are still being tested, edited and validated. Please report inconsistencies or unexpected results via [GitHub issues](../../issues).

------------------------------------------------------------------------

## Overview

**The repository currently includes code and metadata only. The HS6–2012 quarterly tariff dataset (2015 Q1 to 2025 Q2) will be released shortly as a downloadable file.**

The code in this repository aggregates US tariff schedules at the 8-digit Harmonized Tariff Schedule (HTS8) level, harmonises them to HS6-2012, and reports both weighted and simple averages.

Primary sources:

-   USITC annual tariff tables (Chapters 1–97)

-   Global Trade Alert (GTA) policy interventions (Chapter 99 and special measures)

-   Trade data from the US Census Bureau [compiled by Peter K Schott](https://sompks4.github.io/sub_data.html) (for weights and unit values)

------------------------------------------------------------------------

## How the tariff data were obtained

### Core coverage (Chapters 1–97)

-   Base data are drawn from USITC annual tariff tables for Chapters 1–97.
-   These provide official applied and scheduled tariff rates for all HTS8 codes.

The dataset extends the USITC tables with relevant Chapter 99 measures obtained from Global Trade Alert. This includes Section 232 and 301 tariffs, for example.

### Retrieving special measures from Global Trade Alert

1.  Extract all GTA measures implemented since 1 January 2015.

2.  Filter to import tariffs that are:

    -   Implemented at the national level (exclude subnational or firm-specific).
    -   Not simple reclassifications or court rulings on product classification.
    -   Not preference scheme changes (e.g. GSP updates) already captured in USITC lists.
    -   Not other overlapping “tariff changes” captured in USITC Chapter 1–97 tables.

3.  Drop any measures:

    -   With implementation–removal intervals \< 1 day (cancelled before effect).
    -   Affecting valuation rules only (if uniform across all countries).
    -   Based solely on originating content (e.g. aluminium originating in Russia).
    -   Targeting or removing *de minimis* thresholds.

4.  After filtering, about 100 GTA measures remain, each linked to affected jurisdictions and HS-6 sectors.

5.  Each measure is manually coded to indicate whether it adds to or overrides an existing tariff rate.

    -   Default GTA coverage is HS-6; where possible, refined to HS-8 using documents provided by GTA.
    -   Section 301 product lists were extracted from Federal Register PDFs.
    -   Section 232 Canada/Mexico tariffs (announced 4 Mar 2018, exempted 7 Mar 2018) were removed due to origin ambiguity.

6.  Exemptions (e.g. Section 301 exclusions) are flagged so that tariff shifters for that product/program combination are set to zero during the exemption period.

### Weighting and aggregation

-   HTS8 tariffs are aggregated to HS6 using trade weights based on Schott’s US import data (2012–2014 average).
-   Tariffs are then mapped to HS6-2012 using official concordance tables (HS2017→2012 and HS2022→2012).
-   Both weighted and simple averages are reported.
    -   Weighted averages use 2012–2014 trade shares (similar to Fajgelbaum et al., 2019).
    -   Simple averages are provided as an alternative measure.

### References

-   Fajgelbaum, Pablo D., Pinelopi K. Goldberg, Patrick J. Kennedy, and Amit Khandelwal (2019). The Return to Protectionism, *The Quarterly Journal of Economics*, 135(1), pp. 1–55, <https://doi.org/10.1093/qje/qjz036>
-   Liao, Steven, In Song Kim, Sayumi Miyano, Hao Zhang (2020). concordance: Product Concordance. R package version 2.0.0. <https://CRAN.R-project.org/package=concordance>
-   Schott, Peter K. (2008). The Relative Sophistication of Chinese Exports, *Economic Policy*, 23(53), pp. 6–49, <https://doi.org/10.1111/j.1468-0327.2007.00195.x>
-   [USITC Harmonized Tariff Schedule](https://hts.usitc.gov/current)
-   [GTA Methodology](https://www.globaltradealert.org)

------------------------------------------------------------------------

## Contents

-   `code/`: R scripts to build correlation shares, aggregate HTS8→HS6, convert HS6→HS6-2012.
-   `data/temp/`: correlation tables (`correl_hs6_2017_to_2012.csv`, `correl_hs6_2022_to_2012.csv`), base weights, and helper CSVs.

------------------------------------------------------------------------

## Version and validation

This is a beta release intended for testing and early feedback.

Planned improvements include:
- Expanded documentation and validation of HTS8→HS6 weighting.
- Additional checks across years and code revisions.
- Review of specific-rate conversions and special measures coverage.
