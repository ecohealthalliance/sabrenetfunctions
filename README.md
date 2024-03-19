
## SABRENet

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6637927)](https://doi.org/10.5281/zenodo.6637927)
[![License (for code):
MIT](https://img.shields.io/badge/License%20(for%20code)-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![License:
CC0-1.0](https://img.shields.io/badge/License%20(for%20data)-CC0_1.0-lightgrey.svg)](http://creativecommons.org/publicdomain/zero/1.0/)
[![License:
CC-BY-4.0](https://img.shields.io/badge/License%20(for%20text)-CC_BY_4.0-blue.svg)](http://creativecommons.org/publicdomain/zero/1.0/)

This respoitory contains code associated with the SABRENet project in
collaboration with the University of Pretoria, South Africa. This
repository is an offshoot of code originally found in sabrenet-reports
that has now become a package for use in other sabrenet-XXXX
repositories. This package/repository contains useful code for:

- functions that are used across sabrenet-XXXX repositories

##### These functions are used in other sabrenet repositories as a downloaded package

`devtools::install_github("ecohealthalliance/sabrenetfunctions")`

The base is currently maintained by [Morgan
Kain](mailto:kain@ecohealthalliance.org) and [Collin
Schwantes](mailto:schwantes@ecohealthalliance.org)

### [Production Base](https://airtable.com/appi9YHtonEpMFuAz/tblOMNLMeu1VwYAw0/viwfBSc2GGtRTGwFb?blocks=bipcGM7IhxLmkIwxZ)

### Repository Structure and Reproducibility (across all sabrenet-XXXX repositories, some may not show up in -this- repo)

- `data/` contains data from the study and a data dictionary describing
  all variables.
- `auth/` contains creds for google service account. [Click here for
  more
  info](https://ecohealthalliance.github.io/eha-ma-handbook/14-cloud-computing-services.html)
- `R/` contains functions used in this analysis.
- `reports/` contains literate code for R Markdown reports generated in
  the analysis
- `outputs/` contains compiled reports and figures.

### Function and R/ naming conventions (across all sabrenet-XXXX repositories, some may not show up in -this- repo)

- First level function prefixes:
  - get\_ –\> retrieving data from Airtable base
    - mostly associated with `data_input_targets`
  - insert\_ –\> Adding new rows to airtable tables with local data
    - mostly associated with `data_insert_targets`
  - modify\_ –\> modifying entries in existing rows in airtable tables
    with local data
    - mostly associated with `data_insert_targets`
  - process\_ , establish\_ , clean\_ , summarize\_ –\> all data
    manipulation/cleaning for insert\_ , plot\_ , or generate\_
    - mostly part of `data_organization_targets`
  - calculate\_ , estimate\_ –\> Analyses
    - `analysis_targets`
  - generate\_ , plot\_ , render\_ –\> creation of any saved output
    - `table_targets` , `plot_targets`
- Second stage prefixes (analysis classes)
  - PCR – pipeline associated with molecular analysis of samples
  - sero – pipeline associated with serology
  - GPS – any spatial stuff
  - diversity – estiamtes of viral diversity

### Targets Framework

- Unlike other sabrenet-XXXX repositories this repository does not use
  the `targets` package because it just houses some functions that are
  meant to be sourced from within the other repositories

### Versioning

- This project requires R version 4.3.2 (2023-10-31). This project uses
  the [{renv}](https://rstudio.github.io/renv/) framework to record R
  package dependencies and versions. Packages and versions used are
  recorded in `renv.lock` and code used to manage dependencies is in
  `renv/` and other files in the root project directory. On starting an
  R session in the working directory, run `renv::restore()` to install R
  package dependencies.
