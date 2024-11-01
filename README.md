# NPRZ Synthetic Control analysis

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14022992.svg)](https://doi.org/10.5281/zenodo.14022992)

Reproducible code archive accompanying the manuscript "The limited impact of extending the school week on educational outcomes in Dutch primary education: a quasi-experimental study using penalized synthetic control analysis".

This code repository is designed to run on the microdata environment of Statistics Netherlands (CBS). For more information about this, see [the microdata webpage](https://www.cbs.nl/en-gb/our-services/customised-services-microdata/microdata-conducting-your-own-research).

## Installation
The project uses R and RStudio. To run the project, install both and then open the file `nprz_synthetic_controls.Rproj`. Install the dependencies as follows: 

```r
deps <- c("haven", "tidyverse", "mice", "purrr", "furrr", "pensynth", "Synth", "patchwork", "writexl")
install.packages(deps)
```

## Preprocessing
The preprocessing scripts can be run all at once by running the file `01_run_preprocessing.R` from the main directory.

```
RScript 01_run_preprocessing.R
```

In RStudio, you can use a "Job" for this.

This should take about 1 hour 45 minutes.

Each of the preprocessing steps in the folder `preprocessing_scripts` can also be run independently.

## Synthetic control

The scripts 02-06 run and inspect the synthetic control method, as follows:

| Script | Goal |
| :----- | :--- |
| `02_quality_control.R` | Perform several checks on the pre-processed data |
| `03_summarization.R` | Compute school-level variables from student-level data |
| `04(a/b)_dataprep_synthetic_control_(cito/isled).R` | Data preparation, missing value handling, preparation of matrices for performing synthetic control |
| `05(a/b)_compute_synthetic_control_(cito/isled).R` | Compute the synthetic control for each school |
| `06(a/b)_inspect_synthetic_control_(cito/isled).R` | Inspect the results of the synthetic control procedure, produce plots, and more |
| `07_prepare_output.R` | Preparation of results for output check at Statistics Netherlands |

In addition, the `04` and `05` scripts have versions in the folder `robustness_checks` to re-run the analyses with more potential covariates.

## Contact
This is a project by the [ODISSEI Social Data Science (SoDa)](https://odissei-soda.nl/) team.
Do you have questions, suggestions, or remarks on the technical implementation? Create an issue in the [issue tracker](https://github.com/sodascience/nprz_synthetic_controls/issues) or feel free to contact [Erik-Jan van Kesteren](https://github.com/vankesteren) or [Gijs Custers](https://www.eur.nl/en/people/gijs-custers).

<img src="img/soda.png" alt="SoDa logo" width="250px"/> 
