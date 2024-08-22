# Rotterdam Schools Synthetic Control analysis

Reproducible code archive for NPRZ impact assessment study

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

| script | goal |
| :----- | :--- |
| `02_quality_control.R` | Perform several checks on the pre-processed data |
| `03_summarization.R` | Compute school-level variables from student-level data |
| `04(a/b)_dataprep_synthetic_control_(cito/isled).R` | Data preparation, missing value handling, preparation of matrices for performing synthetic control |
| `05(a/b)_compute_synthetic_control_(cito/isled).R` | Compute the synthetic control for each school |
| `06(a/b)_inspect_synthetic_control_(cito/isled).R` | Inspect the results of the synthetic control procedure, produce plots, and more |
| `07_prepare_output.R` | Preparation of results for output check at Statistics Netherlands |

In addition, the `04` and `05` scripts have versions in the folder `robustness_checks` to re-run the analyses with more potential covariates.

