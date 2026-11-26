# Replication: Edelman, Luca & Svirsky (2017)

"Racial Discrimination in the Sharing Economy: Evidence from a Field Experiment",
*American Economic Journal: Applied Economics* 9(2): 1–22. https://doi.org/10.1257/app.20160213

## Folder structure

```
PS3&4/
|-- README.md                 this file
|-- PS3&4.Rproj               RStudio project (sets the working directory)
|-- replication_airbnb.R      master script: cleaning (Part 1), replication (Part 2), extension (Part 3)
|-- replication_data/         raw data from the authors' replication package (not modified)
|     main_data.csv           7,554 host-guest interactions (no header row)
|     name_survey_results.xlsx  survey ratings of the 20 guest names
|     hosts.dta               racial composition of each host's past guests
|-- output/                   all tables and figures (created by the script)
```

## How to run

1. Open `PS3&4.Rproj` in RStudio. All paths in the script are relative to this folder.
2. Install the packages once:
   `install.packages(c("haven", "readxl", "dplyr", "ggplot2", "sandwich", "lmtest", "marginaleffects", "MASS"))`
3. Run `replication_airbnb.R` from top to bottom. It starts from the raw data and writes
   all results to `output/`.

## Software

R 4.4.1; packages haven, readxl, dplyr, ggplot2, sandwich, lmtest, marginaleffects, MASS.
The random seed for the cluster bootstrap of the Manski bounds is set inside the script (`set.seed(1)`).

## Outputs

| File | Content | In the report |
|---|---|---|
| `output/figure2_response_by_race.png` | Figure 2 of the paper | Figure 1 |
| `output/table1_summary_stats.csv` | Table 1 of the paper (balance table) | mentioned in Section 3.1 |
| `output/table2_main_regression.csv` | Table 2 of the paper (col. 3 uses `new_number_of_listings`, matches the published table) | Table 1 |
| `output/table2_main_regression_dofile_version.csv` | Table 2 with the do-file definition of "multiple listings" | Section 3.1 |
| `output/table_ext1_logit.csv` | LPM vs. logit / probit average marginal effects | Section 5.1 |
| `output/table_ext2_response_types.csv` | Effect of an African American name on each type of response + ordered logit | Table 2 |
| `output/table_ext3_manski_bounds.csv` | Manski worst-case bounds with cluster-bootstrap CI | Table 3 |

## Data source

Replication package of Edelman, Luca & Svirsky (2017), openICPSR project 113691:
https://www.openicpsr.org/openicpsr/project/113691/version/V1/view (accessed 23.11.2026).
