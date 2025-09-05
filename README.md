# Statistical Reporting Inconsistencies in Experimental Linguistics

This repository accompanies the manuscript "Statistical Reporting Inconsistencies in Experimental Linguistics" by Dara Leonard Jenssen Etemady & Timo B. Roettger

## Content

### data

-   `statcheck_original_data.csv`: derived data table that extracted statcheck results from the first set of articles in the sample during first submission (originally stored in `Journals/`). Derived by `scripts/01_Analysis.R`.
-   `statcheck_revised_sample.csv`: derived data table that extracted statcheck results from all articles in the current manuscript (originally stored in `Journals/`). Derived by `scripts/01_Analysis.R`.
-   `subsample`: folder containing the `.txt` files with the manually annotated subsample derived by `scripts/02_sampling_for_manual_inspection.R`. Also contains `subsample.csv` and `subsample_for_manual_annotation_merged.csv` data which contains the manual annotations by the authors.

### plots

-   print-ready figures (1-3) for manuscript. Derived by `scripts/01_Analysis.R`.

### scripts

-   `01_Analysis.R`: R script to derive `data/statcheck_data.csv`, table 1 and `plots/figure[1-3]`
-   `02_sampling_for_manual_inspection.R`: R script to extract manual sample and derive table for manual annotations.

### Journals

-   original location of all articles in the sample. Not uploaded due to sharing restrictions

### manuscript

-   relevant files to derive manuscript as `.pdf`, `.docx`, and `.html`
