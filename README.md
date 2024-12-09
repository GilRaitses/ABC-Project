# ABC Project

This project processes whale tag data and creates composite time series dashboards for each event segment.

## Directory Structure

- **data/**: Contains input CSV files (`mn09_203a.csv` and `log_mn09_203a.csv`).
- **scripts/**: Contains the R scripts for processing data and generating dashboards and analysis plots.
- **plots/**: Directory where the generated composite dashboards and analysis plots will be saved.
- **output/**: Directory where the labeled and event data CSV files will be saved.
- **README.md**: This file.
- **.gitignore**: Specifies files and directories to ignore in Git.

## Setup

1. Clone the repository:
    ```bash
    git clone https://github.com/GilRaitses/ABC-Project.git
    cd ABC-Project
    ```

2. Ensure you have the necessary R packages installed:
    ```r
    install.packages(c("dplyr", "fuzzyjoin", "tagtools", "tidyr", "purrr", "stringr", "ggplot2", "patchwork", "scales"))
    ```

3. Ensure the `data/` directory contains the required CSV files:
    - `mn09_203a.csv`
    - `log_mn09_203a.csv`

4. Create the `plots` and `output` directories if they don't exist:
    ```bash
    mkdir -p plots output
    ```

## Running the Scripts

1. **Generate the Event Data**:
    - Open R or RStudio.
    - Set the working directory to the project folder:
        ```r
        setwd("path/to/ABC-Project")
        ```
    - Run the `create_composite_dashboards.R` script:
        ```r
        source("scripts/create_composite_dashboards.R")
        ```

2. **Generate the Analysis Plots**:
    - Run the `analyze_whale_dives.R` script:
        ```r
        source("scripts/analyze_whale_dives.R")
        ```

The output plots will be saved in the `plots/` directory. The CSV files will be saved in the `data/` directory.

## .gitignore

Include the following in your `.gitignore` to ignore the output files and other unnecessary files:
```
output/
plots/
*.RData
*.Rhistory
```
