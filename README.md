# ABC Project

This project processes whale tag data and creates composite time series dashboards for each event segment.

## Directory Structure

- **data/**: Directory where the input CSV files should be placed.
  - `mn09_203a.csv`: Data file for the project.
  - `log_mn09_203a.csv`: Log data file for the project.
- **scripts/**: Contains the R script for processing data and generating dashboards.
  - `create_composite_dashboards.R`: The main script.
- **output/**: Directory where the generated composite dashboards will be saved.
  - `event_composite_dashboards/`: Subdirectory for storing the output dashboards.
- **README.md**: This file.
- **.gitignore**: Specifies files and directories to ignore in Git.

## Setup

1. Clone the repository:
    ```{bash}
    git clone https://github.com/yourusername/ABC-Project.git
    cd ABC-Project
    ```

2. Ensure you have the necessary R packages installed:
    ```{r}
    install.packages(c("dplyr", "fuzzyjoin", "tagtools", "tidyr", "purrr", "stringr", "ggplot2", "patchwork"))
    ```

3. Place the required CSV files in the `data/` directory:
    - `mn09_203a.csv`
    - `log_mn09_203a.csv`

4. Create the `output/event_composite_dashboards/` directory if it does not exist:
    ```{bash}
    mkdir -p output/event_composite_dashboards
    ```

## Running the Script

1. Open R or RStudio.
2. Set the working directory to the project folder:
    ```{r}
    setwd("~/ABC-Project")
    ```
3. Run the script:
    ```{r}
    source("scripts/create_composite_dashboards.R")
    ```

The output files will be saved in the `output/event_composite_dashboards/` directory.

## .gitignore

Include the following in your `.gitignore` to ignore the output files and other unnecessary files:
```
output/
*.RData
*.Rhistory
```
This ensures that the generated output files and temporary files created by R are not included in the Git repository, keeping the repository clean and focused on the source code and data needed to reproduce the results.
