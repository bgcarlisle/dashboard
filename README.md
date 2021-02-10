# Responsible Metrics Dashboard

Code repo including the code to calculate the underlying metrics as well as the Shiny app to present the results.

Forked with many thanks from https://github.com/quest-bih/dashboard

## Repo overview

Put all the scripts that are directly involved in preparing data for use in the dashboard in the `prep/` folder. Please indicate what scripts/software/data sources are used to generate the inputs for these in a comment in the code itself.

Put the final output data of these scripts in the `shiny_app/data/` folder. These should only include the data that the dashboard Shiny app uses directly to generate plots.

The `shiny_app/` folder contains the code that is run to generate the Shiny app.

## Data preparation pipeline

### 1. Original data set based on Web of Science and Dimensions search

The following columns came from a search of Web of Science and Dimensions

| Column                 | Type        | Details                                |
|------------------------|-------------|----------------------------------------|
| `doi`                  | string      | Digital Object Identifier              |
| `city`                 | string      | The university medical centre (UMC)    |
| `year_published`       | numeric     | Year of publication                    |
| `early_access`         | numeric     |                                        |
| `year_searched`        | numeric     |                                        |
| `email`                | string      | Corresponding author email             |
| `corr_author`          | string      | Corresponding author                   |
| `authors`              | string      | Semicolon delimited author list        |
| `affiliations`         | string      | Author affiliations                    |
| `type`                 | string      | Article type                           |
| `pmid_wos`             | numeric     | Pubmed ID (Web of Science)             |
| `wos_categories`       | string      |                                        |
| `language`             | string      | Language of article (Web of Science)   |
| `category_for`         | json        |                                        |
| `mesh_terms`           | json        |                                        |
| `pmid_dimensions`      | numeric     | Pubmed ID (Dimensions)                 |
| `is_for_match`         | boolean     |                                        |
| `is_wos_match`         | boolean     |                                        |
| `is_multidisciplinary` | boolean     |                                        |
| `is_book`              | boolean     |                                        |
| `is_retracted`         | boolean     |                                        |
| `approach`             | categorical | One of three approaches used to id UMC |
| `specificity_any_TF`   | numeric     |                                        |
| `specificity_any_TFU`  | numeric     |                                        |

### 2. Open access information

The data set from (1.) is joined by `pmid_dimensions` with open access information to provide the following additional columns

| Column      | Type        | Details                              |
|-------------|-------------|--------------------------------------|
| `color`     | categorical | Type of Open Access provided         |
| `issn`      | string      | International Standard Serial Number |
| `journal`   | string      | Name of journal                      |
| `publisher` | string      | Name of publisher                    |
| `date`      | date        |                                      |

### 3. Trial registry number (TRN) reporting

The data set from (2.) is joined by `doi` with TRN reporting information to provide the following additional columns

| Column           | Type    | Details                                       |
|------------------|---------|-----------------------------------------------|
| `is_human_ct`    | boolean | Is this a clinical trial                      |
| `abs_trn_1`      | string  | TRN found in abstract (1)                     |
| `abs_trn_2`      | string  | TRN found in abstract (2)                     |
| `abs_trn_3`      | string  | TRN found in abstract (3)                     |
| `abs_trn_4`      | string  | TRN found in abstract (4)                     |
| `abs_trn_5`      | string  | TRN found in abstract (5)                     |
| `abs_trn_6`      | string  | TRN found in abstract (6)                     |
| `abs_registry_1` | string  | Registry corresponding to TRN in abstract (1) |
| `abs_registry_2` | string  | Registry corresponding to TRN in abstract (2) |
| `abs_registry_3` | string  | Registry corresponding to TRN in abstract (3) |
| `abs_registry_4` | string  | Registry corresponding to TRN in abstract (4) |
| `abs_registry_5` | string  | Registry corresponding to TRN in abstract (5) |
| `abs_registry_6` | string  | Registry corresponding to TRN in abstract (6) |
| `si_trn_1`       | string  | TRN found in secondary information (1)        |
| `si_trn_2`       | string  | TRN found in secondary information (2)        |
| `si_trn_3`       | string  | TRN found in secondary information (3)        |
| `si_trn_4`       | string  | TRN found in secondary information (4)        |
| `si_registry_1`  | string  | Registry corresponding to TRN in SI (1)       |
| `si_registry_2`  | string  | Registry corresponding to TRN in SI (2)       |
| `si_registry_3`  | string  | Registry corresponding to TRN in SI (3)       |
| `si_registry_4`  | string  | Registry corresponding to TRN in SI (4)       |

### 4. Animal research

All unique values from `pmid_dimensions` column from (3.) are tested for whether they would appear in a search for animal research developed by [Hooijmans et al (2010)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3104815/), using https://codeberg.org/bgcarlisle/PubmedIntersectionCheck

The resulting data set is joined by `pmid_dimensions` to the data set from (3.) to provide the following additional column (see [merge-animals-and-sciscore.R](prep/merge-animals-and-sciscore.R) for details)

| Column      | Type    | Details                                            |
|-------------|---------|----------------------------------------------------|
| `is_animal` | boolean | Does this paper reflect experimentation on animals |

### 5. Robustness metrics

The data set from (4.) is joined by the `pmid_dimensions` column with robustness metrics provided by Sciscore to provide the following additional columns (see [merge-animals-and-sciscore.R](prep/merge-animals-and-sciscore.R) for details)

| Column                     | Type    | Details                                       |
|----------------------------|---------|-----------------------------------------------|
| `sciscore`                 | numeric | Summary robustness score provided by Sciscore |
| `iacuc`                    | boolean | Presence of an IACUC statement                |
| `irb`                      | boolean | Presence of an IRB statement                  |
| `sex`                      | boolean | Reporting of sex of research subjects         |
| `blinding`                 | boolean | Reporting of blinding                         |
| `randomization`            | boolean | Reporting of randomization                    |
| `power`                    | boolean | Presence of a power calculation               |
| `cell_line_auth`           |         |                                               |
| `antibody_detected`        |         |                                               |
| `antibody_with_rrid`       |         |                                               |
| `antibody_rrid_suggested`  |         |                                               |
| `organism_detected`        |         |                                               |
| `organism_with_rrid`       |         |                                               |
| `organism_rrid_suggested`  |         |                                               |
| `tool_detected`            |         |                                               |
| `tool_with_rrid`           |         |                                               |
| `tool_rrid_suggested`      |         |                                               |
| `cell_line_detected`       |         |                                               |
| `cell_line_with_rrid`      |         |                                               |
| `cell_line_rrid_suggested` |         |                                               |
| `cell_line_contaminated`   |         |                                               |
| `plasmid_detected`         |         |                                               |
| `plasmid_with_rrid`        |         |                                               |
| `plasmid_rrid_suggested`   |         |                                               |

### 6. Open code and open data

The data set from (5.) is joined by `doi`, `city` and `year_published` to provide the following additional columns

| Column                 | Type        | Description                       |
|------------------------|-------------|-----------------------------------|
| `success`              | boolean     |                                   |
| `origin`               | categorical |                                   |
| `article`              | string      | DOI                               |
| `is_open_data`         | boolean     | Whether the article has open data |
| `open_data_category`   | categorical | Location of open data             |
| `is_open_code`         | boolean     | Whether the article has open code |
| `open_data_statements` | string      | Extracted open data statement     |
| `open_code_statements` | string      | Extracted open code statement     |
