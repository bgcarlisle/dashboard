library(tidyverse)
library(DBI)

### Read in the data set that has OA and TRN columns
original <- read_csv(
    "~/Downloads/cleaned_data/2021-01-26_pp-dataset-oa-trn-sciscore-od.csv",
    col_types="ccdddcccccdccccdlllllcddccccDlccccccccccccccccccccddddddddddddddddddddddddlcclclcc"
    ## You may have to manually specify the column types
)

original %>%
    group_by(pmid_dimensions) %>%
    slice_head() %>%
    rename(pmid = pmid_dimensions) %>%
    select(pmid) %>%
    write_csv("pmids.csv")

### Generate the following CSV using the CSV above and the R script at the following address:
### https://codeberg.org/bgcarlisle/PubmedIntersectionCheck

### Use the Pubmed search string from:
### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3104815/

animals <- read_csv("checked-pmids-animal.csv")

joined <- original %>%
    left_join(animals, by=c("pmid_dimensions" = "pmid")) %>%
    rename(is_animal = found)

joined_filename <- Sys.time() %>%
    str_replace_all(":", "-") %>%
    str_replace_all(" ", "_") %>%
    paste0("-joined.csv")

joined %>%
    write_csv(joined_filename)

### Now join with Sciscore data

con <- dbConnect(RSQLite::SQLite(), "~/Academic/2020-12-01-robustness/2019-11-17-sciscore_pmcoai_all2.db")

sciscore_reports <- con %>%
    dbReadTable("sciscore_reports")

original$pmid_dimensions <- original$pmid_dimensions %>%
    as.character

joinedwithsciscore <- original %>%
    left_join(sciscore_reports, by=c("pmid_dimensions" = "pmid"))

joinedwithsciscore_filename <- Sys.time() %>%
    str_replace_all(":", "-") %>%
    str_replace_all(" ", "_") %>%
    paste0("-joinedwithsciscore.csv")

joinedwithsciscore %>%
    write_csv(joinedwithsciscore_filename)
