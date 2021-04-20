library(tidyverse)

iv <- readRDS("2021-02-24-intovalue-enhanced-pmid.rds")
rm_data <- read_csv(
    "2021-01-26_pp-dataset-oa-trn-sciscore-od-animals.csv",
    col_types="ccdddcccccdccccdlllllcddccccDlccccccccccccccccccccddddddddddddddddddddddddlcclclccd"
    ## Need to specify column types here because read_csv
    ## only looks at the first few rows to determine type
    ## automatically, and if they're all empty, assumes
    ## that they're logical. This is a problem when it
    ## gets right to the end of the TRN columns and finds
    ## an NCT number there and kicks back a warning.

    ## NOTE: IF WE EVER ADD MORE COLUMNS, THE COLUMN TYPE
    ## SPECIFICATION WILL NEED TO BE UPDATED MANUALLY
)

## This is the library of transformations
transforms <- read_csv("intovalue-city-transforms.csv")

## This is just to check that it worked
## Count up how many rows there should be
iv$cities <- strsplit(as.character(iv$lead_cities), " ")
iv$cities %>%
    lengths() %>%
    sum()
iv$cities <- NULL ## Get rid of this column so we're not confused later

## Split up the data frame into rows based on the cities
iv <- iv %>%
    mutate (lead_cities = strsplit(as.character(lead_cities), " ")) %>%
    unnest(lead_cities)

## This prints out the unique cities
iv$lead_cities %>%
    unique() %>%
    tibble() %>%
    rename(city = '.') %>%
    arrange(city) %>%
    print(n=50)

## This will apply the transformations
iv <- iv %>%
    left_join(transforms)

# How many are left to do?
iv$city %>%
    is.na() %>%
    sum()

rm_data$city %>%
    unique()

## This prints out the names of the remaining UMC's to be identified
iv.remaining <- iv %>%
    filter( is.na(city) ) %>%
    arrange(lead_cities)

iv.remaining$lead_cities %>% unique()

iv$preregistered <- iv$days_reg_to_start > 0
iv$published_2a <- (iv$days_reg_to_publ < 365*2 & ! is.na(iv$days_reg_to_publ)) |
    (iv$days_to_summary < 365*2 & ! is.na (iv$days_to_summary))
iv$published_5a <- iv$days_reg_to_publ < 365*5 & ! is.na(iv$days_reg_to_publ) |
    (iv$days_to_summary < 365*5 & ! is.na (iv$days_to_summary))

## This writes the final CSV out

iv <- iv %>%
    filter( ! is.na(city) ) %>%
    filter(has_german_umc_lead) %>%
    filter(! is_dupe) %>%
    select(id, city, completion_date, preregistered, published_2a, published_5a)

iv %>%
    write_csv("2021-04-20-IntoValue1-2.csv")
