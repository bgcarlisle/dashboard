library(tidyverse)

iv <- read_csv("2021-06-07-data.csv")

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

## This prints out the names of the remaining UMC's to be identified
iv.remaining <- iv %>%
    filter( is.na(city) ) %>%
    arrange(lead_cities)

iv.remaining$lead_cities %>% unique()

iv$published_2a <- (iv$days_pcd_to_publication < 365*2 & ! is.na(iv$days_pcd_to_publication)) |
    (iv$days_pcd_to_summary < 365*2 & ! is.na (iv$days_pcd_to_summary))
iv$published_2a_sum <- iv$days_pcd_to_summary < 365*2 & ! is.na (iv$days_pcd_to_summary)
iv$published_2a_pub <- iv$days_pcd_to_publication < 365*2 & ! is.na(iv$days_pcd_to_publication)

iv$published_5a <- iv$days_pcd_to_publication < 365*5 & ! is.na(iv$days_pcd_to_publication) |
    (iv$days_pcd_to_summary < 365*5 & ! is.na (iv$days_pcd_to_summary))
iv$published_5a_sum <- iv$days_pcd_to_summary < 365*5 & ! is.na (iv$days_pcd_to_summary)
iv$published_5a_pub <- iv$days_pcd_to_publication < 365*5 & ! is.na(iv$days_pcd_to_publication)

## This writes the final CSV out

iv <- iv %>%
    filter( ! is.na(city) ) %>%
    filter(has_german_umc_lead) %>%
    filter(! is_dupe) %>%
    select(id, city, completion_date, is_prospective, published_2a, published_2a_sum, published_2a_pub, published_5a, published_5a_sum, published_5a_pub, has_iv_trn_abstract, has_iv_trn_ft_pdf, color, color_green_only, permission_postprint, has_reg_pub_link)

iv %>%
    write_csv("2021-06-18-data-iv.csv")
