library(tidyverse)

## WARNING:

## This will write over the CSV's

umcs <- tribble(
    ~umc, ~city,
    "Berlin", "berlin",
    "Bochum", "bochum",
    "Cologne", "cologne",
    "Duisberg-Essen", "duisburg-essen",
    "Erlangen", "erlangen",
    "Hamburg", "hamburg",
    "Hannover", "hannover",
    "Leipzig", "leipzig",
    "Magdeburg", "magdeburg",
    "Oldenburg", "oldenburg",
    "Rostock", "rostock",
    "Witten", "witten",
    "Wurzburg", "wurzburg"    
)

## Open data

od <- read_csv(
    "data/2021-01-26_pp-dataset-oa-od.csv"
)

od %>%
    left_join(umcs) %>%
    mutate(city = NULL) %>%
    rename(city = umc) %>%
    relocate(city, .after = doi) %>%
    write_csv("data/2021-01-26_pp-dataset-oa-od.csv")

## Sciscore

sci <- read_csv(
    "data/2021-01-31_pop_with_oa_trn_sciscore.csv",
    col_types = "ccdddcccccdcccdllllllcddccccDlccccccccccccccccccccdddddddddddddddddddddddd"
)

sci %>%
    left_join(umcs) %>%
    mutate(city = NULL) %>%
    rename(city = umc) %>%
    relocate(city, .after = doi) %>%
    write_csv("data/2021-01-31_pop_with_oa_trn_sciscore.csv")

## IV2

iv <- read_csv(
    "data/2021-02-03-IntoValue2.csv"
)

iv %>%
    left_join(umcs) %>%
    mutate(city = NULL) %>%
    rename(city = umc) %>%
    relocate(city, .after = id) %>%
    write_csv("data/2021-02-03-IntoValue2.csv")

## EUTT

eutt <- read_csv(
    "data/2021-02-03-eutt-pop-umcs.csv"
)

eutt %>%
    left_join(umcs) %>%
    mutate(city = NULL) %>%
    rename(city = umc) %>%
    relocate(city, .after = completion_date) %>%
    write_csv("data/2021-02-03-eutt-pop-umcs.csv")

