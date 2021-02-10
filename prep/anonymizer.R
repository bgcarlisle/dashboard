library(tidyverse)

## WARNING:
## This will over-write the CSV's

## UMC anonymization
anon <- tribble(
    ~city, ~anon,
    "berlin", "UMC 8",
    "bochum", "UMC 10",
    "cologne", "UMC 3",
    "duisburg-essen", "UMC 4",
    "erlangen", "UMC 13",
    "hamburg", "UMC 6",
    "hannover", "UMC 7",
    "leipzig", "UMC 1",
    "magdeburg", "UMC 9",
    "oldenburg", "UMC 2",
    "rostock", "UMC 11",
    "witten", "UMC 12",
    "wurzburg", "UMC 5"
)

## Open data

od <- read_csv(
    "2021-01-26_pp-dataset-oa-od.csv"
)

od %>%
    left_join(anon) %>%
    mutate(city = NULL) %>%
    rename(city = anon) %>%
    relocate(city, .after = doi) %>%
    write_csv("2021-01-26_pp-dataset-oa-od.csv")

## Sciscore

sci <- read_csv(
    "2021-01-31_pop_with_oa_trn_sciscore.csv",
    col_types = "ccdddcccccdcccdllllllcddccccDlccccccccccccccccccccdddddddddddddddddddddddd"
)

sci %>%
    left_join(anon) %>%
    mutate(city = NULL) %>%
    rename(city = anon) %>%
    relocate(city, .after = doi) %>%
    write_csv("2021-01-31_pop_with_oa_trn_sciscore.csv")

## IV2

iv <- read_csv(
    "2021-02-03-IntoValue2.csv"
)

iv %>%
    left_join(anon) %>%
    mutate(city = NULL) %>%
    rename(city = anon) %>%
    relocate(city, .after = id) %>%
    write_csv("2021-02-03-IntoValue2.csv")

## EUTT

eutt <- read_csv(
    "2021-02-03-eutt-pop-umcs.csv"
)

eutt %>%
    left_join(anon) %>%
    mutate(city = NULL) %>%
    rename(city = anon) %>%
    relocate(city, .after = completion_date) %>%
    write_csv("2021-02-03-eutt-pop-umcs.csv")
