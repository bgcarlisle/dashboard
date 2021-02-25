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
    "Würzburg", "wurzburg"    
)

## Main data file

od <- read_csv(
    "2021-01-26_pp-dataset-oa-trn-sciscore-od-animals-before-caps.csv",
    col_types="ccdddcccccdccccdlllllcddccccDlccccccccccccccccccccddddddddddddddddddddddddlcclclccd"
)

od %>%
    left_join(umcs) %>%
    mutate(city = NULL) %>%
    rename(city = umc) %>%
    relocate(city, .after = doi) %>%
    write_csv("2021-01-26_pp-dataset-oa-trn-sciscore-od-animals.csv")

## IV1-2

iv <- read_csv(
    "2021-02-25-IntoValue1-2.csv"
)

iv %>%
    left_join(umcs) %>%
    mutate(city = NULL) %>%
    rename(city = umc) %>%
    relocate(city, .after = id) %>%
    write_csv("2021-02-25-IntoValue1-2-umc.csv")

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

