## Clone the repo
## $ git clone https://github.com/ebmdatalab/euctr-tracker-data.git

## Enter that directory
## $ cd euctr-tracker-data

## Get the commits like this

## $ echo "hash,date" > commits.csv
## $ git log | grep -e '^commit\|^Date' | sed ':a;N;$!ba;s/\nDate:   /,/g' | sed 's/commit //g' >> commits.csv

## Copy commits.csv to this folder

library(tidyverse)
library(tidyjson)

commits <- read_csv("commits.csv")
output_filename <- "output.csv"

sponsors_of_interest <- tribble(
    ~sponsor_name,                              ~city,
    "Charité-Universitätsmedizin Berlin",       "Berlin",
    "Ruhr University Bochum",                   "Bochum",
    "University Duisburg-Essen",                "Duisberg-Essen",
    "University Erlangen-Nuremberg",            "Erlangen",
    "University of Hamburg",                    "Hamburg",
    "Hannover Medical School",                  "Hannover",
    "Leipzig University",                       "Leipzig",
    "Otto von Guericke University Magdeburg",   "Magdeburg",
    "Universität Rostock",                      "Rostock",
    "Julius Maximilian University of Würzburg", "Würzburg"    
)

if (!file.exists(output_filename)) {
    tribble(
        ~city, ~percent_unreported, ~hash, ~date
    ) %>%
        write_csv(output_filename, col_names=TRUE)
}

for (commithash in commits$hash) {

    alreadydone <- read_csv(output_filename)

    if (! commithash %in% alreadydone$hash) {

        url <- paste0("https://github.com/ebmdatalab/euctr-tracker-data/raw/", commithash, "/all_sponsors.json")

        temp <- tempfile()

        download.file(url, temp)

        jsondata <- read_file(temp)

        unlink(temp)

        ## This is because tidyjson has a hard time with NaN's
        jsondata <- str_replace_all(jsondata, "NaN", "null")

        jsondata <- jsondata %>%
            as.tbl_json %>%
            gather_array %>%
            spread_all() %>%
            tibble() %>%
            filter(total_due > 0)

        jsondata <- jsondata %>%
            left_join(sponsors_of_interest) %>%
            filter(! is.na (city)) %>%
            select(city, percent_unreported)

        jsondata$hash <- commithash

        jsondata <- jsondata %>%
            left_join(commits)

        jsondata %>%
            write_csv(output_filename, append=TRUE)
        
    }

}

