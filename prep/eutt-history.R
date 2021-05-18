## Clone the repo
## $ git clone https://github.com/ebmdatalab/euctr-tracker-data.git

## Enter that directory
## $ cd euctr-tracker-data

## Get the commits like this

## $ echo "hash,date" > commits.csv
## $ git log | grep -e '^commit\|^Date' | sed -e ':a' -e 'N' -e '$!ba' -e 's/\nDate:   /,/g' | sed 's/commit //g' >> commits.csv

## Copy commits.csv to this folder

library(tidyverse)
library(tidyjson)

commits <- read_csv("commits.csv")

# Format the date
commits$monthday <- commits$date %>%
    substr(5, 10)
commits$year <- commits$date %>%
    substr(nchar(commits$date)-9, nchar(commits$date)-6)
commits$ymd <- paste(commits$year, commits$monthday)
commits$date <- commits$ymd %>%
    as.Date(format="%Y %b %d")
commits <- commits %>%
    select(hash, date) %>%
    filter(date > Sys.Date()-365*2) #adapt as necessary

output_filename <- paste0(Sys.Date(), "-eutt-history.csv")

# Select sponsors you want to get the data from in EUTT
sponsors_of_interest <- read_csv("eutt-sponsors-of-interest.csv")

if (!file.exists(output_filename)) {
    tribble(
        ~city, ~percent_reported, ~hash, ~date
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
            select(city, percent_reported)

        jsondata$hash <- commithash

        jsondata <- jsondata %>%
            left_join(commits)

        jsondata %>%
            write_csv(output_filename, append=TRUE)
        
    }

}

## Note that this script is written such that if it stops partway
## through, you can run the whole script again and it will start where
## it left off
