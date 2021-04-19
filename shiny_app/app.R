library(plotly)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggvis)
library(ggplot2)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(DT)

## Load data
rm_data <- read_csv(
    "data/2021-03-01_pp-dataset-oa-trn-sciscore-od-animals-permissions-greenoa.csv",
    col_types="ccdddcccccdccccdlllllcddlccccccccccccccccccccddddddddddddddddddddddddlcclclccdccccDlclclllccccdcDlllc"
    ## Need to specify column types here because read_csv
    ## only looks at the first few rows to determine type
    ## automatically, and if they're all empty, assumes
    ## that they're logical. This is a problem when it
    ## gets right to the end of the TRN columns and finds
    ## an NCT number there and kicks back a warning.

    ## NOTE: IF WE EVER ADD MORE COLUMNS, THE COLUMN TYPE
    ## SPECIFICATION WILL NEED TO BE UPDATED MANUALLY
)

## WARNING
## Okay not really a warning
## But kinda
## So this part here will filter out everything but Articles.
## This is because all the plots here assume that we're talking
## about articles only. Things might change if the base data
## set contains Reviews or whatever. We filtered these out
## before uploading the CSV, so if you don't do that extra step
## next time, this will catch you, but if you remove the
## following filter, you'll need to make sure that all your
## plots still  make sense.
rm_data <- rm_data %>%
    filter(
        type == "Article" |
        type == "Article; Data Paper" |
        type == "Article; Proceedings Paper"
    )

eutt_data <- read_csv(
    "data/2021-02-03-eutt-pop-umcs.csv"
    ## This file is data that I copied and pasted directly from
    ## http://eu.trialstracker.net/ into LibreOffice Calc.
)

eutt_hist <- read_csv(
    "data/2021-04-16-eutt-history.csv"
    ## Generate this from the EUTT repo using the script in
    ## prep/eutt-history.R
)

iv_data <- read_csv(
    "data/2021-04-19-IntoValue1-2.csv"
    # This is the IntoValue2 data set.
)

## Generates the UMC list for the drop-down menu
ddumcs <- iv_data %>%
    select(city) %>%
    arrange(city)

umclist <- c(
    "All",
    ddumcs$city %>% unique()
)

## Load functions
source("ui_elements.R")
source("start_page_plots.R")
source("all_umc_plots.R")

## Load pages
source("start_page.R")
source("all_umcs_page.R")
source("methods_page.R")
## source("report_card_page.R")
source("datasets_page.R")
source("about_rm.R")

## Define UI
ui <- tagList(
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    navbarPage(
        "Open science in clinical research", theme = shinytheme("flatly"), id = "navbarTabs",
        start_page,
        all_umcs_page,
        ## report_card_page,
        methods_page,
        datasets_page,
        about_rm_page,
        tags$head
        (
            tags$script
            ('
                        var width = 0;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        '
            )
        )
    )
)

## Define server function
server <- function (input, output, session) {

    ## Define button actions

    observeEvent(
        input$buttonAllUMCs, {
            updateTabsetPanel(
                session, "navbarTabs",
                selected = "tabAllUMCs"
            )
        }
    )
    
    observeEvent(
        input$buttonMethods, {
            updateTabsetPanel(
                session, "navbarTabs",
                selected = "tabMethods"
            )
        }
    )

    observeEvent(
        input$buttonDatasets, {
            updateTabsetPanel(
                session, "navbarTabs",
                selected = "tabDatasets"
            )
        }
    )

    ## Dynamically generate options for UMC drop-down menu

    output$startpage <- renderUI({

        wellPanel(
            br(),
            fluidRow(
                column(
                    8,
                    h1(style = "margin-left:0cm", strong("Dashboard for open science in clinical research"), align = "left"),
                    h4(style = "margin-left:0cm",
                       "This proof-of-principle dashboard provides an overview of several metrics of open and robust
                       research for several German University Medical Centres (UMCs). This dashboard is a pilot
                       that is still under development, and should not be used to compare UMCs or inform policy.
                       More metrics may be added in the future."),
                    h4(style = "margin-left:0cm",
                       "The dashboard includes data of UMCs for which publications could be identified with a
                       precision equal to or higher than 85%. The data displayed is based on a random sample of
                       500 articles per UMC. An example UMC is highlighted. Besides the metrics
                       Summary Results Reporting, Prospective Registration, and Timely Publication, all other
                       metrics are based on publications from 2018. For the Open Science and Robustness metrics,
                       the data can be viewed as 1) the percentage of analyzable publications which display the
                       given metric; 2) the absolute number of eligible publications which display the given
                       metric. For each metric, you can find an overview of the methods and limitations by clicking
                       on the relevant symbols. For more detailed information on the methods and underlying datasets
                       used to calculate those metrics, visit the Methods or Datasets pages."),
                    br()
                ),
                column(
                    4,
                    hr(),
                    br(),
                    br(),
                    actionButton(
                        style = "color: white; background-color: #aa1c7d;",
                        'buttonAllUMCs',
                        'See all UMCs'
                    ),
                    actionButton(
                        style = "color: white; background-color: #aa1c7d;",
                        'buttonMethods',
                        'See methods'
                    ),
                    actionButton(
                        style = "color: white; background-color: #aa1c7d;",
                        'buttonDatasets',
                        'See datasets'
                    ),
                    br()
                )
            ),
            fluidRow(
                column(
                    4,
                    br(),
                    br(),
                    selectInput(
                        "selectUMC",
                        strong("Choose UMC"),
                        choices = umclist,
                        selected = NA
                    )
                )
            )
        )
        
    })

    ## Dynamically determine column width for displayed metrics
    ## at program start; four columns if resolution large enough,
    ## otherwise two columns.

    ## output$robustness_metrics <- renderUI({

    ##     req(input$width)
    ##     req(input$selectUMC)

    ##     if (input$width < 1400) {
    ##         col_width <- 6
    ##         alignment <- "left"
    ##     } else {
    ##         col_width <- 3
    ##         alignment <- "right"
    ##     }

    ##     ## Value for randomization

    ##     if (input$selectUMC == "All") {

    ##         all_numer_rando <- rm_data %>%
    ##             filter(
    ##                 is_animal == 1,
    ##                 language == "English",
    ##                 ! is.na(sciscore),
    ##                 type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
    ##             ) %>%
    ##             select(randomization) %>%
    ##             sum(na.rm=TRUE)
            
    ##     } else {

    ##         all_numer_rando <- rm_data %>%
    ##             filter(city == input$selectUMC) %>%
    ##             filter(
    ##                 is_animal == 1,
    ##                 language == "English",
    ##                 ! is.na(sciscore),
    ##                 type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
    ##             ) %>%
    ##             select(randomization) %>%
    ##             sum(na.rm=TRUE)
            
    ##     }

    ##     ## Value for Blinding

    ##     if (input$selectUMC == "All") {

    ##     all_numer_blinded <- rm_data %>%
    ##         filter(
    ##             is_animal == 1,
    ##             language == "English",
    ##             ! is.na(sciscore),
    ##             type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
    ##         ) %>%
    ##         select(blinding) %>%
    ##         sum(na.rm=TRUE)
            
    ##     } else {

    ##     all_numer_blinded <- rm_data %>%
    ##         filter(city == input$selectUMC) %>%
    ##         filter(
    ##             is_animal == 1,
    ##             language == "English",
    ##             ! is.na(sciscore),
    ##             type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
    ##         ) %>%
    ##         select(blinding) %>%
    ##         sum(na.rm=TRUE)
            
    ##     }

    ##     ## Value for Power calc

    ##     if (input$selectUMC == "All") {

    ##         all_numer_power <- rm_data %>%
    ##             filter(
    ##                 is_animal == 1,
    ##                 language == "English",
    ##                 ! is.na(sciscore),
    ##                 type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
    ##             ) %>%
    ##             select(power) %>%
    ##             sum(na.rm=TRUE)
            
    ##     } else {

    ##         all_numer_power <- rm_data %>%
    ##             filter(city == input$selectUMC) %>%
    ##             filter(
    ##                 is_animal == 1,
    ##                 language == "English",
    ##                 ! is.na(sciscore),
    ##                 type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
    ##             ) %>%
    ##             select(power) %>%
    ##             sum(na.rm=TRUE)
            
    ##     }

    ##     ## all_numer_iacuc <- rm_data %>%
    ##     ##     filter(
    ##     ##         is_animal == 1,
    ##     ##         ! is.na(sciscore),
    ##     ##         type == "Article"
    ##     ##     ) %>%
    ##     ##     select(iacuc) %>%
    ##     ##     sum(na.rm=TRUE)

    ##     if (input$selectUMC == "All") {

    ##         all_denom_animal_sciscore <- rm_data %>%
    ##             filter(
    ##                 is_animal == 1,
    ##                 language == "English",
    ##                 ! is.na(sciscore),
    ##                 type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
    ##             ) %>%
    ##             nrow()
            
    ##     } else {

    ##         all_denom_animal_sciscore <- rm_data %>%
    ##             filter(city == input$selectUMC) %>%
    ##             filter(
    ##                 is_animal == 1,
    ##                 language == "English",
    ##                 ! is.na(sciscore),
    ##                 type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
    ##             ) %>%
    ##             nrow()
            
    ##     }

    ##     all_percent_randomized <- paste0(round(100*all_numer_rando/all_denom_animal_sciscore), "%")
    ##     all_percent_blinded <- paste0(round(100*all_numer_blinded/all_denom_animal_sciscore), "%")
    ##     all_percent_power <- paste0(round(100*all_numer_power/all_denom_animal_sciscore), "%")
    ##     ## all_percent_iacuc <- paste0(round(100*all_numer_iacuc/all_denom_animal_sciscore), "%")

    ##     wellPanel(
    ##         style = "padding-top: 0px; padding-bottom: 0px;",
    ##         h2(strong("Robustness of Animal Studies"), align = "left"),
    ##         checkboxInput(
    ##             "animals_absnum",
    ##             strong("Show absolute numbers"),
    ##             value = FALSE
    ##         ),
    ##         fluidRow(
    ##             column(
    ##                 col_width,
    ##                 metric_box(
    ##                     title = "Randomization",
    ##                     value = all_percent_randomized,
    ##                     value_text = "of analyzable 2018 animal studies report on randomization",
    ##                     plot = plotlyOutput('plot_randomization', height="300px"),
    ##                     info_id = "infoRandomization",
    ##                     info_title = "Randomization",
    ##                     info_text = randomization_tooltip,
    ##                     lim_id = "limRandomization",
    ##                     lim_title = "Limitations: Randomization",
    ##                     lim_text = lim_randomization_tooltip
    ##                 )
    ##             ),
    ##             column(
    ##                 col_width,
    ##                 metric_box(
    ##                     title = "Blinding",
    ##                     value = all_percent_blinded,
    ##                     value_text = "of analyzable 2018 animal studies report on blinding",
    ##                     plot = plotlyOutput('plot_blinding', height="300px"),
    ##                     info_id = "infoBlinding",
    ##                     info_title = "Blinding",
    ##                     info_text = blinding_tooltip,
    ##                     lim_id = "limBlinding",
    ##                     lim_title = "Limitations: Blinding",
    ##                     lim_text = lim_blinding_tooltip
    ##                 )
    ##             ),
    ##             column(
    ##                 col_width,
    ##                 metric_box(
    ##                     title = "Power calculation",
    ##                     value = all_percent_power,
    ##                     value_text = "of analyzable 2018 animal studies report on power calculation",
    ##                     plot = plotlyOutput('plot_power', height="300px"),
    ##                     info_id = "infoPower",
    ##                     info_title = "Power",
    ##                     info_text = power_tooltip,
    ##                     lim_id = "limPower",
    ##                     lim_title = "Limitations: Power",
    ##                     lim_text = lim_power_tooltip
    ##                 )
    ##             )##,
    ##             ## column(
    ##             ##     col_width,
    ##             ##     metric_box(
    ##             ##         title = "IACUC statement",
    ##             ##         value = all_percent_iacuc,
    ##             ##         value_text = "of animal studies report an IACUC statement",
    ##             ##         plot = plotlyOutput('plot_iacuc', height="300px"),
    ##             ##         info_id = "infoIACUC",
    ##             ##         info_title = "IACUC",
    ##             ##         info_text = iacuc_tooltip
    ##             ##     )
    ##             ## )
    ##         )
    ##     )        
        
    ## })

    output$registry_metrics <- renderUI({

        req(input$width)
        req(input$selectUMC)

        if (input$width < 1400) {
            col_width <- 6
            alignment <- "left"
        } else {
            col_width <- 3
            alignment <- "right"
        }

        ## Value for TRN

        if (input$selectUMC == "All") {

            all_numer_trn <- rm_data %>%
                filter(
                    is_human_ct == 1,
                    ! is.na(abs_trn_1)
                ) %>%
                nrow()
            
            all_denom_trn <- rm_data %>%
                filter(is_human_ct == 1) %>%
                nrow()
            
        } else {

            all_numer_trn <- rm_data %>%
                filter(city == input$selectUMC) %>%
                filter(
                    is_human_ct == 1,
                    ! is.na(abs_trn_1)
                ) %>%
                nrow()
            
            all_denom_trn <- rm_data %>%
                filter(city == input$selectUMC) %>%
                filter(is_human_ct == 1) %>%
                nrow()
            
        }

        ## Value for summary results

        if (input$selectUMC == "All") {
            
            sumres_percent <- eutt_hist %>%
                group_by(date) %>%
                mutate(avg = mean(percent_reported)) %>%
                slice_head() %>%
                ungroup() %>%
                slice_tail() %>%
                select(avg) %>%
                pull()

            n_eutt_records <- eutt_hist %>%
                nrow()
            
        } else {

            sumres_percent <- eutt_hist %>%
                filter(city == input$selectUMC) %>%
                slice_head() %>%
                select(percent_reported) %>%
                pull()

            n_eutt_records <- eutt_hist %>%
                filter(city == input$selectUMC) %>%
                nrow()
            
        }

        if (n_eutt_records == 0) {
            sumresval <- "Not applicable"
            sumresvaltext <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
            sumresval <- paste0(sumres_percent, "%")
            sumresvaltext <- "of due clinical trials report summary results"
        }

        ## Value for prereg

        if (input$selectUMC == "All") {

            iv_data_unique <- iv_data %>%
                distinct(id, .keep_all = TRUE)
            
        } else {

            iv_data_unique <- iv_data %>%
                filter(city == input$selectUMC) %>%
                distinct(id, .keep_all = TRUE)
        }

        all_numer_prereg <- iv_data_unique %>%
            filter(preregistered) %>%
            nrow()

        all_denom_prereg <- iv_data_unique %>%
            nrow()

        if (all_denom_prereg == 0) {
            preregval <- "Not applicable"
            preregvaltext <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
            preregval <- paste0(round(100*all_numer_prereg/all_denom_prereg), "%")
            preregvaltext <- "of registered clinical trials were prospectively registered"
        }

        ## Value for timely pub 2a

        all_numer_timpub <- iv_data_unique %>%
            filter(published_2a) %>%
            nrow()

        all_denom_timpub <- iv_data_unique %>%
            nrow()

        if (all_denom_timpub == 0) {
            timpubval <- "Not applicable"
            timpubvaltext <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
            timpubval <- paste0(round(100*all_numer_timpub/all_denom_timpub), "%")
            timpubvaltext <- "of clinical trials published results within 2 years"
        }


        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Clinical Trial Registry Entries"), align = "left"),
            fluidRow(
                column(
                    col_width,
                    metric_box(
                        title = "Summary Results Reporting",
                        value = sumresval,
                        value_text = sumresvaltext,
                        plot = plotlyOutput('plot_clinicaltrials_sumres', height="300px"),
                        info_id = "infoSumRes",
                        info_title = "Summary Results Reporting",
                        info_text = sumres_tooltip,
                        lim_id = "limSumRes",
                        lim_title = "Limitations: Summary Results Reporting",
                        lim_text = lim_sumres_tooltip
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Prospective registration",
                        value = preregval,
                        value_text = preregvaltext,
                        plot = plotlyOutput('plot_clinicaltrials_prereg', height="300px"),
                        info_id = "infoPreReg",
                        info_title = "Prospective registration",
                        info_text = prereg_tooltip,
                        lim_id = "limPreReg",
                        lim_title = "Limitations: Prospective registration",
                        lim_text = lim_prereg_tooltip
                    )
                )
                
            )

        )

        
    })

    output$publication_metrics <- renderUI({

        req(input$width)
        req(input$selectUMC)

        if (input$width < 1400) {
            col_width <- 6
            alignment <- "left"
        } else {
            col_width <- 3
            alignment <- "right"
        }

        ## Value for TRN

        if (input$selectUMC == "All") {

            all_numer_trn <- rm_data %>%
                filter(
                    is_human_ct == 1,
                    ! is.na(abs_trn_1)
                ) %>%
                nrow()
            
            all_denom_trn <- rm_data %>%
                filter(is_human_ct == 1) %>%
                nrow()
            
        } else {

            all_numer_trn <- rm_data %>%
                filter(city == input$selectUMC) %>%
                filter(
                    is_human_ct == 1,
                    ! is.na(abs_trn_1)
                ) %>%
                nrow()
            
            all_denom_trn <- rm_data %>%
                filter(city == input$selectUMC) %>%
                filter(is_human_ct == 1) %>%
                nrow()
            
        }

        ## Value for summary results

        if (input$selectUMC == "All") {
            
            sumres_percent <- eutt_hist %>%
                group_by(date) %>%
                mutate(avg = mean(percent_reported)) %>%
                slice_head() %>%
                ungroup() %>%
                slice_tail() %>%
                select(avg) %>%
                pull()

            n_eutt_records <- eutt_hist %>%
                nrow()
            
        } else {

            sumres_percent <- eutt_hist %>%
                filter(city == input$selectUMC) %>%
                slice_head() %>%
                select(percent_reported) %>%
                pull()

            n_eutt_records <- eutt_hist %>%
                filter(city == input$selectUMC) %>%
                nrow()
            
        }

        if (n_eutt_records == 0) {
            sumresval <- "Not applicable"
            sumresvaltext <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
            sumresval <- paste0(sumres_percent, "%")
            sumresvaltext <- "of due clinical trials report summary results"
        }

        ## Value for prereg

        if (input$selectUMC == "All") {

            iv_data_unique <- iv_data %>%
                distinct(id, .keep_all = TRUE)
            
        } else {

            iv_data_unique <- iv_data %>%
                filter(city == input$selectUMC) %>%
                distinct(id, .keep_all = TRUE)
        }

        all_numer_prereg <- iv_data_unique %>%
            filter(preregistered) %>%
            nrow()

        all_denom_prereg <- iv_data_unique %>%
            nrow()

        if (all_denom_prereg == 0) {
            preregval <- "Not applicable"
            preregvaltext <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
            preregval <- paste0(round(100*all_numer_prereg/all_denom_prereg), "%")
            preregvaltext <- "of registered clinical trials were prospectively registered"
        }

        ## Value for timely pub

        all_numer_timpub <- iv_data_unique %>%
            filter(published_2a) %>%
            nrow()

        all_denom_timpub <- iv_data_unique %>%
            nrow()

        if (all_denom_timpub == 0) {
            timpubval <- "Not applicable"
            timpubvaltext <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
            timpubval <- paste0(round(100*all_numer_timpub/all_denom_timpub), "%")
            timpubvaltext <- "of clinical trials published results within 2 years"
        }

        ## Value for timely pub 5a

        all_numer_timpub5a <- iv_data_unique %>%
            filter(published_5a) %>%
            nrow()

        all_denom_timpub5a <- iv_data_unique %>%
            nrow()

        if (all_denom_timpub5a == 0) {
            timpubval5a <- "Not applicable"
            timpubvaltext5a <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
            timpubval5a <- paste0(round(100*all_numer_timpub/all_denom_timpub), "%")
            timpubvaltext5a <- "of clinical trials published results within 2 years"
        }
        

        if (all_denom_timpub5a == 0) {
            timpubval5a <- "Not applicable"
            timpubvaltext5a <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
            timpubval5a <- paste0(round(100*all_numer_timpub5a/all_denom_timpub5a), "%")
            timpubvaltext5a <- "of clinical trials published results within 2 years"
        }

        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Clinical Trials Reporting in Publications"), align = "left"),
            fluidRow(
                column(
                    col_width,
                    metric_box(
                        title = "Timely publication (2 years)",
                        value = timpubval,
                        value_text = timpubvaltext,
                        plot = plotlyOutput('plot_clinicaltrials_timpub_2a', height="300px"),
                        info_id = "infoTimPub",
                        info_title = "Timely Publication (2 years)",
                        info_text = timpub_tooltip,
                        lim_id = "lim",
                        lim_title = "Limitations: Timely Publication",
                        lim_text = lim_timpub_tooltip
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Publication by 5 years",
                        value = timpubval5a,
                        value_text = timpubvaltext5a,
                        plot = plotlyOutput('plot_clinicaltrials_timpub_5a', height="300px"),
                        info_id = "infoTimPub",
                        info_title = "Publication by 5 years",
                        info_text = timpub_tooltip,
                        lim_id = "lim",
                        lim_title = "Limitations: Timely Publication",
                        lim_text = lim_timpub_tooltip
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Trial Registry Number Reporting",
                        value = paste0(round(100*all_numer_trn/all_denom_trn), "%"),
                        value_text = "of clinical trials reported a registry number in the abstract",
                        plot = plotlyOutput('plot_clinicaltrials_trn', height="300px"),
                        info_id = "infoTRN",
                        info_title = "Trial Registry Number Reporting",
                        info_text = trn_tooltip,
                        lim_id = "limTRN",
                        lim_title = "Limitations: Trial Registry Number Reporting",
                        lim_text = lim_trn_tooltip
                    )
                )
                
            )

        )

        
    })

    output$openscience_metrics <- renderUI({

        req(input$width)
        req(input$selectUMC)

        if (input$width < 1400) {
            col_width <- 6
            alignment <- "left"
        } else {
            col_width <- 3
            alignment <- "right"
        }

        ## Value for Open Access

        if ( input$selectUMC == "All") {
            
            all_numer_oa <- rm_data %>%
                filter(
                    color == "gold" | color == "green" | color == "hybrid"
                    
                ) %>%
                nrow()

            all_denom_oa <- rm_data %>%
                filter(
                    ! is.na(color)
                    
                ) %>%
                nrow()
            
        } else {
            
            all_numer_oa <- rm_data %>%
                filter(city == input$selectUMC) %>%
                filter(
                    color == "gold" | color == "green" | color == "hybrid"
                    
                ) %>%
                nrow()

            all_denom_oa <- rm_data %>%
                filter(city == input$selectUMC) %>%
                filter(
                    ! is.na(color)
                    
                ) %>%
                nrow()
            
        }

        ## Value for Open Data

        ## if ( input$selectUMC == "All") {
            
        ##     all_denom_od <- rm_data %>%
        ##         filter(
        ##             ! is.na (is_open_data),
        ##             language == "English"
        ##         ) %>%
        ##         nrow()

        ##     all_numer_od <- rm_data %>%
        ##         filter(
        ##             is_open_data,
        ##             language == "English"
        ##         ) %>%
        ##         nrow()

        ## } else {
            
        ##     all_denom_od <- rm_data %>%
        ##         filter(city == input$selectUMC) %>%
        ##         filter(
        ##             ! is.na (is_open_data),
        ##             language == "English"
        ##         ) %>%
        ##         nrow()

        ##     all_numer_od <- rm_data %>%
        ##         filter(city == input$selectUMC) %>%
        ##         filter(
        ##             is_open_data,
        ##             language == "English"
        ##         ) %>%
        ##         nrow()

        ## }
        
        ## Value for Open Code

        ## if ( input$selectUMC == "All") {

        ##     all_denom_oc <- rm_data %>%
        ##         filter(
        ##             ! is.na (is_open_code),
        ##             language == "English"
        ##         ) %>%
        ##         nrow()

        ##     all_numer_oc <- rm_data %>%
        ##         filter(
        ##             is_open_code,
        ##             language == "English"
        ##         ) %>%
        ##         nrow()
            
        ## } else {

        ##     all_denom_oc <- rm_data %>%
        ##         filter(city == input$selectUMC) %>%
        ##         filter(
        ##             ! is.na (is_open_code),
        ##             language == "English"
        ##         ) %>%
        ##         nrow()

        ##     all_numer_oc <- rm_data %>%
        ##         filter(city == input$selectUMC) %>%
        ##         filter(
        ##             is_open_code,
        ##             language == "English"
        ##         ) %>%
        ##         nrow()
            
        ## }

        ## Value for Green OA

        # if ( input$selectUMC == "All") {
        # 
        #     denom_greenoa <- rm_data %>%
        #         filter(
        #             color == "closed",
        #             ! is.na(permission_postprint)
        #         ) %>%
        #         nrow()
        # 
        #     numer_greenoa <- rm_data %>%
        #         filter(
        #             color == "closed",
        #             ! is.na(permission_postprint),
        #             permission_postprint == TRUE
        #         ) %>%
        #         nrow()
        # 
        # } else {
        # 
        #     denom_greenoa <- rm_data %>%
        #         filter(
        #             color == "closed",
        #             ! is.na(permission_postprint),
        #             city == input$selectUMC
        #         ) %>%
        #         nrow()
        # 
        #     numer_greenoa <- rm_data %>%
        #         filter(
        #             color == "closed",
        #             ! is.na(permission_postprint),
        #             permission_postprint == TRUE,
        #             city == input$selectUMC
        #         ) %>%
        #         nrow()
        # }
        
        if ( input$selectUMC == "All") {
            
            closed_with_potential <- rm_data %>%
                filter(
                    color_green_only == "closed",
                    ! is.na(permission_postprint),
                    permission_postprint == TRUE
                ) %>%
                nrow()
            
            greenoa_only <- rm_data %>%
                filter(
                    color_green_only == "green",
                ) %>%
                nrow()
            
            denom_greenoa <- closed_with_potential + greenoa_only
            
            numer_greenoa <- greenoa_only
            
            
        } else {
            
            closed_with_potential <- rm_data %>%
                filter(
                    color_green_only == "closed",
                    ! is.na(permission_postprint),
                    permission_postprint == TRUE,
                    city == input$selectUMC
                ) %>%
                nrow()
            
            greenoa_only <- rm_data %>%
                filter(
                    color_green_only == "green",
                    city == input$selectUMC
                ) %>%
                nrow()
            
            denom_greenoa <- closed_with_potential + greenoa_only
            
            numer_greenoa <- greenoa_only
        }
        
        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Open Science"), align = "left"),
            checkboxInput(
                "opensci_absnum",
                strong("Show absolute numbers"),
                value = FALSE
            ),
            fluidRow(
                column(
                    col_width,
                    metric_box(
                        title = "Open Access (OA)",
                        value = paste0(round(100*all_numer_oa/all_denom_oa), "%"),
                        value_text = "of 2018 publications are Open Access",
                        plot = plotlyOutput('plot_opensci_oa', height="300px"),
                        info_id = "infoOpenAccess",
                        info_title = "Open Access",
                        info_text = openaccess_tooltip,
                        lim_id = "limOpenAccess",
                        lim_title = "Limitations: Open Access",
                        lim_text = lim_openaccess_tooltip
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Potential Green OA",
                        value = paste0(round(100*numer_greenoa/denom_greenoa), "%"),
                        value_text = "of publications otherwise behind a paywall are openly accessible via green OA",
                        plot = plotlyOutput('plot_opensci_green_oa', height="300px"),
                        info_id = "infoGreenOA",
                        info_title = "Potential Green Open Access",
                        info_text = greenopenaccess_tooltip,
                        lim_id = "limGreenOA",
                        lim_title = "Limitations: Potential Green Open Access",
                        lim_text = lim_greenopenaccess_tooltip
                    )
                )## ,
                ## column(
                ##     col_width,
                ##     metric_box(
                ##         title = "Any Open Data",
                ##         value = paste0(round(100*all_numer_od/all_denom_od), "%"),
                ##         value_text = "of 2018 analyzable publications mentioned sharing of data",
                ##         plot = plotlyOutput('plot_opensci_od', height="300px"),
                ##         info_id = "infoOpenData",
                ##         info_title = "Any Open Data",
                ##         info_text = opendata_tooltip,
                ##         lim_id = "limOpenData",
                ##         lim_title = "Limitations: Any Open Data",
                ##         lim_text = lim_opendata_tooltip
                ##     )
                ## ),
                ## column(
                ##     col_width,
                ##     metric_box(
                ##         title = "Any Open Code",
                ##         value = paste0(round(100*all_numer_oc/all_denom_oc), "%"),
                ##         value_text = "of 2018 analyzable publications mentioned sharing of code",
                ##         plot = plotlyOutput('plot_opensci_oc', height="300px"),
                ##         info_id = "infoOpenCode",
                ##         info_title = "Any Open Code",
                ##         info_text = opencode_tooltip,
                ##         lim_id = "limOpenCode",
                ##         lim_title = "Limitations: Any Open Code",
                ##         lim_text = lim_opencode_tooltip
                ##     )
                ## )
                
            )
        )

    })

    output$allumc_openscience <- renderUI({

        ## Value for All UMC Open Access
        
        all_numer_oa <- rm_data %>%
            filter(
                color == "gold" | color == "green" | color == "hybrid"
            ) %>%
            nrow()

        all_denom_oa <- rm_data %>%
            filter(
                ! is.na(color)
                
            ) %>%
            nrow()
        
        ## Value for All UMC Open Data

        ## all_denom_od <- rm_data %>%
        ##     filter(
        ##         ! is.na (is_open_data),
        ##         language == "English"
        ##     ) %>%
        ##     nrow()

        ## all_numer_od <- rm_data %>%
        ##     filter(
        ##         is_open_data,
        ##         language == "English"
        ##     ) %>%
        ##     nrow()
        
        ## Value for All UMC Open Code
 
        ## all_denom_oc <- rm_data %>%
        ##     filter(
        ##         ! is.na (is_open_code),
        ##         language == "English"
        ##     ) %>%
        ##     nrow()

        ## all_numer_oc <- rm_data %>%
        ##     filter(
        ##         is_open_code,
        ##         language == "English"
        ##     ) %>%
        ##     nrow()

        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Open Science"), align = "left"),
            fluidRow(
                column(
                    12,
                    metric_box(
                        title = "Open Access",
                        value = paste0(round(100*all_numer_oa/all_denom_oa), "%"),
                        value_text = "of 2018 publications are Open Access",
                        plot = plotlyOutput('plot_allumc_openaccess', height="300px"),
                        info_id = "infoALLUMCOpenAccess",
                        info_title = "Open Access (All UMCs)",
                        info_text = allumc_openaccess_tooltip,
                        lim_id = "limALLUMCOpenAccess",
                        lim_title = "Limitations: Open Access (All UMCs)",
                        lim_text = lim_allumc_openaccess_tooltip
                    )
                )
            ),
            ## fluidRow(
            ##     column(
            ##         12,
            ##         metric_box(
            ##             title = "Any Open Data",
            ##             value = paste0(round(100*all_numer_od/all_denom_od), "%"),
            ##             value_text = "of 2018 analyzable publications mentioned sharing of data",
            ##             plot = plotlyOutput('plot_allumc_opendata', height="300px"),
            ##             info_id = "infoALLUMCOpenData",
            ##             info_title = "Any Open Data (All UMCs)",
            ##             info_text = allumc_opendata_tooltip,
            ##             lim_id = "limALLUMCOpenData",
            ##             lim_title = "Limitations: Any Open Data (All UMCs)",
            ##             lim_text = lim_allumc_opendata_tooltip
            ##         )
            ##     )
            ## ),
            ## fluidRow(
            ##     column(
            ##         12,
            ##         metric_box(
            ##             title = "Any Open Code",
            ##             value = paste0(round(100*all_numer_oc/all_denom_oc), "%"),
            ##             value_text = "of 2018 analyzable publications mentioned sharing of code",
            ##             plot = plotlyOutput('plot_allumc_opencode', height="300px"),
            ##             info_id = "infoALLUMCOpenCode",
            ##             info_title = "Any Open Code (All UMCs)",
            ##             info_text = allumc_opencode_tooltip,
            ##             lim_id = "limALLUMCOpenCode",
            ##             lim_title = "Limitations: Any Open Code (All UMCs)",
            ##             lim_text = lim_allumc_opencode_tooltip
            ##         )
            ##     )
            ## )
            
        )
    })

    output$allumc_clinicaltrials <- renderUI({

        ## Value for All UMC TRN
        
        all_numer_trn <- rm_data %>%
            filter(
                is_human_ct == 1,
                ! is.na(abs_trn_1)
            ) %>%
            nrow()
        
        all_denom_trn <- rm_data %>%
            filter(is_human_ct == 1) %>%
            nrow()

        ## Value for All UMC summary results reporting
           
        all_numer_sumres <- eutt_data %>%
            filter (
                due_or_not == "Due",
                status == "Reported results" |
                status == "Reported results Terminated"
            ) %>%
            nrow()

        all_denom_sumres <- eutt_data %>%
            filter(due_or_not == "Due") %>%
            nrow()

        ## Value for prereg

        iv_data_unique <- iv_data %>%
            distinct(id, .keep_all = TRUE)

        all_numer_prereg <- iv_data_unique %>%
            filter(preregistered) %>%
            nrow()

        all_denom_prereg <- iv_data_unique %>%
            nrow()

        ## Value for timely pub

        all_numer_timpub <- iv_data_unique %>%
            filter(published_2a) %>%
            nrow()

        all_denom_timpub <- iv_data_unique %>%
            nrow()

        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Clinical Trials"), align = "left"),
            fluidRow(
                column(
                    12,
                    metric_box(
                        title = "TRN Reporting",
                        value = paste0(round(100*all_numer_trn/all_denom_trn), "%"),
                        value_text = "of clinical trials reported a TRN in the abstract",
                        plot = plotlyOutput('plot_allumc_clinicaltrials_trn', height="300px"),
                        info_id = "infoALLUMCTRN",
                        info_title = "TRN reporting (All UMCs)",
                        info_text = allumc_clinicaltrials_trn_tooltip,
                        lim_id = "limALLUMCTRN",
                        lim_title = "Limitations: TRN reporting (All UMCs)",
                        lim_text = lim_allumc_clinicaltrials_trn_tooltip
                    )
                )
            ),
            fluidRow(
                column(
                    12,
                    metric_box(
                        title = "Summary Results Reporting",
                        value = paste0(round(100*all_numer_sumres/all_denom_sumres), "%"),
                        value_text = "of due clinical trials report summary results",
                        plot = plotlyOutput('plot_allumc_clinicaltrials_sumres', height="300px"),
                        info_id = "infoALLUMCSumRes",
                        info_title = "Summary results reporting (All UMCs)",
                        info_text = allumc_clinicaltrials_sumres_tooltip,
                        lim_id = "limALLUMCSumRes",
                        lim_title = "Limitations: Summary results reporting (All UMCs)",
                        lim_text = lim_allumc_clinicaltrials_sumres_tooltip
                    )
                )
            ),
            fluidRow(
                column(
                    12,
                    metric_box(
                        title = "Prospective registration",
                        value = paste0(round(100*all_numer_prereg/all_denom_prereg), "%"),
                        value_text = "of registered clinical trials were prospectively registered",
                        plot = plotlyOutput('plot_allumc_clinicaltrials_prereg', height="300px"),
                        info_id = "infoALLUMCPreReg",
                        info_title = "Prospective registration (All UMCs)",
                        info_text = allumc_clinicaltrials_prereg_tooltip,
                        lim_id = "limALLUMCPreReg",
                        lim_title = "Limitations: Prospective registration (All UMCs)",
                        lim_text = lim_allumc_clinicaltrials_prereg_tooltip
                    )
                )
            ),
            fluidRow(
                column(
                    12,
                    metric_box(
                        title = "Timely Publication",
                        value = paste0(round(100*all_numer_timpub/all_denom_timpub), "%"),
                        value_text = "of clinical trials published results within 2 years",
                        plot = plotlyOutput('plot_allumc_clinicaltrials_timpub', height="300px"),
                        info_id = "infoALLUMCTimPub",
                        info_title = "Timely Publication (All UMCs)",
                        info_text = allumc_clinicaltrials_timpub_tooltip,
                        lim_id = "limALLUMCTimPub",
                        lim_title = "Limitations: Timely Publication (All UMCs)",
                        lim_text = lim_allumc_clinicaltrials_timpub_tooltip
                    )
                )
            )
        )
        
    })

    ## output$allumc_robustness <- renderUI({

    ##     ## Values for All UMC Robustness metrics

    ##     all_numer_rando <- rm_data %>%
    ##         filter(
    ##             is_animal == 1,
    ##             ! is.na(sciscore),
    ##             type == "Article"
    ##         ) %>%
    ##         select(randomization) %>%
    ##         sum(na.rm=TRUE)

    ##     all_numer_blinded <- rm_data %>%
    ##         filter(
    ##             is_animal == 1,
    ##             ! is.na(sciscore),
    ##             type == "Article"
    ##         ) %>%
    ##         select(blinding) %>%
    ##         sum(na.rm=TRUE)

    ##     all_numer_power <- rm_data %>%
    ##         filter(
    ##             is_animal == 1,
    ##             ! is.na(sciscore),
    ##             type == "Article"
    ##         ) %>%
    ##         select(power) %>%
    ##         sum(na.rm=TRUE)

    ##     ## all_numer_iacuc <- rm_data %>%
    ##     ##     filter(
    ##     ##         is_animal == 1,
    ##     ##         ! is.na(sciscore),
    ##     ##         type == "Article"
    ##     ##     ) %>%
    ##     ##     select(iacuc) %>%
    ##     ##     sum(na.rm=TRUE)

    ##     all_denom_animal_sciscore <- rm_data %>%
    ##         filter(
    ##             is_animal == 1,
    ##             ! is.na(sciscore),
    ##             type == "Article"
    ##         ) %>%
    ##         nrow()

    ##     all_percent_randomized <- paste0(round(100*all_numer_rando/all_denom_animal_sciscore), "%")
    ##     all_percent_blinded <- paste0(round(100*all_numer_blinded/all_denom_animal_sciscore), "%")
    ##     all_percent_power <- paste0(round(100*all_numer_power/all_denom_animal_sciscore), "%")
    ##     ## all_percent_iacuc <- paste0(round(100*all_numer_iacuc/all_denom_animal_sciscore), "%")

    ##     wellPanel(
    ##         style="padding-top: 0px; padding-bottom: 0px;",
    ##         h2(strong("Robustness of Animal Studies"), align = "left"),
    ##         fluidRow(
    ##             column(
    ##                 12,
    ##                 metric_box(
    ##                     title = "Randomization",
    ##                     value = all_percent_randomized,
    ##                     value_text = "of analyzable 2018 animal studies report on randomization",
    ##                     plot = plotlyOutput('plot_allumc_animal_rando', height="300px"),
    ##                     info_id = "infoAllUMCAnimalRando",
    ##                     info_title = "Randomization",
    ##                     info_text = allumc_animal_rando_tooltip,
    ##                     lim_id = "limAllUMCAnimalRando",
    ##                     lim_title = "Limitations: Randomization",
    ##                     lim_text = lim_allumc_animal_rando_tooltip
    ##                 )
    ##             )
    ##         ),
    ##         fluidRow(
    ##             column(
    ##                 12,
    ##                 metric_box(
    ##                     title = "Blinding",
    ##                     value = all_percent_blinded,
    ##                     value_text = "of analyzable 2018 animal studies report on blinding",
    ##                     plot = plotlyOutput('plot_allumc_animal_blind', height="300px"),
    ##                     info_id = "infoAllUMCAnimalBlind",
    ##                     info_title = "Blinding",
    ##                     info_text = allumc_animal_blind_tooltip,
    ##                     lim_id = "limAllUMCAnimalBlind",
    ##                     lim_title = "Limitations: Blinding",
    ##                     lim_text = lim_allumc_animal_blind_tooltip
    ##                 )
    ##             )
    ##         ),
    ##         fluidRow(
    ##             column(
    ##                 12,
    ##                 metric_box(
    ##                     title = "Power calculation",
    ##                     value = all_percent_power,
    ##                     value_text = "of analyzable 2018 animal studies report on power calculation",
    ##                     plot = plotlyOutput('plot_allumc_animal_power', height="300px"),
    ##                     info_id = "infoAllUMCAnimalPower",
    ##                     info_title = "Power calculation",
    ##                     info_text = allumc_animal_power_tooltip,
    ##                     lim_id = "limAllUMCAnimalPower",
    ##                     lim_title = "Limitations: Power calculation",
    ##                     lim_text = lim_allumc_animal_power_tooltip
    ##                 )
    ##             )
    ##         )##,
    ##         ## fluidRow(
    ##         ##     column(
    ##         ##         12,
    ##         ##         metric_box(
    ##         ##             title = "IACUC statement",
    ##         ##             value = all_percent_iacuc,
    ##         ##             value_text = "of animal studies report an IACUC statement",
    ##         ##             plot = plotlyOutput('plot_allumc_animal_iacuc', height="300px"),
    ##         ##             info_id = "infoAllUMCAnimalIACUC",
    ##         ##             info_title = "IACUC statement",
    ##         ##             info_text = allumc_animal_iacuc_tooltip
    ##         ##         )
    ##         ##     )
    ##         ## )
    ##     )
        
    ## })

    color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A",
                     "#303A3E", "#007265", "#634587", "#000000",   #363457 #533A71 #011638 #634587
                     "#DCE3E5")
    
    color_palette_delwen <- c("#B6B6B6", "#879C9D", "#F1BA50", "#cf9188",  
                              "#303A3E", "#2f4858", "#158376", "#007265", 
                              "#DCE3E5", "#634587", "#000000", "#539d66")

    color_palette_bars <- c("#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587", "#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587")

    ## Start page plots ##
    
    ## Open Access plot
    output$plot_opensci_oa <- renderPlotly({
        return (plot_opensci_oa(rm_data, input$selectUMC, input$opensci_absnum, color_palette_delwen))
    })
    
    ## Open Data plot
    ## output$plot_opensci_od <- renderPlotly({
    ##     return (plot_opensci_od(rm_data, input$selectUMC, input$opensci_absnum, color_palette_delwen))
    ## })
    
    ## Open Code plot
    ## output$plot_opensci_oc <- renderPlotly({
    ##     return (plot_opensci_oc(rm_data, input$selectUMC, input$opensci_absnum, color_palette_delwen))
    ## })
    
    ## Green Open Access plot
    output$plot_opensci_green_oa <- renderPlotly({
        return (plot_opensci_green_oa(rm_data, input$selectUMC, input$opensci_absnum, color_palette_delwen))
    })
    
    ## TRN plot
    output$plot_clinicaltrials_trn <- renderPlotly({
        return (plot_clinicaltrials_trn(rm_data, input$selectUMC, color_palette))
    })
    
    ## Summary results plot
    output$plot_clinicaltrials_sumres <- renderPlotly({
        return (plot_clinicaltrials_sumres(eutt_hist, input$selectUMC, color_palette))
    })
    
    ## Preregistration plot
    output$plot_clinicaltrials_prereg <- renderPlotly({
        return (plot_clinicaltrials_prereg(iv_data, input$selectUMC, color_palette))
    })
    
    ## Timely Publication plot 2a
    output$plot_clinicaltrials_timpub_2a <- renderPlotly({
        return (plot_clinicaltrials_timpub_2a(iv_data, input$selectUMC, color_palette))
    })
    
    ## Timely Publication plot 5a
    output$plot_clinicaltrials_timpub_5a <- renderPlotly({
        return (plot_clinicaltrials_timpub_5a(iv_data, input$selectUMC, color_palette))
    })

    ## Robustness plot
    ## output$plot_randomization <- renderPlotly({
    ##     return (plot_randomization(rm_data, input$selectUMC, input$animals_absnum, color_palette_delwen))
    ## })

    ## Blinding plot
    ## output$plot_blinding <- renderPlotly({
    ##     return(plot_blinding(rm_data, input$selectUMC, input$animals_absnum, color_palette_delwen))
    ## })

    ## Power calc plot
    ## output$plot_power <- renderPlotly({
    ##     return(plot_power(rm_data, input$selectUMC, input$animals_absnum, color_palette_delwen))
    ## })

    ## IACUC plot
    ## output$plot_iacuc <- renderPlotly({
    ##     return(plot_iacuc(rm_data, input$selectUMC, color_palette))
    ## })

    ## All UMC's page plots ##

    ## Open Science

    ## Open Access

    output$plot_allumc_openaccess <- renderPlotly({
        return(plot_allumc_openaccess(rm_data, color_palette))
    })

    ## Open Data

    ## output$plot_allumc_opendata <- renderPlotly({
    ##     return(plot_allumc_opendata(rm_data, color_palette, color_palette_bars))
    ## })

    ## Open Code

    ## output$plot_allumc_opencode <- renderPlotly({
    ##     return(plot_allumc_opencode(rm_data, color_palette, color_palette_bars))
    ## })

    ## Clinical Trials

    ## TRN

    output$plot_allumc_clinicaltrials_trn <- renderPlotly({
        return(plot_allumc_clinicaltrials_trn(rm_data, color_palette))
    })

    ## Summary results

    output$plot_allumc_clinicaltrials_sumres <- renderPlotly({
        return(plot_allumc_clinicaltrials_sumres(eutt_data, color_palette, color_palette_bars))
    })

    ## Preregistration

    output$plot_allumc_clinicaltrials_prereg <- renderPlotly({
        return(plot_allumc_clinicaltrials_prereg(iv_data, color_palette, color_palette_bars))
    })

    ## Timely publication

    output$plot_allumc_clinicaltrials_timpub <- renderPlotly({
        return(plot_allumc_clinicaltrials_timpub(iv_data, color_palette, color_palette_bars))
    })
    
    ## Robustness of Animal Studies

    ## Randomization

    ## output$plot_allumc_animal_rando <- renderPlotly({
    ##     return(plot_allumc_animal_rando(rm_data, color_palette, color_palette_bars))
    ## })

    ## Blinding

    ## output$plot_allumc_animal_blind <- renderPlotly({
    ##     return(plot_allumc_animal_blind(rm_data, color_palette, color_palette_bars))
    ## })

    ## Power calc

    ## output$plot_allumc_animal_power <- renderPlotly({
    ##     return(plot_allumc_animal_power(rm_data, color_palette, color_palette_bars))
    ## })

    ## IACUC

    ## output$plot_allumc_animal_iacuc <- renderPlotly({
    ##     return(plot_allumc_animal_iacuc(rm_data, color_palette, color_palette_bars))
    ## })

    ## Generate data tables

    ## output$data_table_rm_data <- DT::renderDataTable({
    ##     make_datatable(rm_data)
    ## })

    output$data_table_eutt_data <- DT::renderDataTable({
        make_datatable(eutt_data)
    })

    output$data_table_iv_data <- DT::renderDataTable({
        make_datatable(iv_data)
    })

    output$reportcard <- renderUI({

        

        if (str_detect(input$trn, "\\bNCT[0-9]{8}\\b") | str_detect(input$trn, "\\bDRKS[0-9]{8}\\b")) {

            trn <- str_extract(input$trn, "\\bNCT[0-9]{8}\\b|\\bDRKS[0-9]{8}\\b")

            if (nrow(filter(iv_data, id == trn)) > 0) {

                rc <- iv_data %>%
                    filter(id == trn)

                rc_city <- rc %>%
                    select(city) %>%
                    pull()

                rc_city <- paste("UMC:", rc_city)

                rc_completiondate <- rc %>%
                    select(completion_date) %>%
                    pull()

                rc_completiondate <- paste("Completion date:", rc_completiondate)

                rc_preregistered <- rc %>%
                    select(preregistered) %>%
                    pull()

                if (rc_preregistered) {
                    rc_preregistered <- "This study was prospectively registered"
                } else {
                    rc_preregistered <- "This study was not prospectively registered"
                }

                rc_published2a <- rc %>%
                    select(published_2a) %>%
                    pull()

                if (rc_published2a) {
                    rc_published2a <- "This study published results within 2 years of completion"
                } else {
                    rc_published2a <- "This study did not publish results within 2 years of completion"
                }

                fluidRow(
                    column(
                        12,
                        p(rc_city),
                        p(rc_completiondate),
                        p(rc_preregistered),
                        p(rc_published2a)
                    )
                )
                
            } else {
                p("Trial registry number not found")
            }

        } else {

            p("Please enter an NCT or DRKS number")
            
        }
        
    })
    
}

## Create Shiny object
shinyApp(ui, server)
