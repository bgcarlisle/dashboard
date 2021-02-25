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
library(R.utils)

## Load data
rm_data <- read_csv(
    "data/2021-01-26_pp-dataset-oa-trn-sciscore-od-animals.csv",
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

## Generates the UMC list for the drop-down menu
ddumcs <- rm_data %>%
    select(city)

if (sum(substr(ddumcs$city, 1, 4) == "UMC ") == nrow(ddumcs)) {
    ddumcs$umcno <- substr(ddumcs$city, 5, nchar(ddumcs$city)) %>%
        as.numeric()

    ddumcs <- ddumcs %>%
        arrange(umcno) %>%
        select(city)
} else {

    ddumcs <- ddumcs %>%
        arrange(city)
    
}

umclist <- c(
    "All",
    ddumcs$city %>% unique()
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

iv_data <- read_csv(
    "data/2021-02-25-IntoValue1-2.csv"
    # This is the IntoValue2 data set.
)

## Load functions
source("ui_elements.R")
source("start_page_plots.R")
source("all_umc_plots.R")

## Load pages
source("start_page.R")
source("all_umcs_page.R")
source("methods_page.R")
source("datasets_page.R")
source("about_rm.R")

## Define UI
ui <- tagList(
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    navbarPage(
        "Responsible Metrics Dashboard", theme = shinytheme("flatly"), id = "navbarTabs",
        start_page,
        all_umcs_page,
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
                    h1(style = "margin-left:0cm", strong("Proof-of-principle Responsible Metrics Dashboard (2018)"), align = "left"),
                    h4(style = "margin-left:0cm",
                       "This dashboard is a proof-of-principle overview of several metrics of open and robust
                       research for several German University Medical Centres (UMCs). Besides the metrics
                       Summary Results Reporting, Prospective Registration, and Timely Publication, all other
                       publication-based metrics are based on publications published in 2018. For more detailed
                       information on the methods used to calculate those metrics, click one of the following buttons."),
                    h4(style = "margin-left:0cm",
                       "This dashboard is a pilot that is still under development, and should not be used to compare UMC's or inform policy. More metrics may be added in the future."),
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
                        'See all UMC\'s'
                    ),
                    actionButton(
                        style = "color: white; background-color: #aa1c7d;",
                        'buttonMethods',
                        'See methods'
                    ),
                    actionButton(
                        style = "color: white; background-color: #aa1c7d;",
                        'buttonDatasets',
                        'See data sets'
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

    output$robustness_metrics <- renderUI({

        req(input$width)
        req(input$selectUMC)

        if (input$width < 1400) {
            col_width <- 6
            alignment <- "left"
        } else {
            col_width <- 3
            alignment <- "right"
        }

        ## Value for randomization

        if (input$selectUMC == "All") {

            all_numer_rando <- rm_data %>%
                filter(
                    is_animal == 1,
                    ! is.na(sciscore),
                    type == "Article"
                ) %>%
                select(randomization) %>%
                sum(na.rm=TRUE)
            
        } else {

            all_numer_rando <- rm_data %>%
                filter(city == input$selectUMC) %>%
                filter(
                    is_animal == 1,
                    ! is.na(sciscore),
                    type == "Article"
                ) %>%
                select(randomization) %>%
                sum(na.rm=TRUE)
            
        }

        ## Value for Blinding

        if (input$selectUMC == "All") {

        all_numer_blinded <- rm_data %>%
            filter(
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            select(blinding) %>%
            sum(na.rm=TRUE)
            
        } else {

        all_numer_blinded <- rm_data %>%
            filter(city == input$selectUMC) %>%
            filter(
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            select(blinding) %>%
            sum(na.rm=TRUE)
            
        }

        ## Value for Power calc

        if (input$selectUMC == "All") {

            all_numer_power <- rm_data %>%
                filter(
                    is_animal == 1,
                    ! is.na(sciscore),
                    type == "Article"
                ) %>%
                select(power) %>%
                sum(na.rm=TRUE)
            
        } else {

            all_numer_power <- rm_data %>%
                filter(city == input$selectUMC) %>%
                filter(
                    is_animal == 1,
                    ! is.na(sciscore),
                    type == "Article"
                ) %>%
                select(power) %>%
                sum(na.rm=TRUE)
            
        }

        ## all_numer_iacuc <- rm_data %>%
        ##     filter(
        ##         is_animal == 1,
        ##         ! is.na(sciscore),
        ##         type == "Article"
        ##     ) %>%
        ##     select(iacuc) %>%
        ##     sum(na.rm=TRUE)

        if (input$selectUMC == "All") {

            all_denom_animal_sciscore <- rm_data %>%
                filter(
                    is_animal == 1,
                    ! is.na(sciscore),
                    type == "Article"
                ) %>%
                nrow()
            
        } else {

            all_denom_animal_sciscore <- rm_data %>%
                filter(city == input$selectUMC) %>%
                filter(
                    is_animal == 1,
                    ! is.na(sciscore),
                    type == "Article"
                ) %>%
                nrow()
            
        }

        all_percent_randomized <- paste0(round(100*all_numer_rando/all_denom_animal_sciscore), "%")
        all_percent_blinded <- paste0(round(100*all_numer_blinded/all_denom_animal_sciscore), "%")
        all_percent_power <- paste0(round(100*all_numer_power/all_denom_animal_sciscore), "%")
        ## all_percent_iacuc <- paste0(round(100*all_numer_iacuc/all_denom_animal_sciscore), "%")

        wellPanel(
            style = "padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Robustness of Animal Studies"), align = "left"),
            fluidRow(
                column(
                    col_width,
                    metric_box(
                        title = "Randomization",
                        value = all_percent_randomized,
                        value_text = "of analyzable 2018 animal studies report randomization",
                        plot = plotlyOutput('plot_randomization', height="300px"),
                        info_id = "infoRandomization",
                        info_title = "Randomization",
                        info_text = randomization_tooltip
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Blinding",
                        value = all_percent_blinded,
                        value_text = "of analyzable 2018 animal studies report blinding",
                        plot = plotlyOutput('plot_blinding', height="300px"),
                        info_id = "infoBlinding",
                        info_title = "Blinding",
                        info_text = blinding_tooltip
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Power calculation",
                        value = all_percent_power,
                        value_text = "of analyzable 2018 animal studies report a power calculation",
                        plot = plotlyOutput('plot_power', height="300px"),
                        info_id = "infoPower",
                        info_title = "Power",
                        info_text = power_tooltip
                    )
                )##,
                ## column(
                ##     col_width,
                ##     metric_box(
                ##         title = "IACUC statement",
                ##         value = all_percent_iacuc,
                ##         value_text = "of animal studies report an IACUC statement",
                ##         plot = plotlyOutput('plot_iacuc', height="300px"),
                ##         info_id = "infoIACUC",
                ##         info_title = "IACUC",
                ##         info_text = iacuc_tooltip
                ##     )
                ## )
            )
        )        
        
    })

    output$clinicaltrials_metrics <- renderUI({

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
            
        } else {
            
            all_numer_sumres <- eutt_data %>%
                filter(city == input$selectUMC) %>%
                filter (
                    due_or_not == "Due",
                    status == "Reported results" |
                    status == "Reported results Terminated"
                ) %>%
                nrow()

            all_denom_sumres <- eutt_data %>%
                filter(city == input$selectUMC) %>%
                filter(due_or_not == "Due") %>%
                nrow()
            
        }

        if (all_denom_sumres == 0) {
            sumresval <- "Not applicable"
            sumresvaltext <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
            sumresval <- paste0(round(100*all_numer_sumres/all_denom_sumres), "%")
            sumresvaltext <- "of due clinical trials reporting summary results"
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
            preregvaltext <- "of clinical trials were prospectively registered"
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

        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Clinical Trials"), align = "left"),
            fluidRow(
                column(
                    col_width,
                    metric_box(
                        title = "Trial Registry Number Reporting",
                        value = paste0(round(100*all_numer_trn/all_denom_trn), "%"),
                        value_text = "of 2018 PubMed-classified clinical trial publications reported a TRN in the abstract",
                        plot = plotlyOutput('plot_clinicaltrials_trn', height="300px"),
                        info_id = "infoTRN",
                        info_title = "Trial Registry Number Reporting",
                        info_text = trn_tooltip
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Summary Results Reporting",
                        value = sumresval,
                        value_text = sumresvaltext,
                        plot = plotlyOutput('plot_clinicaltrials_sumres', height="300px"),
                        info_id = "infoSumRes",
                        info_title = "Summary Results Reporting",
                        info_text = sumres_tooltip
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
                        info_text = prereg_tooltip
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Timely publication",
                        value = timpubval,
                        value_text = timpubvaltext,
                        plot = plotlyOutput('plot_clinicaltrials_timpub', height="300px"),
                        info_id = "infoTimPub",
                        info_title = "Timely Publication",
                        info_text = timpub_tooltip
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

        if ( input$selectUMC == "All") {
            
            all_denom_od <- rm_data %>%
                filter(
                    ! is.na (is_open_data),
                    language == "English"
                ) %>%
                nrow()

            all_numer_od <- rm_data %>%
                filter(
                    is_open_data,
                    language == "English"
                ) %>%
                nrow()

        } else {
            
            all_denom_od <- rm_data %>%
                filter(city == input$selectUMC) %>%
                filter(
                    ! is.na (is_open_data),
                    language == "English"
                ) %>%
                nrow()

            all_numer_od <- rm_data %>%
                filter(city == input$selectUMC) %>%
                filter(
                    is_open_data,
                    language == "English"
                ) %>%
                nrow()

        }
        
        ## Value for Open Code

        if ( input$selectUMC == "All") {

            all_denom_oc <- rm_data %>%
                filter(
                    ! is.na (is_open_code),
                    language == "English"
                ) %>%
                nrow()

            all_numer_oc <- rm_data %>%
                filter(
                    is_open_code,
                    language == "English"
                ) %>%
                nrow()
            
        } else {

            all_denom_oc <- rm_data %>%
                filter(city == input$selectUMC) %>%
                filter(
                    ! is.na (is_open_code),
                    language == "English"
                ) %>%
                nrow()

            all_numer_oc <- rm_data %>%
                filter(city == input$selectUMC) %>%
                filter(
                    is_open_code,
                    language == "English"
                ) %>%
                nrow()
            
        }
        
        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Open Science"), align = "left"),
            fluidRow(
                column(
                    col_width,
                    metric_box(
                        title = "Open Access",
                        value = paste0(round(100*all_numer_oa/all_denom_oa), "%"),
                        value_text = "of 2018 publications are Open Access",
                        plot = plotlyOutput('plot_opensci_oa', height="300px"),
                        info_id = "infoOpenAccess",
                        info_title = "Open Access",
                        info_text = openaccess_tooltip
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Any Open Data",
                        value = paste0(round(100*all_numer_od/all_denom_od), "%"),
                        value_text = "of 2018 analyzable publications mentioned sharing of data",
                        plot = plotlyOutput('plot_opensci_od', height="300px"),
                        info_id = "infoOpenData",
                        info_title = "Any Open Data",
                        info_text = opendata_tooltip
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Any Open Code",
                        value = paste0(round(100*all_numer_oc/all_denom_oc), "%"),
                        value_text = "of 2018 analyzable publications mentioned sharing of code",
                        plot = plotlyOutput('plot_opensci_oc', height="300px"),
                        info_id = "infoOpenCode",
                        info_title = "Any Open Code",
                        info_text = opencode_tooltip
                    )
                )
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

        all_denom_od <- rm_data %>%
            filter(
                ! is.na (is_open_data),
                language == "English"
            ) %>%
            nrow()

        all_numer_od <- rm_data %>%
            filter(
                is_open_data,
                language == "English"
            ) %>%
            nrow()
        
        ## Value for All UMC Open Code
 
        all_denom_oc <- rm_data %>%
            filter(
                ! is.na (is_open_code),
                language == "English"
            ) %>%
            nrow()

        all_numer_oc <- rm_data %>%
            filter(
                is_open_code,
                language == "English"
            ) %>%
            nrow()

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
                        info_text = allumc_openaccess_tooltip
                    )
                )
            ),
            fluidRow(
                column(
                    12,
                    metric_box(
                        title = "Any Open Data",
                        value = paste0(round(100*all_numer_od/all_denom_od), "%"),
                        value_text = "of 2018 analyzable publications mentioned sharing of data",
                        plot = plotlyOutput('plot_allumc_opendata', height="300px"),
                        info_id = "infoALLUMCOpenData",
                        info_title = "Any Open Data (All UMCs)",
                        info_text = allumc_opendata_tooltip
                    )
                )
            ),
            fluidRow(
                column(
                    12,
                    metric_box(
                        title = "Any Open Code",
                        value = paste0(round(100*all_numer_oc/all_denom_oc), "%"),
                        value_text = "of 2018 analyzable publications mentioned sharing of code",
                        plot = plotlyOutput('plot_allumc_opencode', height="300px"),
                        info_id = "infoALLUMCOpenCode",
                        info_title = "Any Open Code (All UMCs)",
                        info_text = allumc_opencode_tooltip
                    )
                )
            )
            
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
                        title = "Trial Registry Number Reporting",
                        value = paste0(round(100*all_numer_trn/all_denom_trn), "%"),
                        value_text = "of 2018 PubMed-classified clinical trial publications reported a TRN in the abstract",
                        plot = plotlyOutput('plot_allumc_clinicaltrials_trn', height="300px"),
                        info_id = "infoALLUMCTRN",
                        info_title = "TRN reporting (All UMCs)",
                        info_text = allumc_clinicaltrials_trn_tooltip
                    )
                )
            ),
            fluidRow(
                column(
                    12,
                    metric_box(
                        title = "Summary Results Reporting",
                        value = paste0(round(100*all_numer_sumres/all_denom_sumres), "%"),
                        value_text = "of due clinical trials reporting summary results",
                        plot = plotlyOutput('plot_allumc_clinicaltrials_sumres', height="300px"),
                        info_id = "infoALLUMCSumRes",
                        info_title = "Summary results reporting (All UMCs)",
                        info_text = allumc_clinicaltrials_sumres_tooltip
                    )
                )
            ),
            fluidRow(
                column(
                    12,
                    metric_box(
                        title = "Prospective registration",
                        value = paste0(round(100*all_numer_prereg/all_denom_prereg), "%"),
                        value_text = "of clinical trials were prospectively registered",
                        plot = plotlyOutput('plot_allumc_clinicaltrials_prereg', height="300px"),
                        info_id = "infoALLUMCPreReg",
                        info_title = "Prospective registration (All UMCs)",
                        info_text = allumc_clinicaltrials_prereg_tooltip
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
                        info_text = allumc_clinicaltrials_timpub_tooltip
                    )
                )
            )
        )
        
    })

    output$allumc_robustness <- renderUI({

        ## Values for All UMC Robustness metrics

        all_numer_rando <- rm_data %>%
            filter(
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            select(randomization) %>%
            sum(na.rm=TRUE)

        all_numer_blinded <- rm_data %>%
            filter(
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            select(blinding) %>%
            sum(na.rm=TRUE)

        all_numer_power <- rm_data %>%
            filter(
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            select(power) %>%
            sum(na.rm=TRUE)

        ## all_numer_iacuc <- rm_data %>%
        ##     filter(
        ##         is_animal == 1,
        ##         ! is.na(sciscore),
        ##         type == "Article"
        ##     ) %>%
        ##     select(iacuc) %>%
        ##     sum(na.rm=TRUE)

        all_denom_animal_sciscore <- rm_data %>%
            filter(
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            nrow()

        all_percent_randomized <- paste0(round(100*all_numer_rando/all_denom_animal_sciscore), "%")
        all_percent_blinded <- paste0(round(100*all_numer_blinded/all_denom_animal_sciscore), "%")
        all_percent_power <- paste0(round(100*all_numer_power/all_denom_animal_sciscore), "%")
        ## all_percent_iacuc <- paste0(round(100*all_numer_iacuc/all_denom_animal_sciscore), "%")

        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Robustness of Animal Studies"), align = "left"),
            fluidRow(
                column(
                    12,
                    metric_box(
                        title = "Randomization",
                        value = all_percent_randomized,
                        value_text = "of analyzable 2018 animal studies report randomization",
                        plot = plotlyOutput('plot_allumc_animal_rando', height="300px"),
                        info_id = "infoAllUMCAnimalRando",
                        info_title = "Randomization",
                        info_text = allumc_animal_rando_tooltip
                    )
                )
            ),
            fluidRow(
                column(
                    12,
                    metric_box(
                        title = "Blinding",
                        value = all_percent_blinded,
                        value_text = "of analyzable 2018 animal studies report blinding",
                        plot = plotlyOutput('plot_allumc_animal_blind', height="300px"),
                        info_id = "infoAllUMCAnimalBlind",
                        info_title = "Blinding",
                        info_text = allumc_animal_blind_tooltip
                    )
                )
            ),
            fluidRow(
                column(
                    12,
                    metric_box(
                        title = "Power calculation",
                        value = all_percent_power,
                        value_text = "of analyzable 2018 animal studies report a power calculation",
                        plot = plotlyOutput('plot_allumc_animal_power', height="300px"),
                        info_id = "infoAllUMCAnimalPower",
                        info_title = "Power calculation",
                        info_text = allumc_animal_power_tooltip
                    )
                )
            )##,
            ## fluidRow(
            ##     column(
            ##         12,
            ##         metric_box(
            ##             title = "IACUC statement",
            ##             value = all_percent_iacuc,
            ##             value_text = "of animal studies report an IACUC statement",
            ##             plot = plotlyOutput('plot_allumc_animal_iacuc', height="300px"),
            ##             info_id = "infoAllUMCAnimalIACUC",
            ##             info_title = "IACUC statement",
            ##             info_text = allumc_animal_iacuc_tooltip
            ##         )
            ##     )
            ## )
        )
        
    })

    color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A",
                     "#303A3E", "#007265", "#634587", "#000000",   #363457 #533A71 #011638 #634587
                     "#DCE3E5")

    color_palette_bars <- c("#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587", "#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587")

    ## Start page plots ##
    
    ## Open Access plot
    output$plot_opensci_oa <- renderPlotly({
        return (plot_opensci_oa(rm_data, input$selectUMC, color_palette))
    })
    
    ## Open Data plot
    output$plot_opensci_od <- renderPlotly({
        return (plot_opensci_od(rm_data, input$selectUMC, color_palette))
    })
    
    ## Open Code plot
    output$plot_opensci_oc <- renderPlotly({
        return (plot_opensci_oc(rm_data, input$selectUMC, color_palette))
    })
    
    ## TRN plot
    output$plot_clinicaltrials_trn <- renderPlotly({
        return (plot_clinicaltrials_trn(rm_data, input$selectUMC, color_palette))
    })
    
    ## Summary results plot
    output$plot_clinicaltrials_sumres <- renderPlotly({
        return (plot_clinicaltrials_sumres(eutt_data, input$selectUMC, color_palette))
    })
    
    ## Preregistration plot
    output$plot_clinicaltrials_prereg <- renderPlotly({
        return (plot_clinicaltrials_prereg(iv_data, input$selectUMC, color_palette))
    })
    
    ## Timely Publication plot
    output$plot_clinicaltrials_timpub <- renderPlotly({
        return (plot_clinicaltrials_timpub(iv_data, input$selectUMC, color_palette))
    })

    ## Robustness plot
    output$plot_randomization <- renderPlotly({
        return (plot_randomization(rm_data, input$selectUMC, color_palette))
    })

    ## Blinding plot
    output$plot_blinding <- renderPlotly({
        return(plot_blinding(rm_data, input$selectUMC, color_palette))
    })

    ## Power calc plot
    output$plot_power <- renderPlotly({
        return(plot_power(rm_data, input$selectUMC, color_palette))
    })

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

    output$plot_allumc_opendata <- renderPlotly({
        return(plot_allumc_opendata(rm_data, color_palette, color_palette_bars))
    })

    ## Open Code

    output$plot_allumc_opencode <- renderPlotly({
        return(plot_allumc_opencode(rm_data, color_palette, color_palette_bars))
    })

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

    output$plot_allumc_animal_rando <- renderPlotly({
        return(plot_allumc_animal_rando(rm_data, color_palette, color_palette_bars))
    })

    ## Blinding

    output$plot_allumc_animal_blind <- renderPlotly({
        return(plot_allumc_animal_blind(rm_data, color_palette, color_palette_bars))
    })

    ## Power calc

    output$plot_allumc_animal_power <- renderPlotly({
        return(plot_allumc_animal_power(rm_data, color_palette, color_palette_bars))
    })

    ## IACUC

    ## output$plot_allumc_animal_iacuc <- renderPlotly({
    ##     return(plot_allumc_animal_iacuc(rm_data, color_palette, color_palette_bars))
    ## })

    ## Generate data tables

    output$data_table_rm_data <- DT::renderDataTable({
        make_datatable(rm_data)
    })

    output$data_table_eutt_data <- DT::renderDataTable({
        make_datatable(eutt_data)
    })

    output$data_table_iv_data <- DT::renderDataTable({
        make_datatable(iv_data)
    })
    
}

## Create Shiny object
shinyApp(ui, server)
