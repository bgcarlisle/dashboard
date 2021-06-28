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

eutt_data <- read_csv(
    "data/2021-02-03-eutt-pop-umcs.csv"
    ## This file is data that I copied and pasted directly from
    ## http://eu.trialstracker.net/ into LibreOffice Calc.
)

eutt_hist <- read_csv(
    "data/2021-06-16-eutt-history.csv"
    ## Generate this from the EUTT repo using the script in
    ## prep/eutt-history.R
)

iv_data <- read_csv(
    "data/2021-06-18-data-iv.csv"
    # This is the IntoValue 1-2 data set.
)

## Load functions
source("ui_elements.R")
source("start_page_plots.R")
source("umc_plots.R")
source("all_umc_plots.R")

## Load pages
source("start_page.R")
source("umc_page.R")
source("all_umcs_page.R")
source("umc_page.R")
source("methods_page.R")
source("datasets_page.R")
source("about_rm.R")

## Define UI
ui <- tagList(
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    navbarPage(
        "Open science in clinical research", theme = shinytheme("flatly"), id = "navbarTabs",
        start_page,
        all_umcs_page,
        umc_page,
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
        input$buttonUMC, {
            updateTabsetPanel(
                session, "navbarTabs",
                selected = "tabUMC"
            )
        }
    )

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
                       "This is a proof-of-principle dashboard for Open Science in clinical research at University
                       Medical Centers (UMCs) in Germany. This dashboard is a pilot that is still under development,
                       and should not be used to compare UMCs or inform policy. More metrics may be added in the future."),
                    h4(style = "margin-left:0cm",
                       "The dashboard includes data relating to clinical trials of UMCs in Germany. While the dashboard
                       displays the average across all UMCs, you can also view the data for a given UMC by selecting
                       it in the drop-down menu. Once selected, you will see this UMC's data contextualized to the average
                       of all included UMCs. For the Open Access metrics, the data can be viewed as either 1) the percentage
                       of analyzable publications which display the given metric; or 2) the absolute number of eligible
                       publications which display the given metric (click on the toggle to visualise both options). For
                       each metric, you can find an overview of the methods and limitations by clicking on the relevant
                       symbols. For more detailed information on the methods and underlying datasets used to calculate
                       those metrics, visit the Methods or Datasets pages."),
                    br()
                ),
                column(
                    4,
                    hr(),
                    br(),
                    br(),
                    actionButton(
                        style = "color: white; background-color: #aa1c7d;",
                        'buttonUMC',
                        'See one UMC'
                    ),
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
            )
        )
        
    })

    output$umc_registry_metrics <- renderUI({

        if (input$selectUMC != "Select a UMC") {
            ## Nothing will be diplayed if the selector is still on
            ## "Select a UMC"
            
            req(input$width)
            req(input$selectUMC)

            if (input$width < 1400) {
                col_width <- 6
                alignment <- "left"
            } else {
                col_width <- 3
                alignment <- "right"
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
                preregvaltext <- "of clinical trials were prospectively registered in ClinicalTrials.gov"
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

            
            ## Value for TRN

            all_numer_trn <- iv_data %>%
                filter(city == input$selectUMC) %>%
                select(has_iv_trn_abstract) %>%
                filter(has_iv_trn_abstract == TRUE) %>%
                nrow()
            
            all_denom_trn <- iv_data %>%
                filter(city == input$selectUMC) %>%
                filter(! is.na(has_iv_trn_abstract)) %>%
                nrow()

            wellPanel(
                style="padding-top: 0px; padding-bottom: 0px;",
                h2(strong("Trial Registration"), align = "left"),
                fluidRow(
                    column(
                        col_width,
                        metric_box(
                            title = "Prospective registration",
                            value = preregval,
                            value_text = preregvaltext,
                            plot = plotlyOutput('umc_plot_clinicaltrials_prereg', height="300px"),
                            info_id = "infoPreReg",
                            info_title = "Prospective registration",
                            info_text = prereg_tooltip,
                            lim_id = "limPreReg",
                            lim_title = "Limitations: Prospective registration",
                            lim_text = lim_prereg_tooltip
                        )
                    ),
                    column(
                        col_width,
                        metric_box(
                            title = "Reporting of Trial Registration Number in publications",
                            value = paste0(round(100*all_numer_trn/all_denom_trn), "%"),
                            value_text = "of clinical trials reported a trial registration number in the abstract",
                            plot = plotlyOutput('umc_plot_clinicaltrials_trn', height="300px"),
                            info_id = "infoTRN",
                            info_title = "Reporting of Trial Registration Number in publications",
                            info_text = trn_tooltip,
                            lim_id = "limTRN",
                            lim_title = "Limitations: Reporting of Trial Registration Number in publications",
                            lim_text = lim_trn_tooltip
                        )
                    )
                    
                )

            )
            
        }
        
    })

    output$umc_publication_metrics <- renderUI({
        
        if (input$selectUMC != "Select a UMC") {
            ## Nothing will be diplayed if the selector is still on
            ## "Select a UMC"
            
            req(input$width)
            req(input$selectUMC)

            if (input$width < 1400) {
                col_width <- 6
                alignment <- "left"
            } else {
                col_width <- 3
                alignment <- "right"
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
                sumresvaltext <- "of due clinical trials registered in EUCTR reported summary results"
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
                timpubvaltext <- "of clinical trials registered in ClinicalTrials.gov or DRKS reported results within 2 years"
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
                timpubval5a <- paste0(round(100*all_numer_timpub5a/all_denom_timpub5a), "%")
                timpubvaltext5a <- "of clinical trials registered in ClinicalTrials.gov or DRKS reported results within 5 years"
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
                sumresvaltext <- "of due clinical trials registered in EUCTR reported summary results"
            }


            wellPanel(
                style="padding-top: 0px; padding-bottom: 0px;",
                h2(strong("Trial Reporting"), align = "left"),
                fluidRow(
                    column(
                        col_width,
                        metric_box(
                            title = "Summary Results Reporting in EUCTR",
                            value = sumresval,
                            value_text = sumresvaltext,
                            plot = plotlyOutput('umc_plot_clinicaltrials_sumres', height="300px"),
                            info_id = "infoSumRes",
                            info_title = "Summary Results Reporting in EUCTR",
                            info_text = sumres_tooltip,
                            lim_id = "limSumRes",
                            lim_title = "Limitations: Summary Results Reporting in EUCTR",
                            lim_text = lim_sumres_tooltip
                        )
                    ),
                    column(
                        col_width,
                        metric_box(
                            title = "Reporting within 2 years (timely)",
                            value = timpubval,
                            value_text = timpubvaltext,
                            plot = plotlyOutput('umc_plot_clinicaltrials_timpub_2a', height="300px"),
                            info_id = "infoTimPub2",
                            info_title = "Timely Publication (2 years)",
                            info_text = timpub_tooltip2,
                            lim_id = "lim",
                            lim_title = "Limitations: Timely Publication",
                            lim_text = lim_timpub_tooltip2
                        )
                    ),
                    column(
                        col_width,
                        metric_box(
                            title = "Reporting within 5 years",
                            value = timpubval5a,
                            value_text = timpubvaltext5a,
                            plot = plotlyOutput('umc_plot_clinicaltrials_timpub_5a', height="300px"),
                            info_id = "infoTimPub5",
                            info_title = "Publication by 5 years",
                            info_text = timpub_tooltip5,
                            lim_id = "lim",
                            lim_title = "Limitations: Timely Publication",
                            lim_text = lim_timpub_tooltip5
                        )
                    )
                    
                )

            )

            
        }
    })

    output$umc_openscience_metrics <- renderUI({

        if (input$selectUMC != "Select a UMC") {
            ## Nothing will be diplayed if the selector is still on
            ## "Select a UMC"

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

            all_numer_oa <- iv_data %>%
                filter(city == input$selectUMC) %>%
                filter(
                    color == "gold" | color == "green" | color == "hybrid"
                    
                ) %>%
                nrow()

            all_denom_oa <- iv_data %>%
                filter(city == input$selectUMC) %>%
                filter(
                    ! is.na(color)
                    
                ) %>%
                nrow()

            ##
                
            closed_with_potential <- iv_data %>%
                filter(
                    color_green_only == "closed",
                    ! is.na(permission_postprint),
                    permission_postprint == TRUE,
                    city == input$selectUMC
                ) %>%
                nrow()
            
            greenoa_only <- iv_data %>%
                filter(
                    color_green_only == "green",
                    city == input$selectUMC
                ) %>%
                nrow()
            
            denom_greenoa <- closed_with_potential + greenoa_only
            
            numer_greenoa <- greenoa_only

            ##
            
            wellPanel(
                style="padding-top: 0px; padding-bottom: 0px;",
                h2(strong("Open Access"), align = "left"),
                checkboxInput(
                    "umc_opensci_absnum",
                    strong("Show absolute numbers"),
                    value = FALSE
                ),
                fluidRow(
                    column(
                        col_width,
                        metric_box(
                            title = "Open Access (OA)",
                            value = paste0(round(100*all_numer_oa/all_denom_oa), "%"),
                            value_text = "of publications are Open Access",
                            plot = plotlyOutput('umc_plot_opensci_oa', height="300px"),
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
                            plot = plotlyOutput('umc_plot_opensci_green_oa', height="300px"),
                            info_id = "infoGreenOA",
                            info_title = "Potential Green Open Access",
                            info_text = greenopenaccess_tooltip,
                            lim_id = "limGreenOA",
                            lim_title = "Limitations: Potential Green Open Access",
                            lim_text = lim_greenopenaccess_tooltip
                        )
                    )
                    
                )
            )


        }

    })

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

        ## Value for prereg

        iv_data_unique <- iv_data %>%
            distinct(id, .keep_all = TRUE)

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
            preregvaltext <- "of clinical trials were prospectively registered in ClinicalTrials.gov"
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

        
        ## Value for TRN
        
        all_numer_trn <- sum(iv_data$has_iv_trn_abstract, na.rm=TRUE)
        
        all_denom_trn <- iv_data %>%
            filter(! is.na(has_iv_trn_abstract)) %>%
            nrow()

        ## Value for linkage

        linkage <- paste0(round(100*mean(iv_data$has_reg_pub_link, na.rm=TRUE)), "%")
        linkagetext <- "of clinical trial registry entries link to the journal publication"

        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Trial Registration"), align = "left"),
            fluidRow(
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
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Reporting of Trial Registration Number in publications",
                        value = paste0(round(100*all_numer_trn/all_denom_trn), "%"),
                        value_text = "of clinical trials reported a trial registration number in the abstract",
                        plot = plotlyOutput('plot_clinicaltrials_trn', height="300px"),
                        info_id = "infoTRN",
                        info_title = "Reporting of Trial Registration Number in publications",
                        info_text = trn_tooltip,
                        lim_id = "limTRN",
                        lim_title = "Limitations: Reporting of Trial Registration Number in publications",
                        lim_text = lim_trn_tooltip
                    )
                ),
                
                column(
                    col_width,
                    metric_box(
                        title = "Publication link in registry",
                        value = linkage,
                        value_text = linkagetext,
                        plot = plotlyOutput('plot_linkage', height="300px"),
                        info_id = "infoLinkage",
                        info_title = "Publication link in registry",
                        info_text = linkage_tooltip,
                        lim_id = "limLinkage",
                        lim_title = "Limitations: Publication link in registry",
                        lim_text = lim_linkage_tooltip
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
        
        ## Value for summary results
        
        sumres_percent <- eutt_hist %>%
            group_by(date) %>%
            mutate(avg = mean(percent_reported)) %>%
            slice_head() %>%
            ungroup() %>%
            slice_tail() %>%
            select(avg) %>%
            pull() %>%
            format(digits=3)

        n_eutt_records <- eutt_hist %>%
            nrow()

        if (n_eutt_records == 0) {
            sumresval <- "Not applicable"
            sumresvaltext <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
            sumresval <- paste0(sumres_percent, "%")
            sumresvaltext <- "of due clinical trials registered in EUCTR reported summary results"
        }

        ## Value for prereg

        iv_data_unique <- iv_data %>%
            distinct(id, .keep_all = TRUE)

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
            timpubvaltext <- "of clinical trials registered in ClinicalTrials.gov or DRKS reported results within 2 years"
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
            timpubval5a <- paste0(round(100*all_numer_timpub5a/all_denom_timpub5a), "%")
            timpubvaltext5a <- "of clinical trials registered in ClinicalTrials.gov or DRKS reported results within 5 years"
        }

        
        ## Value for summary results
            
        sumres_percent <- eutt_hist %>%
            group_by(date) %>%
            mutate(avg = mean(percent_reported)) %>%
            slice_head() %>%
            ungroup() %>%
            slice_tail() %>%
            select(avg) %>%
            pull() %>%
            format(digits=3)

        n_eutt_records <- eutt_hist %>%
            nrow()
            
        if (n_eutt_records == 0) {
            sumresval <- "Not applicable"
            sumresvaltext <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
            sumresval <- paste0(sumres_percent, "%")
            sumresvaltext <- "of due clinical trials registered in EUCTR reported summary results"
        }


        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Trial Reporting"), align = "left"),
            fluidRow(
                column(
                    col_width,
                    metric_box(
                        title = "Summary Results Reporting in EUCTR",
                        value = sumresval,
                        value_text = sumresvaltext,
                        plot = plotlyOutput('plot_clinicaltrials_sumres', height="300px"),
                        info_id = "infoSumRes",
                        info_title = "Summary Results Reporting in EUCTR",
                        info_text = sumres_tooltip,
                        lim_id = "limSumRes",
                        lim_title = "Limitations: Summary Results Reporting in EUCTR",
                        lim_text = lim_sumres_tooltip
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Reporting within 2 years (timely)",
                        value = timpubval,
                        value_text = timpubvaltext,
                        plot = plotlyOutput('plot_clinicaltrials_timpub_2a', height="300px"),
                        info_id = "infoTimPub2",
                        info_title = "Timely Publication (2 years)",
                        info_text = timpub_tooltip2,
                        lim_id = "lim",
                        lim_title = "Limitations: Timely Publication",
                        lim_text = lim_timpub_tooltip5
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Reporting within 5 years",
                        value = timpubval5a,
                        value_text = timpubvaltext5a,
                        plot = plotlyOutput('plot_clinicaltrials_timpub_5a', height="300px"),
                        info_id = "infoTimPub5",
                        info_title = "Publication by 5 years",
                        info_text = timpub_tooltip2,
                        lim_id = "lim",
                        lim_title = "Limitations: Timely Publication",
                        lim_text = lim_timpub_tooltip5
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
            
        all_numer_oa <- iv_data %>%
            filter(
                color == "gold" | color == "green" | color == "hybrid"
                
            ) %>%
            nrow()

        all_denom_oa <- iv_data %>%
            filter(
                ! is.na(color)
                
            ) %>%
            nrow()
          
        closed_with_potential <- iv_data %>%
            filter(
                color_green_only == "closed",
                ! is.na(permission_postprint),
                permission_postprint == TRUE
            ) %>%
            nrow()
        
        greenoa_only <- iv_data %>%
            filter(
                color_green_only == "green",
                ) %>%
            nrow()
        
        denom_greenoa <- closed_with_potential + greenoa_only
        
        numer_greenoa <- greenoa_only
        
        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Open Access"), align = "left"),
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
                        value_text = "of publications are Open Access",
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
                )
                
            )
        )

    })

    output$allumc_openscience <- renderUI({

        ## Value for All UMC Open Access
        
        all_numer_oa <- iv_data %>%
            filter(
                color == "gold" | color == "green" | color == "hybrid"
            ) %>%
            nrow()

        all_denom_oa <- iv_data %>%
            filter(
                ! is.na(color)
                
            ) %>%
            nrow()
        
        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Open Access"), align = "left"),
            fluidRow(
                column(
                    12,
                    metric_box(
                        title = "Open Access",
                        value = paste0(round(100*all_numer_oa/all_denom_oa), "%"),
                        value_text = "of publications are Open Access",
                        plot = plotlyOutput('plot_allumc_openaccess', height="300px"),
                        info_id = "infoALLUMCOpenAccess",
                        info_title = "Open Access (All UMCs)",
                        info_text = allumc_openaccess_tooltip,
                        lim_id = "limALLUMCOpenAccess",
                        lim_title = "Limitations: Open Access (All UMCs)",
                        lim_text = lim_allumc_openaccess_tooltip
                    )
                )
            )
            
        )
    })

    output$allumc_clinicaltrials <- renderUI({

        ## Value for All UMC TRN

        all_numer_trn <- sum(iv_data$has_iv_trn_abstract, na.rm=TRUE)
        
        all_denom_trn <- iv_data %>%
            filter(! is.na(has_iv_trn_abstract)) %>%
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
                        title = "Reporting a Trial Registration Number in publications",
                        value = paste0(round(100*all_numer_trn/all_denom_trn), "%"),
                        value_text = "of clinical trials reported a trial registration number in the abstract",
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
                        title = "Summary Results Reporting in EUCTR",
                        value = paste0(round(100*all_numer_sumres/all_denom_sumres), "%"),
                        value_text = "of due clinical trials registered in EUCTR reported summary results",
                        plot = plotlyOutput('plot_allumc_clinicaltrials_sumres', height="300px"),
                        info_id = "infoALLUMCSumRes",
                        info_title = "Summary results reporting in EUCTR (All UMCs)",
                        info_text = allumc_clinicaltrials_sumres_tooltip,
                        lim_id = "limALLUMCSumRes",
                        lim_title = "Limitations: Summary results reporting in EUCTR (All UMCs)",
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
                        value_text = "of clinical trials were prospectively registered in ClinicalTrials.gov",
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

    color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A",
                     "#303A3E", "#007265", "#634587", "#000000",   #363457 #533A71 #011638 #634587
                     "#DCE3E5")
    
    color_palette_delwen <- c("#B6B6B6", "#879C9D", "#F1BA50", "#cf9188",  
                              "#303A3E", "#2f4858", "#158376", "#007265", 
                              "#DCE3E5", "#634587", "#000000", "#539d66")

    color_palette_bars <- c("#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587", "#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587", "#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587", "#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587", "#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587", "#AA1C7D")

                                        # Start page plots #
    
    ## Open Access plot
    output$plot_opensci_oa <- renderPlotly({
        return (plot_opensci_oa(iv_data, input$opensci_absnum, color_palette_delwen))
    })
    
    ## Green Open Access plot
    output$plot_opensci_green_oa <- renderPlotly({
        return (plot_opensci_green_oa(iv_data, input$opensci_absnum, color_palette_delwen))
    })
    
    ## TRN plot
    output$plot_clinicaltrials_trn <- renderPlotly({
        return (plot_clinicaltrials_trn(iv_data, color_palette))
    })

    output$plot_linkage <- renderPlotly({
        return (plot_linkage(iv_data, color_palette))
    })
    
    ## Summary results plot
    output$plot_clinicaltrials_sumres <- renderPlotly({
        return (plot_clinicaltrials_sumres(eutt_hist, color_palette))
    })
    
    ## Preregistration plot
    output$plot_clinicaltrials_prereg <- renderPlotly({
        return (plot_clinicaltrials_prereg(iv_data, color_palette))
    })
    
    ## Timely Publication plot 2a
    output$plot_clinicaltrials_timpub_2a <- renderPlotly({
        return (plot_clinicaltrials_timpub_2a(iv_data, color_palette))
    })
    
    ## Timely Publication plot 5a
    output$plot_clinicaltrials_timpub_5a <- renderPlotly({
        return (plot_clinicaltrials_timpub_5a(iv_data, color_palette))
    })

                                        # UMC page plots #

    ## Open Access plot
    output$umc_plot_opensci_oa <- renderPlotly({
        return (umc_plot_opensci_oa(iv_data, input$selectUMC, input$umc_opensci_absnum, color_palette_delwen))
    })
    
    ## Green Open Access plot
    output$umc_plot_opensci_green_oa <- renderPlotly({
        return (umc_plot_opensci_green_oa(iv_data, input$selectUMC, input$umc_opensci_absnum, color_palette_delwen))
    })
    
    ## TRN plot
    output$umc_plot_clinicaltrials_trn <- renderPlotly({
        return (umc_plot_clinicaltrials_trn(iv_data, input$selectUMC, color_palette))
    })
    
    ## Summary results plot
    output$umc_plot_clinicaltrials_sumres <- renderPlotly({
        return (umc_plot_clinicaltrials_sumres(eutt_hist, input$selectUMC, color_palette))
    })
    
    ## Preregistration plot
    output$umc_plot_clinicaltrials_prereg <- renderPlotly({
        return (umc_plot_clinicaltrials_prereg(iv_data, input$selectUMC, color_palette))
    })
    
    ## Timely Publication plot 2a
    output$umc_plot_clinicaltrials_timpub_2a <- renderPlotly({
        return (umc_plot_clinicaltrials_timpub_2a(iv_data, input$selectUMC, color_palette))
    })
    
    ## Timely Publication plot 5a
    output$umc_plot_clinicaltrials_timpub_5a <- renderPlotly({
        return (umc_plot_clinicaltrials_timpub_5a(iv_data, input$selectUMC, color_palette))
    })

                                        # All UMC's page plots #

    ## Open Science

    ## Open Access

    output$plot_allumc_openaccess <- renderPlotly({
        return(plot_allumc_openaccess(iv_data, color_palette))
    })

    ## Clinical Trials

    ## TRN

    output$plot_allumc_clinicaltrials_trn <- renderPlotly({
        return(plot_allumc_clinicaltrials_trn(iv_data, color_palette))
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
    
    output$data_table_eutt_data <- DT::renderDataTable({
        make_datatable(eutt_data)
    })

    output$data_table_iv_data <- DT::renderDataTable({
        make_datatable(iv_data)
    })
    
}

## Create Shiny object
shinyApp(ui, server)
