allumc_openaccess_tooltip <- strwrap("This metric ...")

allumc_opendata_tooltip <- strwrap("This metric ...")

allumc_opencode_tooltip <- strwrap("This metric ...")

allumc_clinicaltrials_trn_tooltip <- strwrap("This metric ...")

## Define the page layout
all_umcs_page <- tabPanel(
    "All UMC's", value = "tabAllUMCs",
    wellPanel(
        br(),
        fluidRow(
            column(
                12,
                h1(
                    style = "margin-left: 0",
                    strong("Responsible Metrics Dashboard: All UMC's"),
                    align = "left"
                ),
                h4(
                    style = "margin-left: 0",
                    "This dashboard provides an overview of the relative performance of several German University Medical Centres (UMC's) on several metrics of open and responsible research. For more detailed information on the methods used to calculate those metrics, the dataset underlying the metrics, or resources to improve your own research practices, click one of the following buttons."
                ),
                h4(style = "margin-left:0cm",
                   "This dashboard is a pilot that is still under development, and should not be used to compare UMC's or inform policy. More metrics may be added in the future."),
                br()
            )
        )
    ),
    uiOutput("allumc_openaccess"),
    uiOutput("allumc_opendata"),
    uiOutput("allumc_opencode"),
    uiOutput("allumc_clinicaltrials_trn")
)
