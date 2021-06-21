## Define the page layout
umc_page <- tabPanel(
    "One UMC", value = "tabUMC",
    wellPanel(
        br(),
        fluidRow(
            column(
                12,
                h1(
                    style = "margin-left: 0",
                    strong("Proof-of-principle Responsible Metrics Dashboard: Individual UMC data"),
                    align = "left"
                ),
                h4(
                    style = "margin-left: 0",
                    "This dashboard provides an overview of the performance of one German University Medical Centres (UMCs) at a time on several metrics of open and responsible research. For more detailed information on the methods used to calculate those metrics, view the Methods page."
                ),
                h4(style = "margin-left:0cm",
                   "This dashboard is a pilot that is still under development, and should not be used to compare UMCs or inform policy. More metrics may be added in the future."),
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
                    strong("Select a UMC to view details"),
                    choices = umclist,
                    selected = NA
                )
            )
        )
    ),
    uiOutput("umc_registry_metrics"),
    uiOutput("umc_publication_metrics"),
    uiOutput("umc_openscience_metrics")
)
