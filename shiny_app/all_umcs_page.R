allumc_openaccess_tooltip <- strwrap("This metric ...")

allumc_opendata_tooltip <- strwrap("This metric ...")

allumc_opencode_tooltip <- strwrap("This metric ...")

allumc_clinicaltrials_trn_tooltip <- strwrap("This metric ...")

allumc_clinicaltrials_sumres_tooltip <- strwrap("This metric ...")

allumc_clinicaltrials_prereg_tooltip <- strwrap("This metric ...")

allumc_clinicaltrials_timpub_tooltip <- strwrap("This metric ...")

allumc_animal_rando_tooltip <- strwrap("This metric ...")

allumc_animal_blind_tooltip <- strwrap("This metric ...")

allumc_animal_power_tooltip <- strwrap("This metric ...")

allumc_animal_iacuc_tooltip <- strwrap("This metric ...")

## Define the page layout
all_umcs_page <- tabPanel(
    "All UMCs", value = "tabAllUMCs",
    wellPanel(
        br(),
        fluidRow(
            column(
                12,
                h1(
                    style = "margin-left: 0",
                    strong("Proof-of-principle Responsible Metrics Dashboard: All UMCs"),
                    align = "left"
                ),
                h4(
                    style = "margin-left: 0",
                    "This dashboard provides an overview of the relative performance of several German University Medical Centres (UMCs) on several metrics of open and responsible research. For more detailed information on the methods used to calculate those metrics, click one of the following buttons."
                ),
                h4(style = "margin-left:0cm",
                   "This dashboard is a pilot that is still under development, and should not be used to compare UMCs or inform policy. More metrics may be added in the future."),
                br()
            )
        )
    ),
    uiOutput("allumc_openscience"),
    uiOutput("allumc_clinicaltrials"),
    uiOutput("allumc_robustness")
)
