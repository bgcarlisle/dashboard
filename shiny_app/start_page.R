start_page <- tabPanel(
    "Start page",
    value = "tabStart",
    ## The following are defined in app.R in the Shiny server object
    uiOutput("startpage"),
    uiOutput("openscience_metrics"),
    uiOutput("clinicaltrials_metrics"),
    uiOutput("robustness_metrics")
)

