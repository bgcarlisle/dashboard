report_card_page <- tabPanel(
    "Report card", value = "tabReportcard",
    h1("Report card"),
    textInput(
        inputId="trn",
        label=strong("Trial registry number"),
        placeholder="NCT00000000 or DRKS00000000"
    ),
    uiOutput("reportcard")
)
