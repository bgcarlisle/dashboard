make_datatable <- function(dataset) {
    DT::datatable(
            data = dataset,
            extensions = 'Buttons',
            filter = 'top',
            options = list(
                dom = 'Blfrtip',
                buttons =
                    list(
                        list(
                            extend = "collection",
                            buttons = c("csv", "excel"),
                            text = "Download"
                        )
                    ),
                orderClasses = TRUE,
                pageLength = 20,
                lengthMenu = list(
                    c(10, 20, 50, 100, -1),
                    c(10, 20, 50, 100, "All")
                )
            )
        )
}

datasets_page <- tabPanel(
    "Datasets", value = "tabDatasets",
    h3("Datasets"),
    ## bsCollapse(
    ##     id="datasetPanels_rm_data",
    ##     bsCollapsePanel(
    ##         strong("UMC publications data set"),
    ##         DT::dataTableOutput("data_table_rm_data"),
    ##         style="default"
    ##     )
    ## ),
    ## bsCollapse(
    ##     id="datasetPanels_eutt_data",
    ##     bsCollapsePanel(
    ##         strong("EU Trials Tracker data set"),
    ##         DT::dataTableOutput("data_table_eutt_data"),
    ##         style="default"
    ##     )
    ## ),
    ## bsCollapse(
    ##     id="datasetPanels_iv_data",
    ##     bsCollapsePanel(
    ##         strong("IntoValue 1 and 2"),
    ##         DT::dataTableOutput("data_table_iv_data"),
    ##         style="default"
    ##     )
    ## )
    p("Where possible, we will share the publication and metric data on this page. All scripts we developed for this dashboard will be made openly accessible in a suitable repository.")
)
