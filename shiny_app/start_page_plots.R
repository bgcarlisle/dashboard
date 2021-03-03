## Open Science plots

## Open Access

plot_opensci_oa <- function (dataset, umc, absval, color_palette) {

    ## Calculate the numerators and the denominator for the
    ## "all" bars

    plot_data <- dataset %>%
        filter( ! is.na (color) )

    all_denom <- plot_data %>%
        nrow()

    all_gold <- plot_data %>%
        filter( color == "gold") %>%
        nrow()

    all_green <- plot_data %>%
        filter( color == "green") %>%
        nrow()

    all_hybrid <- plot_data %>%
        filter( color == "hybrid") %>%
        nrow()

    if ( umc != "all" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage

        umc_denom <- plot_data %>%
            filter(city == umc) %>%
            nrow()

        umc_gold <- plot_data %>%
            filter(
                color == "gold",
                city == umc
            ) %>%
            nrow()

        umc_green <- plot_data %>%
            filter(
                color == "green",
                city == umc
            ) %>%
            nrow()

        umc_hybrid <- plot_data %>%
            filter(
                color == "hybrid",
                city == umc
            ) %>%
            nrow()

        if (absval) {

            plot_data <- tribble(
                ~x_label, ~gold,    ~green,    ~hybrid,
                "All",    all_gold, all_green, all_hybrid,
                umc,      umc_gold, umc_green, umc_hybrid
            )
            
        } else {

            plot_data <- tribble(
                ~x_label, ~gold,                         ~green,                         ~hybrid,
                "All",    round(100*all_gold/all_denom), round(100*all_green/all_denom), round(100*all_hybrid/all_denom),
                umc,      round(100*umc_gold/umc_denom), round(100*umc_green/umc_denom), round(100*umc_hybrid/umc_denom)
            )
            
        }

        plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)

    } else {

        if (absval) {
            
            plot_data <- tribble(
                ~x_label, ~gold,    ~green,    ~hybrid,
                "All",    all_gold, all_green, all_hybrid
            )
            
        } else {
            
            plot_data <- tribble(
                ~x_label, ~gold,                         ~green,                         ~hybrid,
                "All",    round(100*all_gold/all_denom), round(100*all_green/all_denom), round(100*all_hybrid/all_denom)
            )

        }
        
    }

    if (absval) {
        upperlimit <- max(sum(all_gold, all_green, all_hybrid), sum(umc_gold, umc_green, umc_hybrid))
    } else {
        upperlimit <- 100
    }
    
    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~gold,
        name = "Gold",
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        add_trace(
            y = ~green,
            name = "Green",
            marker = list(
                color = color_palette[6],
                line = list(
                    color = 'rgb(0,0,0)',
                    width = 1.5
                )
            )
        ) %>%
        add_trace(
            y = ~hybrid,
            name = "Hybrid",
            marker = list(
                color = color_palette[7],
                line = list(
                    color = 'rgb(0,0,0)',
                    width = 1.5
                )
            )
        )%>%
        layout(
            barmode = 'stack',
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Percentage of publications</b>',
                range = c(0, upperlimit)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
}

## Open Data

plot_opensci_od <- function (dataset, umc, color_palette) {

    ## Remove non-analyzable and non-English data points
    plot_data <- dataset %>%
        filter(
            ! is.na(is_open_data),
            language == "English"
        )

    all_denom <- plot_data %>%
        nrow()

    all_numer <- plot_data$is_open_data %>%
        sum()

    if ( umc != "all" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage

        umc_denom <- plot_data %>%
            filter(city == umc) %>%
            nrow()

        umc_numer <- plot_data %>%
            filter(city == umc, is_open_data == TRUE) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom),
            capitalize(umc), round(100*umc_numer/umc_denom)
        )

        plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)
        
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )

    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Percentage of publications</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Open Code
plot_opensci_oc <- function (dataset, umc, color_palette) {

    ## Remove non-analyzable and non-English data points
    plot_data <- dataset %>%
        filter(
            ! is.na(is_open_code),
            language == "English"
        )

    all_denom <- plot_data %>%
        nrow()

    all_numer <- plot_data$is_open_code %>%
        sum()

    if ( umc != "all" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage

        umc_denom <- plot_data %>%
            filter(city == umc) %>%
            nrow()

        umc_numer <- plot_data %>%
            filter(city == umc, is_open_code == TRUE) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom),
            capitalize(umc), round(100*umc_numer/umc_denom)
        )

        plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)
        
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )

    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Percentage of publications</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Green open access
plot_opensci_green_oa <- function (dataset, umc, color_palette) {

    plot_data <- dataset %>%
        filter(
            color == "closed",
            ! is.na(permission_postprint)
        )

    all_denom <- plot_data %>%
        nrow()

    all_numer <- plot_data$permission_postprint %>%
        sum()

    if ( umc != "all" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage

        umc_denom <- plot_data %>%
            filter(city == umc) %>%
            nrow()

        umc_numer <- plot_data %>%
            filter(city == umc, permission_postprint == TRUE) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom),
            capitalize(umc), round(100*umc_numer/umc_denom)
        )

        plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)
        
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )

    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Percentage of publications</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Clinical Trials plots

## TRN

plot_clinicaltrials_trn <- function (dataset, umc, color_palette) {

    plot_data <- dataset %>%
        filter(is_human_ct == 1)

    all_denom <- plot_data %>%
        nrow()
    
    all_numer_abs <- plot_data %>%
        filter( ! is.na (abs_trn_1) ) %>%
        nrow()

    all_numer_si <- plot_data %>%
        filter( ! is.na (si_trn_1) ) %>%
        nrow()

    if ( umc != "all" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage

        umc_denom <- plot_data %>%
            filter(city == umc) %>%
            nrow()

        umc_numer_abs <- plot_data %>%
            filter(
                ! is.na(abs_trn_1),
                city == umc
            ) %>%
            nrow()

        umc_numer_si <- plot_data %>%
            filter(
                ! is.na(si_trn_1),
                city == umc
            ) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~colour, ~percentage,
            "All", "In abstract", round(100*all_numer_abs/all_denom),
            "All", "Secondary information", round(100*all_numer_si/all_denom),
            capitalize(umc), "In abstract", round(100*umc_numer_abs/umc_denom),
            capitalize(umc), "Secondary information", round(100*umc_numer_si/umc_denom),
        )

        plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)
        
    } else {

        plot_data <- tribble(
            ~x_label, ~colour, ~percentage,
            "All", "In abstract", round(100*all_numer_abs/all_denom),
            "All", "Secondary information", round(100*all_numer_si/all_denom)
        )
        
    }

    plot_ly(
        plot_data,
        x = ~x_label,
        color = ~colour,
        y = ~percentage,
        type = 'bar',
        colors = c(
            "#F1BA50",
            "#007265",
            "#634587"
        ),
        marker = list(
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>TRN reporting (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

# Summary results
plot_clinicaltrials_sumres <- function (dataset, umc, color_palette) {

    plot_data <- dataset %>%
        filter(due_or_not == "Due")

    all_denom <- plot_data %>%
        nrow()
    
    all_numer <- plot_data %>%
        filter(
            status == "Reported results" |
            status == "Reported results Terminated"
        ) %>%
        nrow()

    if ( umc != "all" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage

        umc_denom <- plot_data %>%
            filter(city == umc) %>%
            nrow()

        umc_numer <- plot_data %>%
            filter(
                status == "Reported results" |
                status == "Reported results Terminated",
                city == umc
            ) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom),
            capitalize(umc), round(100*umc_numer/umc_denom),
        )

        plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)
        
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )
        
    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Summary results reporting (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

# Prospective registration
plot_clinicaltrials_prereg <- function (dataset, umc, color_palette) {

    dataset$year <- dataset$completion_date %>%
        format("%Y")

    years <- seq(from=min(dataset$year), to=max(dataset$year))

    if ( umc != "all" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage for each year

        plot_data <- tribble(
            ~year, ~umc_percentage, ~all_percentage
        )

        for (current_year in years) {

            numer_for_year <- dataset %>%
                filter(
                    city == umc,
                    year == current_year,
                    preregistered
                ) %>%
                nrow()

            denom_for_year <- dataset %>%
                filter(
                    city == umc,
                    year == current_year
                ) %>%
                nrow()

            all_numer_for_year <-  dataset %>%
                filter(
                    year == current_year,
                    preregistered
                ) %>%
                nrow()

            all_denom_for_year <- dataset %>%
                filter(
                    year == current_year
                ) %>%
                nrow()

            percentage_for_year <- 100*numer_for_year/denom_for_year

            all_percentage_for_year <- 100*all_numer_for_year/all_denom_for_year
            
            plot_data <- plot_data %>%
                bind_rows(
                    tribble(
                        ~year, ~umc_percentage, ~all_percentage,
                        current_year, percentage_for_year, all_percentage_for_year
                    )
                )
            
        }

        plot_ly(
            plot_data,
            x = ~year,
            y = ~umc_percentage,
            name = umc,
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(
                color = color_palette[3],
                line = list(
                    color = 'rgb(0,0,0)',
                    width = 1.5
                )
            )
        ) %>%
            add_trace(
                y=~all_percentage,
                name='All',
                marker = list(color = color_palette[2])
                ) %>%
                    layout(
                        xaxis = list(
                            title = '<b>UMC</b>',
                            dtick = 1
                        ),
                        yaxis = list(
                            title = '<b>Prospective registration (%)</b>',
                            range = c(0, 100)
                        ),
                        paper_bgcolor = color_palette[9],
                        plot_bgcolor = color_palette[9],
                        legend = list(xanchor= "right")
                    )
                
    } else {

        plot_data <- tribble(
            ~year, ~percentage
        )

        for (current_year in years) {

            numer_for_year <- dataset %>%
                filter(
                    year == current_year,
                    preregistered
                ) %>%
                nrow()

            denom_for_year <- dataset %>%
                filter(
                    year == current_year
                ) %>%
                nrow()

            percentage_for_year <- 100*numer_for_year/denom_for_year
            
            plot_data <- plot_data %>%
                bind_rows(
                    tribble(
                        ~year, ~percentage,
                        current_year, percentage_for_year
                    )
                )
            
        }
        
        plot_ly(
            plot_data,
            x = ~year,
            y = ~percentage,
            name = umc,
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(
                color = color_palette[3],
                line = list(
                    color = 'rgb(0,0,0)',
                    width = 1.5
                )
            )
        ) %>%
            layout(
                yaxis = list(
                    title = '<b>Prospective registration (%)</b>',
                    range = c(0, 100)
                ),
                xaxis = list(
                    title = '<b>UMC</b>',
                    dtick = 1
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9],
                legend = list(xanchor= "right")
            )

    }
    
}

# Timely publication
plot_clinicaltrials_timpub <- function (dataset, umc, color_palette) {

    dataset$year <- dataset$completion_date %>%
        format("%Y")

    years <- seq(from=min(dataset$year), to=max(dataset$year))

    all_denom <- dataset %>%
        nrow()
    
    all_numer <- dataset %>%
        filter(published_2a) %>%
        nrow()

    if ( umc != "all" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage

        plot_data <- tribble(
            ~year, ~umc_percentage, ~all_percentage
        )

        for (current_year in years) {

            umc_numer <-  dataset %>%
                filter(
                    city == umc,
                    year == current_year,
                    published_2a
                ) %>%
                nrow()

            umc_denom <-  dataset %>%
                filter(
                    city == umc,
                    year == current_year
                ) %>%
                nrow()

            all_numer <-  dataset %>%
                filter(
                    year == current_year,
                    published_2a
                ) %>%
                nrow()

            all_denom <-  dataset %>%
                filter(
                    year == current_year
                ) %>%
                nrow()

            umc_percentage <- 100*umc_numer/umc_denom
            all_percentage <- 100*all_numer/all_denom

            plot_data <- plot_data %>%
                bind_rows(
                    tribble(
                        ~year, ~umc_percentage, ~all_percentage,
                        current_year, umc_percentage, all_percentage
                    )
                )
            
        }

        plot_ly(
            plot_data,
            x = ~year,
            y = ~umc_percentage,
            name = umc,
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(
                color = color_palette[3],
                line = list(
                    color = 'rgb(0,0,0)',
                    width = 1.5
                )
            )
        ) %>%
            add_trace(
                y=~all_percentage,
                name='All',
                marker = list(color = color_palette[2])
            ) %>%
            layout(
                xaxis = list(
                    title = '<b>UMC</b>',
                    dtick = 1
                ),
                yaxis = list(
                    title = '<b>Published within 2 years (%)</b>',
                    range = c(0, 100)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9],
                legend = list(xanchor= "right")
            )
        
    } else {
        plot_data <- tribble(
            ~year, ~all_percentage
        )

        for (current_year in years) {

            all_numer <-  dataset %>%
                filter(
                    year == current_year,
                    published_2a
                ) %>%
                nrow()

            all_denom <-  dataset %>%
                filter(
                    year == current_year
                ) %>%
                nrow()
            all_percentage <- 100*all_numer/all_denom

            plot_data <- plot_data %>%
                bind_rows(
                    tribble(
                        ~year, ~all_percentage,
                        current_year, all_percentage
                    )
                )
            
        }

        plot_ly(
            plot_data,
            x = ~year,
            y = ~all_percentage,
            name = umc,
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(
                color = color_palette[3],
                line = list(
                    color = 'rgb(0,0,0)',
                    width = 1.5
                )
            )
        ) %>%
            layout(
                xaxis = list(
                    title = '<b>UMC</b>',
                    dtick = 1
                ),
                yaxis = list(
                    title = '<b>Published within 2 years (%)</b>',
                    range = c(0, 100)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9],
                legend = list(xanchor= "right")
            )
        
        
    }
    
}

## Robustness plots

## Randomisation
plot_randomization <- function (dataset, umc, color_palette) {

    ## Calculate the numerator and denominator for the
    ## "all" bar

    all_numer <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        select(randomization) %>%
        sum(na.rm=TRUE)

    all_denom <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        nrow()

    if ( umc != "all" ) {

        ## If the selected UMC is not "all," calculate
        ## the percentage 
        
        umc_numerator <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            )

        umc_numer <- umc_numerator$randomization %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            capitalize(umc), round(100*umc_numer/umc_denom),
            "All", round(100*all_numer/all_denom)
        )

        plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)
        
        ## message(umc)
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )
        
        ## message("umc not set")
    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Randomized (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Blinding
plot_blinding <- function (dataset, umc, color_palette) {

    ## Calculate the numerator and denominator for the
    ## "all" bar

    all_numer <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        select(blinding) %>%
        sum(na.rm=TRUE)

    all_denom <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        nrow()

    if ( umc != "all" ) {

        ## If the selected UMC is not "all," calculate
        ## the percentage 
        
        umc_numerator <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            )

        umc_numer <- umc_numerator$blinding %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            capitalize(umc), round(100*umc_numer/umc_denom),
            "All", round(100*all_numer/all_denom)
        )

        plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)
        
        ## message(umc)
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )
        
        ## message("umc not set")
    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Blinded (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )

}

## Power calc
plot_power <- function (dataset, umc, color_palette) {

    ## Calculate the numerator and denominator for the
    ## "all" bar

    all_numer <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        select(power) %>%
        sum(na.rm=TRUE)

    all_denom <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        nrow()

    if ( umc != "all" ) {

        ## If the selected UMC is not "all," calculate
        ## the percentage 
        
        umc_numerator <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            )

        umc_numer <- umc_numerator$power %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            capitalize(umc), round(100*umc_numer/umc_denom),
            "All", round(100*all_numer/all_denom)
        )

        plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)
        
        ## message(umc)
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )
        
        ## message("umc not set")
    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Reporting a Power Calculation (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )

}

## IACUC
plot_iacuc <- function (dataset, umc, color_palette) {

    ## Calculate the numerator and denominator for the
    ## "all" bar

    all_numer <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        select(iacuc) %>%
        sum(na.rm=TRUE)

    all_denom <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        nrow()

    if ( umc != "all" ) {

        ## If the selected UMC is not "all," calculate
        ## the percentage 
        
        umc_numerator <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            )

        umc_numer <- umc_numerator$iacuc %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            capitalize(umc), round(100*umc_numer/umc_denom),
            "All", round(100*all_numer/all_denom)
        )
        
        ## message(umc)
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )
        
        ## message("umc not set")
    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Reporting an IACUC statement (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )

}
