## Open Science plots

## Open Access
plot_allumc_openaccess <- function (dataset, color_palette) {

    dataset <- dataset %>%
        filter( ! is.na (color) )

    plot_data <- tribble (
        ~x_label, ~colour, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_gold <- dataset %>%
            filter(
                color == "gold",
                city == umc
            ) %>%
            nrow()

        umc_green <- dataset %>%
            filter(
                color == "green",
                city == umc
            ) %>%
            nrow()

        umc_hybrid <- dataset %>%
            filter(
                color == "hybrid",
                city == umc
            ) %>%
            nrow()

        umc_sum <- umc_gold + umc_green + umc_hybrid

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~colour, ~percentage, ~sum,
                    umc, "Gold", round(100*umc_gold/umc_denom), 100-round(100*umc_sum/umc_denom),
                    umc, "Green", round(100*umc_green/umc_denom), 100-round(100*umc_sum/umc_denom),
                    umc, "Hybrid", round(100*umc_hybrid/umc_denom), 100-round(100*umc_sum/umc_denom)
                )
            )
    
    }

    plot_ly(
        plot_data,
        x = ~reorder(x_label, sum),
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
            barmode = 'stack',
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


## Open Data

plot_allumc_opendata <- function (dataset, color_palette, color_palette_bars) {

    dataset <- dataset %>%
        filter(
            ! is.na(is_open_data),
            language == "English"
        )

    plot_data <- tribble (
        ~x_label, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_numer <- dataset %>%
            filter(
                city == umc,
                is_open_data == TRUE
            ) %>%
            nrow()

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage,
                    umc, round(100*umc_numer/umc_denom)
                )
            )
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette_bars,
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
                range = c(0, ceiling(max(plot_data$percentage)/5)*5)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Open Code

plot_allumc_opencode <- function (dataset, color_palette, color_palette_bars) {

    dataset <- dataset %>%
        filter(
            ! is.na(is_open_code),
            language == "English"
        )

    plot_data <- tribble (
        ~x_label, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_numer <- dataset %>%
            filter(
                city == umc,
                is_open_code == TRUE
            ) %>%
            nrow()

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage,
                    umc, round(100*umc_numer/umc_denom, digits=2)
                )
            )
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette_bars,
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
                range = c(0, ceiling(max(plot_data$percentage)/5)*5)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## TRN

plot_allumc_clinicaltrials_trn <- function (dataset, color_palette) {

    plot_data <- tribble (
        ~x_label, ~colour, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_numer_abs <- dataset %>%
            filter(city == umc) %>%
            select(has_iv_trn_abstract) %>%
            filter(has_iv_trn_abstract == TRUE) %>%
            nrow()

        umc_numer_ft <- dataset %>%
            filter(city == umc) %>%
            select(has_iv_trn_ft_pdf) %>%
            filter(has_iv_trn_ft_pdf == TRUE) %>%
            nrow()

        umc_numer_either <- dataset %>%
            filter(
                city == umc,
                has_iv_trn_abstract == TRUE | has_iv_trn_ft_pdf == TRUE
            ) %>%
            nrow()

        umc_ft_denom <- dataset %>%
            filter(city == umc) %>%
            filter(! is.na(has_iv_trn_ft_pdf)) %>%
            nrow()
        
        umc_abs_denom <- dataset %>%
            filter(city == umc) %>%
            filter(! is.na(has_iv_trn_abstract)) %>%
            nrow()
        
        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~colour, ~percentage, ~either,
                    umc, "In abstract", round(100*umc_numer_abs/umc_abs_denom), 100-round(100*umc_numer_either/umc_abs_denom),
                    umc, "In full-text", round(100*umc_numer_ft/umc_ft_denom), 100-round(100*umc_numer_either/umc_ft_denom)
                )
            )
    }

     plot_ly(
        plot_data,
        x = ~reorder(x_label,either),
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
                range = c(0, ceiling(max(plot_data$percentage)/5)*5)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Summary results

plot_allumc_clinicaltrials_sumres <- function (dataset, color_palette, color_palette_bars) {

    dataset <- dataset %>%
        filter( due_or_not == "Due" )

    plot_data <- tribble (
        ~x_label, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_numer <- dataset %>%
            filter(
                city == umc,
                status == "Reported results" |
                status == "Reported results Terminated"
            ) %>%
            nrow()

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage,
                    umc, round(100*umc_numer/umc_denom),
                )
            )
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

     plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette_bars,
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
                range = c(0, ceiling(max(plot_data$percentage)/5)*5)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Prospective registration

plot_allumc_clinicaltrials_prereg <- function (dataset, color_palette, color_palette_bars) {

    plot_data <- tribble (
        ~x_label, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_numer <- dataset %>%
            filter(
                city == umc,
                preregistered
            ) %>%
            nrow()

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage,
                    umc, round(100*umc_numer/umc_denom),
                )
            )
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

     plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette_bars,
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
                title = '<b>Prospective registration (%)</b>',
                range = c(0, ceiling(max(plot_data$percentage)/5)*5)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Timely reporting

plot_allumc_clinicaltrials_timpub <- function (dataset, color_palette, color_palette_bars) {

    plot_data <- tribble (
        ~x_label, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_numer <- dataset %>%
            filter(
                city == umc,
                published_2a
            ) %>%
            nrow()

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage,
                    umc, round(100*umc_numer/umc_denom),
                )
            )
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

     plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette_bars,
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
                title = '<b>Published within 2 years (%)</b>',
                range = c(0, ceiling(max(plot_data$percentage)/5)*5)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Animal studies

## Randomization
plot_allumc_animal_rando <- function (dataset, color_palette, color_palette_bars) {

    dataset <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        )

    plot_data <- tribble (
        ~x_label, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_numerator <- dataset %>%
            filter(
                city == umc
            )

        umc_numer <- umc_numerator$randomization %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage,
                    umc, round(100*umc_numer/umc_denom)
                )
            )
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette_bars,
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
                range = c(0, ceiling(max(plot_data$percentage)/5)*5)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Blinding
plot_allumc_animal_blind <- function (dataset, color_palette, color_palette_bars) {

    dataset <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        )

    plot_data <- tribble (
        ~x_label, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_numerator <- dataset %>%
            filter(
                city == umc
            )

        umc_numer <- umc_numerator$blinding %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage,
                    umc, round(100*umc_numer/umc_denom)
                )
            )
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette_bars,
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
                range = c(0, ceiling(max(plot_data$percentage)/5)*5)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Power calc
plot_allumc_animal_power <- function (dataset, color_palette, color_palette_bars) {

    dataset <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        )

    plot_data <- tribble (
        ~x_label, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_numerator <- dataset %>%
            filter(
                city == umc
            )

        umc_numer <- umc_numerator$power %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage,
                    umc, round(100*umc_numer/umc_denom)
                )
            )
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette_bars,
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
                range = c(0, ceiling(max(plot_data$percentage)/5)*5)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}


## IACUC
plot_allumc_animal_iacuc <- function (dataset, color_palette, color_palette_bars) {

    dataset <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        )

    plot_data <- tribble (
        ~x_label, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_numerator <- dataset %>%
            filter(
                city == umc
            )

        umc_numer <- umc_numerator$iacuc %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage,
                    umc, round(100*umc_numer/umc_denom)
                )
            )
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette_bars,
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
                range = c(0, ceiling(max(plot_data$percentage)/5)*5)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}
