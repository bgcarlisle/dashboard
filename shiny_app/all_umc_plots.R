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

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~colour, ~percentage,
                    capitalize(umc), "Gold", round(100*umc_gold/umc_denom),
                    capitalize(umc), "Green", round(100*umc_green/umc_denom),
                    capitalize(umc), "Hybrid", round(100*umc_hybrid/umc_denom)
                )
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
                title = '<b>Open Access (%)</b>',
                range = c(0, round(max(plot_data$percentage)/5)*5)
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
                    capitalize(umc), round(100*umc_numer/umc_denom)
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
                title = '<b>Open Data (%)</b>',
                range = c(0, round(max(plot_data$percentage)/5)*5)
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
                    capitalize(umc), round(100*umc_numer/umc_denom, digits=2)
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
                title = '<b>Open Code (%)</b>',
                range = c(0, round(max(plot_data$percentage)/5)*5)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## TRN

plot_allumc_clinicaltrials_trn <- function (dataset, color_palette) {

    dataset <- dataset %>%
        filter( is_human_ct == 1 )

    plot_data <- tribble (
        ~x_label, ~colour, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_numer_abs <- dataset %>%
            filter(
                ! is.na(abs_trn_1),
                city == umc
            ) %>%
            nrow()

        umc_numer_si <- dataset %>%
            filter(
                ! is.na(si_trn_1),
                city == umc
            ) %>%
            nrow()

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~colour, ~percentage,
                    capitalize(umc), "In abstract", round(100*umc_numer_abs/umc_denom),
                    capitalize(umc), "Secondary information", round(100*umc_numer_si/umc_denom),
                )
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
                range = c(0, round(max(plot_data$percentage)/5)*5)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}
