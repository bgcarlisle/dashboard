## Open Science plots

## Open Access

## TODO Open Access: reviews not currently in pop dataset, but Open Access should include both articles and reviews.

plot_opensci_oa <- function (dataset, absnum, color_palette) {

    umc <- "All"

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

    all_na <- dataset %>%
        filter( is.na(color) ) %>%
        nrow()

    all_closed <- plot_data %>%
        filter( color == "closed") %>%
        nrow()

    all_bronze <- plot_data %>%
        filter( color == "bronze") %>%
        nrow()

    if ( umc != "All" ) {
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

        umc_na <- dataset %>%
            filter(
                is.na(color),
                city == umc
            ) %>%
            nrow()

        umc_closed <- plot_data %>%
            filter(
                color == "closed",
                city == umc
            ) %>%
            nrow()

        umc_bronze <- plot_data %>%
            filter(
                color == "bronze",
                city == umc
            ) %>%
            nrow()

        if (absnum) {

            plot_data <- tribble(
                ~x_label, ~gold,    ~green,    ~hybrid,    ~na,    ~closed,    ~bronze,
                umc,      umc_gold, umc_green, umc_hybrid, umc_na, umc_closed, umc_bronze
            )

            upperlimit <- 1.1*sum(umc_gold, umc_green, umc_hybrid, umc_na, umc_closed, umc_bronze)
            ylabel <- "Number of publications"
            
        } else {

            plot_data <- tribble(
                ~x_label, ~gold,                         ~green,                         ~hybrid,
                "All",    round(100*all_gold/all_denom), round(100*all_green/all_denom), round(100*all_hybrid/all_denom),
                umc,      round(100*umc_gold/umc_denom), round(100*umc_green/umc_denom), round(100*umc_hybrid/umc_denom)
            )

            plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)

            upperlimit <- 100
            ylabel <- "Percentage of publications"
            
        }

    } else {

        if (absnum) {
            
            plot_data <- tribble(
                ~x_label, ~gold,    ~green,    ~hybrid,    ~na,    ~closed,    ~bronze,
                "All",    all_gold, all_green, all_hybrid, all_na, all_closed, all_bronze
            )

            upperlimit <- 1.1*sum(all_gold, all_green, all_hybrid, all_na, all_closed, all_bronze)
            ylabel <- "Number of publications"
            
        } else {
            
            plot_data <- tribble(
                ~x_label, ~gold,                         ~green,                         ~hybrid,
                "All",    round(100*all_gold/all_denom), round(100*all_green/all_denom), round(100*all_hybrid/all_denom)
            )

            upperlimit <- 100
            ylabel <- "Percentage of publications"

        }
        
    }

    if (absnum) {
        
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
                    color = color_palette[8],
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
                    color = color_palette[10],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~bronze,
                name = "Bronze",
                marker = list(
                    color = color_palette[4],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~closed,
                name = "Closed",
                marker = list(
                    color = color_palette[1],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~na,
                name = "No data",
                marker = list(
                    color = color_palette[11],
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
                    title = paste('<b>', ylabel, '</b>'),
                    range = c(0, upperlimit)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9]
            )
        
    } else {
        
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
                    color = color_palette[8],
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
                    color = color_palette[10],
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
                    title = paste('<b>', ylabel, '</b>'),
                    range = c(0, upperlimit)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9]
            )
        
    }
    
}

## Open Data

## TODO: add filter for Articles if the pop dataset contains Articles and Reviews

plot_opensci_od <- function (dataset, umc, absnum, color_palette) {

    ## Remove non-analyzable and non-English data points
    plot_data <- dataset %>%
        filter(
            ! is.na(is_open_data),
            language == "English"
        )

    all_denom <- plot_data %>%
        nrow()

    ## has data sharing
    all_numer <- plot_data$is_open_data %>%
        sum()

    ## non-English publication or no language information
    all_non_eng <- dataset %>%
        filter(
            language != "English" | is.na(language)
        ) %>%
        nrow()

    ## no full text
    all_no_ft <- dataset %>%
        filter(
            language == "English",
            is.na(is_open_data)
        ) %>%
        nrow()

    all_no_data_sharing <- dataset %>%
        filter(
            language == "English",
            ! is_open_data
        ) %>%
        nrow()

    if ( umc != "All" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage

        umc_denom <- plot_data %>%
            filter(city == umc) %>%
            nrow()

        umc_numer <- plot_data %>%
            filter(city == umc, is_open_data == TRUE) %>%
            nrow()

        umc_non_eng <- dataset %>%
            filter(
                city == umc,
                language != "English" | is.na(language)
            ) %>%
            nrow()

        ## no full text
        umc_no_ft <- dataset %>%
            filter(
                city == umc,
                language == "English",
                is.na(is_open_data)
            ) %>%
            nrow()

        umc_no_data_sharing <- dataset %>%
            filter(
                city == umc,
                language == "English",
                ! is_open_data
            ) %>%
            nrow()

        if (absnum) {

            plot_data <- tribble(
                ~x_label, ~percentage, ~non_eng,    ~no_ft,    ~no_data_sharing,
                umc,      umc_numer,   umc_non_eng, umc_no_ft, umc_no_data_sharing
            )

            upperlimit <- 1.1*sum(umc_numer,   umc_non_eng, umc_no_ft, umc_no_data_sharing)
            ylabel <- "Number of articles"
            
        } else {

            plot_data <- tribble(
                ~x_label, ~percentage,
                "All", round(100*all_numer/all_denom),
                umc, round(100*umc_numer/umc_denom)
            )
        
            plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)

            upperlimit <- 100
            ylabel <- "Percentage of articles"

        }
        
    } else { ## "All" is selected

        if (absnum) {

            plot_data <- tribble(
                ~x_label, ~percentage, ~non_eng,    ~no_ft,    ~no_data_sharing,
                "All",    all_numer,   all_non_eng, all_no_ft, all_no_data_sharing
            )

            upperlimit <- 1.1*sum(all_numer,   all_non_eng, all_no_ft, all_no_data_sharing)
            ylabel <- "Number of articles"
            
        } else {

            plot_data <- tribble(
                ~x_label, ~percentage,
                "All", round(100*all_numer/all_denom)
            )

            upperlimit <- 100
            ylabel <- "Percentage of articles"
            
        }

    }

    if (absnum) {

        plot_ly(
            plot_data,
            x = ~x_label,
            y = ~percentage,
            name = "Any Open Data",
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
                y = ~no_data_sharing,
                name = "No Open Data",
                marker = list(
                    color = color_palette[12],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~no_ft,
                name = "No full text",
                marker = list(
                    color = color_palette[7],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~non_eng,
                name = "Non-English/NA",
                marker = list(
                    color = color_palette[6],
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
                    title = paste('<b>', ylabel, '</b>'),
                    range = c(0, upperlimit)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9]
            )
        
    } else {

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
                title = paste('<b>', ylabel, '</b>'),
                range = c(0, upperlimit)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
        
    }
    
}

## Open Code

## TODO: add filter for Articles if the pop dataset contains Articles and Reviews

plot_opensci_oc <- function (dataset, umc, absnum, color_palette) {

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

    all_non_eng <- dataset %>%
        filter(
            language != "English" | is.na(language)
        ) %>%
        nrow()

    all_no_ft <- dataset %>%
        filter(
            language == "English",
            is.na(is_open_code)
        ) %>%
        nrow()

    all_no_code_sharing <- dataset %>%
        filter(
            language == "English",
            ! is_open_code
        ) %>%
        nrow()

    if ( umc != "All" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage

        umc_denom <- plot_data %>%
            filter(city == umc) %>%
            nrow()

        umc_numer <- plot_data %>%
            filter(city == umc, is_open_code == TRUE) %>%
            nrow()

        umc_non_eng <- dataset %>%
            filter(
                city == umc,
                language != "English" | is.na(language)
            ) %>%
            nrow()

        umc_no_ft <- dataset %>%
            filter(
                city == umc,
                language == "English",
                is.na(is_open_code)
            ) %>%
            nrow()

        umc_no_code_sharing <- dataset %>%
            filter(
                city == umc,
                language == "English",
                !is_open_code
            ) %>%
            nrow()

        if (absnum) {

            plot_data <- tribble(
                ~x_label, ~percentage, ~non_eng,    ~no_ft,    ~no_code_sharing,
                umc,      umc_numer,   umc_non_eng, umc_no_ft, umc_no_code_sharing
            )

            upperlimit <- 1.1*sum(umc_numer, umc_non_eng, umc_no_ft, umc_no_code_sharing)
            ylabel <- "Number of articles"
            
        } else {

            plot_data <- tribble(
                ~x_label, ~percentage,
                "All", round(100*all_numer/all_denom),
                umc, round(100*umc_numer/umc_denom)
            )

            plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)

            upperlimit <- 100
            ylabel <- "Percentage of articles"
        }
        
    } else {

        if (absnum) {

            plot_data <- tribble(
                ~x_label, ~percentage, ~non_eng,    ~no_ft,    ~no_code_sharing,
                "All", all_numer,   all_non_eng, all_no_ft, all_no_code_sharing
            )

            upperlimit <- 1.1*sum(all_numer,   all_non_eng, all_no_ft, all_no_code_sharing)
            ylabel <- "Number of articles"
            
        } else {
            
            plot_data <- tribble(
                ~x_label, ~percentage,
                "All", round(100*all_numer/all_denom)
            )

            upperlimit <- 100
            ylabel <- "Percentage of articles"
            
        }

    }

    if (absnum) {

        plot_ly(
            plot_data,
            x = ~x_label,
            y = ~percentage,
            name = "Any Open Code",
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
                y = ~no_code_sharing,
                name = "No Open Code",
                marker = list(
                    color = color_palette[12],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~no_ft,
                name = "No full text",
                marker = list(
                    color = color_palette[7],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~non_eng,
                name = "Non-English/NA",
                marker = list(
                    color = color_palette[6],
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
                    title = paste('<b>', ylabel, '</b>'),
                    range = c(0, upperlimit)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9]
            )
        
    } else {

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
                    title = paste('<b>', ylabel, '</b>'),
                    range = c(0, upperlimit)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9]
            )
    }    
}

## Green open access
# plot_opensci_green_oa <- function (dataset, umc, absnum, color_palette) {
# 
#     plot_data <- dataset %>%
#         filter(
#             color == "closed",
#             ! is.na(permission_postprint)
#         )
# 
#     all_denom <- plot_data %>%
#         nrow()
# 
#     all_numer <- plot_data$permission_postprint %>%
#         sum()
# 
#     all_cant_archive <- dataset %>%
#         filter(
#             ! is.na(permission_postprint),
#             color == "closed",
#             ! permission_postprint
#         ) %>%
#         nrow()
# 
#     all_no_data <- dataset %>%
#         filter(
#             color == "closed",
#             is.na(permission_postprint)
#         ) %>%
#         nrow()
# 
#     if ( umc != "All" ) {
#         ## If the selected UMC is not "all," calculate
#         ## the percentage
# 
#         umc_denom <- plot_data %>%
#             filter(city == umc) %>%
#             nrow()
# 
#         umc_numer <- plot_data %>%
#             filter(city == umc, permission_postprint == TRUE) %>%
#             nrow()
# 
#         umc_cant_archive <- dataset %>%
#             filter(
#                 city == umc,
#                 ! is.na(permission_postprint),
#                 color == "closed",
#                 ! permission_postprint
#             ) %>%
#             nrow()
# 
#         umc_no_data <- dataset %>%
#             filter(
#                 city == umc,
#                 color == "closed",
#                 is.na(permission_postprint)
#             ) %>%
#             nrow()
# 
#         if (absnum) {
# 
#             plot_data <- tribble(
#                 ~x_label, ~percentage, ~cant_archive,    ~no_data,
#                 umc,      umc_numer,   umc_cant_archive, umc_no_data
#             )
# 
#             upperlimit <- 1.1 * sum(umc_numer, umc_cant_archive, umc_no_data)
#             ylabel <- "Number of publications"
#             
#         } else {
# 
#             plot_data <- tribble(
#                 ~x_label, ~percentage,
#                 "All", round(100*all_numer/all_denom),
#                 umc, round(100*umc_numer/umc_denom)
#             )
# 
#             plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)
# 
#             upperlimit <- 100
#             ylabel <- "Percentage of publications"
#             
#         }
#         
#     } else {
# 
#         if (absnum) {
# 
#             plot_data <- tribble(
#                 ~x_label, ~percentage, ~cant_archive,    ~no_data,
#                 "All",    all_numer,   all_cant_archive, all_no_data
#             )
# 
#             upperlimit <- 1.1 * sum(all_numer, all_cant_archive, all_no_data)
#             ylabel <- "Number of publications"
#             
#         } else {
# 
#             plot_data <- tribble(
#                 ~x_label, ~percentage,
#                 "All", round(100*all_numer/all_denom)
#             )
# 
#             upperlimit <- 100
#             ylabel <- "Percentage of publications"
#             
#         }
# 
#     }
# 
#     if (absnum) {
# 
#         plot_ly(
#             plot_data,
#             x = ~x_label,
#             y = ~percentage,
#             name = "Permitted",
#             type = 'bar',
#             marker = list(
#                 color = color_palette[3],
#                 line = list(
#                     color = 'rgb(0,0,0)',
#                     width = 1.5
#                 )
#             )
#         ) %>%
#             add_trace(
#                 y = ~cant_archive,
#                 name = "Not permitted",
#                 marker = list(
#                     color = color_palette[12],
#                     line = list(
#                         color = 'rgb(0,0,0)',
#                         width = 1.5
#                     )
#                 )
#             ) %>%
#             add_trace(
#                 y = ~no_data,
#                 name = "No data",
#                 marker = list(
#                     color = color_palette[7],
#                     line = list(
#                         color = 'rgb(0,0,0)',
#                         width = 1.5
#                     )
#                 )
#             ) %>%
#             layout(
#                 barmode = 'stack',
#                 xaxis = list(
#                     title = '<b>UMC</b>'
#                 ),
#                 yaxis = list(
#                     title = paste('<b>', ylabel, '</b>'),
#                     range = c(0, upperlimit)
#                 ),
#                 paper_bgcolor = color_palette[9],
#                 plot_bgcolor = color_palette[9]
#             )
#         
#     } else {
# 
#         plot_ly(
#             plot_data,
#             x = ~x_label,
#             y = ~percentage,
#             type = 'bar',
#             marker = list(
#                 color = color_palette[3],
#                 line = list(
#                     color = 'rgb(0,0,0)',
#                     width = 1.5
#                 )
#             )
#         ) %>%
#             layout(
#                 xaxis = list(
#                     title = '<b>UMC</b>'
#                 ),
#                 yaxis = list(
#                     title = paste('<b>', ylabel, '</b>'),
#                     range = c(0, upperlimit)
#                 ),
#                 paper_bgcolor = color_palette[9],
#                 plot_bgcolor = color_palette[9]
#             )
#         
#     }
#     
# }

plot_opensci_green_oa <- function (dataset, absnum, color_palette) {

    umc <- "All"

    all_closed_with_potential <- dataset %>%
        filter(
            color_green_only == "closed",
            ! is.na(permission_postprint),
            permission_postprint == TRUE
        ) %>%
        nrow()
    
    all_greenoa_only <- dataset %>%
        filter(
            color_green_only == "green"
        ) %>%
        nrow()
    
    all_denom <- all_closed_with_potential + all_greenoa_only
    
    all_numer <- all_greenoa_only
    
    all_can_archive <- all_closed_with_potential
    
    all_cant_archive <- dataset %>%
        filter(
            ! is.na(permission_postprint),
            color_green_only == "closed",
            ! permission_postprint
        ) %>%
        nrow()
    
    all_no_data <- dataset %>%
        filter(
            color_green_only == "closed",
            is.na(permission_postprint)
        ) %>%
        nrow()
    
    if ( umc != "All" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage
        
        umc_closed_with_potential <- dataset %>%
            filter(
                city == umc,
                color_green_only == "closed",
                ! is.na(permission_postprint),
                permission_postprint == TRUE
            ) %>%
            nrow()
        
        umc_greenoa_only <- dataset %>%
            filter(
                city == umc,
                color_green_only == "green"
            ) %>%
            nrow()
        
        umc_denom <- umc_closed_with_potential + umc_greenoa_only
        
        umc_numer <- umc_greenoa_only
        
        umc_can_archive <- umc_closed_with_potential
        
        umc_cant_archive <- dataset %>%
            filter(
                city == umc,
                ! is.na(permission_postprint),
                color_green_only == "closed",
                ! permission_postprint
            ) %>%
            nrow()
        
        umc_no_data <- dataset %>%
            filter(
                city == umc,
                color_green_only == "closed",
                is.na(permission_postprint)
            ) %>%
            nrow()
        
        if (absnum) {
            
            plot_data <- tribble(
                ~x_label, ~percentage, ~can_archive,   ~cant_archive,    ~no_data,
                umc,      umc_numer,   umc_can_archive, umc_cant_archive, umc_no_data
            )
            
            upperlimit <- 1.1 * sum(umc_numer, umc_can_archive, umc_cant_archive, umc_no_data)
            ylabel <- "Number of publications"
            
        } else {
            
            plot_data <- tribble(
                ~x_label, ~percentage,
                "All", round(100*all_numer/all_denom),
                umc, round(100*umc_numer/umc_denom)
            )
            
            plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)
            
            upperlimit <- 100
            ylabel <- "Percentage of publications"
            
        }
        
    } else {
        
        if (absnum) {
            
            plot_data <- tribble(
                ~x_label, ~percentage, ~can_archive,   ~cant_archive,    ~no_data,
                "All",    all_numer,   all_can_archive, all_cant_archive, all_no_data
            )
            
            upperlimit <- 1.1 * sum(all_numer, all_can_archive, all_cant_archive, all_no_data)
            ylabel <- "Number of publications"
            
        } else {
            
            plot_data <- tribble(
                ~x_label, ~percentage,
                "All", round(100*all_numer/all_denom)
            )
            
            upperlimit <- 100
            ylabel <- "Percentage of publications"
            
        }
        
    }
    
    if (absnum) {
        
        plot_ly(
            plot_data,
            x = ~x_label,
            y = ~percentage,
            name = "Archived",
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
                y = ~can_archive,
                name = "Can archive",
                marker = list(
                    color = color_palette[12],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>% 
            add_trace(
                y = ~cant_archive,
                name = "Cannot archive",
                marker = list(
                    color = color_palette[7],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~no_data,
                name = "No data",
                marker = list(
                    color = color_palette[6],
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
                    title = paste('<b>', ylabel, '</b>'),
                    range = c(0, upperlimit)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9]
            )
        
    } else {
        
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
                    title = paste('<b>', ylabel, '</b>'),
                    range = c(0, upperlimit)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9]
            )
        
    }
    
}

## Clinical Trials plots

## TRN

plot_clinicaltrials_trn <- function (dataset, color_palette) {

    umc <- "All"

    plot_data <- dataset

    all_denom <- plot_data %>%
        nrow()
    
    all_numer_abs <- sum(plot_data$has_iv_trn_abstract, na.rm=TRUE)
    abs_denom <- plot_data %>%
        filter(! is.na(has_iv_trn_abstract)) %>%
        nrow()
    all_numer_ft <- sum(plot_data$has_iv_trn_ft_pdf, na.rm=TRUE)
    ft_denom <- plot_data %>%
        filter(! is.na(has_iv_trn_ft_pdf)) %>%
        nrow()
    
    plot_data <- tribble(
        ~x_label, ~colour, ~percentage,
        "All", "In abstract", round(100*all_numer_abs/abs_denom),
        ## "All", "Secondary information", round(100*all_numer_si/all_denom),
        "All", "In full text", round(100*all_numer_ft/ft_denom)
    )

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
plot_clinicaltrials_sumres <- function (dataset, color_palette) {

    umc <- "All"

    dataset <- dataset %>%
        filter (date > Sys.Date()-365*1.5) ## Only look at the last year and a half
    
    if (umc != "All") {

        all_data <- dataset %>%
            group_by(date) %>%
            mutate(avg = mean(percent_reported)) %>%
            slice_head() %>%
            select(date, hash, avg) %>%
            rename(percent_reported = avg) %>%
            mutate(city = "All") %>%
            ungroup()
            
        city_data <- dataset %>%
            filter(city == umc)

        plot_data <- rbind(all_data, city_data)

        plot_ly(
            plot_data,
            x = ~date,
            y = ~percent_reported,
            name = ~city,
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
                    title = '<b>Date</b>'
                ),
                yaxis = list(
                    title = '<b>Reported within 1 year (%)</b>',
                    range = c(0, 100)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9],
                legend = list(xanchor= "right")
            )
        
    } else {

        plot_data <- dataset %>%
            group_by(date) %>%
            mutate(avg = mean(percent_reported)) %>%
            slice_head() %>%
            select(date, avg) %>%
            rename(percent_reported = avg) %>%
            mutate(city = "All") %>%
            ungroup()

        plot_ly(
            plot_data,
            x = ~date,
            y = ~percent_reported,
            name = "All",
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
                    title = '<b>Date</b>'
                ),
                yaxis = list(
                    title = '<b>Reported within 1 year (%)</b>',
                    range = c(0, 100)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9],
                legend = list(xanchor= "right")
            )
        
    }
    
}

# Prospective registration
plot_clinicaltrials_prereg <- function (dataset, color_palette) {

    umc <- "All"

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
                            title = '<b>Year</b>',
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

## Linkage
plot_linkage <- function (dataset, color_palette) {

    plot_data <- tribble(
        ~x_label, ~percentage,
        "All", round(100*mean(dataset$has_reg_pub_link, na.rm=TRUE))
    )

    upperlimit <- 100
    ylabel <- "Percentage of publications"

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
                title = paste('<b>', ylabel, '</b>'),
                range = c(0, upperlimit)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

# Timely publication within 5 years
plot_clinicaltrials_timpub_5a <- function (dataset, color_palette) {

    umc <- "All"

    dataset$year <- dataset$completion_date %>%
        format("%Y")

    years <- seq(from=min(dataset$year), to=max(dataset$year))

    all_denom <- dataset %>%
        nrow()
    
    all_numer <- dataset %>%
        filter(published_5a) %>%
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
                    published_5a
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
                    published_5a
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
                    title = '<b>Completion year</b>',
                    dtick = 1
                ),
                yaxis = list(
                    title = '<b>Reported within 5 years (%)</b>',
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
                    published_5a
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
                    title = '<b>Reported within 5 years (%)</b>',
                    range = c(0, 100)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9],
                legend = list(xanchor= "right")
            )
        
        
    }
    
}

# Timely publication within 2 years
plot_clinicaltrials_timpub_2a <- function (dataset, color_palette) {

    umc <- "All"

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
                    title = '<b>Completion year</b>',
                    dtick = 1
                ),
                yaxis = list(
                    title = '<b>Reported within 2 years (%)</b>',
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

plot_randomization <- function (dataset, umc, absnum, color_palette) {

    ## Calculate the numerator and denominator for the
    ## "all" bar

    all_numer <- dataset %>%
        filter(
            is_animal == 1,
            language == "English",
            ! is.na(sciscore),
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
        ) %>%
        select(randomization) %>%
        sum(na.rm=TRUE)

    all_denom <- dataset %>%
        filter(
            is_animal == 1,
            language == "English",
            ! is.na(sciscore),
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
        ) %>%
        nrow()

    all_nosciscore <- dataset %>%
        filter(
            is_animal == 1,
            language == "English",
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
            is.na(sciscore)
        ) %>%
        nrow()

    all_norando <- dataset %>%
        filter(
            is_animal == 1,
            language == "English",
            ! is.na(sciscore),
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
            randomization == 0
        ) %>%
        nrow()
    
    all_non_eng <- dataset %>%
        filter(
            is_animal == 1,
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
            language != "English" | is.na(language)
        ) %>%
        nrow()

    if ( umc != "All" ) {

        ## If the selected UMC is not "all," calculate
        ## the percentage 
        
        umc_numerator <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                language == "English",
                ! is.na(sciscore),
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
            )

        umc_numer <- umc_numerator$randomization %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                language == "English",
                ! is.na(sciscore),
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
            ) %>%
            nrow()

        umc_nosciscore <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                language == "English",
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
                is.na(sciscore)
            ) %>%
            nrow()

        umc_norando <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                language == "English",
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
                ! is.na(sciscore),
                randomization == 0
            ) %>%
            nrow()
        
        umc_non_eng <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
                language != "English" | is.na(language)
            ) %>%
            nrow()

        if (absnum) {

            plot_data <- tribble(
                ~x_label, ~percentage, ~nosciscore,    ~norando,   ~non_eng,
                umc,      umc_numer,   umc_nosciscore, umc_norando, umc_non_eng
            )

            upperlimit <- 1.1*sum(umc_numer, umc_nosciscore, umc_norando, umc_non_eng)
            ylabel <- "Number of animal studies"
            
        } else {

            plot_data <- tribble(
                ~x_label, ~percentage,
                umc, round(100*umc_numer/umc_denom),
                "All", round(100*all_numer/all_denom)
            )

            plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)
            
            upperlimit <- 100
            ylabel <- "Percentage of animal studies"
            
        }
        
    } else {

        if (absnum) {

            plot_data <- tribble(
                ~x_label, ~percentage, ~nosciscore, ~norando,   ~non_eng,
                "All",    all_numer,   all_nosciscore, all_norando, all_non_eng
            )

            upperlimit <- 1.1*sum(all_numer, all_nosciscore, all_norando, all_non_eng)
            ylabel <- "Number of animal studies"
            
        } else {

            plot_data <- tribble(
                ~x_label, ~percentage,
                "All", round(100*all_numer/all_denom)
            )
            
            upperlimit <- 100
            ylabel <- "Percentage of animal studies"

        }        
    }

    if (absnum) {

        plot_ly(
            plot_data,
            x = ~x_label,
            y = ~percentage,
            name = "Reported",
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
                y = ~norando,
                name = "Not reported",
                marker = list(
                    color = color_palette[12],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~nosciscore,
                name = "No data",
                marker = list(
                    color = color_palette[7],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~non_eng,
                name = "Non-English/NA",
                marker = list(
                    color = color_palette[6],
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
                    title = paste('<b>', ylabel, '</b>'),
                    range = c(0, upperlimit)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9]
            )
        
    } else {

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
                    title = paste('<b>', ylabel, '</b>'),
                    range = c(0, upperlimit)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9]
            )

    }
    
}

## Blinding
plot_blinding <- function (dataset, umc, absnum, color_palette) {

    ## Calculate the numerator and denominator for the
    ## "all" bar

    all_numer <- dataset %>%
        filter(
            is_animal == 1,
            language == "English",
            ! is.na(sciscore),
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
        ) %>%
        select(blinding) %>%
        sum(na.rm=TRUE)

    all_denom <- dataset %>%
        filter(
            is_animal == 1,
            language == "English",
            ! is.na(sciscore),
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
        ) %>%
        nrow()

    all_nosciscore <- dataset %>%
        filter(
            is_animal == 1,
            language == "English",
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
            is.na(sciscore)
        ) %>%
        nrow()

    all_noblind <- dataset %>%
        filter(
            is_animal == 1,
            language == "English",
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
            ! is.na(sciscore),
            blinding == 0
        ) %>%
        nrow()
    
    all_non_eng <- dataset %>%
        filter(
            is_animal == 1,
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
            language != "English" | is.na(language)
        ) %>%
        nrow()

    if ( umc != "All" ) {

        ## If the selected UMC is not "all," calculate
        ## the percentage 
        
        umc_numerator <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                language == "English",
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
                ! is.na(sciscore)
            )

        umc_numer <- umc_numerator$blinding %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                language == "English",
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
                ! is.na(sciscore)
            ) %>%
            nrow()

        umc_nosciscore <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                language == "English",
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
                is.na(sciscore)
            ) %>%
            nrow()

        umc_noblind <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                language == "English",
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
                ! is.na(sciscore),
                blinding == 0
            ) %>%
            nrow()
        
        umc_non_eng <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
                language != "English" | is.na(language)
            ) %>%
            nrow()

        if (absnum) {

            plot_data <- tribble(
                ~x_label, ~percentage, ~nosciscore,    ~noblind,   ~non_eng,
                umc,      umc_numer,   umc_nosciscore, umc_noblind, umc_non_eng
            )

            upperlimit <- 1.1*sum(umc_numer, umc_nosciscore, umc_noblind, umc_non_eng)
            ylabel <- "Number of animal studies"
            
        } else {

            plot_data <- tribble(
                ~x_label, ~percentage,
                umc, round(100*umc_numer/umc_denom),
                "All", round(100*all_numer/all_denom)
            )

            plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)
            
            upperlimit <- 100
            ylabel <- "Percentage of animal studies"
            
        }
        
    } else {

        if (absnum) {

            plot_data <- tribble(
                ~x_label, ~percentage, ~nosciscore, ~noblind,     ~non_eng,
                "All",    all_numer,   all_nosciscore, all_noblind, all_non_eng
            )

            upperlimit <- 1.1*sum(all_numer, all_nosciscore, all_noblind, all_non_eng)
            ylabel <- "Number of animal studies"
            
        } else {

            plot_data <- tribble(
                ~x_label, ~percentage,
                "All", round(100*all_numer/all_denom)
            )
            
            upperlimit <- 100
            ylabel <- "Percentage of animal studies"
            
        }
    }

    if (absnum) {

        plot_ly(
            plot_data,
            x = ~x_label,
            y = ~percentage,
            name = "Reported",
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
                y = ~noblind,
                name = "Not reported",
                marker = list(
                    color = color_palette[12],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~nosciscore,
                name = "No data",
                marker = list(
                    color = color_palette[7],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~non_eng,
                name = "Non-English/NA",
                marker = list(
                    color = color_palette[6],
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
                    title = paste('<b>', ylabel, '</b>'),
                    range = c(0, upperlimit)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9]
            )
        
    } else {

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
                title = paste('<b>', ylabel, '</b>'),
                range = c(0, upperlimit)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    }
}

## Power calc
plot_power <- function (dataset, umc, absnum, color_palette) {

    ## Calculate the numerator and denominator for the
    ## "all" bar

    all_numer <- dataset %>%
        filter(
            is_animal == 1,
            language == "English",
            ! is.na(sciscore),
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
        ) %>%
        select(power) %>%
        sum(na.rm=TRUE)

    all_denom <- dataset %>%
        filter(
            is_animal == 1,
            language == "English",
            ! is.na(sciscore),
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
        ) %>%
        nrow()

    all_nosciscore <- dataset %>%
        filter(
            is_animal == 1,
            language == "English",
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
            is.na(sciscore)
        ) %>%
        nrow()

    all_nopower <- dataset %>%
        filter(
            is_animal == 1,
            language == "English",
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
            ! is.na(sciscore),
            power == 0
        ) %>%
        nrow()
    
    all_non_eng <- dataset %>%
        filter(
            is_animal == 1,
            type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
            language != "English" | is.na(language)
        ) %>%
        nrow()

    if ( umc != "All" ) {

        ## If the selected UMC is not "all," calculate
        ## the percentage 
        
        umc_numerator <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                language == "English",
                ! is.na(sciscore),
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
            )

        umc_numer <- umc_numerator$power %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                language == "English",
                ! is.na(sciscore),
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper"
            ) %>%
            nrow()

        umc_nosciscore <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                language == "English",
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
                is.na(sciscore)
            ) %>%
            nrow()

        umc_nopower <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                language == "English",
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
                ! is.na(sciscore),
                power == 0
            ) %>%
            nrow()
        
        umc_non_eng <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                type == "Article" | type == "Article; Data Paper" | type == "Article; Proceedings Paper",
                language != "English" | is.na(language)
            ) %>%
            nrow()

         if (absnum) {

            plot_data <- tribble(
                ~x_label, ~percentage, ~nosciscore,    ~nopower,    ~non_eng,
                umc,      umc_numer,   umc_nosciscore, umc_nopower, umc_non_eng
            )

            upperlimit <- 1.1*sum(umc_numer, umc_nosciscore, umc_nopower, umc_non_eng)
            ylabel <- "Number of animal studies"
            
         } else {

             plot_data <- tribble(
                 ~x_label, ~percentage,
                 umc, round(100*umc_numer/umc_denom),
                 "All", round(100*all_numer/all_denom)
             )

             plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)
            
            upperlimit <- 100
            ylabel <- "Percentage of animal studies"
             
         }
        
    } else {

        if (absnum) {

            plot_data <- tribble(
                ~x_label, ~percentage, ~nosciscore, ~nopower,    ~non_eng,
                "All",    all_numer,   all_nosciscore, all_nopower, all_non_eng
            )

            upperlimit <- 1.1*sum(all_numer, all_nosciscore, all_nopower, all_non_eng)
            ylabel <- "Number of animal studies"
            
        } else {

            plot_data <- tribble(
                ~x_label, ~percentage,
                "All", round(100*all_numer/all_denom)
            )
            
            upperlimit <- 100
            ylabel <- "Percentage of animal studies"
            
        }
        
    }

    if (absnum) {

        plot_ly(
            plot_data,
            x = ~x_label,
            y = ~percentage,
            name = "Reported",
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
                y = ~nopower,
                name = "Not reported",
                marker = list(
                    color = color_palette[12],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~nosciscore,
                name = "No data",
                marker = list(
                    color = color_palette[7],
                    line = list(
                        color = 'rgb(0,0,0)',
                        width = 1.5
                    )
                )
            ) %>%
            add_trace(
                y = ~non_eng,
                name = "Non-English/NA",
                marker = list(
                    color = color_palette[6],
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
                    title = paste('<b>', ylabel, '</b>'),
                    range = c(0, upperlimit)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9]
            )
        
    } else {

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
                    title = paste('<b>', ylabel, '</b>'),
                    range = c(0, upperlimit)
                ),
                paper_bgcolor = color_palette[9],
                plot_bgcolor = color_palette[9]
            )

    }

    
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
            umc, round(100*umc_numer/umc_denom),
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
