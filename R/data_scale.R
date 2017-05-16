


#' PIVOT generate relative frequency table given counts data
#'
#' @export
generateRelativeFreq <- function(raw){
    as.data.frame(apply(raw,2,function(col) col/sum(col)))
}

#' PIVOT data scale module
#'
#' @description
#' Controls which transformed data should be used as input for analysis modules, this is the UI part of the module.
#'
#' @export
pivot_dataScale_UI <- function(id, include = c("Counts (raw)", "Counts (normalized)", "Relative Frequency", "Log10 Counts", "Standardized Counts", "Log10 & Standardized"), selected = "Counts (normalized)") {
    ns<- NS(id)
    names(include) <- include
    tagList(
        selectInput(ns("data_scale"), label = "Data Scale",
                    choices = as.list(include), selected = selected)
    )

}


#' PIVOT data scale module
#'
#' @description
#' Controls which transformed data should be used as input for analysis modules, this is the server part of the module.
#'
#' @import dplyr tibble
#' @export
pivot_dataScale <- function(input, output, session, r_data, order_by = NULL, keep_stats = FALSE, ercc_iso = FALSE) {
    data0 <- reactive({
        if(is.null(input$data_scale) || is.null(r_data$df)) return(NULL)
        raw <- r_data$raw
        df <- r_data$df
        log <- log10(df + 1)
        nm <- as.data.frame(t(scale(t(df))))
        log_nm <- as.data.frame(t(scale(t(log10(df + 1)))))

        if(input$data_scale == "Counts (normalized)")
            new_df <- df
        else if(input$data_scale == "Log10 Counts")
            new_df <- log
        else if(input$data_scale == "Standardized Counts")
            new_df <- nm
        else if(input$data_scale == "Log10 & Standardized")
            new_df <- log_nm
        else if(input$data_scale == "Counts (raw)")
            new_df <- raw
        else if(input$data_scale == "Relative Frequency")
            new_df <- generateRelativeFreq(raw)

        if(!is.null(order_by) && order_by != "custom") {
            if(order_by == "none") {
                new_df <- new_df %>% tibble::rownames_to_column("feature")
            } else if(order_by == "variance") {
                new_df <- new_df %>%
                    tibble::rownames_to_column("feature") %>%
                    dplyr::mutate(variance = apply(new_df,1,var)) %>% # Compute variance of feature across sample
                    dplyr::arrange(desc(variance))
            } else if(order_by == "row_average") {
                new_df <- new_df %>%
                    tibble::rownames_to_column("feature") %>%
                    dplyr::mutate(average = apply(new_df,1,mean)) %>%
                    dplyr::arrange(desc(average))
            } else if(order_by == "fano_factor"){
                new_df <- new_df %>%
                    tibble::rownames_to_column("feature") %>%
                    dplyr::mutate(average = apply(new_df,1,mean)) %>%
                    dplyr::mutate(variance = apply(new_df,1,var)) %>% # Compute variance of feature across sample
                    dplyr::mutate(fano_factor = variance/average) %>%
                    dplyr::arrange(desc(fano_factor))
            } else if(order_by == "row_median") {
                new_df <- new_df %>%
                    tibble::rownames_to_column("feature") %>%
                    dplyr::mutate(median = apply(new_df,1,median)) %>%
                    dplyr::arrange(desc(median))
            }

            rownames(new_df) <- new_df$feature
            new_df <- new_df %>% dplyr::select(-feature)
            if(!keep_stats) {
                new_df <- new_df[,!(colnames(new_df)%in%c("variance", "average", "fano_factor", "median"))]
            }
        }
        return(new_df)
    })

    return(list(
        df = data0(),
        scale = input$data_scale,
        order = order_by
    ))
}

#' a wrapper module for scaling and range selection for heatmap
#'
#' @description
#' This is the UI part of the module.
#' @import PIVOT.data
#' @export
pivot_dataScaleRange_UI <- function(id, bound = 500, value = 100, include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts", "Standardized Counts", "Log10 & Standardized")) {
    ns<- NS(id)

    if (bound > 500) {
        max_bound = 500
    } else {
        max_bound = bound
    }

    tagList(
        fluidRow(
            column(3, pivot_dataScale_UI(ns("hm_scale"), include = include, selected = "Log10 Counts")),
            column(3, selectInput(ns("rank_method"), label = "Select features by", choices = list("None" = "none", "Variance" = "variance", "Fano Factor" = "fano_factor", "Row Average" = "row_average", "Row Median" = "row_median", "Custom feature list" = "custom"), selected = "fano_factor")),
            column(6, sliderInput(ns("feature_range"), label = "Rank Range", min = 1, max = max_bound, value = c(1, value), step = 1))
        ),
        conditionalPanel(sprintf("input['%s'] != 'custom'", ns("rank_method")),
                         fluidRow(
                             column(3,tags$b("Manually input a range:")),
                             column(3, uiOutput(ns("min_rank_ui"))),
                             column(3, uiOutput(ns("max_rank_ui"))),
                             column(3, actionButton(ns("update_range"), "Update Range", class = "btn btn-info"))
                         )
        ),
        conditionalPanel(sprintf("input['%s'] == 'custom'", ns("rank_method")),
                         fluidRow(
                             column(12, pivot_featureInputModal_UI(ns("ft_hmap"), label = "Input custom feature list"))
                         ),
                         tags$br()

        )
    )
}

#' a wrapper module for scaling and range selection for heatmap
#'
#' @description
#' This is the server part of the module.
#' @import PIVOT.data
#' @export
pivot_dataScaleRange <- function(input, output, session, r_data, keep_stats = FALSE) {

    output$min_rank_ui <- renderUI({
        if(is.null(r_data$df)) return()
        feature_num = nrow(r_data$df)

        if (!is.null(input$feature_range)) {
            cur_val = input$feature_range[1]
        } else {
            cur_val = 1
        }

        numericInput_1(session$ns("min_rank"), label = "Min:", value = cur_val, min = 1, max = feature_num, step = 1)
    })

    output$max_rank_ui <- renderUI({
        if(is.null(r_data$df)) return()
        feature_num = nrow(r_data$df)

        if (!is.null(input$feature_range)) {
            cur_val = input$feature_range[2]
        } else {
            cur_val = feature_num
        }

        numericInput_1(session$ns("max_rank"), label = "Max:", value = cur_val, min = 2, max = feature_num, step = 1)
    })

    observeEvent(input$update_range, {
        feature_num = nrow(r_data$df)
        if(input$min_rank <= feature_num & input$max_rank <= feature_num) {
            if(input$max_rank - input$min_rank + 1 < 2) {
                session$sendCustomMessage(type = "showalert", "Max must be larger than min by at least 1!")
                return()
            }
            if(input$max_rank - input$min_rank + 1 > 1000) {
                session$sendCustomMessage(type = "showalert", "You specified more than 1000 features, please choose less.")
                return()
            }
            updateSliderInput(session, "feature_range",
                              label = "Rank Range",
                              min = input$min_rank, max = input$max_rank, value = c(input$min_rank, input$max_rank), step = 1)
        } else {
            session$sendCustomMessage(type = "showalert", "You don't have that many features in the data set.")
            return()
        }
    })

    if(input$rank_method == "custom") {
        flist <- callModule(pivot_featureInputModal, "ft_hmap", r_data = r_data)
    }

    data0 <- reactive({
        #flist <- callModule(pivot_featureInputModal, "ft_hmap", r_data = r_data)
        rsList <- callModule(pivot_dataScale, "hm_scale", r_data, ercc_iso = FALSE, keep_stats = keep_stats, order_by = input$rank_method)
        hm_data <- rsList$df

        if(input$rank_method == "custom") {
            if(is.null(flist)){
                return(NULL)
            }
            rs<-hm_data[flist,]
        } else {
            if(is.null(hm_data) || is.null(input$feature_range)) return()
            if(input$feature_range[2] > nrow(hm_data)) {
                return(NULL)
            }
            rs<-hm_data[input$feature_range[1]:input$feature_range[2],]
        }
        if(nrow(rs) == 0) {
            return(NULL)
        } else {
            return(rs)
        }
    })

    return(list(
        df = data0(),
        scale = input$scale,
        order = input$rank_method,
        range = c(input$feature_range[1],input$feature_range[2])
    ))
}












