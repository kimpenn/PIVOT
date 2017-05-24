# Copyright (c) 2015,2016, Qin Zhu and Junhyong Kim, University of Pennsylvania.
# All Rights Reserved.
#
# You may not use this file except in compliance with the Kim Lab License
# located at
#
#     http://kim.bio.upenn.edu/software/LICENSE
#
# Unless required by applicable law or agreed to in writing, this
# software is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License
# for the specific language governing permissions and limitations
# under the License.


output$filter_ui <- renderUI({
    if(is.null(r_data$glb.raw)) return(
        tags$p("Please upload data first.")
    )
    list(
        tags$div(
            tags$b("Feature Filter Type:"),
            pivot_help_UI("filter_type", title = "What is a feature filter"),
            class = "param_setting_title"
        ),
        fluidRow(
            column(6,
                   selectInput("feature_filter_type", label = "Filter based on", choices = list("Expression" = "range", "Feature List" = "marker"))
            ),
            column(6,
                   selectInput("is_neg_filter", label = "Keep/Delete Feature", choices = list("Keep Selected" = FALSE, "Delete Selected" = TRUE), selected = FALSE)
            )
        ),
        fluidRow(
            column(6,
                   checkboxInput(
                       "keep_filter",
                       tags$span(
                           "Keep filtering the current dataset",
                           shinyBS::tipify(
                               bsButton("keep_filter_tooltip", label = NULL, icon = icon("question-circle"), style = "link", size = "extra-small"),
                               title = "If unchecked, previous filtering effect will be removed. Otherwise filtering will be perfermed on the current data (retain any previous filtering effects).",
                               options = list(container = "body")
                           )
                       ),
                       value = FALSE
                   )
            )
        ),

        hr(),
        uiOutput("filters"),

        hr(),
        fluidRow(
            column(9),
            column(3, actionButton('undo_filter_feature', label = "Undo Filter", icon = icon("times"), class = "btn btn-danger"))
        )
    )
})

output$filters <- renderUI({
    if(is.null(r_data$glb.raw)) return()
    if(input$feature_filter_type == "range") {
        list(
            tags$div(tags$b("Expression Filter:"), class = "param_setting_title"),
            wellPanel(
                fluidRow(
                    column(4,
                           selectInput("filter_type1", "Filter based on counts:", choices = c("Average" = "average", "Sum" = "sum"))
                    ),
                    column(6,
                           uiOutput("filter_type2_ui")
                    )
                ),
                fluidRow(
                    column(8,
                           uiOutput("feature_cnt_filter")
                    )
                ),
                fluidRow(
                    column(8,
                           fluidRow(
                               column(5,uiOutput("min_cnt_ui")),
                               column(2, tags$p("-")),
                               column(5, uiOutput("max_cnt_ui"))
                           )
                    ),
                    column(4,
                           uiOutput("range_filter_btn_ui")
                    )
                )
            ),
            hr(),
            wellPanel(
                fluidRow(
                    column(8, uiOutput("min_express_cells_ui")),
                    column(4, uiOutput("min_express_cells_percent"))
                ),
                uiOutput("express_filter_btn_ui")
            )
        )
    } else if(input$feature_filter_type == "marker") {
        list(
            tags$div(tags$b("Feature List Filter:"),
                     pivot_help_UI("marker_filter", title = "File format requirements"),
                     class = "param_setting_title"),
            fluidRow(
                column(6,
                       wellPanel(
                           tags$b("Upload Feature List:"),
                           pivot_fileInput_UI("marker_filter"),
                           uiOutput("mk_submit_ui")
                       )
                ),
                column(6,
                       tags$p("[feature name in 1st column, case insensitive]"),
                       pivot_filePreview_UI("marker_pv")
                )
            )
        )
    } else {
        return()
    }
})

output$min_express_cells_ui <- renderUI({
    if(is.null(filter_data())) return()
    express_min <- min(rowSums(filter_data()$raw > 0))
    numericInput("min_express_cells", "Filter based on minimum number of cells expressed", min = 1, max = length(r_data$sample_name), value = express_min)
})

output$min_express_cells_percent <- renderUI({
    if(is.null(filter_data()) || is.null(input$min_express_cells)) return()
    list(
        tags$b("Percentage"),
        tags$p(),
        tags$p(paste0(round(input$min_express_cells/length(r_data$sample_name) * 100, digits = 1),"%"))
    )
})

############################### feature Filter module ###############################

# Filter data (global or local)
filter_data <- reactive({
    if(input$keep_filter) {
        return(list(raw = r_data$raw, df = r_data$df))
    } else {
        s <- which(r_data$his_tbl$is_activated == "Y")
        p_n <- search_subset_node(r_data$his_tbl, node = r_data$his_tbl$name[s])
        p_s <- which(r_data$his_tbl$name == p_n)
        p_df <- r_data$history[[p_s]]$lists$df
        return(list(raw = r_data$glb.raw[, r_data$sample_name], df = p_df[, r_data$sample_name]))
    }
})


get_max_min <- function() {
    if(input$filter_type2 == "normalized"){
        if(input$filter_type1 == "average")
            list(max = ceiling(max(rowMeans(filter_data()$df))), min = floor(min(rowMeans(filter_data()$df))))
        else if(input$filter_type1 == "sum")
            list(max = ceiling(max(rowSums(filter_data()$df))), min = floor(min(rowSums(filter_data()$df))))
    } else if(input$filter_type2 == "raw"){
        if(input$filter_type1 == "average")
            list(max = ceiling(max(rowMeans(filter_data()$raw))), min = floor(min(rowMeans(filter_data()$raw))))
        else if(input$filter_type1 == "sum")
            list(max = ceiling(max(rowSums(filter_data()$raw))), min = floor(min(rowSums(filter_data()$raw))))
    }
}

# select range filter type
output$filter_type2_ui <- renderUI({
    if(is.null(filter_data())) return()
    if(input$proc_method != "none") {
        selectInput("filter_type2", label = br(), choices = list("raw counts" = "raw", "normalized counts" = "normalized"), selected = "raw")
    } else {
        selectInput("filter_type2", label = br(), choices = list("raw counts" = "raw"), selected = "raw")
    }
})

# define range and output ui
output$feature_cnt_filter <- renderUI({
    if(is.null(filter_data()) || is.null(input$filter_type2)) return()
    cnt <- get_max_min()
    if(is.null(r_data$f_range)) {
        cur_val <- c(cnt$min, cnt$max)
    } else {
        cur_val <- r_data$f_range
    }
    sliderInput("range_feature_cnt", label = paste("Select range of", input$filter_type1, input$filter_type2, "counts"), min = cnt$min, max = cnt$max, value = cur_val, step = 1)
})

output$min_cnt_ui <- renderUI({
    if(is.null(filter_data()) || is.null(input$filter_type2)) return()
    cnt <- get_max_min()

    if(is.null(input$range_feature_cnt)) {
        cur_value = cnt$min
    } else {
        cur_value = input$range_feature_cnt[1]
    }
    numericInput("min_cnt", label = NULL, value = cur_value, min = cnt$min, max = cnt$max, step = 1)
})

output$max_cnt_ui <- renderUI({
    if(is.null(filter_data()) || is.null(input$filter_type2)) return()
    cnt <- get_max_min()

    if(is.null(input$range_feature_cnt)) {
        cur_value = cnt$max
    } else {
        cur_value = input$range_feature_cnt[2]
    }
    numericInput("max_cnt", label = NULL, value = cur_value, min = cnt$min, max = cnt$max, step = 1)
})

# range filter button
output$range_filter_btn_ui <- renderUI({
    if(is.null(input$is_neg_filter)) return()
    negf <- as.logical(input$is_neg_filter)
    if(!negf) {
        actionButton('range_filter_btn', label = "Select", class = "btn btn-info")
    } else if(negf) {
        actionButton('range_filter_btn', label = "Delete", class = "btn btn-info")
    }
})

output$express_filter_btn_ui <- renderUI({
    if(is.null(input$is_neg_filter)) return()
    negf <- as.logical(input$is_neg_filter)
    if(!negf) {
        actionButton('express_filter_btn', label = "Select", class = "btn btn-info")
    } else if(negf) {
        actionButton('express_filter_btn', label = "Delete", class = "btn btn-info")
    }
})


# range filter: respond to button click
observeEvent(input$range_filter_btn, {
    negf <- as.logical(input$is_neg_filter)

    # Set the new count range according to numeric input
    cnt <- get_max_min()

    if(input$min_cnt <= cnt$max & input$max_cnt<= cnt$max) {
        if(input$max_cnt - input$min_cnt + 1 < 2) {
            session$sendCustomMessage(type = "showalert", "Max must be larger than min by at least 1!")
            return()
        }
    } else {
        session$sendCustomMessage(type = "showalert", "The range must be within the slider range!")
        return()
    }

    withProgress(message = 'Filtering', value = 0, {
        r_data <- clear_results(r_data)

        incProgress(0.3, detail = "Getting new feature list...")
        r_data$f_range <- c(input$min_cnt, input$max_cnt)
        # Now do the filtering
        new_df <- filter_data()$df
        new_raw <- filter_data()$raw

        if(input$filter_type2 == "normalized") {
            # normalized_cnts decides feature list
            if(input$filter_type1 == "average")
                tmp_df <- new_df[rowMeans(new_df) > input$min_cnt & rowMeans(new_df) <= input$max_cnt, ] # filter the current dataset
            else if(input$filter_type1 == "sum")
                tmp_df <- new_df[rowSums(new_df) > input$min_cnt & rowSums(new_df) <= input$max_cnt, ]
            if(nrow(tmp_df) == 0) {
                session$sendCustomMessage(type = "showalert", "No features are found in this range!")
                return()
            }
            flist <- rownames(tmp_df) # Get the filtered list (key)
        } else if(input$filter_type2 == "raw") {
            # Raw decides feature list
            if(input$filter_type1 == "average")
                tmp_raw <- new_raw[rowMeans(new_raw) > input$min_cnt & rowMeans(new_raw) <= input$max_cnt, ] # filter the current dataset
            else if(input$filter_type1 == "sum")
                tmp_raw <- new_raw[rowSums(new_raw) > input$min_cnt & rowSums(new_raw) <= input$max_cnt, ]
            if(nrow(tmp_raw) == 0) {
                session$sendCustomMessage(type = "showalert", "No features are found in this range!")
                return()
            }
            flist <- rownames(tmp_raw) # Get the filtered list (key)
        }

        neglist <- rownames(new_raw)[!rownames(new_raw)%in%flist]

        if(negf) {
            flist <- neglist
        }

        if(length(flist) < 2) {
            session$sendCustomMessage(type = "showalert", "Too few features left!")
            return()
        }

        slist <- r_data$sample_name

        if(negf) {
            tmpText <- "Remove"
        } else {
            tmpText <- "Keep"
        }

        actionText <- paste(tmpText, "features in range:", input$min_cnt, "≤", input$filter_type1, input$filter_type2, "counts", "≤", input$max_cnt)

        r_data <- create_subset(r_data, input, flist, slist, keep_filter = input$keep_filter, renorm = F, erccStds = erccStds, actionType = "Filter", actionText = actionText)

        setProgress(1)
    })
})

# range filter: respond to button click
observeEvent(input$express_filter_btn, {
    negf <- as.logical(input$is_neg_filter)

    # Set the new count range according to numeric input
    maxnumcells <- length(r_data$sample_name)
    if(input$min_express_cells > maxnumcells & input$min_express_cells > maxnumcells) {
        if(input$max_cnt - input$min_cnt + 1 < 2) {
            session$sendCustomMessage(type = "showalert", "Max must be larger than min by at least 1!")
            return()
        }
    }

    withProgress(message = 'Filtering', value = 0, {
        r_data <- clear_results(r_data)
        new_raw <- filter_data()$raw

        express <- rowSums(filter_data()$raw > 0)
        incProgress(0.3, detail = "Getting new feature list...")
        # Now do the filtering
        flist <- rownames(new_raw[express >= input$min_express_cells,])
        neglist <- rownames(new_raw)[!rownames(new_raw)%in%flist]

        if(negf) {
            flist <- neglist
        }

        if(length(flist) < 2) {
            session$sendCustomMessage(type = "showalert", "Too few features left!")
            return()
        }

        slist <- r_data$sample_name # No change for samples

        if(negf) {
            tmpText <- "Remove"
        } else {
            tmpText <- "Keep"
        }

        actionText <- paste(tmpText, "features expressed in more than", input$min_express_cells, "cells")

        r_data <- create_subset(r_data, input, flist, slist, keep_filter = input$keep_filter, renorm = F, erccStds = erccStds, actionType = "Filter", actionText = actionText)

        # use flist to update, do not interfere with r_data until success

        setProgress(1)
    })
})



# Press the undo filtering has the same effect of filtering larger than 0
observeEvent(input$undo_filter_feature, {
    withProgress(message = 'Processing...', value = 0, {
        r_data <- clear_results(r_data)
        callModule(pivot_fileInput, "marker_filter", reset = TRUE)
        callModule(pivot_filePreview, "marker_pv", NULL)

        incProgress(0.3, detail = "Retrieving unfiltered dataset...")
        p_n <- search_subset_node(r_data$his_tbl, node = r_data$his_tbl$name[which(r_data$his_tbl$is_activated == "Y")])
        r_data <- switch_to_dataset(r_data, which(r_data$his_tbl$name == p_n))

        incProgress(0.3, detail = "Updating UI...")
        r_data$f_range <- NULL
        # Reset ui
        cnt <- get_max_min()
        updateSliderInput(session, "range_feature_cnt",
                          label = NULL,
                          min = cnt$min, max = cnt$max, value = c(cnt$min, cnt$max), step = 1)
        updateNumericInput(session, "min_cnt", label = NULL, value = cnt$min, min = cnt$min, max = cnt$max, step = 1)
        updateNumericInput(session, "max_cnt", label = NULL, value = cnt$max, min = cnt$min, max = cnt$max, step = 1)

        setProgress(1)
    })
})


output$mk_submit_ui <- renderUI({
    if(!is.null(input$is_neg_filter)) {
        negf <- as.logical(input$is_neg_filter)
        if(!negf) {
            actionButton('submit_marker', label = "Select", class = "btn btn-info")
        } else if(negf) {
            actionButton('submit_marker', label = "Delete", class = "btn btn-info")
        }
    }
})



observe({
    df <- callModule(pivot_fileInput, "marker_filter")
    callModule(pivot_filePreview, "marker_pv", df$df)
})


# get marker_feature list when the user click submit button
observeEvent(input$submit_marker, {
    negf <- as.logical(input$is_neg_filter)
    inFile <- callModule(pivot_fileInput, "marker_filter")
    marker_tbl <- as.data.frame(inFile$df)
    # First process the marker feature file and get the list

    marker_names <- make.names(as.character(unique(marker_tbl[,1])))

    cur_flist <- rownames(filter_data()$raw)


    flist <- cur_flist[match(toupper(marker_names), toupper(cur_flist))]
    flist <- flist[!is.na(flist)]
    found_num <- length(marker_names) - length(flist)
    dup_num <- sum(duplicated(flist))
    flist <- flist[!duplicated(flist)] # remove duplicated

    # Some feature in the list may have been filtered out due to < threshold counts,
    withProgress(message = 'Filtering...', value = 0, {
        r_data <- clear_results(r_data)

        if(length(flist) != length(marker_names)) {
            message_gl <- paste0(length(marker_names) - length(flist)," features in your marker list (", length(marker_names),") are not found in the current dataset.")
            if(dup_num) {
                message_gl <- paste(message_gl, "Note some of your matched features have duplicated entries in the uploaded list.")
            }
            session$sendCustomMessage(type = "showalert", message_gl)
        }

        neglist <- cur_flist[!cur_flist%in%flist]

        if(negf) {
            flist <- neglist
        }

        if(length(flist) < 2) {
            session$sendCustomMessage(type = "showalert", "Too few features left!")
            return()
        }

        slist <- r_data$sample_name

        if(negf) {
            tmpText <- "Remove"
        } else {
            tmpText <- "Keep"
        }
        actionText <- paste(tmpText, length(flist), "marker features")
        r_data <- create_subset(r_data, input, flist, slist, keep_filter = input$keep_filter, renorm = F, erccStds = erccStds, actionType = "Filter", actionText = actionText)

        setProgress(1)
    })
})
