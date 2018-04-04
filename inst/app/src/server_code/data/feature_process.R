# PIVOT: Platform for Interactive analysis and Visualization Of Transcriptomics data
# Copyright (c) 2015-2018, Qin Zhu and Junhyong Kim, University of Pennsylvania.
# All Rights Reserved.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.



output$filter_ui <- renderUI({
    if(is.null(r_data$glb.raw)) return(
        tags$p("Please upload data first.")
    )
    list(
        tags$div(
            tags$b("Feature Id/Name Conversion:"),
            pivot_help_UI("feature_convert", title = "Feature id/name conversion mechanism"),
            class = "param_setting_title"
        ),
        fluidRow(
            column(4, uiOutput("feature_species_ui")),
            column(4,
                   uiOutput("input_feature_type_ui")
            ),
            column(4,
                   uiOutput("output_feature_type_ui")
            )
        ),
        # fluidRow(
        #     column(4,
        #            tags$span(
        #                tags$b("One map to many solution:"),
        #                shinyBS::tipify(
        #                    bsButton("fconvert_one_tooltip", label = NULL, icon = icon("question-circle"), style = "link", size = "extra-small"),
        #                    title = "Feature conversion could result in cases where one map to many, such as gene A map to gene id 1,2,3.",
        #                    options = list(container = "body")
        #                ),
        #                selectInput("fconvert_one_sol", NULL,
        #                            choices = list("Only keep the first"="first", "Use same entry for all"="all"))
        #     )),
        #     column(4,
        #         tags$span(
        #         tags$b("Many map to one solution:"),
        #         shinyBS::tipify(
        #             bsButton("fconvert_many_tooltip", label = NULL, icon = icon("question-circle"), style = "link", size = "extra-small"),
        #             title = "Feature conversion could result in cases where many map to one, such as gene gene id 1,2,3 map to gene A.",
        #             options = list(container = "body")
        #         ),
        #         selectInput("fconvert_many_sol", NULL,
        #                     choices = list("Sum"="sum", "Average"="average"))
        #     )),
        #     column(4,
        #            tags$span(
        #                tags$b("Map to none solution:"),
        #                shinyBS::tipify(
        #                    bsButton("fconvert_none_tooltip", label = NULL, icon = icon("question-circle"), style = "link", size = "extra-small"),
        #                    title = "For features that cannot be mapped to any id, choose whether to discard them or to keep them.",
        #                    options = list(container = "body")
        #                ),
        #                selectInput("fconvert_none_sol", NULL,
        #                            choices = list("Discard"="discard", "Keep"="keep"))
        #            ))
        # ),
        fluidRow(
            #column(6, uiOutput("feature_convert_renormalize_ui")),
            column(12, actionButton("feature_convert_btn", "Convert Feature", class = "btn btn-info btn_rightAlign"))
        ),
        tags$div(
            tags$b("Feature Filter:"),
            pivot_help_UI("filter_type", title = "What is a feature filter"),
            class = "param_setting_title"
        ),
        fluidRow(
            column(4,
                   selectInput("feature_filter_type", label = "Filter based on", choices = list("Expression" = "range", "Feature List" = "marker", "P value" = "pval"))
            ),
            column(4,
                   selectInput("is_neg_filter", label = "Keep/Delete Feature", choices = list("Keep Selected" = FALSE, "Delete Selected" = TRUE), selected = FALSE)
            )
        ),
        uiOutput("filters"),
        fluidRow(
            column(12,
                   actionButton('undo_filter_feature', label = "Undo Filter", icon = icon("times"), class = "btn btn-danger btn_rightAlign"),
                   uiOutput("range_filter_btn_ui"),
                   uiOutput("filter_wt_list_ui")
            )
        )
    )
})

output$feature_species_ui <- renderUI({
    species<-read.csv("src/built_in_files/bmart_species.csv")
    species_options <- as.character(species$dataset)
    names(species_options) <- species$description
    selectInput("feature_species","Choose Species:", choices = species_options, selected = "hsapiens_gene_ensembl")
})

output$input_feature_type_ui <- renderUI({
    filters <- read.csv("src/built_in_files/support_filters.csv")
    support_filters <- as.character(filters$support_filters)
    names(support_filters) <- filters$description
    selectInput("input_feature_type", "Input Feature Type", choices=support_filters)
})

output$output_feature_type_ui <- renderUI({
    attris <- read.csv("src/built_in_files/support_attris.csv")
    support_attris <- as.character(attris$support_attris)
    names(support_attris) <- attris$description
    selectInput("output_feature_type", "Output Feature Type", choices=support_attris)
})

output$feature_convert_renormalize_ui <- renderUI({
    checkboxInput(
        "feature_convert_renormalize",
        tags$span(
            paste0("Convert with renormalization (", input$proc_method, ")"),
            shinyBS::tipify(
                bsButton("fconvert_renorm_tooltip", label = NULL, icon = icon("question-circle"), style = "link", size = "extra-small"),
                title = "You can change normalization methods and parameters in the File panel. If checked, PIVOT will re-normalize raw data after merging duplicates. Otherwise duplicated features will be directly merged for the normalized count matrix.",
                options = list(container = "body")
            )
        ),
        width = "120%",
        value = FALSE
    )
})

observeEvent(input$feature_convert_btn, {
    req(input$feature_species, input$input_feature_type, input$output_feature_type)
    withProgress(message = 'Converting Features...', value = 0.3, {
        tryCatch({
            bmart_db = biomaRt::useMart("ensembl", dataset=input$feature_species)
        }, error=function (e){
            showNotification("Request to BioMart web service failed. Please make sure you have internet connection.", type="error", duration=10)
            return()
        })

        old_id <- rownames(r_data$glb.raw)

        bmResult <-biomaRt::getBM(
            filters=input$input_feature_type,
            attributes=c(input$input_feature_type, input$output_feature_type),
            values=old_id,
            mart=bmart_db)
        new_id<-bmResult[[2]]
        if(length(new_id) < 2) {
            showNotification("Most of your features cannot be converted. Aborting conversion. Please recheck.", type="error", duration=10)
            return()
        }

        r_data <- clear_results(r_data)
        r_data <- switch_to_dataset(r_data, 1)

        # Remove the current subset node
        r_data$history <- r_data$history[1]
        r_data$his_tbl <- r_data$his_tbl[1,,drop=F]

        # Update node and edge table
        neList<- generate_data_node(his_tbl = r_data$his_tbl, reg_tbl = r_data$reg_tbl)
        r_data$his_nodes <- neList$nodes
        r_data$his_edges <- neList$edges

        otm_num <- sum(duplicated(new_id)) # Number of one map to many
        mto_num <- sum(duplicated(bmResult[[1]])) # Number of many map to one
        # assign("old_id", old_id, env=.GlobalEnv)
        # assign("bmResult",bmResult, env=.GlobalEnv)
        df_match<-match(toupper(old_id), toupper(bmResult[[1]]))
        na_num <- sum(is.na(df_match))

        if(otm_num != 0) {
            showNotification(paste("Found",otm_num,"one map to many entries."), type="warning", duration=10)
        }
        if(mto_num != 0) {
            showNotification(paste("Found",mto_num,"many map to one entries."), type="warning", duration=10)
        }
        if(na_num != 0) {
            showNotification(paste(na_num,"entries cannot be mapped."), type="warning", duration=10)
        }
        df_id<-bmResult[[2]][df_match]
        df_id[which(is.na(df_id))] <- old_id[which(is.na(df_id))]
        df_id <- make.names(df_id, unique=T)
        rownames(r_data$glb.raw) <- df_id
        rownames(r_data$raw) <- df_id
        rownames(r_data$df) <- df_id
        r_data$feature_list <- df_id
        r_data <- init_meta(r_data)
        r_data$history <- NULL
        r_data <- update_history(r_data, NA, "Input", "Input with feature id/name conversion", list(feature = r_data$feature_list, sample = r_data$sample_name, df = r_data$df), r_data$norm_param$method, r_data$norm_param)
    })
})


############ Filter ##############

output$filters <- renderUI({
    req(r_data$glb.raw)
    scater_plot_choices <- c("highest-expression", "exprs-freq-vs-mean", "pairwise feature metadata plots")
    names(scater_plot_choices) <- scater_plot_choices

    scater_feature_choices <- c("mean_counts","log10_mean_counts","rank_counts","n_cells_counts", "pct_dropout_counts", "total_counts","log10_total_counts")
    names(scater_feature_choices) <- scater_feature_choices

    if(input$feature_filter_type == "range") {
        list(
            fluidRow(
                column(4, selectInput("scater_feature_plot_type", "Scater Plot Type", choices = scater_plot_choices)),
                uiOutput("scater_feature_pair_ui")
            ),
            plotOutput("scater_feature_plot"),
            fluidRow(
                column(4,
                       selectInput("scater_filter_type", "Filter criteria:", choices = scater_feature_choices)
                ),
                column(4,
                       uiOutput("scater_filter_range_ui")
                ),
                column(2, uiOutput("scater_min_ui")),
                column(2, uiOutput("scater_max_ui"))
            )
        )
    } else if(input$feature_filter_type == "marker") {
        list(
            fluidRow(
                column(6,
                       wellPanel(
                           tags$b("Upload Feature List:"),
                           pivot_help_UI("marker_filter", title = "File format requirements"),
                           pivot_fileInput_UI("marker_filter")
                       )
                ),
                column(6,
                       tags$p("[feature name in 1st column, case insensitive]"),
                       pivot_filePreview_UI("marker_pv")
                )
            )
        )
    } else if(input$feature_filter_type == "pval") {
        list(
            fluidRow(
                pivot_featureList_UI("filter", include = c("deseq", "edgeR", "scde", "monocle", "mwu"), selected = "deseq", width = 8)
            ),
            tags$div(tags$b("Loaded DE Table:"), class = "param_setting_title"),
            DT::dataTableOutput("filter_de_preview")
        )
    } else {
        return()
    }
})

############################### feature Filter module ###############################

output$scater_feature_pair_ui <- renderUI({
    req(r_data$sceset, input$scater_feature_plot_type == "pairwise feature metadata plots")
    choices <- colnames(rowData(r_data$sceset))
    choices <- choices[!choices %in% c("gene", "cap_name", "is_feature_control")]
    names(choices) <- choices
    list(
        column(4, selectInput("scater_feature_plot_x", "X Var", choices = choices)),
        column(4, selectInput("scater_feature_plot_y", "Y Var", choices = choices))
    )
})

output$scater_feature_plot <- renderPlot({
    req(r_data$sceset, input$scater_feature_plot_type)
    if(input$scater_feature_plot_type == "highest-expression") {
        scater::plotQC(r_data$sceset, type = input$scater_feature_plot_type, exprs_values = "counts")
    } else if (input$scater_feature_plot_type == "exprs-freq-vs-mean"){
        scater::plotQC(r_data$sceset, type = input$scater_feature_plot_type)
    } else {
        req(input$scater_feature_plot_x, input$scater_feature_plot_y)
        scater::plotFeatureData(r_data$sceset, ggplot2::aes_string(x = input$scater_feature_plot_x, y = input$scater_feature_plot_y))
    }
})

# define range and output ui
output$scater_filter_range_ui <- renderUI({
    req(r_data$sceset, input$scater_filter_type)
    ftbl <- fInfo(r_data$sceset)
    feature_vec <- ftbl[[input$scater_filter_type]]
    sliderInput("scater_filter_range", label = paste("Range of", input$scater_filter_type), min = floor(min(feature_vec)), max = ceiling(max(feature_vec)), value = c(floor(min(feature_vec)), ceiling(max(feature_vec))))
})

output$scater_min_ui <- renderUI({
    req(input$scater_filter_type, input$scater_filter_range)
    ftbl <- fInfo(r_data$sceset)
    feature_vec <- ftbl[[input$scater_filter_type]]
    cur_value = input$scater_filter_range[1]
    numericInput("scater_min", label = "Min", value = cur_value, min = floor(min(feature_vec)), max = ceiling(max(feature_vec)))
})

output$scater_max_ui <- renderUI({
    req(input$scater_filter_type, input$scater_filter_range)
    ftbl <- fInfo(r_data$sceset)
    feature_vec <- ftbl[[input$scater_filter_type]]
    cur_value = input$scater_filter_range[2]
    numericInput("scater_max", label = "Max", value = cur_value, min = floor(min(feature_vec)), max = ceiling(max(feature_vec)))
})


# range filter button
output$range_filter_btn_ui <- renderUI({
    if(is.null(input$is_neg_filter)) return()
    if(input$feature_filter_type != "range") return()
    negf <- as.logical(input$is_neg_filter)
    if(!negf) {
        actionButton('range_filter_btn', label = "Select", class = "btn btn-info  btn_rightAlign")
    } else if(negf) {
        actionButton('range_filter_btn', label = "Delete", class = "btn btn-info btn_rightAlign")
    }
})

# range filter: respond to button click
observeEvent(input$range_filter_btn, {
    req(r_data$sceset, input$scater_filter_type, input$scater_filter_range)

    negf <- as.logical(input$is_neg_filter)
    ftbl <- fInfo(r_data$sceset)
    feature_vec <- ftbl[[input$scater_filter_type]]

    if(input$scater_min > input$scater_max) {
        session$sendCustomMessage(type = "showalert", "Invalid range.")
        return()
    }

    withProgress(message = 'Filtering', value = 0, {
        #r_data <- clear_results(r_data)
        incProgress(0.3, detail = "Getting new feature list...")
        matched <- (feature_vec >= input$scater_min & feature_vec <= input$scater_max)

        if(negf) {
            matched <- !matched
        }

        if(sum(matched) < 2) {
            session$sendCustomMessage(type = "showalert", "Too few features left.")
            return()
        }

        flist <- r_data$feature_list[matched]
        slist <- r_data$sample_name

        if(negf) {
            keep_or_remove <- "Remove"
        } else {
            keep_or_remove <- "Keep"
        }

        actionText <- paste0(keep_or_remove, " features satisfying: ", input$scater_min, "≤", input$scater_filter_type, "≤", input$scater_max)
        r_data <- create_subset(r_data, input, flist, slist, keep_filter = T, renorm = F, erccStds = erccStds, actionType = "Filter", actionText = actionText)
        setProgress(1)
    })
})


# Press the undo filtering has the same effect of filtering larger than 0
observeEvent(input$undo_filter_feature, {
    withProgress(message = 'Processing...', value = 0, {
        #r_data <- clear_results(r_data)
        callModule(pivot_fileInput, "marker_filter", reset = TRUE)
        callModule(pivot_filePreview, "marker_pv", NULL)

        incProgress(0.3, detail = "Retrieving unfiltered dataset...")
        p_n <- search_subset_node(r_data$his_tbl, node = r_data$his_tbl$name[which(r_data$his_tbl$is_activated == "Y")])
        r_data <- switch_to_dataset(r_data, which(r_data$his_tbl$name == p_n))

        incProgress(0.3, detail = "Updating UI...")

        setProgress(1)
    })
})


output$filter_wt_list_ui <- renderUI({
    req(input$feature_filter_type)
    if(input$feature_filter_type == "range") return()
    if(!is.null(input$is_neg_filter)) {
        negf <- as.logical(input$is_neg_filter)
        if(!negf) {
            actionButton('filter_wt_list', label = "Select", class = "btn btn-info btn_rightAlign")
        } else if(negf) {
            actionButton('filter_wt_list', label = "Delete", class = "btn btn-info btn_rightAlign")
        }
    }
})



observe({
    df <- callModule(pivot_fileInput, "marker_filter")
    callModule(pivot_filePreview, "marker_pv", df$df, height = "250px")
})


# get marker_feature list when the user click submit button
observeEvent(input$filter_wt_list, {
    req(input$feature_filter_type)
    negf <- as.logical(input$is_neg_filter)
    cur_flist <- rownames(r_data$raw)
    if(negf) {
        keep_or_remove <- "Remove"
    } else {
        keep_or_remove <- "Keep"
    }

    if(input$feature_filter_type == "marker") {
        feature_type = "marker features"
        inFile <- callModule(pivot_fileInput, "marker_filter")
        marker_tbl <- as.data.frame(inFile$df)
        # First process the marker feature file and get the list
        marker_names <- make.names(as.character(unique(marker_tbl[,1])))
        flist <- cur_flist[match(toupper(marker_names), toupper(cur_flist))]
        flist <- flist[!is.na(flist)]
        dup_num <- sum(duplicated(flist))
        flist <- flist[!duplicated(flist)] # remove duplicated
        if(length(flist) != length(marker_names)) {
            message_gl <- paste0(length(marker_names) - length(flist)," features in your marker list (", length(marker_names),") are not found in the current dataset.")
            if(dup_num) {
                message_gl <- paste(message_gl, "Note some of your matched features have duplicated entries in the uploaded list.")
            }
            session$sendCustomMessage(type = "showalert", message_gl)
        }
    } else if (input$feature_filter_type == "pval") {
        feature_type = "DE features"
        if(is.null(filter_feature_tbl()) || nrow(filter_feature_tbl()) == 0) {
            session$sendCustomMessage(type = "showalert", "Feature set not detected. Please run corresponding DE tests first.")
            return()
        }
        delist <-filter_feature_tbl()$gene
        flist <- delist[which(delist %in% cur_flist)]
        if(length(flist) != length(delist)) {
            session$sendCustomMessage(type = "showalert", paste(length(delist) - length(flist), "DE genes are not found in the current dataset"))
        }
    }

    # Some feature in the list may have been filtered out due to < threshold counts,
    withProgress(message = 'Filtering...', value = 0, {
        #r_data <- clear_results(r_data)
        neglist <- cur_flist[!cur_flist%in%flist]
        if(negf) {
            flist <- neglist
        }
        if(length(flist) < 2) {
            session$sendCustomMessage(type = "showalert", "Too few features left!")
            return()
        }
        actionText <- paste(keep_or_remove, length(flist), feature_type)
        r_data <- create_subset(r_data, input, flist = flist, slist = r_data$sample_name, keep_filter = T, renorm = F, erccStds = erccStds, actionType = "Filter", actionText = actionText)
    })
})

filter_feature_tbl <- callModule(pivot_featureList, "filter", r_data = r_data)

output$filter_de_preview <- DT::renderDataTable({
    req(filter_feature_tbl())
    DT::datatable(filter_feature_tbl(), options = list(scrollX = TRUE, scrollY = "250px"))
})





