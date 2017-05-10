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


output$subset_ui <- renderUI({

    if(is.null(r_data$glb.raw)) return(tags$p("Please upload data first."))

    list(
        tags$div(tags$b("Sample Subsetter Type:"), class = "param_setting_title"),
        #a(id = "subset_type_help_btn", icon("question-circle")),
        #shinyBS::bsModal(id = "subset_type_help", title = "Subsetter help", trigger = "subset_type_help_btn", size = "large", list(
        #    tags$li("You can select or delete samples from the input set to create subset for analysis."),
        #    tags$li("An implicit filtering will happen to get features with >0 total expression."),
        #    tags$li("Previous filtering will be undone.")
        #)),

        fluidRow(
            column(6,
                   selectInput("subsetter_type", label = "Subsetter type", choices = list("Manually pick samples" = "manual", "Upload a sample list" = "list", "Subset based on sample stats" = "stats"), selected = "manual")
            ),
            column(6,
                   selectInput("is_neg_subsetter", label = "Select/Delete sample", choices = list("Positive subsetter (select)" = FALSE, "Negative subsetter (delete)" = TRUE), selected = FALSE)
            )
        ),
        uiOutput("subsetter_renormalize_ui"),

        hr(),

        conditionalPanel(
            condition = "input.subsetter_type == 'manual'",
            fluidRow(
                column(6,
                       tags$div(tags$b("Subset by Sample:"), class = "param_setting_title"),
                       textInput("sample_search", label = "Name Filter", value = ""),
                       uiOutput("manual_select_ui"),
                       uiOutput("manual_subset_btn_ui")
                ),
                column(6,
                       uiOutput("category_subset_ui")
                )
            )
        ),

        conditionalPanel(
            condition = "input.subsetter_type == 'list'",
            tags$div(tags$b("Upload Sample List:"),
                     pivot_help_UI("subset_upload", "File format requirement"),
                     class = "param_setting_title"),
            fluidRow(
                column(6,
                       pivot_fileInput_UI("subset"),
                       uiOutput("sb_submit_ui")
                ),
                column(6,
                       tags$p("[sample name in 1st column]"),
                       pivot_filePreview_UI("subset_pv")
                )
            )
        ),
        conditionalPanel(
            condition = "input.subsetter_type == 'stats'",
            tags$div(tags$b("Choose Range on Graph:"), class = "param_setting_title"),
            fluidRow(
                column(6,
                       uiOutput("sample_stats_plt_type_ui")
                ),
                column(6,
                       uiOutput("sample_stats_group_ui")
                )
            ),
            uiOutput("sample_stats_instr_ui"),
            uiOutput("sample_stats_ui")
        ),
        fluidRow(
            column(12,
                   shinyBS::tipify(actionButton('undo_subset_sample', label = "Undo Subset", icon = icon("times"), class = "btn-danger btn_rightAlign"),
                                   title = "Data will return to the original input dataset. All filtration/subsetting will be undone.",
                                   placement = "bottom", options = list(container = "body")))
        )
    )
})


output$is_neg_subsetter_ui <- renderUI({
    if(is.null(input$subsetter_type)) return()
    if(input$subsetter_type == "stats") {
        return(tags$p("You can only select samples by specifying ranges in the graph below."))
    } else {

    }
})


output$subsetter_renormalize_ui <- renderUI({
    checkboxInput(
        "subsetter_renormalize",
        tags$span(
            paste0("Subset with renormalization (", input$proc_method, ")"),
            shinyBS::tipify(
                bsButton("subset_renorm_tooltip", label = NULL, icon = icon("question-circle"), style = "link", size = "extra-small"),
                title = "You can change normalization methods and parameters in the File panel. The normalization method of subsets can be different from input normalization. Filtering is performed on the normalized data subset.",
                options = list(container = "body")
            )
        ),
        value = FALSE
    )
})


### Visualize stats ###
# A copy of sample_stats_tbl to put at sample subsetter
output$input_sample_stats_tbl <- DT::renderDataTable({
    if(is.null(r_data$sample_meta)) return()
    DT::datatable(r_data$sample_meta, selection = 'single', options = list(
        scrollX = T, scrollY = "500px", lengthMenu = c(20, 50, 100)
    )
    )
})

output$download_sample_stats_tbl <- downloadHandler(
    filename = "sample_stats.csv",
    content = function(file) {
        write.csv(r_data$sample_meta, file)
    }
)

output$sample_stats_plt_type_ui <- renderUI({
    options <- list("Number of genes expressed" = "num_genes_expressed", "Total raw counts" = "total_raw_reads")
    if(r_data$norm_param$method %in% c("DESeq", "Modified_DESeq")) {
        options$"Total normalized counts" <- "total_normalized_counts"
        options$"DESeq size factor" <- "deseq_size_factor"
        options$"Cook's Distance" <- "cooks"
    } else if (r_data$norm_param$method != "None"){
        options$"Total normalized counts" <- "total_normalized_counts"
    }
    selectInput("sample_stats_plt_type", "Subset based on",
                choices = options
    )
})

output$sample_stats_group_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2) return()
    categories = colnames(r_data$glb.meta)[-1]
    names(categories) <- categories
    options <- as.list(categories)
    options$None = "None"
    selectInput("sample_stats_group", label = "Color by", choices = options)
})

output$sample_stats_instr_ui <- renderUI({
    if(is.null(input$sample_stats_plt_type)) return()
    if(input$sample_stats_plt_type == "cooks") {
        tags$p("See DESeq2 vignettes for details. This module only provide visualization. You need to manually remove the sample with abnormal cook's distance.")
    } else {
        tags$p("Click and drag in the y axis direction to specify range (inclusive, only y range will be used for filtering samples).")
    }
})

output$sample_stats_ui <- renderUI({
    if(is.null(input$sample_stats_plt_type)) return()

    if(input$sample_stats_plt_type == "cooks") {
        list(
            actionButton("cooks_btn", "Compute cook's distance", class = "btn-info"),
            plotOutput("cooks_distance_plt")
        )
    } else {
        list(
            plotly::plotlyOutput("sample_stats_plot"),
            tags$br(),
            uiOutput("current_sample_ui"),
            uiOutput("plt_subset_btn_ui")
        )
    }
})

output$plt_subset_btn_ui <- renderUI({
    if(is.null(input$subsetter_renormalize) || is.null(input$is_neg_subsetter)) return()
    negf <- as.logical(input$is_neg_subsetter)
    rnorm <- as.logical(input$subsetter_renormalize)
    if(negf) {
        session$sendCustomMessage(type = "showalert", "Negative subsetter (Delete) mode is not allowed with plot range selection.")
        return()
    }
    if(!rnorm) {
        actionButton('plt_subset_btn', label = "Select", class = "btn btn-info")
    } else if(rnorm) {
        actionButton('plt_subset_btn', label = "Select & Renorm", class = "btn btn-info")
    }
})

output$sample_stats_plot <- render_Plotly({
    if(is.null(r_data$sample_meta) || is.null(input$sample_stats_plt_type) || input$sample_stats_plt_type == "cooks") return()
    r_data$glb.meta
    input$sample_stats_group
    isolate({
        withProgress(message = 'Processing...', value = 0.5, {
            tbl <- r_data$sample_meta %>% tibble::rownames_to_column("sample")
            colnames(tbl)[which(colnames(tbl) == input$sample_stats_plt_type)] <- "y"
            if(!is.null(input$sample_stats_group) && input$sample_stats_group != "None") {
                tbl$Group <- r_data$glb.meta[,input$sample_stats_group][match(tbl$sample,r_data$glb.meta[,1])]
                plt1 <- tbl %>% plotly::plot_ly(x = ~sample, y = ~y, type = "bar", color = as.character(tbl$Group), source = "sample_range_select")
            } else {
                plt1 <- tbl %>% plotly::plot_ly(x = ~sample, y = ~y, type = "bar", source = "sample_range_select")
            }

            plt1 %>% plotly::layout(
                xaxis = list(title = "sample"),
                yaxis = list(title = input$sample_stats_plt_type))
        })

    })
})

output$cooks_distance_plt <- renderPlot({
    if(is.null(r_data$cooks)) return()
    boxplot(r_data$cooks, range=0, las=2)
    title("Boxplot of Cook's Distance")
})

observeEvent(input$cooks_btn,  {
    withProgress(message = 'Processing...', value = 0.8, {
        error_I <- 0
        tryCatch({
            samplesAll <- data.frame(row.names=colnames(r_data$raw), celltype=rep("nt",length(colnames(r_data$raw))))
            dds <- DESeq2::DESeqDataSetFromMatrix(countData = r_data$raw, colData=samplesAll, design = ~ 1)
            dds <- DESeq2::estimateSizeFactors(dds)
            dds <- DESeq2::DESeq(dds)
            r_data$cooks<-log10(assays(dds)[["cooks"]])
        },
        error = function(e){
            error_I <<- 1
        }
        )

        if(error_I) {
            session$sendCustomMessage(type = "showalert", "Failed to compute Cook's distance.")
            return()
        }
    })
})



cur_sp_range <- reactiveValues()
cur_sp_range$lower <- NULL
cur_sp_range$upper <- NULL
cur_sp_range$inlist <- NULL
cur_sp_range$outlist <- NULL


observe({
    evt1 <- plotly::event_data("plotly_relayout", source = "sample_range_select")
    if(is.null(evt1$`yaxis.range[0]`)) return()
    cur_sp_range$lower <- round(evt1$`yaxis.range[0]`, digits = 1)
    cur_sp_range$upper <- round(evt1$`yaxis.range[1]`, digits = 1)
})

output$current_sample_ui <- renderUI({
    if(is.null(r_data$sample_meta) || is.null(input$sample_stats_plt_type)) return()

    if(is.null(cur_sp_range$lower) || is.null(cur_sp_range$upper)) return()
    if(input$sample_stats_plt_type == "cooks") return()

    tbl <- r_data$sample_meta %>% tibble::rownames_to_column("sample")
    cnts <- tbl[,which(colnames(tbl) == input$sample_stats_plt_type)]

    cur_sp_range$inlist <- tbl$sample[which(cnts >= cur_sp_range$lower & cnts <= cur_sp_range$upper)]
    cur_sp_range$outlist <- tbl$sample[which(!tbl$sample %in% cur_sp_range$inlist)]

    list(
        tags$li(paste0("Selected range: ", cur_sp_range$lower, " ≤ ", input$sample_stats_plt_type, " ≤ ", cur_sp_range$upper)),
        tags$p(),
        tags$li(paste0("Samples within range: ", paste(cur_sp_range$inlist, collapse = ", "))),
        tags$p(),
        tags$li(paste0("Samples to be removed: ", paste(cur_sp_range$outlist, collapse = ", "))),
        tags$br()
    )
})

observeEvent(input$plt_subset_btn, {
    if(is.null(cur_sp_range$inlist)){
        session$sendCustomMessage(type = "showalert", "Please specify a range in the graph.")
        return()
    } else if (length(cur_sp_range$inlist) < 2){
        session$sendCustomMessage(type = "showalert", "Too few samples left!")
        return()
    }

    withProgress(message = 'Subsetting', value = 0, {
        incProgress(0.3, detail = "Getting new sample list...")
        r_data <- clear_results(r_data)

        slist <- cur_sp_range$inlist # Update sample_name

        new_raw <- r_data$glb.raw[, slist] # first subset raw counts
        new_raw <- new_raw[rowSums(new_raw) > 0, ] # Reselecting features with larger than 0 total counts in the new set.
        flist <- rownames(new_raw) # the feature list thus may change (implicit filtration)

        actionText <- paste("Subset samples within range: ", cur_sp_range$lower, " ≤ ", input$sample_stats_plt_type, " ≤ ", cur_sp_range$upper)

        error_I <- 0
        tryCatch({
            r_data <- create_subset(r_data, input, flist, slist, keep_filter = F, renorm = as.logical(input$subsetter_renormalize), actionType = "Subset", actionText = actionText)
        },error = function(e){
            error_I <<- 1
        })
        if(error_I) {
            session$sendCustomMessage(type = "showalert", "Failed to create subset, please recheck normalization parameters.")
            return()
        }

        setProgress(1)
    })
})

############################### Data subsetter module ###############################
# This module facilitates the sample subsetting from the global data set. It will update two things in "data" object:
# r_data$raw and r_data$sample_name. r_data$sample_name will be used as the key to extract normalized data from global set when "Analyze!" button is pressed.
# r_data$raw is updated here because it gives updates to the data preview module to allow the user have a direct visualization of the subsetting result.
# r_data$raw won't be used for analysis.

# manual subsetter UI module


output$manual_select_ui <- renderUI({
    if(is.null(r_data$glb.raw)) return()

    samples = colnames(r_data$glb.raw)
    if(!is.null(r_data$glb.meta) && dim(r_data$glb.meta)[2] > 1 && !is.null(input$category_subset) && !is.null(input$group_subset_input)) {
        samples <- samples[which(r_data$glb.meta[, input$category_subset] %in% input$group_subset_input)]
    }

    sample_lis <- grep(input$sample_search, samples, value = "TRUE")

    if(length(sample_lis) <= 20) {
        inlen <- length(sample_lis)
    } else {
        inlen <- 20
    }

    selectInput('sample_subset_input',
                label = NULL,
                choices = sample_lis,
                selected = sample_lis,
                selectize = F,
                size = inlen,
                multiple = TRUE)
})

output$category_subset_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2) return()
    categories = colnames(r_data$glb.meta)[-1]
    names(categories) <- categories
    options <- as.list(categories)
    list(
        tags$div(tags$b("Subset by Category:"), class = "param_setting_title"),
        selectInput("category_subset", label = "Choose category", choices = options),
        uiOutput("group_subset_ui")
    )
})

output$group_subset_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2 || is.null(input$category_subset)) return() # Redundant conditions to prevent error when change meta file
    group_lis <- as.character(unique(r_data$glb.meta[, input$category_subset]))

    if(length(group_lis) <= 20) {
        inlen <- length(group_lis)
    } else {
        inlen <- 20
    }
    list(
        selectInput('group_subset_input',
                    label = "Select Group",
                    choices = group_lis,
                    selected = group_lis,
                    selectize = F,
                    size = inlen,
                    multiple = TRUE),
        tags$p("The samples in the chosen groups will be selected in the left panel.")
    )
})

output$manual_subset_btn_ui <- renderUI ({
    if(is.null(input$subsetter_renormalize) || is.null(input$is_neg_subsetter)) return()
    negf <- as.logical(input$is_neg_subsetter)
    rnorm <- as.logical(input$subsetter_renormalize)
    if(!negf && !rnorm) {
        actionButton('manual_subset_btn', label = "Select", class = "btn btn-info")
    } else if(negf && !rnorm) {
        actionButton('manual_subset_btn', label = "Delete", class = "btn btn-info")
    } else if(!negf && rnorm) {
        actionButton('manual_subset_btn', label = "Select & Renorm", class = "btn btn-info")
    } else if(negf && rnorm) {
        actionButton('manual_subset_btn', label = "Delete & Renorm", class = "btn btn-info")
    }
})

observeEvent(input$manual_subset_btn, {
    negf <- as.logical(input$is_neg_subsetter)

    if(is.null(input$sample_subset_input)){
        session$sendCustomMessage(type = "showalert", "No sample selected!")
        return()
    } else if ((length(input$sample_subset_input) < 2 && !negf) || ((length(colnames(r_data$glb.raw)) - length(input$sample_subset_input)) < 2 && negf)){
        session$sendCustomMessage(type = "showalert", "Too few samples left!")
        return()
    }
    withProgress(message = 'Subsetting', value = 0, {
        incProgress(0.3, detail = "Getting new sample list...")
        r_data <- clear_results(r_data)
        slist <- input$sample_subset_input # Update sample_name

        neglist <- colnames(r_data$glb.raw)[!colnames(r_data$glb.raw)%in%slist]

        if(negf) {
            slist <- neglist
        }

        new_raw <- r_data$glb.raw[, slist] # first subset raw counts
        new_raw <- new_raw[rowSums(new_raw) > 0, ] # Reselecting features with larger than 0 total counts in the new set.
        flist <- rownames(new_raw) # the feature list thus may change (implicit filtration)

        actionText <- paste("Subset manually picked samples")
        error_I <- 0
        tryCatch({
            r_data <- create_subset(r_data, input, flist, slist, keep_filter = F, renorm = as.logical(input$subsetter_renormalize), actionType = "Subset", actionText = actionText)
        },error = function(e){
            error_I <<- 1
        })
        if(error_I) {
            session$sendCustomMessage(type = "showalert", "Failed to create subset, please recheck normalization parameters.")
            return()
        }
        setProgress(1)
    })
})


observeEvent(input$undo_subset_sample, {
    withProgress(message = 'Processing', value = 0, {
        incProgress(0.3, detail = "Retrieving input dataset...")

        r_data <- clear_results(r_data)
        r_data <- switch_to_dataset(r_data, 1)

        setProgress(1)
    })
})


output$sb_submit_ui <- renderUI({
    if(!is.null(input$is_neg_subsetter)) {
        negf <- as.logical(input$is_neg_subsetter)
        rnorm <- as.logical(input$subsetter_renormalize)
        if(!negf && !rnorm) {
            actionButton('file_subset_btn', label = "Select", class = "btn btn-info")
        } else if(negf && !rnorm) {
            actionButton('file_subset_btn', label = "Delete", class = "btn btn-info")
        } else if(!negf && rnorm) {
            actionButton('file_subset_btn', label = "Select & Renorm", class = "btn btn-info")
        } else if(negf && rnorm) {
            actionButton('file_subset_btn', label = "Delete & Renorm", class = "btn btn-info")
        }
    }
})

# process the upload sample list
observe({
    df <- callModule(pivot_fileInput, "subset")
    callModule(pivot_filePreview, "subset_pv", df$df)
})


# get marker_feature list when the user click submit button
observeEvent(input$file_subset_btn, {
    # First process the marker feature file and get the list
    withProgress(message = 'Subsetting', value = 0, {
        negf <- as.logical(input$is_neg_subsetter)

        inFile <- callModule(pivot_fileInput, "subset")
        sample_tbl <- as.data.frame(inFile$df)

        sample_names <- as.character(sample_tbl[,1])
        slist <- sample_names[which(sample_names %in% colnames(r_data$glb.raw))]

        if(length(slist) != length(sample_names)) {
            message_ss <- paste0(length(sample_names) - length(slist)," samples in your sample list (", length(sample_names),") are not found in the global dataset.")
            session$sendCustomMessage(type = "showalert", message_ss)
        }

        neglist <- colnames(r_data$glb.raw)[!colnames(r_data$glb.raw) %in% slist]

        if(negf) {
            if(length(neglist) < 2) {
                session$sendCustomMessage(type = "showalert", "Too few samples left!")
                return(NULL)
            }
            slist <- neglist
        } else {
            if(length(slist) < 2) {
                session$sendCustomMessage(type = "showalert", "Too few samples left!")
                return(NULL)
            }
        }
        r_data <- clear_results(r_data)
        new_raw <- r_data$glb.raw[, slist] # first subset raw counts
        new_raw <- new_raw[rowSums(new_raw) > 0, ] # Reselecting features with larger than 0 total counts in the new set.
        flist <- rownames(new_raw) # the feature list thus may change (implicit filtration)
        actionText <- paste("Subset based on uploaded sample sheet")

        error_I <- 0
        tryCatch({
            r_data <- create_subset(r_data, input, flist, slist, keep_filter = F, renorm = as.logical(input$subsetter_renormalize), actionType = "Subset", actionText = actionText)
        },error = function(e){
            error_I <<- 1
        })
        if(error_I) {
            session$sendCustomMessage(type = "showalert", "Failed to create subset, please recheck normalization parameters.")
            return()
        }
        setProgress(1)
    })
})

