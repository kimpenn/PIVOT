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


# Cell state ordering by monocle

output$monocle_state_ui <- renderUI({
    if(is.null(r_data$monocle_ok)) {
        return(tags$li("Please initiate monocle celldataset first."))
    }

    list(
        enhanced_box(
            width = 12,
            title = "Cell State Ordering/Unsupervised Clustering",
            id = "monocle_state",
            status = "info",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("STEP 1: Set Genes for Ordering/Clustering"), class = "param_setting_title"),
            tags$li("You must initialize monocle CellDataSet in the previous page before running this module."),
            fluidRow(
                column(4,
                       selectInput("ordering_genes_type", "Genes to be used for cell ordering/clustering",
                                   choices = list("Use all genes in the current dataset for ordering" = "all",
                                                  "Use variably expressed genes for ordering" = "var",
                                                  "Use Monocle DE (qval < 0.1)" = "de"))
                ),
                column(8,
                       uiOutput("mn_order_gene_params")
                )
            ),
            fluidRow(
                column(4,
                       tags$div(tags$b("Preview of selected genes for ordering"), class = "param_setting_title"),
                       DT::dataTableOutput("mn_ordering_genes"),
                       actionButton("mn_set_ordering_gene", "Set Ordering Genes", class = "btn-info btn_rightAlign")
                ),
                column(8,
                       tags$div(tags$b("Dispersion plot highlighting ordering genes"), class = "param_setting_title"),
                       plotOutput("monocle_var_plot"),
                       uiOutput("monocle_order_ok"),
                       downloadButton("mn_ordering_genes_download","Download Ordering Gene List", class = "btn btn-success btn_rightAlign")
                )
            )
        ),

        enhanced_box(
            width = 12,
            title = NULL,
            status = "info",
            solidHeader = F,
            tags$div(tags$b("STEP 2: Cell State Ordering/Unsupervised Clustering"), class = "param_setting_title"),
            selectInput("mn_run_type", "Analysis Type", choices = list("Cell state ordering" = "state", "Unsupervised clustering" = "clust")),
            uiOutput("mn_run_params_ui"),
            uiOutput("mn_run_btn_ui"),
            hr(),
            fluidRow(
                column(6,
                       tags$div(tags$b("Ordering/Clustering Result"), class = "param_setting_title"),
                       DT::dataTableOutput("monocle_state_tbl"),
                       downloadButton("download_monocle_state_tbl", "Download", class = "btn btn-success")
                ),
                column(6,
                       tags$div(tags$b("State/Cluster - Group Comparison"), class = "param_setting_title"),
                       uiOutput("monocle_state_group_compare_ui"),
                       DT::dataTableOutput("monocle_state_group_matrix"),
                       plotOutput("monocle_state_group_plt")
                )
            ),
            hr(),
            tags$div(tags$b("Cell Trajectory Plot"), class = "param_setting_title"),
            fluidRow(
                column(4,
                       DT::dataTableOutput("monocle_state_gene_tbl")
                       #actionButton("mn_state_reset", "Reset Selection", class = "btn-danger btn_rightAlign")
                ),
                column(8,
                       tags$p("Click genes on the left table and see their expression."),
                       fluidRow(
                           column(6,
                                  uiOutput("monocle_state_plt_color_ui")
                           ),
                           column(6,
                                  selectInput("monocle_state_show","Plot content", choices = list("Only show points" = "none", "Show tree" = "tree"), selected = "tree")
                           )
                       ),
                       plotOutput("monocle_state_plt", height = "450px")
                )
            )
        ),

        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Cole Trapnell and Davide Cacchiarelli et al (2014): The dynamics and regulators of cell fate decisions are revealed by pseudo-temporal
                        ordering of single cells. Nature Biotechnology", class = "citation"),
                tags$li("Monocle Website:", a("http://cole-trapnell-lab.github.io/monocle-release/", src = "http://cole-trapnell-lab.github.io/monocle-release/")),
                tags$li("Monocle was written by Cole Trapnell with input from Davide Cacchiarelli and is provided under the OSI-approved Artistic License (version 2.0).")
                )
        )
    )
})

output$mn_run_params_ui <- renderUI({
    dim_choices <- c("DDRTree","ICA")
    names(dim_choices) <- dim_choices
    cluster_choices <- c("DDRTree")
    names(cluster_choices) <- cluster_choices
    if(input$mn_run_type == "state") {
        fluidRow(
            column(4, selectInput("mn_rd_method", "Dimensionality reduction method", choices = dim_choices)),
            column(4, numericInput_1("monocle_num_paths", "Number of end-point cell states allowed", value = 2)),
            column(4, radioButtons("monocle_reverse","Reverse the biological process?", choices = list("No Reverse" = F, "Reverse" = T), inline= T, selected = F))
        )
    } else {
        fluidRow(
            column(4, numericInput("mn_cell_clust_num", "Number of clusters", min = 1, max = 20, step =1, value = 2)),
            column(4, selectInput("mn_cell_clust_method", "Clustering method", choices = cluster_choices))
        )
    }
})


output$mn_order_gene_params <- renderUI({
    if(input$ordering_genes_type == "all") {
        return()
    } else if(input$ordering_genes_type == "var") {
        return(fluidRow(
            column(6, numericInput("mn_mean_limit", "Set lower mean limit", value = 1, step = 1, min = 0)),
            column(6, numericInput("mn_var_limit", "Set lower dispersion limit", value = 1, step = 1, min = 0, max = 10))
        ))
    } else if(input$ordering_genes_type == "de"){
        if(is.null(r_data$monocle_results)) return()
        ordering_genes <- row.names(subset(r_data$monocle_results, qval <= 0.1))
        if(length(ordering_genes) > 1000) {
            return(sliderInput("state_order_top_gene", "Number of top genes (ranked by qval) to be used for ordering:", min = 2, max = length(ordering_genes), value = length(ordering_genes), step = 1, round = T))
        }
    } else {
        return()
    }
})


output$mn_run_btn_ui <- renderUI({
    if(input$mn_run_type == "state") {
        actionButton("mn_generate_state", "Perform ordering", class = "btn-info")
    } else {
        actionButton("mn_generate_clust", "Perform clustering", class = "btn-info")
    }

})

ordering_genes <- reactive({
    input$ordering_genes_type
    input$state_order_top_gene
    input$mn_mean_limit
    input$mn_var_limit

    isolate({
        if(is.null(r_data$cellset)) return()
        if(is.null(input$ordering_genes_type) || input$ordering_genes_type == "all") {
            ordering_genes <- row.names(r_data$raw)
        } else if (input$ordering_genes_type == "var") {
            if(is.null(input$mn_var_limit)) return()
            if(is.null(r_data$cellset@dispFitInfo[["blind"]])) {
                session$sendCustomMessage(type = "showalert", "Dispersion estimation is not available for the expression family you selected for your data.")
                return()
            }
            disp_table <- dispersionTable(r_data$cellset)
            ordering_genes <- subset(disp_table, mean_expression >= input$mn_mean_limit & dispersion_empirical >= input$mn_var_limit * dispersion_fit)$gene_id
        } else if (input$ordering_genes_type == "de") { # Use monocle genes
            if(is.null(r_data$monocle_results)) {
                session$sendCustomMessage(type = "showalert", "Please perform Monocle DE analysis first.")
                updateSelectInput(session, "ordering_genes_type", "Choose which genes to be used for cell ordering",
                                  choices = list("Use all genes in the current dataset for ordering" = "all",
                                                 "Use variably expressed genes for ordering" = "var",
                                                 "Use Monocle DE genes (qval < 0.1)" = "de"))
                updateTabItems(session, "tabs", "monocle")
                return()
            } else {
                ordering_genes <- row.names(subset(r_data$monocle_results, qval <= 0.1))
                if(!is.null(input$state_order_top_gene)) {
                    ordering_genes <- ordering_genes[1:input$state_order_top_gene]
                }
            }
            if(length(ordering_genes) < 2) {
                fData(r_data$cellset)$use_for_ordering <- FALSE
                session$sendCustomMessage(type = "showalert", "Too few genes for ordering.")
                return()
            }
        }
        return(ordering_genes)
    })
})

output$monocle_var_plot <- renderPlot({
    if(is.null(r_data$cellset)) return()
    if(is.null(r_data$cellset@dispFitInfo[["blind"]])) return()
    plot_ordering_genes(r_data$cellset)
})


output$mn_ordering_genes <- DT::renderDataTable({
    req(r_data$cellset)
    tbl<- data.frame(gene = ordering_genes())
    DT::datatable(tbl, rownames = FALSE, options = list(scrollX = F, scrollY = "270px",lengthMenu = c(20, 50, 100)))
})

output$mn_ordering_genes_download <- downloadHandler(
    filename = "monocle_gene_for_ordering.csv",
    content = function(file) {
        tbl<- data.frame(gene_for_ordering = fData(r_data$cellset)$gene_short_name[which(fData(r_data$cellset)$use_for_ordering == TRUE)])
        write.csv(tbl, file)
    }
)

observeEvent(input$mn_set_ordering_gene, {
    req(r_data$cellset, ordering_genes())
    r_data$cellset <- setOrderingFilter(r_data$cellset, ordering_genes())
    session$sendCustomMessage(type = "showalert", "Ordering genes set.")
})

output$monocle_order_ok <- renderUI({
    if(!is.null(fData(r_data$cellset)$use_for_ordering) && sum(fData(r_data$cellset)$use_for_ordering)){
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b("Ordering genes found."), style = "font-size:110%;")
    } else {
        return()
    }
})


# Perform ordering
observeEvent(input$mn_generate_state, {
    req(r_data$cellset, !is.null(fData(r_data$cellset)$use_for_ordering))
    error_I <- 0
    withProgress(message = 'Processing...', value = 0.8, {
        tryCatch({
            r_data$cellset <- reduceDimension(r_data$cellset, reduction_method = input$mn_rd_method)
            r_data$cellset <- orderCells(r_data$cellset, num_paths = input$monocle_num_paths, reverse = input$monocle_reverse)
        },
        error = function(e){
            error_I <<- 1
        })
    })
    if(error_I) {
        session$sendCustomMessage(type = "showalert", "Ordering failed. Note: if you are running ICA, it has been noticed that monocle2 has a bug in the get_next_node_id function, which does not seem to affect the result.")
        return()
    }
})

observeEvent(input$mn_generate_clust, {
    if(is.null(r_data$cellset)) return()

    error_I <- 0
    withProgress(message = 'Processing...', value = 0.8, {
        tryCatch({
            r_data$cellset <- clusterCells(r_data$cellset, num_clusters=input$mn_cell_clust_num, method = input$mn_cell_clust_method)
        },
        error = function(e){
            error_I <<- 1
        })
    })
    if(error_I) {
        session$sendCustomMessage(type = "showalert", "Clustering failed.")
        return()
    }
})



output$monocle_state_tbl <- DT::renderDataTable({
    req(r_data$cellset, !is.null(pData(r_data$cellset)$State))

    tbl <- pData(r_data$cellset)
    tbl <- tbl[, which(!colnames(tbl) %in% c("sample", "Size_factor"))]
    DT::datatable(tbl, options = list(
                      scrollY = "500px", scrollX = T, lengthMenu = c(20, 50, 100)
                  )
    )
})

output$download_monocle_state_tbl <- downloadHandler(
    filename = "monocle_state_tbl.csv",
    content = function(file) {
        req(r_data$cellset, !is.null(pData(r_data$cellset)$State))
        tbl <- pData(r_data$cellset)
        tbl <- tbl[, which(!colnames(tbl) %in% c("sample", "Size_factor"))]
        write.csv(tbl, file)
    }
)

output$monocle_state_group_compare_ui <- renderUI({
    req(r_data$cellset)
    if(is.null(r_data$meta) || ncol(r_data$meta) < 2) return()
    optionsTo <- list()
    if(!is.null(pData(r_data$cellset)$State)) {
        optionsTo$State <- "State"
    }
    if(!is.null(pData(r_data$cellset)$Cluster)) {
        optionsTo$Cluster <- "Cluster"
    }
    categories = colnames(r_data$meta)[-1]
    names(categories) <- categories
    optionsFrom <- as.list(categories)
    fluidRow(
        column(2, tags$b("Compare")),
        column(4, selectInput("mn_compare_from", NULL, choices = optionsFrom)),
        column(2, tags$b("TO")),
        column(4, selectInput("mn_compare_to", NULL, choices = optionsTo))
    )

})

output$monocle_state_group_matrix <- DT::renderDataTable({
    req(r_data$cellset, input$mn_compare_from, input$mn_compare_to)
    tbl <- table(pData(r_data$cellset)[[input$mn_compare_from]], pData(r_data$cellset)[[input$mn_compare_to]])
    DT::datatable(as.data.frame.matrix(tbl))
})

output$monocle_state_group_plt <- renderPlot({
    req(r_data$cellset, input$mn_compare_from, input$mn_compare_to)
    plot(as.factor(pData(r_data$cellset)[[input$mn_compare_from]]), as.factor(pData(r_data$cellset)[[input$mn_compare_to]]), xlab=input$mn_compare_from, ylab = input$mn_compare_to)
})

output$monocle_state_gene_tbl <- DT::renderDataTable({ # The same as monocle_genelist_tbl except only allow single gene choice, used for mst plot
    req(r_data$cellset)
    #input$mn_state_reset
    tbl<- data.frame(gene_for_ordering = fData(r_data$cellset)$gene_short_name[which(fData(r_data$cellset)$use_for_ordering == TRUE)])

    if(nrow(tbl) == 0) return()
    DT::datatable(tbl, selection = 'single', rownames = FALSE, options = list(
        scrollX = F, scrollY = "380px", lengthMenu = c(20, 50, 100)
    )
    )
})


output$monocle_state_plt_color_ui <- renderUI({
    if(is.null(r_data$cellset)) return()
    options <- list()
    if(!is.null(pData(r_data$cellset)$State)) {
        options$State <- "State"
        options$Pseudotime = "Pseudotime"
    }
    if(!is.null(pData(r_data$cellset)$Cluster)) {
        options$Cluster <- "Cluster"
    }

    categories = colnames(r_data$meta)[-1]
    names(categories) <- categories
    options <- c(options, as.list(categories))

    selectInput("monocle_state_plt_color", "Color cells by", choices = options)
})

output$monocle_state_plt <- renderPlot({
    req(r_data$cellset, pData(r_data$cellset)$State, input$monocle_state_plt_color)
    tree1 = ifelse(input$monocle_state_show %in% c("tree", "both"), T, F)
    mst1 = ifelse(input$monocle_state_show %in% c("mst", "both"), T, F)

    tbl<- data.frame(gene_for_ordering = fData(r_data$cellset)$gene_short_name[which(fData(r_data$cellset)$use_for_ordering == TRUE)])

    s = input$monocle_state_gene_tbl_row_last_clicked
    if(length(s))  {
        s <- as.character(tbl$gene_for_ordering[s])
        plot_cell_trajectory(r_data$cellset,  color_by = input$monocle_state_plt_color, show_tree = tree1, show_backbone = mst1, markers = s)
    } else {
        plot_cell_trajectory(r_data$cellset,  color_by = input$monocle_state_plt_color, show_tree = tree1, show_backbone = mst1, markers = NULL)
    }
})
