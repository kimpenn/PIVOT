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


# Gene expression Plot/Clustering by monocle




output$monocle_gene_ui <- renderUI({
    if(is.null(r_data$monocle_ok)) {
        return(tags$li("Please initiate monocle celldataset first."))
    }

    list(
        enhanced_box(
            width = 12,
            title = "Monocle Gene Expression Pattern",
            id = "monocle_gene",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("Gene Expression Plot"), class = "param_setting_title"),
            fluidRow(
                column(3,
                       selectInput("monocle_glist_type", "Gene List:", choices = list("Monocle DE Genes" = "de", "Genes selected for ordering" = "order")),
                       DT::dataTableOutput("monocle_genelist_tbl"),
                       actionButton("mn_gene_reset", "Reset Selection", class = "btn-danger btn_rightAlign")
                ),
                column(9,
                       tags$p("Click genes on the left table and see their expression."),
                       br(),
                       fluidRow(
                           column(4, selectInput("monocle_gene_plt_type", "Plot type", choices = list("Gene expression in pseudotime" = "time", "Expression jitter plot" = "jitter"))),
                           uiOutput("monocle_time_plt_color_ui")
                       ),
                       uiOutput("monocle_time_plt_ui")
                )
            )

        ),

        enhanced_box(
            width = 12,
            title = NULL,
            status = "primary",
            solidHeader = F,
            tags$div(tags$b("Clustering genes by pseudotemporal expression pattern"), class = "param_setting_title"),
            fluidRow(
                column(4, selectInput("mn_gene_for_clust", "Choose genes for clustering", choices = list("Custom gene list" = "custom", "Monocle DE genes" = "de"))),
                column(4, numericInput("mn_gene_clust_num", "Number of clusters", min = 1, max = 10, step =1, value = 4)),
                column(4, numericInput("mn_gene_clust_ncore", "Number of cores:", value = 1, min = 1, step=1))
            ),
            fluidRow(
                column(4, uiOutput("mn_gene_upload_ui")),
                column(4, uiOutput("mn_gene_clust_num_check")),
                column(4, actionButton("mn_gene_clust", "Cluster genes", class = "btn-info btn_rightAlign") )
            ),

            hr(),
            fluidRow(
                column(4,
                       DT::dataTableOutput("monocle_geneclust_tbl"),
                       downloadButton("download_monocle_geneclust_tbl", "Download", class = "btn btn-success")
                ),
                column(8,
                       plotOutput("monocle_clust_plt", height = "600px")
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



output$monocle_genelist_tbl <- DT::renderDataTable({
    req(r_data$cellset, input$monocle_glist_type)
    # For reset
    input$mn_gene_reset

    if(input$monocle_glist_type == "de") {
        if(is.null(r_data$monocle_results)) return()
        tbl <- subset(r_data$monocle_results, qval <= 0.1) %>% tibble::rownames_to_column("gene")%>% dplyr::select(gene, pval, qval)
    } else {
        tbl<- data.frame(gene_for_ordering = fData(r_data$cellset)$gene_short_name[which(fData(r_data$cellset)$use_for_ordering == TRUE)])
    }

    if(nrow(tbl) == 0) return()
    DT::datatable(tbl, rownames = FALSE, options = list(
        scrollX = F, scrollY = "380px", lengthMenu = c(20, 50, 100)
    )
    )
})


output$monocle_time_plt_ui <- renderUI({
    req(r_data$cellset, pData(r_data$cellset)$State, input$monocle_glist_type)

    s = input$monocle_genelist_tbl_rows_selected
    n<-length(s)
    h1 <- paste0(250 * log(n/2 + 3), "px")
    plotOutput("monocle_time_plt", height = h1)
})

output$monocle_time_plt_color_ui <- renderUI({
    req(r_data$cellset, input$monocle_gene_plt_type)

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
    if(input$monocle_gene_plt_type == "time") {
        group_ui <- NULL
    } else {
        group_ui <- column(4, selectInput("monocle_time_plt_group", "Group cells by", choices = options))
    }
    list(
        group_ui,
        column(4, selectInput("monocle_time_plt_color", "Color cells by", choices = options))
    )
})

output$monocle_time_plt <- renderPlot({
    req(r_data$cellset, pData(r_data$cellset)$State, input$monocle_glist_type, input$monocle_time_plt_color)

    s = input$monocle_genelist_tbl_rows_selected

    if(input$monocle_glist_type == "de") {
        req(r_data$monocle_results)
        tbl <- subset(r_data$monocle_results, qval <= 0.1) %>% tibble::rownames_to_column("gene")%>% dplyr::select(gene, pval, qval)
    } else {
        tbl<- data.frame(gene_for_ordering = fData(r_data$cellset)$gene_short_name[which(fData(r_data$cellset)$use_for_ordering == TRUE)])
    }


    if (length(s)) {
        selected_gene <- tbl[s, 1]
    } else {
        return()
    }

    cds_subset <- r_data$cellset[selected_gene, ]
    if(input$monocle_gene_plt_type == "time") {
        plot_genes_in_pseudotime(cds_subset, color_by = input$monocle_time_plt_color, cell_size = 3, ncol = 2)
    } else if(input$monocle_gene_plt_type == "jitter") {
        req(input$monocle_time_plt_group)
        plot_genes_jitter(cds_subset, plot_trend = T, grouping = input$monocle_time_plt_group, color_by = input$monocle_time_plt_color, cell_size = 3, ncol = 2)
    }
})




#################### Cluster genes ##################

output$mn_gene_upload_ui <- renderUI({
    req(input$mn_gene_for_clust == "custom")
    pivot_featureInputModal_UI("mn_gene_upload", "Input custom feature list")
})

monocle_clust_gene <- callModule(pivot_featureInputModal, "mn_gene_upload", r_data = r_data, match_rdata = T)

observe({
    req(input$mn_gene_for_clust)
    if(input$mn_gene_for_clust == "custom") {
        req(monocle_clust_gene())
        r_data$monocle_gene_for_clust <- monocle_clust_gene()
    } else {
        if(is.null(r_data$monocle_results)) {
            session$sendCustomMessage(type = "showalert", "Please perform Monocle DE analysis first.")
            updateSelectInput(session, "mn_gene_for_clust", "Choose genes for clustering", choices = list("Custom gene list" = "custom", "Monocle DE genes" = "de"))
            updateTabItems(session, "tabs", "monocle")
            return()
        } else {
            r_data$monocle_gene_for_clust <- row.names(subset(r_data$monocle_results, qval <= 0.1))
        }
    }
})

output$mn_gene_clust_num_check <- renderUI({
    if(is.null(r_data$monocle_gene_for_clust)) return()
    if(length(r_data$monocle_gene_for_clust) > 2000) {
        return(tags$p("Do not support more than 2000 genes."))
    }
})

output$monocle_geneclust_tbl <- DT::renderDataTable({
    if(is.null(r_data$monocle_gene_clusters)) return()
    tbl <- r_data$monocle_gene_clusters$tbl
    DT::datatable(tbl,
                  options = list(
                      scrollX = F, scrollY = "400px", lengthMenu = c(20, 50, 100)
                  )
    )
})

output$download_monocle_geneclust_tbl <- downloadHandler(
    filename = "monocle_geneclust_tbl.csv",
    content = function(file) {
        tbl <-r_data$monocle_gene_clusters$tbl
        write.csv(tbl, file, row.names = T)
    }
)


observeEvent(input$mn_gene_clust,{
    req(r_data$cellset, r_data$monocle_gene_for_clust)
    if(is.null(pData(r_data$cellset)$State)) {
        session$sendCustomMessage(type = "showalert", "Please perform Monocle state ordering first.")
        return()
    }

    if(length(r_data$monocle_gene_for_clust) > 2000) {
        session$sendCustomMessage(type = "showalert", "Do not support more than 2000 genes.")
        return()
    }

    flist <- r_data$monocle_gene_for_clust
    error_I <- 0
    withProgress(message = 'Processing...', value = 0.8, {
        tryCatch({
            cds_subset <- r_data$cellset[flist,]
            r_data$monocle_gene_clusters <- plot_pseudotime_heatmap(cds_subset, num_clusters = input$mn_gene_clust_num, cores = input$mn_gene_clust_ncore, show_rownames = T, return_heatmap = T)
            r_data$monocle_gene_clusters$tbl <- data.frame(cluster=cutree(r_data$monocle_gene_clusters$tree_row, k=input$mn_gene_clust_num))
        },
        error = function(e){
            error_I <<- 1
        })
    })
    if(error_I) {
        session$sendCustomMessage(type = "showalert", "Gene clustering failed.")
        return()
    }
})


output$monocle_clust_plt <- renderPlot({
    if(is.null(r_data$monocle_gene_clusters)) return()
    grid::grid.draw(r_data$monocle_gene_clusters$gtable)
})


