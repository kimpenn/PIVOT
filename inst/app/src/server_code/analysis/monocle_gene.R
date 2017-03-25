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
            status = "danger",
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
            status = "danger",
            solidHeader = F,
            tags$div(tags$b("Clustering genes by pseudotemporal expression pattern"), class = "param_setting_title"),
            fluidRow(
                column(6, selectInput("mn_gene_for_clust", "Choose genes for clustering", choices = list("Custom gene list" = "custom", "Monocle DE genes" = "de")),
                       uiOutput("mn_gene_clust_upload_ui")),
                column(6, numericInput("mn_gene_clust_num", "Number of clusters", min = 1, max = 10, step =1, value = 4))
            ),
            fluidRow(
                column(8, uiOutput("mn_gene_clust_num_check")),
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




# Cluster genes

output$mn_gene_clust_upload_ui <- renderUI({
    if(input$mn_gene_for_clust != "custom") return()
    content <- list(
        fluidRow(
            column(6,
                   wellPanel(
                       fileInput('mn_list_file', label = NULL, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                       checkboxInput('mn_header', 'Header', value = F),
                       radioButtons('mn_sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), selected = '\t'),
                       radioButtons('mn_quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"'),
                       actionButton("mn_list_submit", "Submit List", class = "btn btn-info btn_rightAlign")
                   ),
                   uiOutput("mn_gene_clust_upload_text_inmodal")
            ),
            column(6,
                   tags$p("[feature name in 1st column]"),
                   DT::dataTableOutput('mn_list_tbl_show')
            )
        )
    )

    list(
        actionButton("mn_custom_btn", label = "Upload a custom gene list for clustering", class = "btn-warning btn_rightAlign"),
        shinyBS::bsModal(id = "mn_custom_modal", "Upload a custom gene list", "mn_custom_btn", size = "large", content),
        uiOutput("mn_gene_clust_upload_text")
    )
})

output$mn_gene_clust_upload_text <- renderUI({
    if(is.null(r_data$monocle_gene_submitted)) return()
    list(
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b(paste(length(r_data$monocle_gene_submitted), "genes have been successfully uploaded.")), style = "font-size:110%;")
    )
})

output$mn_gene_clust_num_check <- renderUI({
    if(is.null(r_data$monocle_gene_for_clust)) return()
    if(length(r_data$monocle_gene_for_clust) > 1000) {
        return(tags$p("Do not support more than 1000 genes."))
    }
})

output$mn_gene_clust_upload_text_inmodal <- renderUI({
    if(is.null(r_data$monocle_gene_submitted)) return()
    list(
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b(paste(length(r_data$monocle_gene_submitted), "genes have been successfully uploaded.")), style = "font-size:110%;")
    )
})


mn_clust_gene <- reactiveValues()
mn_clust_gene$tbl <- NULL

# process the upload feature list
observe({
    inFile <- input$mn_list_file
    error_I <- 0
    if (!is.null(inFile)) {
        tryCatch({
            mn_clust_gene$tbl <- read.csv(inFile$datapath, header=input$mn_header, sep=input$mn_sep, quote=input$mn_quote)
        },
        error = function(e){
            error_I <<- 1
        })
    }
    if(error_I) {
        session$sendCustomMessage(type = "showalert", "Unsupported file format.")
        return()
    }
})

output$mn_list_tbl_show <- DT::renderDataTable({
    if(is.null(mn_clust_gene$tbl)) return()
    DT::datatable(mn_clust_gene$tbl, options = list(scrollX = TRUE, scrollY = "350px", searching = FALSE))
})

observeEvent(input$mn_list_submit, {

    # First process the marker feature file and get the list
    if (is.null(mn_clust_gene$tbl) || nrow(mn_clust_gene$tbl) == 0)
    {
        session$sendCustomMessage(type = "showalert", "Give me something!")
        return(NULL)
    }
    marker_names <- make.names(as.character(unique(mn_clust_gene$tbl[,1])))

    cur_flist <- rownames(r_data$raw)


    flist <- cur_flist[match(toupper(marker_names), toupper(cur_flist))]
    flist <- flist[!is.na(flist)]
    if(length(flist) != length(marker_names)) {
        message_gl <- paste0(length(marker_names) - length(flist)," features in your gene list (", length(marker_names),") are not found in the current dataset.")
        session$sendCustomMessage(type = "showalert", message_gl)
    }
    r_data$monocle_gene_submitted <- flist
})



output$monocle_geneclust_tbl <- DT::renderDataTable({
    if(is.null(r_data$monocle_clusters)) return()
    tbl <- r_data$monocle_clusters$tbl
    DT::datatable(tbl,
                  options = list(
                      scrollX = F, scrollY = "400px", lengthMenu = c(20, 50, 100)
                  )
    )
})

output$download_monocle_geneclust_tbl <- downloadHandler(
    filename = "monocle_geneclust_tbl.csv",
    content = function(file) {
        tbl <-r_data$monocle_clusters$tbl
        write.csv(tbl, file)
    }
)

observe({
    if(is.null(input$mn_gene_for_clust)) return()
    if(input$mn_gene_for_clust == "custom") {
        if(is.null(r_data$monocle_gene_submitted)) {
            return()
        } else {
            r_data$monocle_gene_for_clust <- r_data$monocle_gene_submitted
        }
    } else if(input$mn_gene_for_clust == "de") {
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


observeEvent(input$mn_gene_clust,{
    req(r_data$cellset, r_data$monocle_gene_for_clust)
    if(is.null(pData(r_data$cellset)$State)) {
        session$sendCustomMessage(type = "showalert", "Please perform Monocle state ordering first.")
        return()
    }

    if(length(r_data$monocle_gene_for_clust) > 1000) {
        session$sendCustomMessage(type = "showalert", "Do not support more than 1000 genes.")
        return()
    }

    flist <- r_data$monocle_gene_for_clust
    error_I <- 0
    withProgress(message = 'Processing...', value = 0.8, {
        tryCatch({
            cds_subset <- r_data$cellset[flist,]
            r_data$monocle_clusters <- plot_pseudotime_heatmap2(cds_subset, num_clusters = input$mn_gene_clust_num, cores = 1, show_rownames = T, return_list = T)
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
    if(is.null(r_data$monocle_clusters)) return()
    ph_res <- r_data$monocle_clusters$hmap
    grid::grid.rect(gp = grid::gpar("fill", col = NA))
    grid::grid.draw(ph_res$gtable)
})



# function override
table.ramp <- function (n, mid = 0.5, sill = 0.5, base = 1, height = 1)
{
    x <- seq(0, 1, length.out = n)
    y <- rep(0, length(x))
    sill.min <- max(c(1, round((n - 1) * (mid - sill/2)) + 1))
    sill.max <- min(c(n, round((n - 1) * (mid + sill/2)) + 1))
    y[sill.min:sill.max] <- 1
    base.min <- round((n - 1) * (mid - base/2)) + 1
    base.max <- round((n - 1) * (mid + base/2)) + 1
    xi <- base.min:sill.min
    yi <- seq(0, 1, length.out = length(xi))
    i <- which(xi > 0 & xi <= n)
    y[xi[i]] <- yi[i]
    xi <- sill.max:base.max
    yi <- seq(1, 0, length.out = length(xi))
    i <- which(xi > 0 & xi <= n)
    y[xi[i]] <- yi[i]
    height * y
}

rgb.tables <- function (n, red = c(0.75, 0.25, 1), green = c(0.5, 0.25, 1),
                        blue = c(0.25, 0.25, 1))
{
    rr <- do.call("table.ramp", as.list(c(n, red)))
    gr <- do.call("table.ramp", as.list(c(n, green)))
    br <- do.call("table.ramp", as.list(c(n, blue)))
    rgb(rr, gr, br)
}

plot_pseudotime_heatmap2 <- function (cds_subset, cluster_rows = TRUE, hclust_method = "ward.D2",
                                      num_clusters = 6, hmcols = NULL, add_annotation_row = NULL,
                                      add_annotation_col = NULL, show_rownames = FALSE, use_gene_short_name = TRUE,
                                      norm_method = c("vstExprs", "log"), scale_max = 3, scale_min = -3,
                                      trend_formula = "~sm.ns(Pseudotime, df=3)", return_list = FALSE,
                                      cores = 1)
{
    newdata <- data.frame(Pseudotime = seq(0, max(pData(cds_subset)$Pseudotime),
                                           length.out = 100))
    m <- monocle::genSmoothCurves(cds_subset, cores = cores, trend_formula = trend_formula,
                                  relative_expr = T, new_data = newdata)
    m = m[!apply(m, 1, sum) == 0, ]
    norm_method <- match.arg(norm_method)
    if (norm_method == "vstExprs" && is.null(cds_subset@dispFitInfo[["blind"]]$disp_func) ==
        FALSE) {
        m = monocle::vstExprs(cds_subset, expr_matrix = m)
    }
    else if (norm_method == "log") {
        m = log10(m + pseudocount)
    }
    m = m[!apply(m, 1, sd) == 0, ]
    m = Matrix::t(scale(Matrix::t(m), center = TRUE))
    m = m[is.na(row.names(m)) == FALSE, ]
    m[is.nan(m)] = 0
    m[m > scale_max] = scale_max
    m[m < scale_min] = scale_min
    heatmap_matrix <- m
    row_dist <- as.dist((1 - cor(Matrix::t(heatmap_matrix)))/2)
    row_dist[is.na(row_dist)] <- 1
    bks <- seq(-3.1, 3.1, by = 0.1)
    if (is.null(hmcols)) {
        hmcols <- rgb.tables(length(bks) - 1, red = c(0.8, 0.2, 1), green = c(0.5, 0.4, 0.8),
                             blue = c(0.2, 0.2, 1))
    }
    ph <- pheatmap::pheatmap(heatmap_matrix, useRaster = T, cluster_cols = FALSE,
                             cluster_rows = cluster_rows, show_rownames = F, show_colnames = F,
                             clustering_distance_rows = row_dist, clustering_method = hclust_method,
                             cutree_rows = num_clusters, silent = TRUE, filename = NA,
                             breaks = bks, color = hmcols)
    annotation_row <- data.frame(Cluster = factor(cutree(ph$tree_row,
                                                         num_clusters)))
    if (use_gene_short_name == TRUE) {
        if (is.null(fData(cds_subset)$gene_short_name) == FALSE) {
            feature_label <- as.character(fData(cds_subset)[row.names(heatmap_matrix),
                                                            "gene_short_name"])
            feature_label[is.na(feature_label)] <- row.names(heatmap_matrix)
            row_ann_labels <- as.character(fData(cds_subset)[row.names(annotation_row),
                                                             "gene_short_name"])
            row_ann_labels[is.na(row_ann_labels)] <- row.names(annotation_row)
        }
        else {
            feature_label <- row.names(heatmap_matrix)
            row_ann_labels <- row.names(annotation_row)
        }
    }
    else {
        feature_label <- row.names(heatmap_matrix)
        row_ann_labels <- row.names(annotation_row)
    }
    row.names(heatmap_matrix) <- feature_label
    row.names(annotation_row) <- row_ann_labels
    colnames(heatmap_matrix) <- c(1:ncol(heatmap_matrix))
    ph_res <- pheatmap::pheatmap(heatmap_matrix[, ], useRaster = T, cluster_cols = FALSE,
                                 cluster_rows = cluster_rows, show_rownames = show_rownames,
                                 show_colnames = F, clustering_distance_rows = row_dist,
                                 clustering_method = hclust_method, cutree_rows = num_clusters,
                                 annotation_row = annotation_row, treeheight_row = 20,
                                 breaks = bks, fontsize = 6, color = hmcols, silent = TRUE,
                                 filename = NA)
    grid::grid.rect(gp = grid::gpar("fill", col = NA))
    grid::grid.draw(ph_res$gtable)
    if (return_list) {
        return(list(hmap = ph_res, tbl = annotation_row))
    }
}



