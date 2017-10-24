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


# hierachical clustering
output$hclust_ui <- renderUI({
    list(
        enhanced_box(
            title = "Hierarchical Clustering",
            id = "hierarchical_clust",
            status = "primary",
            width = 12,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("Clustering Settings:"), class = "param_setting_title"),
            fluidRow(
                pivot_dataScale_UI("hc_scale", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts", "Standardized Counts", "Log10 & Standardized", "Projection Matrix"), selected = "Log10 Counts", width = 9)
            ),
            fluidRow(
                column(3, selectInput("hc_dist_method", "Distance measure", choices = list("Euclidean" = "euclidean", "Maximum" = "maximum", "Manhattan" = "manhattan", "Canberra" = "canberra", "Binary" = "binary", "Correlation Distance" = "corr", "SCDE Adjusted Distance" = 'scde'), selected = "euclidean")),
                uiOutput("hclust_corr_ui"),  uiOutput("hclust_scde_ui"),
                column(3, selectInput("hc_agglo_method", "Agglomeration method", choices = list("Ward.D" = "ward.D", "Ward.D2" = "ward.D2","Single"= "single", "Complete"="complete", "Average"= "average", "Mcquitty"="mcquitty", "Median"= "median", "Centroid" = "centroid"), selected = "complete")),
                column(3, numericInput("hclust_num", label = "Number of Clusters", min = 2, max = length(r_data$sample_name) - 1, value = 2, step = 1))
            ),
            fluidRow(column(12,actionButton("run_hc", "Run", class = "btn-info btn_rightAlign"))),
            tags$div(tags$b("Visualization Settings:"), class = "param_setting_title"),
            fluidRow(
                column(3, selectInput("hclust_package", "Plotting Package", choices = list("Dendexdend" = "dendextend", "networkD3" = "networkD3"), selected = "dendextend")),
                pivot_groupBy_UI("hclust", r_data$category, multiple = T, width = 6),
                column(3, tags$br(), checkboxInput("hclust_show", "Show Cluster", value = T))
            ),
            uiOutput("dend_hclust"),
            uiOutput("d3_hclust"),
            hr(),
            fluidRow(
                column(6,
                       tags$div(tags$b("Confusion Matrix"), class = "param_setting_title"),
                       fluidRow(
                           pivot_groupBy_UI("hclust2", r_data$category, append_none = T, choose_color = F, width = 12)
                       ),
                       DT::dataTableOutput("hclust_tbl"),
                       plotOutput("hclust_conf_plot")
                ),
                column(6,
                       tags$div(tags$b("Clustering Results"), class = "param_setting_title"),
                       DT::dataTableOutput("hclust_assignment"),
                       downloadButton("hclust_download", "Download", class = "btn btn-success")
                )
            )
        )
    )
})


output$dend_hclust<-renderUI({
    if(input$hclust_package != "dendextend") return()
    plotOutput("hclust_plot", height = "450px")
})

output$hclust_corr_ui <- renderUI({
    if (input$hc_dist_method != 'corr') return()
    column(3, selectInput("hclust_cor_method", label = "Correlation method", choices = list("Pearson" = "pearson","Spearman" = "spearman", "Kendall" = "kendall"), selected = "pearson"))
})

output$hclust_scde_ui <- renderUI({
    if (input$hc_dist_method != 'scde') return()
    column(3, selectInput("hclust_scde_dist_method", "SCDE distance", choices = list("Direct drop-out" = "ddo", "Reciprocal weighting" = "rw", "Mode-relative weighting" = "mrw"), selected = "rw"))
})

output$d3_hclust <- renderUI({
    if(input$hclust_package != "networkD3") return()
    list(
        fluidRow(
            column(6, selectInput("hc_dd_ori", "Orientation of the plot", choices = list("Vertical" = 'vertical', "Horizontal" = 'horizontal'))),
            column(6, selectInput("hc_dd_link_type", "Link type", choices = list("Diagonal" = "diagonal", "Elbow" = "elbow")))
        ),
        fluidRow(
            column(1),
            column(11,
                   networkD3::dendroNetworkOutput("hclust.d3", height = "500px")
            )
        )
    )
})



# distance measure
hcList <- callModule(pivot_dataScale, "hc_scale", r_data)
observeEvent(input$run_hc, {
    req(hcList()$df)
    hc_data <- hcList()$df
    if(!(input$hc_dist_method %in% c('scde', 'corr'))) {
        r_data$hc <- t(hc_data) %>% dist(method = input$hc_dist_method) %>% hclust(method = input$hc_agglo_method)
    } else if(input$hc_dist_method == 'corr') {
        r_data$hc <- as.dist(1 - cor(hc_data, method = input$hclust_cor_method)) %>% hclust(method = input$hc_agglo_method)
    } else if(input$hc_dist_method == 'scde') {
        if(is.null(input$hclust_scde_dist_method)) return()
        if(is.null(r_data$scde_ddo) && is.null(r_data$scde_rw) && is.null(r_data$scde_mrw)) {
            session$sendCustomMessage(type = "showalert", "Please compute the SCDE-adjusted distance you want to use in SCDE module first.")
            updateTabItems(session, "tabs", "scde")
            return()
        }
        if(input$hclust_scde_dist_method == "ddo") {
            if(is.null(r_data$scde_ddo)) {
                session$sendCustomMessage(type = "showalert", "Please compute Direct drop-out adjusted distance in SCDE module first.")
                updateTabItems(session, "tabs", "scde")
                return()
            } else {
                r_data$hc <- r_data$scde_ddo %>% hclust(method = input$hc_agglo_method)
            }
        } else if(input$hclust_scde_dist_method == "rw") {
            if(is.null(r_data$scde_rw)) {
                session$sendCustomMessage(type = "showalert", "Please compute Reciprocal weighting adjusted distance in SCDE module first.")
                updateTabItems(session, "tabs", "scde")
                return()
            } else {
                r_data$hc <- r_data$scde_rw %>% hclust(method = input$hc_agglo_method)
            }
        } else if(input$hclust_scde_dist_method == "mrw") {
            if(is.null(r_data$scde_mrw)) {
                session$sendCustomMessage(type = "showalert", "Please compute Mode-relative weighting adjusted distance in SCDE module first.")
                updateTabItems(session, "tabs", "scde")
                return()
            } else {
                r_data$hc <- r_data$scde_mrw %>% hclust(method = input$hc_agglo_method)
            }
        }
    }
})


output$hclust_plot <- renderPlot({
    par(mar=c(8, 8, 8, 8), cex = 1, xpd=TRUE)
    req(r_data$hc)
    hc0 <- r_data$hc
    # get max height of the tree, this will be used to adjust the group bar height
    max_height <- max(hc0$height)
    hc1 <- hc0 %>% as.dendrogram()
    # Cut tree to get cluster
    r_data$meta$hierarchical_cluster <- as.character(dendextend::cutree(hc1, k = input$hclust_num))

    rsList <- callModule(pivot_groupBy, "hclust", meta = r_data$meta)
    if(is.null(rsList$meta)) {
        plot(hc1)
    } else {
        # extract meta data
        meta <- rsList$meta
        meta_color <- rsList$meta_color

        if (length(rsList$group_by) == 1) {
            #assign("hc1", hc1, env= .GlobalEnv)
            selected_color <- as.character(meta_color[,1])
            dendextend::labels_colors(hc1) <- selected_color[order.dendrogram(hc1)]
            plot(hc1)
            dendextend::colored_bars(colors = meta_color, dend = hc1, rowLabels = rsList$group_by)
            legend("topright", inset = c(-0.1,0), legend = unique(meta[,1]),  bty="n",
                   title = colnames(meta)[1], title.col = "black", cex = 1,
                   fill = unique(selected_color), text.col = unique(selected_color), border = FALSE)
        } else if (length(rsList$group_by) <= 5){
            plot(hc1)
            dendextend::colored_bars(colors = meta_color, dend = hc1, rowLabels = rsList$group_by)
            inset_h = 0
            for(i in 1: length(rsList$group_by)) {
                legend("topright", inset = c(-0.1, inset_h), legend = unique(meta[,i]),
                       title = colnames(meta)[i], title.col = "black", cex = 1, bty="n",
                       fill = unique(as.character(meta_color[,i])), text.col = unique(as.character(meta_color[,i])), border = F)
                inset_h = inset_h + 0.15 +0.057 * length(unique(meta[,i]))
            }
        } else {
            session$sendCustomMessage(type = "showalert", "Unable to plot more than 5 categories.")
            return()
        }
    }
    if(input$hclust_show){
        hc1 %>% dendextend::rect.dendrogram(k=input$hclust_num, border = 8, lty = 5, lwd = 2)
    }
})

output$hclust.d3 <- networkD3::renderDendroNetwork({
    req(r_data$hc)
    Root <- r_data$hc

    rsList <- callModule(pivot_groupBy, "hclust", meta = r_data$meta)
    if(is.null(rsList$meta)) {
        networkD3::dendroNetwork(Root, fontSize = 15, treeOrientation = input$hc_dd_ori, linkColour = 'navyblue', nodeColour = 'grey', textOpacity = 1, opacity = 1, zoom = T, linkType = input$hc_dd_link_type)
    } else {
        # extract meta data
        meta <- rsList$meta
        meta_color <- rsList$meta_color

        if (length(rsList$group_by) == 1) {
            #assign("hc1", hc1, env= .GlobalEnv)
            selected_color <- as.character(meta_color[,1])
        } else {
            return(NULL)
        }
        networkD3::dendroNetwork(Root, fontSize = 15, textColour = selected_color, treeOrientation = input$hc_dd_ori, linkColour = 'navyblue', nodeColour = 'grey', textOpacity = 1, opacity = 1,  zoom = T, linkType = input$hc_dd_link_type)
    }
})



output$hclust_tbl <- DT::renderDataTable({
    req(r_data$meta$hierarchical_cluster)
    gList <- callModule(pivot_groupBy, "hclust2", meta = r_data$meta)

    if(is.null(gList$meta) || length(unique(gList$meta[,1])) < 2)
    {
        tbl <- as.data.frame(table(r_data$meta$hierarchical_cluster))
        colnames(tbl) <- c("Group", "Number of samples assigned")
        DT::datatable(tbl, options = list(paging = FALSE, searching = FALSE))
    } else {
        sample_gp <- gList$meta[,1]
        names(sample_gp) <- r_data$sample_name
        tbl <- as.data.frame.matrix(table(r_data$meta$hierarchical_cluster, sample_gp))
        colnam <- names(tbl)
        names(tbl) <- sapply(colnam, function(x) paste("Is", x))
        rownam <- rownames(tbl)
        rownames(tbl) <- sapply(rownam, function(x) paste("Allocated to cluster", x))
        DT::datatable(tbl, options = list(paging = FALSE, searching = FALSE))
    }
})

output$hclust_conf_plot <- renderPlot({
    req(r_data$meta$hierarchical_cluster)
    gList <- callModule(pivot_groupBy, "hclust2", meta = r_data$meta)

    if(is.null(gList$meta) || length(unique(gList$meta[,1])) < 2)
    {
        return()
    } else {
        sample_gp <- gList$meta[,1]
        names(sample_gp) <- r_data$sample_name
        plot(as.factor(sample_gp), as.factor(r_data$meta$hierarchical_cluster), xlab="Group", ylab = "Cluster")
    }

})

hclust_assign_tbl <- reactive({
    req(r_data$meta$hierarchical_cluster)
    gList <- callModule(pivot_groupBy, "hclust2", meta = r_data$meta)

    if(is.null(gList$meta) || length(unique(gList$meta[,1])) == 0)
    {
        tbl <- r_data$meta[,c(1, which(colnames(r_data$meta) == "hierarchical_cluster"))]
    } else {
        actual_group <- gList$meta[,1]
        names(actual_group) <- r_data$sample_name
        tbl <- data.frame(actual_group)
        tbl$assigned_cluster <- r_data$meta$hierarchical_cluster
    }
    tbl
})

output$hclust_assignment <- DT::renderDataTable({
    DT::datatable(hclust_assign_tbl(), options = list(scrollX = TRUE, scrollY = "400px", lengthMenu = c(20, 50, 100)))
})

output$hclust_download <- downloadHandler(
    filename = function() {
        "hierarchical_clustering_assigment.csv"
    },
    content = function(file) {
        write.csv(hclust_assign_tbl(), file)
    }
)

