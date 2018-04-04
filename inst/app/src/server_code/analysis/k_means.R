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


output$kmeans_ui <- renderUI({
    req(r_data$df)
    list(
        enhanced_box(
            width = 12,
            title = "K-means Clustering",
            id = "kmeans_clust",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            tags$div(tags$b("Clustering Settings:"), class = "param_setting_title"),
            fluidRow(
                pivot_dataScale_UI("kmeans", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts", "Projection Matrix"), selected = "Log10 Counts", width = 12)
            ),
            fluidRow(
                column(4, numericInput("kmeans_seed", label = "Set Seed", value = 1, min = 1, max = 5000, step = 1)),
                column(4, shinyBS::tipify(numericInput("km_centers", label = "Number of Clusters", min = 2, max = length(r_data$sample_name) - 1, value = 2, step = 1),
                                          title = "A random set of (distinct) samples is chosen as the initial k centres", placement = "bottom", options = list(container = "body"))),
                shinyBS::tipify(column(4, numericInput("km_nstart", label = "nstart", min = 1, max = 500, value = 10, step = 1)),
                                title = "The number of initial random sets to be chosen.", placement = "bottom", options = list(container = "body"))
            ),
            fluidRow(column(12,actionButton("run_kmeans", "Run", class = "btn-info btn_rightAlign")))
        ),
        box(
            title = NULL,
            status = "primary",
            width = 12,
            tags$div(tags$b("Result Summary"), class = "param_setting_title"),
            uiOutput("kmeans_summary"),
            fluidRow(
                column(6,
                       tags$div(tags$b("Confusion Matrix"), class = "param_setting_title"),
                       fluidRow(
                           pivot_groupBy_UI("kmeans", r_data$category, append_sample = T, choose_color = F, width = 12)
                       ),
                       DT::dataTableOutput("km_tbl"),
                       plotOutput("km_conf_plot")
                ),
                column(6,
                       tags$div(tags$b("Clustering Results"), class = "param_setting_title"),
                       DT::dataTableOutput("km_assignment"),
                       downloadButton("km_as_download", "Download", class = "btn btn-success")
                )
            )
        )
    )
})

kmeansList <- callModule(pivot_dataScale, "kmeans", r_data)
# k-means
observeEvent(input$run_kmeans, {
    req(input$km_centers, input$km_nstart, input$kmeans_seed)
    set.seed(input$kmeans_seed)
    req(kmeansList()$df)
    kmeans_data <- kmeansList()$df
    r_data$kmeans<-kmeans(t(kmeans_data), centers = input$km_centers, nstart = input$km_nstart)
    r_data$meta$kmeans_cluster<- as.character(r_data$kmeans$cluster)
    r_data$category <- colnames(r_data$meta)
})


output$km_tbl <- DT::renderDataTable({
    req(r_data$kmeans)
    gList <- callModule(pivot_groupBy, "kmeans", meta = r_data$meta)

    if(is.null(gList$meta) || length(unique(gList$meta[,1])) < 2)
    {
        tbl <- as.data.frame(table(r_data$meta$kmeans_cluster))
        colnames(tbl) <- c("Group", "Number of samples assigned")
        DT::datatable(tbl, options = list(paging = FALSE, searching = FALSE))
    } else {
        sample_gp <- gList$meta[,1]
        names(sample_gp) <- r_data$sample_name
        tbl <- as.data.frame.matrix(table(r_data$meta$kmeans_cluster, sample_gp))
        colnam <- names(tbl)
        names(tbl) <- sapply(colnam, function(x) paste("Is", x))
        rownam <- rownames(tbl)
        rownames(tbl) <- sapply(rownam, function(x) paste("Allocated to cluster", x))
        DT::datatable(tbl, options = list(paging = FALSE, searching = FALSE))
    }
})

output$km_conf_plot <- renderPlot({
    req(r_data$kmeans)
    gList <- callModule(pivot_groupBy, "kmeans", meta = r_data$meta)

    if(is.null(gList$meta) || length(unique(gList$meta[,1])) < 2)
    {
        return()
    } else {
        sample_gp <- gList$meta[,1]
        names(sample_gp) <- r_data$sample_name
        tbl <- as.data.frame.matrix(table(r_data$meta$kmeans_cluster, sample_gp))
        plot(as.factor(sample_gp), as.factor(r_data$meta$kmeans_cluster), xlab="Group", ylab = "Cluster")
    }

})

km_assign_tbl <- reactive({
    req(r_data$kmeans)
    gList <- callModule(pivot_groupBy, "kmeans", meta = r_data$meta)

    if(is.null(gList$meta) || length(unique(gList$meta[,1])) == 0)
    {
        tbl <-r_data$meta[,c(1, which(colnames(r_data$meta) == "kmeans_cluster"))]
    } else {
        actual_group <- gList$meta[,1]
        names(actual_group) <- r_data$sample_name
        tbl <- data.frame(actual_group)
        tbl$assigned_cluster <- r_data$meta$kmeans_cluster
    }
    tbl
})

output$km_assignment <- DT::renderDataTable({
    DT::datatable(km_assign_tbl(), options = list(scrollX = TRUE, scrollY = "400px", lengthMenu = c(20, 50, 100)))
})

output$km_as_download <- downloadHandler(
    filename = function() {
        "Kmeans_assigment.csv"
    },
    content = function(file) {
        write.csv(km_assign_tbl(), file)
    }
)

output$kmeans_summary <- renderUI({
    req(r_data$kmeans)
    tagList(
        tags$b("Within cluster sum of squares by cluster:"),
        tags$p(paste(r_data$kmeans$withinss, collapse = ", ")),
        tags$b("between_SS / total_SS:"),
        tags$p(paste0(round(r_data$kmeans$betweenss/r_data$kmeans$totss * 100, 1), "%"))
    )
})

