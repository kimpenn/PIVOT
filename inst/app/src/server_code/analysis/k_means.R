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

            tags$div(tags$b("General Settings:"), class = "param_setting_title"),
            fluidRow(
                column(4, pivot_dataScale_UI("kmeans", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts"), selected = "Log10 Counts")),
                pivot_colorBy_UI("kmeans", meta = r_data$meta, append_none = T, choose_color = F, width = 4)
            ),
            tags$div(tags$b("K-means Settings:"), class = "param_setting_title"),

            fluidRow(
                column(4, numericInput("kmeans_seed", label = "Set seed", value = 1, min = 1, max = 5000, step = 1)),
                column(4, uiOutput("km_centers_ui")),
                shinyBS::tipify(column(4, sliderInput("km_nstart", label = tags$p("nstart"), min = 1, max = 500, value = 10, step = 1)), title = "The number of initial random sets to be chosen.", placement = "bottom", options = list(container = "body"))
            )
        ),
        box(
            title = NULL,
            status = "primary",
            width = 12,
            fluidRow(
                column(6,
                       tags$div(tags$b("Confusion Matrix"), class = "param_setting_title"),
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


# k-means
km1 <- reactive({
    req(input$km_centers)
    req(input$km_nstart)
    set.seed(input$kmeans_seed)
    rsList <- callModule(pivot_dataScale, "kmeans", r_data)
    kmeans_data <- rsList$df
    kmeans(t(kmeans_data), centers = input$km_centers, nstart = input$km_nstart)
})

output$km_centers_ui <- renderUI({
    gList <- callModule(pivot_colorBy, "kmeans", meta = r_data$meta)

    if(is.null(gList$meta) || length(unique(gList$meta[,1])) < 2)
        shinyBS::tipify(sliderInput("km_centers", label = tags$p("number of clusters (k)"), min = 1, max = length(r_data$sample_name) - 1, value = 1, step = 1), title = "A random set of (distinct) samples is chosen as the initial k centres", placement = "bottom", options = list(container = "body"))
    else
        shinyBS::tipify(sliderInput("km_centers", label = tags$p("number of clusters"), min = 1, max = length(r_data$sample_name) - 1, value = length(unique(gList$meta[,1])), step = 1), title = "A random set of (distinct) rows is chosen as the initial k centres", placement = "bottom", options = list(container = "body"))
})

output$km_tbl <- DT::renderDataTable({
    req(km1())
    gList <- callModule(pivot_colorBy, "kmeans", meta = r_data$meta)

    if(is.null(gList$meta) || length(unique(gList$meta[,1])) < 2)
    {
        tbl <- as.data.frame(table(km1()$cluster))
        colnames(tbl) <- c("Group", "Number of samples assigned")
        DT::datatable(tbl, options = list(paging = FALSE, searching = FALSE))
    } else {
        sample_gp <- gList$meta[,1]
        names(sample_gp) <- r_data$sample_name
        tbl <- as.data.frame.matrix(table(km1()$cluster, sample_gp))
        colnam <- names(tbl)
        names(tbl) <- sapply(colnam, function(x) paste("Is", x))
        rownam <- rownames(tbl)
        rownames(tbl) <- sapply(rownam, function(x) paste("Allocated to cluster", x))
        DT::datatable(tbl, options = list(paging = FALSE, searching = FALSE))
    }
})

output$km_conf_plot <- renderPlot({
    req(km1())
    gList <- callModule(pivot_colorBy, "kmeans", meta = r_data$meta)

    if(is.null(gList$meta) || length(unique(gList$meta[,1])) < 2)
    {
        return()
    } else {
        sample_gp <- gList$meta[,1]
        names(sample_gp) <- r_data$sample_name
        tbl <- as.data.frame.matrix(table(km1()$cluster, sample_gp))
        plot(as.factor(sample_gp), as.factor(km1()$cluster), xlab="Group", ylab = "Cluster")
    }

})

km_assign_tbl <- reactive({
    req(km1())
    gList <- callModule(pivot_colorBy, "kmeans", meta = r_data$meta)

    if(is.null(gList$meta) || length(unique(gList$meta[,1])) == 0)
    {
        tbl <- data.frame(km1()$cluster)
        colnames(tbl) <- c("assigned_cluster")
    } else {
        actual_group <- gList$meta[,1]
        names(actual_group) <- r_data$sample_name
        tbl <- data.frame(actual_group)
        tbl$assigned_cluster <- km1()$cluster
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

