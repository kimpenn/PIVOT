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

################################ PCA UI #################################

output$pca_ui <- renderUI({
    list(
        fluidRow(
            column(12,
                   enhanced_box(
                       title = "Principal Component Analysis",
                       status = "primary",
                       id = "pca",
                       width = NULL,
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       register_analysis= T,
                       tags$div(tags$b("General Settings:"), class = "param_setting_title"),
                       fluidRow(
                           pivot_dataScale_UI("pca", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts"), selected = "Log10 Counts", width = 3),
                           column(3, selectInput("pca_scale", label = "PCA Scale", choices = list("Scale to Unit Varience" = T, "None" = F))),
                           pivot_groupBy_UI("pca", r_data$category, append_none = T, multiple = F, width = 6)
                       ),
                       actionButton("run_pca", "Run", class = "btn-info btn_rightAlign")
                   )
            )
        ),

        fluidRow(
            column(6,
                   tabBox(
                       title = "Summary",
                       width = NULL,
                       #status = "success",
                       #solidHeader = T,
                       tabPanel(
                           title = "Variance Explained",
                           DT::dataTableOutput("pca1_summary_tbl"),
                           downloadButton('download_pca1_summary', 'Download', class = "btn btn-success")
                       ),
                       tabPanel(
                           title = "Loading Table",
                           DT::dataTableOutput("pca1_loadings_tbl"),
                           downloadButton('download_pca1_loadings', 'Download', class = "btn btn-success")
                       ),
                       tabPanel(
                           title = "Projection Table",
                           DT::dataTableOutput("pca1_projection_tbl"),
                           downloadButton('download_pca1_projection', 'Download', class = "btn btn-success")
                       )
                   )
            ),
            column(6,
                   enhanced_box(
                       width = NULL,
                       title = "Scree Plot",
                       status = "info",
                       solidHeader = T,
                       plotly::plotlyOutput("pca_scree")
                   )
            )
        ),
        fluidRow(
            column(6,
                   enhanced_box(
                       width = NULL,
                       title = "1D projection",
                       id = "pca_box_1d",
                       status = "primary",
                       solidHeader = T,
                       pivot_Plot1d_UI("pca_plot1d", type = "pca")
                   )
            ),
            column(6,
                   enhanced_box(
                       width = NULL,
                       title = "2D projection",
                       id = "pca_box_2d",
                       status = "warning",
                       solidHeader = T,
                       pivot_Plot2d_UI("pca_plot2d", type = "pca")
                   )
            )
        ),
        fluidRow(
            column(8,
                   enhanced_box(
                       width = NULL,
                       title = "3D projection",
                       id = "pca_box_3d",
                       status = "danger",
                       solidHeader = T,
                       pivot_Plot3d_UI("pca_plot3d", type = "pca")
                   )
            )
        )
    )
})

pcaList <- callModule(pivot_dataScale, "pca", r_data)

observeEvent(input$run_pca, {
    req(r_data$df, r_data$meta)
    pca_data <- pcaList()$df
    req(pca_data, input$pca_scale)

    tryCatch({
        r_data$pca <- prcomp(t(pca_data), center = TRUE, scale. = as.logical(input$pca_scale))
    },
    error = function(e) {
        session$sendCustomMessage(type = "showalert", "PCA failed.")
        r_data$pca <- NULL
    })
})


pca_minfo<- reactive(callModule(pivot_groupBy, "pca", meta = r_data$meta))

observe({
    req(pca_minfo(), r_data$pca)
    callModule(pivot_Plot1d, "pca_plot1d", type = "pca", r_data$pca, proj = as.data.frame(r_data$pca$x), minfo = pca_minfo())
})


pca_selected_sample <- reactiveValues()

observe({
    req(pca_minfo(), r_data$pca)
    drag <-reactive(plotly::event_data("plotly_selected", source = "pca_drag"))
    obj <- callModule(pivot_Plot2d, "pca_plot2d", type = "pca", r_data$pca, proj = as.data.frame(r_data$pca$x), minfo = pca_minfo(),
                      source = "pca_drag", event = drag, selected = pca_selected_sample)
    isolate({
        if(!is.null(obj$group)) {
            if(is.null(r_data$meta$pca_group)) {
                r_data$meta$pca_group <- rep("not specified", nrow(r_data$meta))
                r_data$category <- colnames(r_data$meta)
            }
            r_data$meta$pca_group[match(obj$group, r_data$meta$sample)] <- names(obj$group)[1]
        }
    })
})

observe({
    req(pca_minfo(), r_data$pca)
    callModule(pivot_Plot3d, "pca_plot3d", type = "pca", r_data$pca, proj = as.data.frame(r_data$pca$x), minfo = pca_minfo())
})


# pca summary
output$pca1_summary_tbl <- DT::renderDataTable({
    if(is.null(r_data$pca)) return ()
    DT::datatable(summary(r_data$pca)$importance, options = list(scrollX = TRUE, scrollY = "210px", searching=F))
})

output$download_pca1_summary <- downloadHandler(
    filename = function() {
        "pca_vars_explained.csv"
    },
    content = function(file) {
        write.csv(summary(r_data$pca)$importance, file)
    }
)

# PCA projections

output$pca1_projection_tbl <- DT::renderDataTable({
    if (is.null(r_data$pca)) return()
    DT::datatable(t(r_data$pca$x), options = list(scrollX = TRUE, scrollY = "210px", searching=T))
})

output$download_pca1_projection <- downloadHandler(
    filename = function() {
        "pca_projection.csv"
    },
    content = function(file) {
        write.csv(t(r_data$pca$x), file)
    }
)

# PCA loadings
output$pca1_loadings_tbl <- DT::renderDataTable({
    if(is.null(r_data$pca)) return ()
    DT::datatable(r_data$pca$rotation, options = list(scrollX = TRUE, scrollY = "210px"))
})

output$download_pca1_loadings <- downloadHandler(
    filename = function() {
        "pca_loadings.csv"
    },
    content = function(file) {
        write.csv(r_data$pca$rotation, file)
    }
)

# PCA scree plot
output$pca_scree <- plotly::renderPlotly({
    if(is.null(r_data$pca)) return()
    pca_var <- data.frame(pcomp=1:length(r_data$pca$sdev), variances=r_data$pca$sdev^2)
    pca_var$variance_explained <- pca_var$variances / sum(pca_var$variances) * 100
    pca_var$cumulative_variance_explained <- cumsum(pca_var$variance_explained)
    pca_var %>% plotly::plot_ly(x = ~pcomp, y = ~variance_explained, type = "bar")
})




