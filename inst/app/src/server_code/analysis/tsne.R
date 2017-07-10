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


################################## t-SNE ######################################

output$tsne_ui <- renderUI({
    list(
        fluidRow(
            column(12,
                   enhanced_box(
                       title = "T-SNE",
                       status = "primary",
                       id = "tsne",
                       width = NULL,
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       register_analysis= T,
                       tags$div(tags$b("t-SNE Settings:"), class = "param_setting_title"),
                       fluidRow(
                           pivot_dataScale_UI("tsne", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts", "Standardized Counts", "Log10 & Standardized"), selected = "Log10 Counts"),
                           column(4, numericInput("tsne_perplexity", label = "Perplexity", min = 1, max = 50, value = 1, step = 1)),
                           column(4, numericInput("tsne_seed", label = "Set Seed", min = 1, max = 5000, value = 1, step = 1))
                       ),
                       fluidRow(
                           uiOutput("tsne_color_by_ui"),
                           column(4, radioButtons("tsne_pca", label = "Perform initial PCA step?", choices = list("Yes" = "TRUE", "No" = "FALSE"), selected = "TRUE", inline = TRUE))
                       )
                   )
            )
        ),
        fluidRow(
            column(6,
                   enhanced_box(
                       width = NULL,
                       title = "1D projection",
                       id = "tsne_box_1d",
                       status = "primary",
                       solidHeader = T,
                       pivot_Plot1d_UI("tsne_plot1d", type = "tsne")
                   )
            ),
            column(6,
                   enhanced_box(
                       width = NULL,
                       title = "2D projection",
                       id = "tsne_box_2d",
                       status = "warning",
                       solidHeader = T,
                       pivot_Plot2d_UI("tsne_plot2d", type = "tsne")
                   )
            )
        ),
        fluidRow(
            column(8,
                   enhanced_box(
                       width = NULL,
                       title = "3D projection",
                       id = "tsne_box_3d",
                       status = "danger",
                       solidHeader = T,
                       pivot_Plot3d_UI("tsne_plot3d", type = "tsne")
                   )
            )
        ),
        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Rtsne package is used here, which implements Barnes-Hut t-SNE."),
                tags$li("Jesse Krijthe (2015). Rtsne: T-Distributed Stochastic Neighbor Embedding using Barnes-Hut Implementation. R package version 0.10. http://CRAN.R-project.org/package=Rtsne.", class = "citation"),
                tags$li("Amir, E. A. D., Davis, K. L., Tadmor, M. D., Simonds, E. F., Levine, J. H., Bendall, S. C., ... & Pe'er, D. (2013). viSNE enables visualization of high dimensional single-cell data and reveals phenotypic heterogeneity of leukemia. Nature biotechnology, 31(6), 545-552.", class = "citation"),
                tags$li("Darmanis, S., Sloan, S. A., Zhang, Y., Enge, M., Caneda, C., Shuer, L. M., ... & Quake, S. R. (2015). A survey of human brain transcriptome diversity at the single cell level. Proceedings of the National Academy of Sciences, 201507125.", class = "citation")
            )
        )
    )

})

output$tsne_color_by_ui <- renderUI({
    pivot_groupBy_UI("tsne", r_data$category, append_none = T, multiple = F, width = 8)
})

tsneList <- callModule(pivot_dataScale, "tsne", r_data)

observe({
    tsne_data <- tsneList()$df
    req(tsne_data, input$tsne_seed, input$tsne_pca)
    tryCatch({
        set.seed(input$tsne_seed)
        tsne_1d <- Rtsne::Rtsne(t(tsne_data),perplexity = input$tsne_perplexity, theta = 0, dims = 1, pca = as.logical(input$tsne_pca))
        tsne_2d <- Rtsne::Rtsne(t(tsne_data),perplexity = input$tsne_perplexity, theta = 0, dims = 2, pca = as.logical(input$tsne_pca))
        tsne_3d <- Rtsne::Rtsne(t(tsne_data),perplexity = input$tsne_perplexity, theta = 0, dims = 3, pca = as.logical(input$tsne_pca))
        tsne_1d <- as.data.frame(tsne_1d$Y)
        tsne_2d <- as.data.frame(tsne_2d$Y)
        tsne_3d <- as.data.frame(tsne_3d$Y)
        rownames(tsne_1d) <- colnames(tsne_data)
        rownames(tsne_2d) <- colnames(tsne_data)
        rownames(tsne_3d) <- colnames(tsne_data)
        r_data$tsne <- list(tsne_1d = tsne_1d, tsne_2d = tsne_2d, tsne_3d = tsne_3d)
    },
    error = function(e) {
        session$sendCustomMessage(type = "showalert", "t-SNE failed.")
        r_data$tsne <- NULL
    })
})

tsne_minfo<- reactive(callModule(pivot_groupBy, "tsne", meta = r_data$meta))

observe({
    req(tsne_minfo(), r_data$tsne)
    callModule(pivot_Plot1d, "tsne_plot1d", type = "tsne", obj = NULL, proj = r_data$tsne$tsne_1d, minfo = tsne_minfo())
})

tsne_selected_sample <- reactiveValues()

observe({
    req(tsne_minfo(), r_data$tsne)
    drag <-reactive(plotly::event_data("plotly_selected", source = "tsne_drag"))

    obj <- callModule(pivot_Plot2d, "tsne_plot2d", type = "tsne", obj = NULL, proj = r_data$tsne$tsne_2d, minfo = tsne_minfo(),
                      source = "tsne_drag", event = drag, selected = tsne_selected_sample)
    isolate({
        if(!is.null(obj$group)) {
            if(is.null(r_data$meta$tsne_group)) {
                r_data$meta$tsne_group <- rep("not specified", nrow(r_data$meta))
                r_data$category <- colnames(r_data$meta)
            }
            r_data$meta$tsne_group[match(obj$group, r_data$meta$sample)] <- names(obj$group)[1]
        }
    })
})

observe({
    req(tsne_minfo(), r_data$tsne)
    callModule(pivot_Plot3d, "tsne_plot3d", type = "tsne", obj = NULL, proj = r_data$tsne$tsne_3d, minfo = tsne_minfo())
})


