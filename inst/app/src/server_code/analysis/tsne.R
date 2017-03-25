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
                           column(4, pivot_dataScale_UI("tsne", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts", "Standardized Counts", "Log10 & Standardized"), selected = "Log10 Counts")),
                           column(4, numericInput("tsne_perplexity", label = "Perplexity", min = 1, max = 50, value = 1, step = 1)),
                           column(4, radioButtons("tsne_pca", label = "Perform initial PCA step?", choices = list("Yes" = "TRUE", "No" = "FALSE"), selected = "TRUE", inline = TRUE))
                       ),
                       tags$div(tags$b("Visualization Settings:"), class = "param_setting_title"),
                       fluidRow(
                           pivot_colorBy_UI("tsne", meta = r_data$meta, multiple = F, width = 8)
                       )
                   )
            )
        ),
        pivot_dimScatter_UI("tsne", type = "tsne"),
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


observe({
    if(is.null(r_data$glb.raw)) return()
    rsList <- callModule(pivot_dataScale, "tsne", r_data)
    tsne_data <- rsList$df
    if(is.null(tsne_data)) {
        return(NULL)
    }

    error_I <- 0

    r_data$tsne <- tryCatch({
        tsne_1d <- Rtsne::Rtsne(t(tsne_data),perplexity = input$tsne_perplexity, theta = 0, dims = 1, pca = as.logical(input$tsne_pca))
        tsne_2d <- Rtsne::Rtsne(t(tsne_data),perplexity = input$tsne_perplexity, theta = 0, dims = 2, pca = as.logical(input$tsne_pca))
        tsne_3d <- Rtsne::Rtsne(t(tsne_data),perplexity = input$tsne_perplexity, theta = 0, dims = 3, pca = as.logical(input$tsne_pca))
        list(tsne_1d = tsne_1d, tsne_2d = tsne_2d, tsne_3d = tsne_3d)
    },
    error = function(e) {
        session$sendCustomMessage(type = "showalert", "t-SNE failed.")
        r_data$tsne <- NULL
        return()
    })

    tsne_minfo<-callModule(pivot_colorBy, "tsne", meta = r_data$meta)
    callModule(pivot_dimScatter, "tsne", type = "tsne", obj = r_data$tsne, minfo = tsne_minfo)
})

