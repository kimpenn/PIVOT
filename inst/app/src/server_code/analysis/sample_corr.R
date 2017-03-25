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


# corr heatmap
output$cor_sp_ui <- renderUI({
    enhanced_box(
        width = 12,
        title = "Sample Correlation",
        id = "sample_corr_heatmap",
        status = "primary",
        solidHeader = T,
        collapsible = T,
        reportable = T,
        get_html = T,
        register_analysis= T,

        tags$div(tags$b("General and Heatmap Settings:"), class = "param_setting_title"),
        fluidRow(
            column(4, pivot_dataScale_UI("cor_sp_scale", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts", "Standardized Counts", "Log10 & Standardized"), selected = "Log10 Counts")),
            #column(3, selectInput("cor_sp_dist", label = "Distance measure", choices = list("Correlation Distance (1-r)" = "corr"))),
            column(4, selectInput("cor_sp_method", label = "Correlation method", choices = list("pearson" = "pearson","spearman" = "spearman", "kendall" = "kendall"), selected = "pearson")),
            column(4, selectInput("cor_sp_agglo_method", "Agglomeration method", choices = list("Ward.D" = "ward.D", "Ward.D2" = "ward.D2","Single"= "single", "Complete"="complete", "Average"= "average", "Mcquitty"="mcquitty", "Median"= "median", "Centroid" = "centroid"), selected = "complete"))
        ),
        tags$div(tags$b("Visualization Settings:"), class = "param_setting_title"),
        fluidRow(
            column(3, selectInput("cor_sp_package", label = "Plotting package", choices = list("gplots"="gplots", "heatmaply"="heatmaply"), multiple = F)),
            column(3, selectInput("cor_sp_hmcolor", label = "Heatmap color", choices = c(get_brewer_set("sequential"), list("viridis" = "viridis", "magma" = "magma", "plasma" = "plasma", "inferno" = "inferno")), multiple = F)),
            pivot_colorBy_UI("cor_sp", meta = r_data$meta, append_none = T, width = 6)
        ),
        tags$hr(),
        conditionalPanel("input.cor_sp_package == 'gplots'", plotOutput("cor_sp_gplot", height = "700px")),
        #conditionalPanel("input.cor_sp_package == 2", "d3heatmap", d3heatmap::d3heatmapOutput("hmap_cor_d3", height = "600px")) #No longer use d3heatmap
        conditionalPanel("input.cor_sp_package == 'heatmaply'", plotly::plotlyOutput("cor_sp_plotly", height = "700px"))
    )
})


output$cor_sp_gplot <- renderPlot({
    rsList <- callModule(pivot_dataScale, "cor_sp_scale", r_data, ercc_iso = FALSE)
    sp_data <- rsList$df
    if(is.null(sp_data)) return()
    sample_cor <- cor(sp_data, method = input$cor_sp_method)
    distfun1 = function(c) as.dist(1 - c)
    hclustfun1 = function(x, method=input$cor_sp_agglo_method, ...) hclust(x, method=method, ...)

    rsList <- callModule(pivot_colorBy, "cor_sp", meta = r_data$meta)

    if(!is.null(rsList$meta)) {
        gplots::heatmap.2(sample_cor, scale="none", Rowv=T, symm = T,dendrogram="both",
                          distfun=distfun1,
                          hclustfun =hclustfun1,
                          trace="none", col=get_heatmap_color(input$cor_sp_hmcolor), key.par=list(cex.axis=0.7), key.title=NA, key.xlab="  ", key.ylab=NA, keysize=1, density.info="density", revC=T,
                          ColSideColors = rsList$meta_color[,1], margins = c(10,15))

        legend("topright", legend = unique(rsList$meta[,1]), bty="n", fill = unique(rsList$meta_color[,1]), text.col = unique(rsList$meta_color[,1]), border=FALSE, y.intersp = 1.2, cex = 0.9)
    } else {
        gplots::heatmap.2(sample_cor, scale="none", Rowv = T, symm=TRUE, dendrogram="both",
                          distfun=distfun1,
                          hclustfun =hclustfun1,
                          trace="none", col=get_heatmap_color(input$cor_sp_hmcolor), key.par=list(cex.axis=1), key.title=NA, key.xlab="  ", key.ylab=NA, keysize=1, density.info="density", revC=T, margins=c(8,6))
    }
})

# No longer use d3heatmap
# output$hmap_cor_d3 <- d3heatmap::renderD3heatmap({
#     rsList <- callModule(pivot_dataScale, "cor_sp_scale", r_data, ercc_iso = FALSE)
#     sp_data <- rsList$df
#     if(is.null(sp_data)) return ()
#     sample_cor <- cor(sp_data, method = input$cor_sp_method)
#     distfun1 = function(c) as.dist(1 - c)
#     hclustfun1 = function(x, method=input$cor_sp_agglo_method, ...) hclust(x, method=method, ...)
#
#     d3heatmap::d3heatmap(sample_cor, scale="none", Rowv = T,
#               distfun=distfun1,
#               hclustfun =hclustfun1,
#               dendrogram="both", colors=hmcol$val, revC=T)
# })

output$cor_sp_plotly <- plotly::renderPlotly({
    rsList <- callModule(pivot_dataScale, "cor_sp_scale", r_data, ercc_iso = FALSE)
    sp_data <- rsList$df
    if(is.null(sp_data)) return()
    sample_cor <- cor(sp_data, method = input$cor_sp_method)
    distfun1 = function(c) as.dist(1 - c)
    hclustfun1 = function(x, method=input$cor_sp_agglo_method, ...) hclust(x, method=method, ...)

    # Sadly current version of heatmaply does not support column side coloring... advantage compared to d3heatmap is speed...
    hmap<-heatmaply::heatmapr(sample_cor, scale="none", Rowv = T, symm=TRUE, dendrogram="both", revC=T,
                              distfun=distfun1,hclustfun =hclustfun1)
    heatmaply::heatmaply(hmap, colors=get_heatmap_color(input$cor_sp_hmcolor))
})



