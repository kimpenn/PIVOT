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
hmcol <- reactiveValues()
hmcol$val <- RColorBrewer::brewer.pal(9,"Blues")
hmcol$idx <- 1

output$cor_ft_ui <- renderUI({
    if(is.null(r_data$df)) return()

    list(
        enhanced_box(
            width = 12,
            title = "Feature Correlation",
            id = "feature_corr",
            status = "danger",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("General Settings:"), class = "param_setting_title"),
            tags$p("Heatmap support plotting of up to 1000 features. Please rank your feature. The top ranked features will be plotted."),
            pivot_dataScaleRange_UI("cor_ft", bound = nrow(r_data$df), value = 100),
            tags$div(tags$b("Heatmap Settings:"), class = "param_setting_title"),
            fluidRow(
                column(4, selectInput("cor_ft_dist", label = "Distance measure", choices = list("Correlation Distance (1-r)" = "corr1", "Absolute Correlation (1-|r|)" = "corr2"))),
                column(4, selectInput("cor_ft_method", label = "Correlation method", choices = list("pearson" = "pearson","spearman" = "spearman", "kendall" = "kendall"), selected = "pearson")),
                column(4, selectInput("cor_ft_agglo_method", "Agglomeration method", choices = list("Ward.D" = "ward.D", "Ward.D2" = "ward.D2","Single"= "single", "Complete"="complete", "Average"= "average", "Mcquitty"="mcquitty", "Median"= "median", "Centroid" = "centroid"), selected = "complete"))
            ),
            tags$div(tags$b("Visualization Settings:"), class = "param_setting_title"),
            fluidRow(
                column(4, selectInput("cor_ft_package", label = "Plotting package", choices = list("gplots"="gplots", "heatmaply"="heatmaply"), multiple = F)),
                column(4, selectInput("cor_ft_hmcolor", label = "Heatmap color", choices = c(get_brewer_set("sequential"), list("viridis" = "viridis", "magma" = "magma", "plasma" = "plasma", "inferno" = "inferno")), multiple = F))
            ),
            uiOutput("cor_ft_exceed_max_plot"),
            tags$hr(),
            conditionalPanel("input.cor_ft_package == 'gplots'", plotOutput("cor_ft_gplots", height = "800px")),
            conditionalPanel("input.cor_ft_package == 'heatmaply'", plotly::plotlyOutput("cor_ft_plotly", height = "800px"))
        ),
        enhanced_box(
            width = 12,
            title = "Genes Correlated/Coexpressed with Target Gene",
            id = "feature_corr_target",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("Search Settings:"), class = "param_setting_title"),
            fluidRow(
                column(3, textInput("ft_coe_target", "Target gene name:", placeholder = "E.g., Myc")),
                column(3, selectInput("ft_coe_method", "Correlation method", choices = list("Pearson" = "pearson", "Kendall" = "kendall", "Spearman" = "spearman"))),
                column(3, selectInput("ft_coe_abs", "Correlation direction", choices = list("Positive (r)" = "positive", "Negative (-r)" = "negative", "Both (|r|)" = "both"))),
                column(3, numericInput("ft_coe_threshold", "Threshold", min = 0.5, max = 1, step = 0.1, value = 0.5))
            ),
            actionButton("ft_coe_search", "Search", class = "btn-info"),
            tags$hr(),
            fluidRow(
                column(3,
                       tags$div(tags$b("List of Correlated Features:"), class = "param_setting_title"),
                       DT::dataTableOutput("coe_ft_tbl"),
                       downloadButton("coe_ft_tbl_download", class = "btn btn-success")
                ),
                column(9,
                       tags$div(tags$b("Heatmap of Correlated Features:"), class = "param_setting_title"),
                       fluidRow(
                           column(4, pivot_dataScale_UI("cor_ft_hmap_scale", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts", "Standardized Counts", "Log10 & Standardized"), selected = "Log10 Counts")),
                           pivot_colorBy_UI("cor_ft_hmap_col", meta = r_data$meta, append_none = T)
                       ),
                       uiOutput("coe_ft_hmap_ui")
                )
            )
        )
    )

})


cor_ft_data <- callModule(pivot_dataScaleRange, "cor_ft", r_data)


output$cor_ft_exceed_max_plot <- renderUI({
    if(is.null(cor_ft_data())) return()
    if(nrow(cor_ft_data()$df) > 1000) {
        tags$p("Exceeding maximum plotting capability (1000).")
    } else {
        return()
    }
})

output$cor_ft_gplots <- renderPlot({
    if(is.null(cor_ft_data()) || nrow(cor_ft_data()$df) > 1000) return ()
    feature_cor <- cor(t(cor_ft_data()$df), method = input$cor_ft_method)

    hclustfun1 = function(x, method=input$cor_ft_agglo_method, ...) hclust(x, method=method, ...)

    if(input$cor_ft_dist == "corr1") {
        distfun1 = function(c) as.dist(1 - c)
    } else if(input$cor_ft_dist == "corr2") {
        distfun1 = function(c) as.dist(1 - abs(c))
    }

    gplots::heatmap.2(feature_cor, scale="none", Rowv = T, symm=TRUE, dendrogram="both",revC=T,
              distfun=distfun1, hclustfun = hclustfun1,
              trace="none", col=get_heatmap_color(input$cor_ft_hmcolor), key.par=list(cex.axis=1), key.title=NA, key.xlab="  ", key.ylab=NA, keysize=1, density.info="density", margins=c(8,6))
})

output$cor_ft_plotly <- plotly::renderPlotly({
    if(is.null(cor_ft_data()) || nrow(cor_ft_data()$df) > 1000) return ()
    feature_cor <- cor(t(cor_ft_data()$df), method = input$cor_ft_method)

    hclustfun1 = function(x, method=input$cor_ft_agglo_method, ...) hclust(x, method=method, ...)

    if(input$cor_ft_dist == "corr1") {
        distfun1 = function(c) as.dist(1 - c)
    } else if(input$cor_ft_dist == "corr2") {
        distfun1 = function(c) as.dist(1 - abs(c))
    }
    hmap<-heatmaply::heatmapr(feature_cor, scale="none", Rowv = T, symm=TRUE, dendrogram="both", revC=T,
                              distfun=distfun1,hclustfun =hclustfun1)
    heatmaply::heatmaply(hmap, colors=get_heatmap_color(input$cor_ft_hmcolor))
})


# output$cor_ft_d3 <- d3heatmap::renderD3heatmap({
#     if(is.null(cor_ft_data()) || nrow(cor_ft_data()) > 1000) return ()
#     feature_cor <- cor(t(cor_ft_data()), method = input$cor_ft_method)
#
#     hclustfun1 = function(x, method=input$cor_ft_agglo_method, ...) hclust(x, method=method, ...)
#
#     if(input$cor_ft_dist == "corr1") {
#         distfun1 = function(c) as.dist(1 - c)
#     } else if(input$cor_ft_dist == "corr2") {
#         distfun1 = function(c) as.dist(1 - abs(c))
#     }
#
#     d3heatmap::d3heatmap(feature_cor, scale="none", Rowv = T,
#               distfun=distfun1,
#               hclustfun = hclustfun1,
#               dendrogram="both", colors=hmcol$val, revC=T)
# })


######## Search for correlated genes ########

observeEvent(input$ft_coe_search, {
    target_gene <- input$ft_coe_target

    if(is.null(r_data$df)) return()

    if(target_gene == "" || is.null(target_gene)) {
        session$sendCustomMessage(type = "showalert", "Please input your target gene.")
        return()
    }

    mat <- as.matrix(r_data$df)
    idx <- which(rownames(mat) == target_gene)

    if(!length(idx)) {
        session$sendCustomMessage(type = "showalert", "Gene not found in the current dataset.")
        return()
    } else {
        gene <- mat[idx,]
    }

    withProgress(message = 'Processing...', value = 0.8, {

    resp <- c()
    est <- c()

    if(input$ft_coe_abs == "positive") {
        cust_f <- function(x){x}
    } else if(input$ft_coe_abs == "negative") {
        cust_f <- function(x){-x}
    } else {
        cust_f <- abs
    }

    mat <- as.matrix(r_data$df)

    tbl <- do.call(rbind, apply(mat, 1, function(row) {
        x <- cor.test(gene, row, method = input$ft_coe_method)
        if (cust_f(x$estimate[[1]]) >= input$ft_coe_threshold){
            c(x$estimate[[1]], x$p.value)
        }
    }))

    if(input$ft_coe_method == "pearson") {
        colnames(tbl) <- c("cor", "p.value")
    } else if(input$ft_coe_method == "kendall") {
        colnames(tbl) <- c("tau", "p.value")
    } else if(input$ft_coe_method == "spearman"){
        colnames(tbl) <- c("rho", "p.value")
    }

    r_data$coe_ft_target <- target_gene
    r_data$coe_ft_tbl <- tbl
    })
})


output$coe_ft_tbl <- DT::renderDataTable({
    if(is.null(r_data$coe_ft_tbl)) return()
    DT::datatable(r_data$coe_ft_tbl, selection = 'single',
                  options = list(scrollX = TRUE, scrollY = "450px", lengthMenu = c(20, 50, 100)))
})

output$coe_ft_tbl_download <-downloadHandler(
    filename = paste0("genes_correlated_with_", r_data$coe_ft_target, ".csv"),
    content = function(file) {
        if(is.null(r_data$coe_ft_tbl)) return()
        write.csv(r_data$coe_ft_tbl, file)
    })


output$coe_ft_hmap_ui <- renderUI({
    if(is.null(r_data$coe_ft_tbl)) return()
    features <- rownames(r_data$coe_ft_tbl)
    if(length(features) < 2) {
        return(tags$p("Too few features for heatmap plotting."))
    } else if(length(features) > 1000) {
        return(tags$p("Exceeding maximum plotting capability (1000)."))
    } else {
        plotOutput("coe_ft_hmap", height = "800px")
    }
})

output$coe_ft_hmap <- renderPlot({
    if(is.null(r_data$coe_ft_tbl)) return()
    features <- rownames(r_data$coe_ft_tbl)

    tmp_data <- list()
    tmp_data$df <- r_data$df[which(rownames(r_data$df) %in% features), ]
    tmp_data$raw <-r_data$raw[which(rownames(r_data$raw) %in% features), ]
    rsList <- callModule(pivot_dataScale, "cor_ft_hmap_scale", tmp_data)
    hm_data <- rsList$df

    rsList <- callModule(pivot_colorBy, "cor_ft_hmap_col", meta = r_data$meta)
    if(!is.null(rsList$meta)) {
        group <- rsList$meta[,1]
        group_color<-rsList$meta_color[,1]
        legend_col <- unique(group_color)
        names(legend_col) <- unique(group)
        gplots::heatmap.2(as.matrix(hm_data), scale="none", trace="none", col=RColorBrewer::brewer.pal(9,"Blues"),
                          key.par=list(cex.axis=0.7), key.title=NA, key.xlab="  ", key.ylab=NA, keysize=.7, density.info="density", revC=T, cexCol = 1.2, margins=c(10,15),
                          ColSideColors = group_color)
        legend("topright", legend = unique(group), bty="n", fill = legend_col, text.col = legend_col, border=FALSE, y.intersp = 1.2, cex = 0.9)
    } else {
        gplots::heatmap.2(as.matrix(hm_data), scale="none", trace="none", col=RColorBrewer::brewer.pal(9,"Blues"),
                          key.par=list(cex.axis=0.7), key.title=NA, key.xlab="  ", key.ylab=NA, keysize=.7, density.info="density", revC=T, cexCol = 1.2, margins=c(8,6))
    }
})



