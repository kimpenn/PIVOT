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


######## Some control ui #########

output$hm_ui <- renderUI({
    req(r_data$df)
    list(
        enhanced_box(
            width = 12,
            title = "Feature Heatmap",
            id = "feature_heatmap",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            get_pdf = T,
            register_analysis = T,

            tags$div(tags$b("General Settings:"), class = "param_setting_title"),
            tags$p("Heatmap support plotting of up to 10000 features. Please rank your feature. The top ranked features will be plotted."),
            pivot_dataScaleRange_UI("ft_hmap", bound = nrow(r_data$df)),
            tags$div(tags$b("Heatmap Settings:"), class = "param_setting_title"),
            fluidRow(
                column(4, selectInput("hm_sp_order_type", "Sample (Column) Order", choices = list("Order sample by hierarchical cluster" = "hc", "Do not cluster sample" = "no"), selected = "hc")),
                column(4, selectInput("hm_ft_order_type", "Feature (Row) Order", choices = list("Order feature by hierarchical cluster" = "hc", "Do not cluster feature" = "no"), selected = "hc"))
            ),
            uiOutput("hm_sp_hc_control"),
            uiOutput("hm_ft_hc_control"),
            tags$div(tags$b("Visualization Settings:"), class = "param_setting_title"),
            fluidRow(
                column(3, selectInput("hmap_package", label = "Plotting package", choices = list("gplots"="gplots", "heatmaply"="heatmaply"), multiple = F)),
                column(3, selectInput("hmap_hmcolor", label = "Heatmap color", choices = c(get_brewer_set("sequential"), list("viridis" = "viridis", "magma" = "magma", "plasma" = "plasma", "inferno" = "inferno")), multiple = F)),
                pivot_groupBy_UI("hmap", r_data$category, append_none = T, width = 6)
            ),
            tags$hr(),
            conditionalPanel(
                "input.hmap_package == 'gplots'",
                plotOutput("ft_hmap_gplots", height = "800px")
            ),
            conditionalPanel(
                "input.hmap_package == 'heatmaply'",
                #d3heatmap::d3heatmapOutput("hmap_d3", height = "800px") # no longer use d3heatmap
                plotly::plotlyOutput("ft_hmap_plotly", height = "800px")
            ),

            downloadButton('download_featurevar', 'Download Ranked Table',class = "btn btn-success")
        )
    )
})

output$hm_sp_hc_control <- renderUI({
    req(input$hm_sp_order_type)
    if(input$hm_sp_order_type == "no") return()
    fluidRow(
        column(4, selectInput("hm_sp_dist_method", "Sample distance", choices = list("Euclidean" = "euclidean", "Maximum" = "maximum", "Manhattan" = "manhattan", "Canberra" = "canberra", "Binary" = "binary", "Correlation Distance" = "corr"), selected = "euclidean")),
        uiOutput("hm_sp_corr_ui"),
        column(4, selectInput("hm_sp_agglo_method", "Sample agglomeration method", choices = list("Ward.D" = "ward.D", "Ward.D2" = "ward.D2","Single"= "single", "Complete"="complete", "Average"= "average", "Mcquitty"="mcquitty", "Median"= "median", "Centroid" = "centroid"), selected = "complete"))
    )
})

output$hm_ft_hc_control <- renderUI({
    req(input$hm_ft_order_type)
    if(input$hm_ft_order_type == "no") return()
    fluidRow(
        column(4, selectInput("hm_ft_dist_method", "Feature distance", choices = list("Euclidean" = "euclidean", "Manhattan" = "manhattan", "Correlation Distance (1-r)" = "corr1", "Absolute Correlation (1-|r|)" = "corr2"), selected = "euclidean")),
        uiOutput("hm_ft_corr_ui"),
        column(4, selectInput("hm_ft_agglo_method", "Feature agglomeration method", choices = list("Ward.D" = "ward.D", "Ward.D2" = "ward.D2","Single"= "single", "Complete"="complete", "Average"= "average", "Mcquitty"="mcquitty", "Median"= "median", "Centroid" = "centroid"), selected = "complete"))
    )
})


output$hm_sp_corr_ui <- renderUI({
    if (input$hm_sp_dist_method != 'corr') return()
    column(4,selectInput("hm_sp_cor_method", label = "Sample correlation method", choices = list("Pearson" = "pearson","Spearman" = "spearman", "Kendall" = "kendall"), selected = "pearson"))
})

output$hm_ft_corr_ui <- renderUI({
    if (!(input$hm_ft_dist_method %in% c('corr1','corr2'))) return()
    column(4,selectInput("hm_ft_cor_method", label = "Feature correlation method", choices = list("Pearson" = "pearson","Spearman" = "spearman", "Kendall" = "kendall"), selected = "pearson"))
})


###### Actual heatmap plotting module ######

# rank by fano factor

hmList <- callModule(pivot_dataScaleRange, "ft_hmap", r_data)


# sample clustering result
hm_sp_dend <- reactive({
    req(hmList(), input$hm_sp_order_type, input$hm_sp_dist_method)

    if(input$hm_sp_order_type == 'hc') {
        if(input$hm_sp_dist_method == 'corr') {
            as.dist(1 - cor(hmList()$df, method = input$hm_sp_cor_method)) %>% hclust(method = input$hm_sp_agglo_method) %>% as.dendrogram()
        } else {
            t(hmList()$df) %>% dist(method = input$hm_sp_dist_method) %>% hclust(method = input$hm_sp_agglo_method) %>% as.dendrogram()
        }
    } else {
        return(FALSE)
    }

})

hm_ft_dend <- reactive({
    req(hmList(), input$hm_ft_order_type, input$hm_ft_dist_method)
    if(input$hm_ft_order_type == 'hc') {
        if(input$hm_ft_dist_method == 'corr1') {
            as.dist(1 - cor(t(hmList()$df), method = input$hm_ft_cor_method)) %>% hclust(method = input$hm_ft_agglo_method) %>% as.dendrogram()
        } else if(input$hm_ft_dist_method == 'corr2') {
            as.dist(1 - abs(cor(t(hmList()$df), method = input$hm_ft_cor_method))) %>% hclust(method = input$hm_ft_agglo_method) %>% as.dendrogram()
        } else {
            hmList()$df %>% dist(method = input$hm_ft_dist_method) %>% hclust(method = input$hm_ft_agglo_method) %>% as.dendrogram()
        }
    } else {
        return(FALSE)
    }

})

# No longer use d3heatmap
# output$hmap_d3 <- d3heatmap::renderD3heatmap({
#     if(is.null(hmList()$df) || is.null(hm_sp_dend()) || is.null(hm_ft_dend())) return ()
#     d3heatmap::d3heatmap(as.matrix(hmList()$df), scale="none", Rowv = hm_ft_dend(), Colv = hm_sp_dend(), dendrogram="both", colors=get_heatmap_color(input$hmap_hmcolor), revC=F, margins=c(8,6))
# })

output$ft_hmap_gplots <- renderPlot({
    if(is.null(hmList()) || is.null(hm_sp_dend()) || is.null(hm_ft_dend())) return ()
    if(input$hm_sp_order_type == 'hc' && input$hm_ft_order_type == 'hc') {
        dend1 <- 'both'
    } else if(input$hm_sp_order_type == 'hc' && input$hm_ft_order_type != 'hc') {
        dend1 <- 'column'
    } else if(input$hm_sp_order_type != 'hc' && input$hm_ft_order_type == 'hc') {
        dend1 <- 'row'
    } else {
        dend1 <- 'none'
    }

    rsList <- callModule(pivot_groupBy, "hmap", meta = r_data$meta)

    if(!is.null(rsList$meta)) {
        group <- rsList$meta[,1]
        group_color<-rsList$meta_color[,1]
        legend_col <- unique(group_color)
        names(legend_col) <- unique(group)
        gplots::heatmap.2(as.matrix(hmList()$df), scale="none", Rowv = hm_ft_dend(), Colv = hm_sp_dend(), dendrogram=dend1, trace="none", col=get_heatmap_color(input$hmap_hmcolor),
                          key.par=list(cex.axis=0.7), key.title=NA, key.xlab="  ", key.ylab=NA, keysize=.7, density.info="density", revC=T, cexCol = 1.2, margins=c(10,15),
                          ColSideColors = group_color)
        legend("topright", legend = unique(group), bty="n", fill = legend_col, text.col = legend_col, border=FALSE, y.intersp = 1.2, cex = 0.9)
    } else {
        gplots::heatmap.2(as.matrix(hmList()$df), scale="none", Rowv = hm_ft_dend(), Colv = hm_sp_dend(), dendrogram=dend1, trace="none", col=get_heatmap_color(input$hmap_hmcolor),
                          key.par=list(cex.axis=0.7), key.title=NA, key.xlab="  ", key.ylab=NA, keysize=.7, density.info="density", revC=T, cexCol = 1.2, margins=c(8,6))
    }
})

output$ft_hmap_plotly <- plotly::renderPlotly({
    if(is.null(hmList()) || is.null(hm_sp_dend()) || is.null(hm_ft_dend())) return ()
    if(input$hm_sp_order_type == 'hc' && input$hm_ft_order_type == 'hc') {
        dend1 <- 'both'
    } else if(input$hm_sp_order_type == 'hc' && input$hm_ft_order_type != 'hc') {
        dend1 <- 'column'
    } else if(input$hm_sp_order_type != 'hc' && input$hm_ft_order_type == 'hc') {
        dend1 <- 'row'
    } else {
        dend1 <- 'none'
    }

    # Sadly current version of heatmaply does not support column side coloring... advantage compared to d3heatmap is speed...
    hmap<-heatmaply::heatmapr(as.matrix(hmList()$df), scale="none", Rowv = hm_ft_dend(), Colv = hm_sp_dend(), dendrogram=dend1,
                              revC=T)
    heatmaply::heatmaply(hmap, colors=get_heatmap_color(input$hmap_hmcolor))
})


output$download_featurevar <- downloadHandler(
    filename = function(){
        paste0("features_ranked_by_",hmList()$order,".csv")
    },
    content = function(file) {
        rsList <- callModule(pivot_dataScaleRange, "ft_hmap", r_data, keep_stats = T)
        hm_data <- rsList$df
        write.csv(hm_data, file, row.names = T)
    }
)

