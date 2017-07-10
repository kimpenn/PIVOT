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


output$mst_ui <- renderUI({
    req(r_data$df)
    list(
        enhanced_box(
            width = 12,
            title = "Community Detection with Minimum Spanning Tree",
            id = "community_mst",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("Minimum Spanning Tree Settings:"), class = "param_setting_title"),
            fluidRow(
                pivot_dataScale_UI("mst_scale", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts", "Standardized Counts", "Log10 & Standardized", "Projection Matrix"), selected = "Log10 Counts", width = 12)
            ),
            fluidRow(
                column(4, selectInput("mst_dist_method", "Distance measure", choices = list("Euclidean" = "euclidean", "Correlation Distance" = "correlation", "SCDE Adjusted Distance" = 'scde'), selected = "correlation")),
                uiOutput('mst_corr_ui'),
                uiOutput('mst_scde_ui')
            ),
            tags$div(tags$b("Community Detection Settings:"), class = "param_setting_title"),
            fluidRow(
                column(4, selectInput("com_algorithm", "Community Detection Algorithm", choices = list("Walktrap" = "walktrap"))),
                column(4, numericInput("com_wt_step", "Walktrap Steps", value = 4, min = 1, max = 100, step = 1))
            ),
            tags$div(tags$b("Visualization Settings:"), class = "param_setting_title"),
            fluidRow(
                column(4, selectInput("mst_package", "Plotting package", choices = list("igraph" = "igraph", "networkD3" = "networkD3"), selected = "igraph")),
                pivot_groupBy_UI("community", r_data$category, append_none = T, width = 8)
            ),
            fluidRow(
                column(4, sliderInput("vertex_size", "Vertex size", min = 1, max = 20, value = 5)),
                uiOutput("mst_igraph_param")
            )
        ),
        box(
            title = NULL,
            status = "primary",
            width = 12,
            fluidRow(
                column(4,
                       tags$div(tags$b("Community Detection Results"), class = "param_setting_title"),
                       DT::dataTableOutput("community_assignment"),
                       uiOutput("down_com_ui")
                ),
                column(8,
                       tags$div(tags$b("Minimum Spanning Tree"), class = "param_setting_title"),
                       uiOutput("mst_plt_ui")
                )
            )
        ),
        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Csardi G, Nepusz T: The igraph software package for complex network research, InterJournal, Complex Systems 1695. 2006. http://igraph.org", class = "citation"),
                tags$li("Darmanis, S., Sloan, S. A., Zhang, Y., Enge, M., Caneda, C., Shuer, L. M., ... & Quake, S. R. (2015). A survey of human brain transcriptome diversity at the single cell level. Proceedings of the National Academy of Sciences, 201507125.", class = "citation")
            )
        )
    )

})

output$mst_corr_ui <- renderUI({
    if(input$mst_dist_method != "correlation") return()
    column(4, selectInput("mst_cor_method", label = "Correlation method", choices = list("Pearson" = "pearson","Spearman" = "spearman", "Kendall" = "kendall"), selected = "pearson"))
})

output$mst_scde_ui <- renderUI({
    if(input$mst_dist_method != 'scde') return()
    column(4, selectInput("mst_scde_dist_method", "SCDE Distance", choices = list("Direct drop-out" = "ddo", "Reciprocal weighting" = "rw", "Mode-relative weighting" = "mrw"), selected = "rw"))
})

output$mst_igraph_param <- renderUI({
    if(is.null(input$mst_package)) return()
    if(input$mst_package == "igraph") {
        list(
            column(4,checkboxInput("color_com", "Show community", value = F)),
            column(4,checkboxInput("mst_lbl", "Show vertex label", value = F))
        )
    } else return()
})

output$down_com_ui <- renderUI({
    if(is.null(r_data$meta$community)) return()
    downloadButton("download_community", "Download Community", class = "btn btn-success")
})

# Construct graph object from data
mstList <- callModule(pivot_dataScale, "mst_scale", r_data)
mst_graph <- reactive({
    req(mstList()$df)
    mst_data <- mstList()$df
    if(input$mst_dist_method == 'euclidean') {
        dist_mtx <- t(mst_data) %>% dist() %>% as.matrix()
    } else if(input$mst_dist_method == 'correlation') {
        dist_mtx <- 1 - cor(mst_data, method = input$mst_cor_method)
    } else if(input$mst_dist_method == 'scde') {
        if(is.null(input$mst_scde_dist_method)) return()
        if(is.null(r_data$scde_ddo) && is.null(r_data$scde_rw) && is.null(r_data$scde_mrw)) {
            session$sendCustomMessage(type = "showalert", "Please compute the SCDE-adjusted distance you want to use in SCDE module first.")
            #updateTabItems(session, "tabs", "scde")
            return()
        }
        if(input$mst_scde_dist_method == "ddo") {
            if(is.null(r_data$scde_ddo)) {
                session$sendCustomMessage(type = "showalert", "Please compute Direct drop-out adjusted distance in SCDE module first.")
                #updateTabItems(session, "tabs", "scde")
                return()
            } else {
                dist_mtx <-r_data$scde_ddo %>% as.matrix()
            }
        } else if(input$mst_scde_dist_method == "rw") {
            if(is.null(r_data$scde_rw)) {
                session$sendCustomMessage(type = "showalert", "Please compute Reciprocal weighting adjusted distance in SCDE module first.")
                #updateTabItems(session, "tabs", "scde")
                return()
            } else {
                dist_mtx <-r_data$scde_rw %>% as.matrix()
            }
        } else if(input$mst_scde_dist_method == "mrw") {
            if(is.null(r_data$scde_mrw)) {
                session$sendCustomMessage(type = "showalert", "Please compute Mode-relative weighting adjusted distance in SCDE module first.")
                #updateTabItems(session, "tabs", "scde")
                return()
            } else {
                dist_mtx <-r_data$scde_mrw %>% as.matrix()
            }
        }
    }

    cList <- callModule(pivot_groupBy, "community", meta = r_data$meta)
    rsList<-generate_mst(dist_mtx, method = input$com_algorithm, color_list = cList, step = input$com_wt_step)
    r_data$meta$community <- paste0("community_",as.character(igraph::membership(rsList$community)))
    r_data$category <- colnames(r_data$meta)
    return(rsList) # Return the graph for plotting
})

# Minimum spanning tree plot
output$mst_plt_ui <- renderUI({
    if(is.null(input$mst_package)) return()
    if(input$mst_package == "igraph") {
        plotOutput("mst_plt", height = "600px")
    } else if(input$mst_package == "networkD3"){
        networkD3::forceNetworkOutput("mst_d3")
    } else return()
})

output$mst_plt <- renderPlot({
    if(is.null(mst_graph())) return()
    mst0 <- mst_graph()$g
    label <- NA
    if(input$mst_lbl) {
        label <- V(mst0)$label
    }

    if(!is.null(input$color_com) && input$color_com) {
        igraph::plot.igraph(mst0, vertex.label=label, vertex.size=input$vertex_size, mark.groups = mst_graph()$community)
    } else {
        igraph::plot.igraph(mst0, vertex.label=label, vertex.size=input$vertex_size)
    }
    legend("topright", legend = unique(V(mst0)$group),  bty="n", fill = unique(V(mst0)$color), text.col = unique(V(mst0)$color), border = FALSE)
})

output$mst_d3 <- networkD3::renderForceNetwork({
    if(is.null(mst_graph())) return()
    mst0 <- mst_graph()$g
    d3_g1<-networkD3::igraph_to_networkD3(mst0, group = V(mst0)$group)
    d3_g1$nodes$size <- rep(input$vertex_size, length(r_data$sample_name))
    ColourScale <- paste0('d3.scaleOrdinal().domain([\"',
                          paste0(unique(as.character(d3_g1$nodes$group)), collapse = "\",\""),
                          '\"]).range([\"',
                          paste0(unique(as.character(V(mst0)$color)), collapse = "\",\""),
                          '\"]);')
    networkD3::forceNetwork(Links = d3_g1$links, Nodes = d3_g1$nodes, Source = "source", Target = "target",
                            NodeID = "name", Nodesize = "size", Group = "group", legend = T,
                            radiusCalculation = "Math.sqrt(d.nodesize) * 2",
                            opacity = .8, zoom = T,  colourScale = networkD3::JS(ColourScale))
})


output$community_assignment <- DT::renderDataTable({
    req(r_data$meta$community)
    tbl <- r_data$meta[,c("sample", "community")]
    DT::datatable(tbl, options = list(scrollX = TRUE, scrollY = "400px", lengthMenu = c(20, 50, 100)))
})

output$download_community <- downloadHandler(
    filename = function() {
        "walktrap_community.csv"
    },
    content = function(file) {
        coms <- r_data$meta$community
        names(coms) <- r_data$sample_name
        write.csv(as.data.frame(coms), file)
    }
)


