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



output$meta_ui <- renderUI({
    list(
        enhanced_box(width = 12,
                     title = "Design Table",
                     status = "custom2",
                     solidHeader = T,
                     fluidRow(
                         column(7,
                                DT::dataTableOutput("meta_tbl"),
                                downloadButton("download_meta_tbl", "Download", class = "btn-success btn_rightAlign")
                         ),
                         column(5,
                                fluidRow(
                                    pivot_colorBy_UI("pie", meta = r_data$meta, append_none = F, multiple = F, width = 12)
                                ),
                                br(),
                                plotly::plotlyOutput("design_pie")
                         )
                     )
        ),
        enhanced_box(width = 12,
                     title = "Sample Statistics",
                     status = "custom5",
                     solidHeader = T,
                     fluidRow(
                         column(7,
                                DT::dataTableOutput("sample_stats_tbl"),
                                downloadButton("download_sample_stats_tbl", "Download", class = "btn btn-success btn_rightAlign")
                         ),
                         column(5,
                                tags$p("Select sample in the table to view its count distribution over features."),
                                fluidRow(
                                    column(4,
                                           selectInput("sample_count_scale", "Choose Count Scale",
                                                       choices = c("Counts (raw)" = "Counts (raw)", "Counts (normalized)" = "Counts (normalized)",
                                                                   "Log10 Counts (raw)" = "Log10 Counts (raw)", "Log10 Counts (normalized)" = "Log10 Counts (normalized)"),
                                                       selected = "Log10 Counts (raw)"
                                           )
                                    ),
                                    column(4,
                                           selectInput("sample_count_plot_type", "Plot Type",
                                                       choices = c("Histogram" = "histogram", "Density Plot" = "density"),
                                                       selected = "histogram")
                                    ),
                                    column(4,
                                           sliderInput("sample_count_step",.1, 2, value = .1, step = .1, label = "Band/Bin Width Adjustment")
                                    )
                                ),
                                plotly::plotlyOutput("sample_count_distribution")
                         )
                     ),
                     tags$div(tags$b("Sample Stats Plot"), class = "param_setting_title"),
                     fluidRow(
                         column(3,
                                uiOutput("sample_plot_stats_ui")
                         ),
                         column(3,
                                selectInput("sample_plot_type", "Plot Type",
                                            choices = c("Bar Plot" = "bar", "Histogram" = "histogram", "Density Plot" = "density"),
                                            selected = "bar")
                         ),
                         pivot_colorBy_UI("sample_stats", meta = r_data$meta, append_none = T, multiple = F, width = 4),
                         column(2, uiOutput("sample_bin_width_ui"))
                     ),
                     plotly::plotlyOutput("sample_stats_plot")
        ),
        enhanced_box(width = 12,
                     title = "Feature Statistics",
                     status = "custom4",
                     solidHeader = T,
                     fluidRow(
                         column(12,
                                DT::dataTableOutput("feature_info_tbl"),
                                downloadButton("download_feature_info_tbl", "Download", class = "btn btn-success btn_rightAlign")
                         )
                     ),
                     pivot_featurePlot_UI("meta_tbl_plt", meta = r_data$meta)
        )
    )
})

output$meta_tbl <- DT::renderDataTable({
    if(is.null(r_data$glb.meta)) return()
    DT::datatable(r_data$glb.meta, selection = 'single',
                  options = list(
                      scrollX = T, scrollY = "400px", lengthMenu = c(20, 50, 100)
                  )
    )
})

output$download_meta_tbl <- downloadHandler(
    filename = "meta_tbl.csv",
    content = function(file) {
        write.csv(r_data$glb.meta, file)
    }
)

output$design_pie <- render_Plotly({
    rsList <- callModule(pivot_colorBy, "pie", meta = r_data$meta)
    req(rsList$meta)
    tbl<-as.data.frame(table(rsList$meta))
    colnames(tbl) <- c(rsList$group_by, "sample_number")
    pal = unique(rsList$meta_color[,1])
    plotly::plot_ly(tbl, labels = as.formula(paste("~", rsList$group_by)), values = ~sample_number, type = 'pie',textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            marker = list(colors = pal, line = list(color = '#FFFFFF', width = 1))) %>%
        plotly::layout(title = paste("Pie chart of category", rsList$group_by),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% plotly::config(displayModeBar = F)
})

output$sample_stats_tbl <- DT::renderDataTable({
    if(is.null(r_data$sample_stats)) return()
    DT::datatable(r_data$sample_stats, selection = 'single', options = list(
        scrollX = T, scrollY = "450px", lengthMenu = c(20, 50, 100)
    )
    )
})

output$download_sample_stats_tbl <- downloadHandler(
    filename = "sample_stats.csv",
    content = function(file) {
        write.csv(r_data$sample_stats, file)
    }
)

output$sample_plot_stats_ui <- renderUI({
    req(r_data$sample_stats)
    options <- colnames(r_data$sample_stats)
    names(options) <- options
    selectInput("sample_plot_stats", "Plot Stats",
                choices = options
    )
})


output$sample_bin_width_ui <- renderUI({
    req(input$sample_plot_type %in% c("histogram", "density"))
    sliderInput("sample_stats_step",.1, 2, value = .1, step = .1, label = "Band/Bin Width Adjustment")
})

# Sample Stats Plot

output$sample_stats_plot <- render_Plotly({
    req(r_data$sample_stats,input$sample_plot_stats)
    tbl <- r_data$sample_stats %>% tibble::rownames_to_column("sample")
    rsList <- callModule(pivot_colorBy, "sample_stats", meta = r_data$meta)
    if(!is.null(rsList$meta)) {
        tbl$Group <- rsList$meta[,1]
        pal = unique(rsList$meta_color[,1])
    } else {
        tbl$Group <- rep("sample", nrow(tbl))
        pal = NULL
    }

    if(input$sample_plot_type == "bar") {
        plt1 <- tbl %>%
            plotly::plot_ly(x = ~sample, y = as.formula(paste0("~", input$sample_plot_stats)), type = "bar",
                            source = "selectSampleStats", color = as.character(tbl$Group), colors = pal) %>%
            plotly::layout(margin = list(b=100))
    } else if(input$sample_plot_type == "density") {
        dens<-tapply(tbl[,input$sample_plot_stats], INDEX = tbl$Group, function(x){density(x,adjust = input$sample_stats_step)})
        df <- data.frame(
            x = unlist(lapply(dens, "[[", "x")),
            y = unlist(lapply(dens, "[[", "y")),
            Group = rep(names(dens[!sapply(dens, is.null)]), each = length(dens[[1]]$x))
        )
        plt1 <- plotly::plot_ly(df, x = ~x, y = ~y, color = ~Group, colors = pal,
                                type  = "scatter", mode = "lines", fill = "tozeroy") %>%
            plotly::layout(xaxis = list(title = input$sample_plot_stats))
    } else if (input$sample_plot_type == "histogram") {
        start = min(tbl[,input$sample_plot_stats])
        end = max(tbl[,input$sample_plot_stats])
        plt1 <- plotly::plot_ly(tbl, x = as.formula(paste0("~", input$sample_plot_stats)), type = "histogram",
                                xbins=list(start = start, end = end, size = (end - start)*input$sample_stats_step/2),
                                autobinx=F, color = as.character(tbl$Group), colors = pal, opacity = 0.8)
    }
    plt1
})

output$sample_count_distribution <- render_Plotly({
    req(r_data$sample_stats)
    s = input$sample_stats_tbl_row_last_clicked
    tbl<-as.data.frame(r_data$sample_stats)

    if (length(s)) {
        sample <- rownames(tbl[s, , drop = FALSE])
    } else {
        return()
    }

    if(input$sample_count_scale == "Counts (raw)") {
        tbl <- r_data$raw
    } else if (input$sample_count_scale == "Counts (normalized)") {
        tbl <- r_data$df
    } else if (input$sample_count_scale == "Log10 Counts (raw)") {
        tbl <- log10(r_data$raw + 1)
    } else if (input$sample_count_scale == "Log10 Counts (normalized)") {
        tbl <- log10(r_data$df + 1)
    }
    if(input$sample_count_plot_type == "density") {
        dens<-tapply(tbl[,sample], INDEX = rep(1, nrow(tbl)), function(x){density(x,adjust = input$sample_count_step)})
        df <- data.frame(
            x = unlist(lapply(dens, "[[", "x")),
            y = unlist(lapply(dens, "[[", "y"))
        )
        plt1<-plotly::plot_ly(df, x = ~x, y = ~y, type  = "scatter", mode = "lines", fill = "tozeroy")
    } else if(input$sample_count_plot_type == "histogram") {
        start = min(tbl[,sample])
        end = max(tbl[,sample])
        plt1<-plotly::plot_ly(tbl, x = as.formula(paste0("~", sample)), type = "histogram",
                        xbins=list(start = start, end = end, size = (end - start)*input$sample_count_step/2),
                        autobinx=F, opacity = 0.8)
    }
    plt1 %>% plotly::layout(xaxis = list(title = paste(input$sample_count_scale, "of", sample)),
                     yaxis = list(title = "Features"))

})


output$download_feature_info_tbl <- downloadHandler(
    filename = "feature_info_tbl.csv",
    content = function(file) {
        ftbl <-r_data$feature_meta
        ftbl <- ftbl[, -which(names(ftbl) %in% c("use_for_ordering", "cap_name", "STRING_id"))]
        write.csv(ftbl, file)
    }
)

# A copy of the above table to be put in feature filter tab
output$feature_info_tbl <- DT::renderDataTable({
    req(r_data$feature_meta)
    ftbl <-r_data$feature_meta
    ftbl <- ftbl[, -which(names(ftbl) %in% c("use_for_ordering", "cap_name", "STRING_id"))]
    DT::datatable(ftbl, selection = 'single', rownames = FALSE, options = list(
        scrollX = T, scrollY = "450px", lengthMenu = c(20, 50, 100)
    )
    )
})

observe({
    req(r_data$feature_meta)
    s = input$feature_info_tbl_row_last_clicked
    tbl<-as.data.frame(r_data$feature_meta)

    if (length(s)) {
        selected_gene <- rownames(tbl[s, , drop = FALSE])
    } else {
        return()
    }

    d <- as.data.frame(t(r_data$df[selected_gene,])) %>% tibble::rownames_to_column()
    colnames(d) <- c("sample", "expression_level")
    callModule(pivot_featurePlot, "meta_tbl_plt", meta = r_data$meta, df = d, gene = selected_gene)
})

