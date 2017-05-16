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
                         column(12,
                                DT::dataTableOutput("meta_tbl"),
                                downloadButton("download_meta_tbl", "Download", class = "btn-success btn_rightAlign")
                         )
                     )
        ),
        enhanced_box(width = 12,
                     title = "Sample Statistics",
                     status = "custom5",
                     solidHeader = T,
                     fluidRow(
                         column(12,
                                DT::dataTableOutput("sample_info_tbl"),
                                downloadButton("download_sample_info_tbl", "Download", class = "btn btn-success btn_rightAlign")
                         )
                     ),
                     tags$div(tags$b("Sample Stats Plot"), class = "param_setting_title"),
                     fluidRow(
                         column(6,
                                uiOutput("sample_info_plt_type_ui")
                         ),
                         column(6,
                                uiOutput("sample_info_group_ui")
                         )
                     ),
                     uiOutput("sample_info_ui")
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
                      scrollX = T, scrollY = "450px", lengthMenu = c(20, 50, 100)
                  )
    )
})

output$download_meta_tbl <- downloadHandler(
    filename = "meta_tbl.csv",
    content = function(file) {
        write.csv(r_data$glb.meta, file)
    }
)

output$sample_info_tbl <- DT::renderDataTable({
    if(is.null(r_data$sample_meta)) return()
    DT::datatable(r_data$sample_meta, selection = 'single', options = list(
        scrollX = T, scrollY = "450px", lengthMenu = c(20, 50, 100)
    )
    )
})

output$download_sample_info_tbl <- downloadHandler(
    filename = "sample_stats.csv",
    content = function(file) {
        write.csv(r_data$sample_meta, file)
    }
)

output$sample_info_plt_type_ui <- renderUI({
    req(r_data$norm_param$method)
    options <- list("Number of genes expressed" = "num_genes_expressed", "Total raw counts" = "total_raw_reads")
    if(r_data$norm_param$method %in% c("DESeq", "Modified_DESeq")) {
        options$"Total normalized counts" <- "total_normalized_counts"
        options$"DESeq size factor" <- "deseq_size_factor"
        options$"Cook's Distance" <- "cooks"
    } else if (r_data$norm_param$method != "None"){
        options$"Total normalized counts" <- "total_normalized_counts"
    }
    selectInput("sample_info_plt_type", "Plot Stats",
                choices = options
    )
})

output$sample_info_group_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2) return()
    categories = colnames(r_data$glb.meta)[-1]
    names(categories) <- categories
    options <- as.list(categories)
    options$None = "None"
    selectInput("sample_info_group", label = "Color by", choices = options)
})

output$sample_info_ui <- renderUI({
    if(is.null(input$sample_info_plt_type)) return()

    if(input$sample_info_plt_type == "cooks") {
        list(
            actionButton("meta_cooks_btn", "Compute cook's distance", class = "btn-info"),
            plotOutput("meta_cooks_distance_plt")
        )
    } else {
        plotly::plotlyOutput("sample_info_plot")
    }
})

output$sample_info_plot <- render_Plotly({
    if(is.null(r_data$sample_meta) || is.null(input$sample_info_plt_type) || input$sample_info_plt_type == "cooks") return()
    r_data$glb.meta
    input$sample_info_group
    isolate({
        withProgress(message = 'Processing...', value = 0.5, {
            tbl <- r_data$sample_meta %>% tibble::rownames_to_column("sample")
            colnames(tbl)[which(colnames(tbl) == input$sample_info_plt_type)] <- "y"
            if(!is.null(input$sample_info_group) && input$sample_info_group != "None") {
                tbl$Group <- r_data$glb.meta[,input$sample_info_group][match(tbl$sample,r_data$glb.meta[,1])]
                plt1 <- tbl %>% plotly::plot_ly(x = ~sample, y = ~y, type = "bar", color = as.character(tbl$Group))
            } else {
                plt1 <- tbl %>% plotly::plot_ly(x = ~sample, y = ~y, type = "bar")
            }

            plt1 %>% plotly::layout(
                xaxis = list(title = "sample"),
                yaxis = list(title = input$sample_info_plt_type))
        })

    })
})

output$meta_cooks_distance_plt <- renderPlot({
    req(r_data$cooks)
    boxplot(r_data$cooks, range=0, las=2)
    title("Boxplot of Cook's Distance")
})

observeEvent(input$meta_cooks_btn,  {
    withProgress(message = 'Processing...', value = 0.8, {
        error_I <- 0
        tryCatch({
            samplesAll <- data.frame(row.names=colnames(r_data$raw), celltype=rep("nt",length(colnames(r_data$raw))))
            dds <- DESeq2::DESeqDataSetFromMatrix(countData = r_data$raw, colData=samplesAll, design = ~ 1)
            dds <- DESeq2::estimateSizeFactors(dds)
            dds <- DESeq2::DESeq(dds)
            r_data$cooks<-log10(assays(dds)[["cooks"]])
        },
        error = function(e){
            error_I <<- 1
        }
        )

        if(error_I) {
            session$sendCustomMessage(type = "showalert", "Failed to compute Cook's distance.")
            return()
        }
    })
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

