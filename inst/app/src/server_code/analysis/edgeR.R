

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



output$edgeR_ui <- renderUI({
    if(is.null(r_data$meta) || ncol(r_data$meta) < 2){
        return(
            list(
                tags$li("This module requires design information input.")
            )
        )
    }

    list(
        enhanced_box(
            width = 12,
            title = "edgeR2 Differential Expression Analysis",
            id = "edgeR_results",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("General Settings:"), class = "param_setting_title"),
            fluidRow(
                pivot_deGroupBy_UI("edgeR", r_data$meta, width = 12, method = "edgeR", model = c("condition", "condition_batch", "timecourse1"))
            ),

            uiOutput("perform_edgeR_ui")
        ),
        uiOutput("edgeR_results_box"),
        enhanced_box(
            width = 12,
            title = NULL,
            status = "primary",
            tags$div(tags$b("MA Plot:"), class = "param_setting_title"),
            plotOutput("edgeR_ma_plt", height = "600px")
        ),
        enhanced_box(
            width = 12,
            title = NULL,
            status = "primary",
            solidHeader = F,
            uiOutput("edgeR_gene_plot_ui")
        ),
        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Michael I Love, Wolfgang Huber and Simon Anders (2014): Moderated estimation of fold change and dispersion for RNA-Seq data with edgeR2. Genome Biology.", class = "citation")
            )
        )
    )

})

edgeRModel <- callModule(pivot_deGroupBy, "edgeR", r_data = r_data)

output$perform_edgeR_ui <- renderUI({
    req(edgeRModel())
    # examine if required edgeR_sanity check are passed
    method_ui <- selectInput("edgeR_test_method", "Test Method", choices = list("Wald" = "Wald", "LRT" = "LRT"), selected = "Wald")
    if(edgeRModel()$design %in% c("timecourse1", "timecourse2")) {
        method_ui <- selectInput("edgeR_test_method", "Test Method", choices = list("LRT" = "LRT"))
    }
    list(
        fluidRow(
            column(4, method_ui),
            column(8,
                   uiOutput("edgeR_test_explain")
            )
        ),
        actionButton("perform_edgeR", "Perform DE Analysis", class = "btn-info btn_leftAlign")
    )
})

output$edgeR_test_explain <- renderUI({
    req(input$edgeR_test_method)
    if(input$edgeR_test_method == "Wald") {
        list(
            tags$b("Wald test for the GLM coefficients: "),
            tags$li("This function tests for significance of coefficients in a Negative Binomial GLM."),
            tags$li("Note: This is the default test for edgeR.")
        )
    } else if(input$edgeR_test_method == "LRT") {
        list(
            tags$b("Likelihood ratio test (chi-squared test) for GLMs: "),
            tags$li("This function tests for significance of change in deviance between a full and reduced model."),
            tags$li("Note: Useful for testing multiple terms at once, conceptually similar to ANOVA.")
        )
    } else {
        return()
    }
})

observeEvent(input$perform_edgeR, {
    req(r_data$meta, edgeRModel(), input$edgeR_test_method, ncol(r_data$meta) >= 2)

    # Clear previous results if exist
    if(!is.null(r_data$dds)) {
        r_data$dds <- NULL
        r_data$edgeR_params <- NULL
        r_data$edgeR_group <- NULL
        r_data$edgeR_results <- NULL
    }

    withProgress(message = 'Processing...', value = 0.5, {
        error_I <- 0
        # Perform size factor re-estimation if necessary
        tryCatch({
            samplesAll <- data.frame(row.names=colnames(r_data$raw), celltype=rep("nt",length(colnames(r_data$raw))))
            dds <- edgeR2::edgeRDataSetFromMatrix(countData = r_data$raw, colData=samplesAll, design = ~ 1) # Design here does not matter, overwrite later

            # If the data was normalized by edgeR modified, use the new size factor estimation
            if(r_data$norm_param$method == "Modified_edgeR") {
                edgeR2::sizeFactors(dds) <- r_data$norm_param$sizeFactor$size_factor
            } else {
                # If the data was not normalized by edgeR modified, re-estimate size factors using edgeR2
                dds <- edgeR2::estimateSizeFactors(dds)
            }
        },
        error = function(e){
            error_I <<- 1
        }
        )

        if(error_I) {
            session$sendCustomMessage(type = "showalert", "edgeR failed.")
            return()
        }
        groups <- r_data$meta[, edgeRModel()$group_cate]
        SummarizedExperiment::colData(dds)$group <- factor(groups, levels = unique(groups)) # update dds with group info
        if(edgeRModel()$design == "condition_batch") {
            batches = r_data$meta[,edgeRModel()$batch_cate]
            SummarizedExperiment::colData(dds)$batch <- factor(batches, levels = unique(batches))
        } else if(edgeRModel()$design == "timecourse1") {
            timecourse = r_data$meta[,edgeRModel()$time_cate]
            SummarizedExperiment::colData(dds)$timecourse <- factor(timecourse, levels = unique(timecourse))
        }
        BiocGenerics::design(dds) <- edgeRModel()$model$full
        if(input$edgeR_test_method == "Wald") {
            r_data$dds <- edgeR2::edgeR(dds)
        } else if(input$edgeR_test_method == "LRT") {
            r_data$dds <- edgeR2::edgeR(dds, test = "LRT", reduced = edgeRModel()$model$reduced)
        }

        r_data$edgeR_params <- list(design = edgeRModel()$design, test = input$edgeR_test_method, condition = edgeRModel()$group_cate, batch = edgeRModel()$batch_cate, timecourse = edgeRModel()$time_cate)
    })
})

output$edgeR_results_box <- renderUI({
    req(r_data$meta, ncol(r_data$meta) >= 2, r_data$dds, r_data$edgeR_params)

    groups = unique(as.character(r_data$meta[,r_data$edgeR_params$condition]))
    names(groups) = groups

    edgeR_group_ui <- list(
        if(r_data$edgeR_params$test != "LRT") {
            fluidRow(
                column(4, tags$br(),tags$b("Pairwise comparison:")),
                column(3, selectInput("edgeR_group1", "Group 1", choices = as.list(groups), selected = groups[[1]])),
                column(1, tags$b("vs")),
                column(3, selectInput("edgeR_group2", "Group 2", choices = as.list(groups), selected = groups[[2]]))
            )
        } else {
            options<-edgeR2::resultsNames(r_data$dds)[-1]
            names(options) <- options
            list(
                fluidRow(
                    column(4, selectInput("edgeR_pval_type", "P value type", choices = list("LRT" = "LRT", "Wald" = "Wald"), selected = "LRT")),
                    column(8,
                           selectInput("edgeR_result_name", "Choose comparison/individual points",
                                       choices = as.list(options))
                    )
                )
            )

        }

    )

    if(!is.null(r_data$batch)) {
        edgeR_batch_ui <- checkboxInput("edgeR_batch_yes", "Control Batch Effects", value = F)
    } else {
        edgeR_batch_ui <- NULL
    }

    enhanced_box(
        width = 12,
        title = NULL,
        status = "primary",
        solidHeader = T,
        tags$div(tags$b("Results Table:"), class = "param_setting_title"),
        edgeR_group_ui,
        fluidRow(
            column(4,
                   uiOutput("edgeR_test_method_text")
            ),
            column(4, numericInput("edgeR_alpha", "FDR cutoff", value = 0.1, min = 0, max = 0.5, step = 0.001)),
            column(4, checkboxInput("edgeR_cuttbl", "Only show significant genes", value = F))
        ),
        DT::dataTableOutput("edgeR_result_tbl"),
        uiOutput("download_edgeR_result_ui"),
        hr(),
        uiOutput("edgeR_sig_genes")
    )
})

output$edgeR_sig_genes <- renderUI({
    req(r_data$edgeR_results)
    sm <- capture.output(edgeR2::summary.edgeRResults(r_data$edgeR_results))
    list(
        tags$h4("Summary"),
        tags$li(paste0("Total number of significant genes: ", sum(r_data$edgeR_results$padj < input$edgeR_alpha, na.rm = T), ".")),
        tags$li(sm[4]),
        tags$li(sm[5]),
        tags$li(sm[6]),
        tags$li(paste(sm[7], sm[8]))
    )
})

output$edgeR_test_method_text <- renderUI({
    req(r_data$edgeR_results)
    if(r_data$edgeR_params$test == "Wald") {
        test_text1 <- r_data$edgeR_results@elementMetadata$description[4]
        test_text2 <- "Note: The Wald p-value will be different for different pairwise comparisons / individual points."
    } else {
        test_text1 <- r_data$edgeR_results@elementMetadata$description[4]
        test_text2 <- "Note: The LRT p-value does not depend on the group choice."
    }
    return(
        list(
            tags$li(test_text1),
            tags$li(test_text2)
        )
    )
})

output$download_edgeR_result_ui <- renderUI({
    req(r_data$edgeR_results)
    tbl<-as.data.frame(r_data$edgeR_results)
    if(nrow(tbl) == 0) return()
    download_edgeR_result_ui <- downloadButton("download_edgeR_result","Download", class = "btn btn-success")
})

observe({
    req(r_data$meta, r_data$dds, r_data$edgeR_params, ncol(r_data$meta) >= 2)

    if(r_data$edgeR_params$test == "LRT") {
        req(input$edgeR_pval_type, input$edgeR_result_name)
    } else {
        req(input$edgeR_group1, input$edgeR_group1 != input$edgeR_group2)
    }
    withProgress(message = 'Processing...', value = 0.5, {
        if(r_data$edgeR_params$test == "LRT") {
            #assign("dds",r_data$dds, env = .GlobalEnv)
            #assign("edgeR_param", r_data$edgeR_params, env = .GlobalEnv)
            res1 <- edgeR2::results(r_data$dds, test = input$edgeR_pval_type,
                                    name = input$edgeR_result_name,
                                    alpha = input$edgeR_alpha)
        } else {
            res1 <- edgeR2::results(r_data$dds, contrast = c("group", input$edgeR_group1, input$edgeR_group2), alpha = input$edgeR_alpha)
        }

        resOrdered <- res1[order(res1$padj),]
        r_data$edgeR_group <- c(input$edgeR_group1, input$edgeR_group2)
        if(input$edgeR_cuttbl) {
            r_data$edgeR_results <- BiocGenerics::subset(resOrdered, padj <= input$edgeR_alpha)
        } else {
            r_data$edgeR_results <- resOrdered
        }
    })
})

output$edgeR_result_tbl <- DT::renderDataTable({
    req(r_data$edgeR_results)
    tbl<-as.data.frame(r_data$edgeR_results)
    if(nrow(tbl) == 0) return()
    DT::datatable(tbl, selection = 'single', options = list(scrollX = TRUE, scrollY = "250px", searching=T, order = list(list(6, 'asc')) , orderClasses = T))
})

output$download_edgeR_result <- downloadHandler(
    filename = function() {
        "edgeR_results.csv"
    },
    content = function(file) {
        write.csv(as.data.frame(r_data$edgeR_results), file)
    }
)

output$edgeR_ma_plt <- renderPlot({
    if(is.null(r_data$edgeR_results)) return()
    BiocGenerics::plotMA(r_data$edgeR_results, main="edgeR2", ylim=c(-2,2))
})

output$edgeR_gene_plot_ui <- renderUI({
    req(r_data$edgeR_params)
    if(r_data$edgeR_params$test == "LRT") {
        pivot_featurePlot_UI("edgeR_gene_plt", meta = r_data$meta) # We do not allow plotting for paired comparison while using LRT. (Since it's ANOVA like)
    } else {
        pivot_featurePlot_UI("edgeR_gene_plt", meta = r_data$meta, ids = 1) # ids = 1 indicate we are going to input ids in the caller.
    }
})

observe({
    req(r_data$edgeR_results)
    s = input$edgeR_result_tbl_row_last_clicked
    tbl<-as.data.frame(r_data$edgeR_results)

    if (length(s)) {
        selected_gene <- rownames(tbl[s, , drop = FALSE])
    } else {
        return()
    }

    d <- as.data.frame(t(r_data$df[selected_gene,])) %>% tibble::rownames_to_column()
    colnames(d) <- c("sample", "expression_level")

    if(r_data$edgeR_params$test == "LRT") {
        samples = NULL
    } else {
        req(input$edgeR_group1, input$edgeR_group2)
        samples = r_data$meta[,1][which(r_data$meta[,r_data$edgeR_params$condition] %in% c(input$edgeR_group1, input$edgeR_group2))]
    }

    callModule(pivot_featurePlot, "edgeR_gene_plt", meta = r_data$meta, df = d, gene = selected_gene, ids = samples)
})



