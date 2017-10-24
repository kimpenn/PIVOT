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


# this module handles the initiation of the Monocle dataset

output$monocle_ui <- renderUI({
    if(is.null(r_data$raw)) return()
    list(
        enhanced_box(
            width = 12,
            title = "Monocle CellDataSet and Differential Expression Analysis",
            id = "monocle_de",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            tags$div(tags$b("STEP 0: Initiate Monocle CellDataSet"), class = "param_setting_title"),
            fluidRow(
                column(4, uiOutput("mn_data_ui"), uiOutput("mn_norm_ui")),
                column(4, numericInput("mn_min_expr", "Minimum expression level (detection limit)",  min = 0.01, value = 0.1, step = 1))
            ),
            fluidRow(
                column(4,
                       selectInput("mn_family_fun", "Choose a distribution for your expression data",
                                   choices = list("Tobits" = "tobit", "Negbinomial" = "negbinomial", "Negbinomial.size" = "negbinomial.size", "Gaussianff" = "gaussianff"),
                                   selected = "negbinomial")
                ),
                column(8,
                       uiOutput("mn_data_dist_text_ui")
                )
            ),
            fluidRow(
                column(4, actionButton("init_monocle", "Create Monocle CellDataSet", class = "btn btn-info")),
                column(8, uiOutput("monocle_ok"))
            )
        ),
        enhanced_box(
            title = NULL,
            status = "primary",
            width = 12,
            solidHeader = F,
            tags$div(tags$b("OPTIONAL: Testing for Differential Expression"), class = "param_setting_title"),
            uiOutput("monocle_de_ui"),
            hr(),
            pivot_featurePlot_UI("monocle_gene_plt", meta = r_data$meta)
        ),

        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Cole Trapnell and Davide Cacchiarelli et al (2014): The dynamics and regulators of cell fate decisions are revealed by pseudo-temporal
                        ordering of single cells. Nature Biotechnology", class = "citation"),
                tags$li("Monocle Website:", a("http://cole-trapnell-lab.github.io/monocle-release/", src = "http://cole-trapnell-lab.github.io/monocle-release/")),
                tags$li("Monocle was written by Cole Trapnell with input from Davide Cacchiarelli and is provided under the OSI-approved Artistic License (version 2.0).")
                )
        )
    )
})


output$mn_data_ui <- renderUI({
    if(r_data$norm_param$method %in% c("DESeq","Modified_DESeq","TMM","upperquartile")) {
        options <- c("Raw count")
    } else if(r_data$norm_param$method %in% c("TMM-RPKM","upperquartile-RPKM","CPM","RPKM","TPM")) {
        options <- c("Raw count", paste(r_data$norm_param$method, "normalized count"))
    } else if(r_data$norm_param$method %in% c("ERCC-RLM","Census")) {
        options <- c("Raw count", paste(r_data$norm_param$method, "normalized count"))
    } else if(r_data$norm_param$method == "none") {
        options <- c("Raw count")
    }
    names(options) <- options
    selectInput("mn_data", "Data Input", choices = options, selected = "Raw count")
})

output$mn_norm_ui <- renderUI({
    if(r_data$norm_param$method %in% c("DESeq","Modified_DESeq","TMM","upperquartile")) {
        tags$p("Recommend using raw read count input with NB model.")
    } else if(r_data$norm_param$method %in% c("TMM-RPKM","upperquartile-RPKM","CPM","RPKM","TPM")) {
        tags$p("Recommend using raw read count input with NB model or FPKM/TPM type of values with tobit model.")
    } else if(r_data$norm_param$method %in% c("ERCC-RLM","Census")) {
        tags$p("Recommend using absolute transcript counts with NB model or raw counts with NB model.")
    } else if(r_data$norm_param$method == "none") {
        tags$p("Please make sure the specified model fits your data.")
    }
})

output$mn_data_dist_text_ui <- renderUI({
    if(input$mn_family_fun == "tobit") {
        fluidRow(
            column(4,
                   tags$b("Data type"),
                   tags$p("FPKM, TPM. ")
            ),
            column(8,
                   tags$b("Notes"),
                   tags$p("Tobits are truncated normal distributions. Using tobit() will tell Monocle to log-transform your data where appropriate.")
            )
        )
    } else if(input$mn_family_fun == "negbinomial") {
        fluidRow(
            column(4,
                   tags$b("Data type"),
                   tags$p("UMIs, Transcript counts from experiments with spike-ins or census normalization, raw read counts. ")
            ),
            column(8,
                   tags$b("Notes"),
                   tags$p("Using negbinomial() can be slow for very large datasets. In these cases, consider negbinomial.size().")
            )
        )
    } else if(input$mn_family_fun == "negbinomial.size") {
        fluidRow(
            column(4,
                   tags$b("Data type"),
                   tags$p("UMIs, Transcript counts from experiments with spike-ins, raw read counts. ")
            ),
            column(8,
                   tags$b("Notes"),
                   tags$p("Slightly less accurate for differential expression than negbinomial(), but much, much faster.")
            )
        )
    } else if(input$mn_family_fun == "gaussianff") {
        fluidRow(
            column(4,
                   tags$b("Data type"),
                   tags$p("log-transformed FPKM/TPMs, Ct values from single-cell qPCR.")
            ),
            column(8,
                   tags$b("Notes"),
                   tags$p("If you want to use Monocle on data you have already transformed to be normally distributed, you can use this function, though some Monocle features may not work well.")
            )
        )
    }
})



observeEvent(input$init_monocle, {
    req(input$mn_data)

    if(input$mn_data == "Raw count") {
        print("Raw")
        df <- r_data$raw
    } else {
        print("norm")
        df <- r_data$df
    }

    sample_meta <- r_data$meta
    rownames(sample_meta) <- r_data$meta[,1]
    pd <- new("AnnotatedDataFrame", data = sample_meta)
    feature_meta <- Biobase::fData(r_data$sceset)
    colnames(feature_meta)[1] <- "gene_short_name"
    fd <- new("AnnotatedDataFrame", data = feature_meta)

    cellset <- monocle::newCellDataSet(as.matrix(df),
                                       phenoData = pd,
                                       featureData = fd,
                                       lowerDetectionLimit = input$mn_min_expr,
                                       expressionFamily=do.call(input$mn_family_fun, list()))

    error_I <- 0
    if(input$mn_family_fun %in% c("negbinomial", "negbinomial.size")) {
        cellset <- estimateSizeFactors(cellset)
        tryCatch({
            cellset <- estimateDispersions(cellset)
        },
        error = function(e) {
            error_I <<- 1
        })
    } else {
        cellset <- estimateSizeFactors(cellset)
    }
    if(!error_I) {
        r_data$cellset <- cellset
        r_data$monocle_ok <- 1
        session$sendCustomMessage(type = "showalert", "Monocle CellDataSet created.")
    } else {
        session$sendCustomMessage(type = "showalert", "Failed to initiate monocle cellset, please try a different minimum expression level.")
        r_data$monocle_ok <- NULL
        r_data$cellset <- NULL
        return()
    }
})

output$monocle_ok <- renderUI({
    if(!is.null(r_data$cellset)){
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b("Monocle CellDataSet successfully initiated. You can now use other monocle modules."), style = "font-size:110%;")
    } else {
        return()
    }
})


# DE analysis

output$monocle_de_ui <- renderUI({
    req(r_data$cellset)
    infos <- list()
    if(is.null(r_data$meta) || ncol(r_data$meta) < 2) {
        infos[[length(infos) + 1]] <- tags$li("This module requires design information.")
    }

    if(is.null(r_data$cellset)) {
        infos[[length(infos) + 1]] <- tags$li("Please initiate monocle celldataset first.")
    }

    if(length(infos)) {
        return(infos)
    }


    list(
        tags$li("Please check the background R session while running this DE analysis. If you do not see progress, please restart a fresh R session and try again."),

        fluidRow(
            pivot_deGroupBy_UI("monocle", r_data$meta, width = 12, reduced = "yes", model = c("condition", "condition_batch", "custom"))
        ),

        uiOutput("perform_monocle_ui"),
        fluidRow(
            column(12,
                   hr(),
                   tags$div(tags$b("Monocle DE Test Result"), class = "param_setting_title"),
                   fluidRow(
                       column(6, numericInput_1("monocle_alpha", "Adjusted-P cutoff", value = 0.1, min = 0, max = 0.5, step = 0.001)),
                       column(6, checkboxInput("monocle_cuttbl", "Only show significant genes", value = T))
                   ),
                   DT::dataTableOutput("monocle_de_result_tbl"),
                   downloadButton("download_monocle_de_result", "Download", class = "btn btn-success"),
                   hr(),
                   uiOutput("monocle_de_summary")
            )
        )
    )
})

monocleModel <- callModule(pivot_deGroupBy, "monocle", meta = r_data$meta, reduced = "yes")

output$perform_monocle_ui <- renderUI({
    req(monocleModel())
    # examine if required monocle_sanity check are passed
    fluidRow(
        column(4, numericInput("monocle_de_ncore", "Number of cores:", value = 1, min = 1, step=1)),
        column(4, tags$br(),actionButton("perform_monocle_de", "Run DE", class = "btn-info"))
    )
})

observeEvent(input$perform_monocle_de, {
    req(r_data$cellset, monocleModel(), input$monocle_de_ncore)
    if(!is.null(r_data$monocle_results)) {
        r_data$monocle_results <- NULL
    }

    modelFull = Reduce(paste, deparse(monocleModel()$model$full))
    modelReduced = Reduce(paste, deparse(monocleModel()$model$reduced))

    withProgress(message = 'Processing...', value = 0.8, {
        tmp_tbl <- monocle::differentialGeneTest(r_data$cellset, fullModelFormulaStr = modelFull, reducedModelFormulaStr = modelReduced, verbose = T, cores = input$monocle_de_ncore)
        r_data$monocle_results <- tmp_tbl[order(tmp_tbl$qval),] %>% dplyr::select(status, family, pval, qval)
    })
})


output$monocle_de_result_tbl <- DT::renderDataTable({
    req(r_data$monocle_results)
    if(input$monocle_cuttbl) {
        tbl <- subset(r_data$monocle_results, qval <= input$monocle_alpha)
    } else {
        tbl <- r_data$monocle_results
    }
    if(nrow(tbl) == 0) return()
    DT::datatable(tbl, selection = 'single', options = list(scrollY = "400px", lengthMenu = c(20, 50, 100), order = list(list(2, 'asc')), orderClasses = T
    ))
})

output$download_monocle_de_result <- downloadHandler(
    filename = "monocle_de_result.csv",
    content = function(file) {
        if(input$monocle_cuttbl) {
            tbl <- subset(r_data$monocle_results, qval <= input$monocle_alpha)
        } else {
            tbl <- r_data$monocle_results
        }
        if(nrow(tbl) == 0) return()
        write.csv(tbl, file)
    }
)

output$monocle_de_summary <- renderUI({
    tags$li(paste0("Total number of significant genes: ", sum(r_data$monocle_results$qval <= input$monocle_alpha, na.rm = T), "."))
})


observe({
    req(r_data$monocle_results, !is.null(input$monocle_cuttbl))
    s = input$monocle_de_result_tbl_row_last_clicked
    if(input$monocle_cuttbl) {
        tbl <- subset(r_data$monocle_results, qval <= input$monocle_alpha)
    } else {
        tbl <- r_data$monocle_results
    }

    if (length(s)) {
        selected_gene <- rownames(tbl[s, , drop = FALSE])
    } else {
        return()
    }

    # Here use monocle normalized counts
    df <- as.data.frame(exprs(r_data$cellset)/pData(r_data$cellset)$Size_Factor)

    d <- as.data.frame(t(df[selected_gene,])) %>% tibble::rownames_to_column()
    colnames(d) <- c("sample", "expression_level")

    callModule(pivot_featurePlot, "monocle_gene_plt", meta = r_data$meta, df = d, gene = selected_gene)
})



