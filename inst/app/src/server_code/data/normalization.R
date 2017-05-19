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


# normalization method
output$proc_method_ui <- renderUI({
    if(input$file_format %in% c("dir", "single")) {
        selectInput("proc_method", label = "Normalization Method",
                    choices = list("DESeq" = "DESeq",
                                   "Modified DESeq" = "Modified_DESeq",
                                   "Trimmed Mean of M-values (TMM)" = "TMM",
                                   "Trimmed Mean of M-values (TMM) - RPKM" = "TMM-RPKM",
                                   "Upperquartile" = "upperquartile",
                                   "Upperquartile-RPKM" = "upperquartile-RPKM",
                                   "Counts per Million (CPM)" = "CPM",
                                   "Reads per Kilobase per Million (RPKM)" = "RPKM",
                                   "Transcripts per Million (TPM)" = "TPM",
                                   "ERCC normalization with robust linear regression" = "ERCC-RLM",
                                   "Census normalization" = "Census",
                                   "None" = "none"),
                    selected = "DESeq")
    } else {
        return()
    }
})

output$norm_text_ui <- renderUI({
    if(input$file_format == "state") {
        return()
    }
    if(is.null(input$proc_method)) return()
    if(input$proc_method == "DESeq") {
        tags$p("Input must be raw read counts. The data will be normalized using the DESeq2 package. ")
    } else if(input$proc_method == "Modified_DESeq") {
        tags$p("Input must be raw read counts. The original DESeq2 uses genes expressed in ALL cells to calculate size factors.
               This modified method uses more genes (genes expressed in x% samples) to estimate size factors,
               which is more suitable for sparse expression matrix. ")
    } else if(input$proc_method == "TMM") {
        tags$p("Input must be raw read counts. Data is transformed to TMM normalized counts per million (CPM). ")
    } else if(input$proc_method == "TMM-RPKM") {
        tags$p("Requires raw read counts and gene lengths. Data is transformed to TMM normalized reads per kilobase per million (RPKM).")
    } else if(input$proc_method == "upperquartile") {
        tags$p("Input must be raw read counts. Data is transformed to upperquartile normalized counts per million (CPM). ")
    } else if(input$proc_method == "upperquartile-RPKM") {
        tags$p("Requires raw read counts and gene lengths. Data is transformed to upperquartile normalized reads per kilobase per million (RPKM). ")
    } else if(input$proc_method == "CPM") {
        tags$p("Requires raw read counts. Data is transformed to counts per million (CPM). ")
    } else if(input$proc_method == "RPKM") {
        tags$p("Requires raw read counts and gene lengths. Data is transformed to reads per kilobase per million (RPKM). ")
    } else if(input$proc_method == "TPM") {
        tags$p("Requires raw read counts and gene lengths. Data is transformed to transcripts per million (TPM). ")
    } else if(input$proc_method == "ERCC-RLM") {
        tags$p("Input: Relative expression values such as RPKM/TPM containing ERCC (in every sample). Data is transformed to mRNAs per cell (RPC, absolute counts). ")
    } else if(input$proc_method == "Census") {
        tags$p("For single-cell RNA Seq data. Suggests relative expression value such as RPKM/TPM. Data is transformed to mRNAs per cell (RPC, absolute counts). ")
    } else if(input$proc_method == "none") {
        tags$p("Input does NOT need to be raw counts, can be any data that are suitable for direct analysis (PIVOT will assume the data has already been normalized by the user).")
    }
})


output$norm_params_ui <- renderUI({
    if(is.null(input$proc_method)) return()

    if(input$proc_method == "Modified_DESeq") {
        list(
            sliderInput(inputId = "deseq_threshold",
                        label = tags$span(
                            "Include genes expressed in at least",
                            shinyBS::tipify(
                                bsButton("deseq_threshold_tooltip", label = NULL, icon = icon("question-circle"), style = "link", size = "extra-small"),
                                title = "100% is exact DESeq, choose lower threshold to include more genes for normalization.",
                                options = list(container = "body")
                            )
                        ),
                        min = 0, max = 100, value = 70, step = 1, round = T,
                        post = "% of the samples")
        )
    } else if(input$proc_method == "ERCC-RLM") {
        list(
            fluidRow(
                column(3, numericInput("norm_ercc_added", label = "Amount of ERCC added (µL)", value = 0.9, min = 0, max = 100, step = .1)),
                column(3, numericInput("norm_ercc_ratio", label = "ERCC dilution, 1 : ", value = 4000000, min = 0, max = 10e9, step = 100)),
                column(3, selectInput("norm_ercc_mix_type", label = "Mix 1/2", choices = c("Mix 1" = 1, "Mix 2" = 2), selected = 1)),
                column(3,
                       shinyBS::tipify(
                           numericInput("ercc_detection_threshold", label = "Detection Threshold : ", value = 800, min = 0.01430512, max = 7500),
                           title = "the lowest concentration of spikein transcript considered for the regression. Default is 800 which will ensure (almost) all included spike transcripts expressed in all the cells.",
                           options = list(container = "body")
                       )
                )
            )
        )
    }

})

# Gene lengths normalization ui
# TOTRY: This better be written into r_data for subset normalization.

gene_length <- reactiveValues()
gene_length$tbl <- NULL

output$gene_length_ui <- renderUI({
    if(!is.null(input$proc_method)) {
        if(grepl("RPKM", input$proc_method) || input$proc_method == "TPM") {
            list(
                tags$br(),
                actionButton("gene_length_custom_btn", label = "Upload Lengths", class = "btn-info")
            )
        } else if(input$proc_method %in% c("Census", "ERCC-RLM")) {
            shinyBS::tipify(
                numericInput("expected_capture_rate", "RNA capture rate", min = 0.01, max = 1, step = 0.01, value = 0.25),
                title = "the expected fraction of RNA molecules in the lysate that will be captured as cDNAs during reverse transcription",
                placement = "right", options = list(container = "body")
            )
        }

    } else {
        return()
    }
})

observeEvent(input$gene_length_custom_btn, {
    content <- list(
        fluidRow(
            column(6,
                   wellPanel(
                       fileInput('gene_length_list_file', label = NULL, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                       checkboxInput('gene_length_header', 'Header', value = F),
                       radioButtons('gene_length_sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), selected = '\t'),
                       radioButtons('gene_length_quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"'),
                       actionButton("gene_length_list_submit", "Submit List", class = "btn btn-info")
                   ),
                   uiOutput("gene_length_png_ui")
            ),
            column(6,
                   tags$p("[gene name in 1st column, lengths(#bases) in 2nd column]"),
                   DT::dataTableOutput('gene_length_tbl_show')
            )
        )
    )
    showModal(modalDialog(
        title = "Upload a Gene Length Table",
        size = "l",
        content,
        easyClose = TRUE
    ))
})


observe({
    inFile <- input$gene_length_list_file
    error_I <- 0
    if (!is.null(inFile)) {
        tryCatch({
            gene_length$tbl <- read.table(inFile$datapath, header=input$gene_length_header, sep=input$gene_length_sep, quote=input$gene_length_quote)
        },
        error = function(e){
            error_I <<- 1
        })
    }
    if(error_I) {
        session$sendCustomMessage(type = "showalert", "Unsupported file format.")
        return()
    }
})

output$gene_length_tbl_show <- DT::renderDataTable({
    if(is.null(gene_length$tbl)) return()
    DT::datatable(gene_length$tbl, options = list(scrollY = "350px", searching = TRUE))
})

output$gene_length_png_ui <- renderUI({
    if(is.null(r_data$gene_len)) return()
    list(
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b(paste(length(r_data$gene_len), "gene lengths have been successfully uploaded.")), style = "font-size:110%;")
    )
})

observeEvent(input$gene_length_list_submit, {
    error_I <- 0
    # First process the marker feature file and get the list
    if (is.null(gene_length$tbl) || nrow(gene_length$tbl) == 0)
    {
        session$sendCustomMessage(type = "showalert", "Give me something!")
        return()
    }
    if(sum(duplicated(toupper(gene_length$tbl[,1])))) {
        session$sendCustomMessage(type = "showalert", "Duplicated names found. Please recheck your file.")
        return()
    }
    if(ncol(gene_length$tbl) != 2) {
        session$sendCustomMessage(type = "showalert", "Please make sure your file only has 2 columns.")
        return()
    }
    tryCatch({
        gene_name <- as.character(make.names(gene_length$tbl[,1]), unique = T)
        gene_len <- as.numeric(as.character(gene_length$tbl[,2]))
        },
        error = function(e){
            error_I <<- 1
        }
    )
    if(error_I) {
        session$sendCustomMessage(type = "showalert", "Something went wrong, please recheck your file.")
        return()
    }

    if(any(is.na(gene_len))) {
        session$sendCustomMessage(type = "showalert", "Some gene lengths are not numeric values, please recheck.")
        return()
    }
    names(gene_len) <- gene_name
    r_data$gene_len <- gene_len
})


# Switch to normalization details
output$norm_details_ui <- renderUI({
    if(is.null(r_data$norm_param)) return()
    actionButton("norm_details", label = "Normalization Details", class = "btn-warning")
})

observeEvent(input$norm_details, {
    if(r_data$norm_param$method == "Modified_DESeq") {
        gene_info <- list(
            tags$li(paste("Number of genes included in the normalization:", r_data$norm_param$numGene)),
            tags$li(paste("These genes are expressed in at least", r_data$norm_param$threshold * 100, "% of the samples."))
        )
    } else if (r_data$norm_param$method %in% c("TMM", "upperquartile")) {
        gene_info <- list(
            tags$li("lib.size: total raw counts of each sample, used as the original library size."),
            tags$li(paste(paste("norm.factors: scaling factors computed using the", r_data$norm_param$method), "method.")),
            tags$li("effective.lib.size: product of the original library size and the scaling factor, in millions of reads.")
        )
    } else if(grepl("ERCC", r_data$norm_param$method))  {
        gene_info <- list(
            tags$li(paste("ERCC added volumn:", r_data$norm_param$ercc_added)),
            tags$li(paste("ERCC dilution: 1 :", r_data$norm_param$ercc_dilution)),
            tags$li(paste("ERCC mixture type:", r_data$norm_param$ercc_mix_type)),
            tags$li(paste("ERCC detection threshold:", r_data$norm_param$ercc_detection_threshold))
        )
    } else {
        gene_info <- NULL
    }

    content <- list(
        tags$li(paste("Normalization method of the current dataset:"), r_data$norm_param$method),
        gene_info,
        tags$br(),
        DT::dataTableOutput("norm_param_tbl"),
        tags$p(),
        downloadButton("download_norm_param", "Download", class = "btn-success btn_leftAlign")
    )

    showModal(modalDialog(
        title = "Normalization method of the current dataset",
        size = "m",
        content,
        easyClose = TRUE
    ))
})

output$norm_param_tbl <- DT::renderDataTable({
    if(is.null(r_data$norm_param)) return()
    if(!is.null(r_data$norm_param$sizeFactor)) {
        DT::datatable(data.frame(size_factor = r_data$norm_param$sizeFactor), options = list(scrollY = "400px", paging = F, searching = F))
    } else if(r_data$norm_param$method == "Census") {
        DT::datatable(data.frame(t_estimate = r_data$norm_param$t_estimate, expected_total_mRNAs = r_data$norm_param$expected_total_mRNAs), options = list(scrollY = "400px", paging = F, searching = F))
    } else if(r_data$norm_param$method == "ERCC-RLM") {
        DT::datatable(r_data$norm_param$k_b_solution, options = list(scrollY = "400px", paging = F, searching = F))
    }
})

output$download_norm_param <- downloadHandler(
    filename = function() { paste0("norm_info-",r_data$norm_param$method,".csv") },
    content = function(file) {
        if(!is.null(r_data$norm_param$sizeFactor)) {
            tbl<-data.frame(size_factor = r_data$norm_param$sizeFactor)
        } else if(r_data$norm_param$method == "Census") {
            tbl<-data.frame(t_estimate = r_data$norm_param$t_estimate, expected_total_mRNAs = r_data$norm_param$expected_total_mRNAs)
        } else if(r_data$norm_param$method == "ERCC-RLM") {
            tbl <- r_data$norm_param$k_b_solution
        }
        write.csv(tbl, file)
    }
)






