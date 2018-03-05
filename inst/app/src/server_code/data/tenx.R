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

################################# 10x data folder processor #####################################

output$tenx_package_check_ui <- renderUI({
    if(input$file_format != "tenx") return()
    if(!"cellrangerRkit" %in% installed.packages()) {
        return(
            column(8,
                   tags$p("You must install cellrangerRkit manually from here: https://support.10xgenomics.com/single-cell-gene-expression/software/pipelines/latest/rkit.")
            )
        )
    } else {
        return(
            list(
                column(4),
                column(4,
                       selectInput("tenx_symbol_id", "Use Gene Symbol/ID", choices = list("Gene Symbol" = "symbol", "Gene ID" = "id"))
                )
            )
        )
    }
})


shinyFiles::shinyDirChoose(input, 'tenx_folder', session=session, roots=c(home='~'))

output$tenx_folder_show <- renderPrint(
    if(is.null(input$tenx_folder)){
        "No folder is selected"
    } else {
        shinyFiles::parseDirPath(roots=c(home='~'), input$tenx_folder)
    }
)

observe({
    if(is.null(input$tenx_folder)) return()
    isolate({
        if(!is.null(r_data$glb.raw)) {
            if(!identical(r_data$file_info$path, input$tenx_folder$path)){
                r_data <- init_state(r_data)
                r_data <- clear_design(r_data)
            }
        }
    })
})

##### Single file data submission module #####
observeEvent(input$submit_tenx, {
    if(!"cellrangerRkit" %in% installed.packages()) {
        session$sendCustomMessage(type = "showalert", "Please install Cell Ranger R Kit first.")
        return()
    }
    if(is.null(input$tenx_folder)){
        session$sendCustomMessage(type = "showalert", "Please specify your 10x data folder.")
        return()
    }

    if(!is.null(r_data$glb.raw)) # This is not first time submission, then clean up previous session
    {
        r_data <- init_state(r_data)
        r_data <- clear_design(r_data)
    }

    r_data$file_info$type <- "tenx"
    cellranger_pipestance_path <- shinyFiles::parseDirPath(roots=c(home='~'), input$tenx_folder)
    r_data$file_info$path <- cellranger_pipestance_path

    withProgress(message = 'Processing', value = 0, {
        error_I <- 0
        tryCatch({
            gbm <- cellrangerRkit::load_cellranger_matrix(cellranger_pipestance_path)
            use_genes <- cellrangerRkit::get_nonzero_genes(gbm)
            r_data$glb.raw <- as.data.frame(as.matrix(Biobase::exprs(gbm[use_genes,])))
            #analysis_results <- load_cellranger_analysis_results(cellranger_pipestance_path)
            # filter unexpressed genes
            gbm_bcnorm <- cellrangerRkit::normalize_barcode_sums_to_median(gbm[use_genes,])
            r_data$df <- as.data.frame(as.matrix(Biobase::exprs(gbm_bcnorm)))
            # Make valid R names for the convenience of future analysis
            colnames(r_data$glb.raw) <- make.names(colnames(r_data$glb.raw))
            colnames(r_data$df) <-make.names(colnames(r_data$df))
            if(input$tenx_symbol_id == "symbol"){
                f_data <- fData(gbm_bcnorm)
                rownames(r_data$glb.raw) <- make.names(f_data$symbol, unique=T)
                rownames(r_data$df) <- rownames(r_data$glb.raw)
            }
        },error=function(e){
            error_I <<-1
            error_msg <<- e
        })
        if(error_I) {
            r_data <- init_state(r_data)
            r_data <- clear_design(r_data)
            session$sendCustomMessage(type = "showalert", paste0("Error detected: ", error_msg, "Please recheck."))
            return()
        }

        feature_tbl <- fData(gbm_bcnorm)

        r_data$sample_name <- colnames(r_data$glb.raw)
        r_data$feature_list <- rownames(r_data$glb.raw)

        np <- list(method = "normalize_barcode_sums_to_median")
        r_data$norm_param <- np
        r_data$raw <- r_data$glb.raw
        incProgress(0.3, detail = "Adding metadata...")
        r_data$glb.meta <- data.frame(sample = r_data$sample_name)
        r_data <- init_meta(r_data)
        r_data <- update_history(r_data, NA, "Input", "Input count table", list(feature = r_data$feature_list, sample = r_data$sample_name, df = r_data$df), r_data$norm_param$method, r_data$norm_param)

        setProgress(1)
    })
})



