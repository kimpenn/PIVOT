# PIVOT: Platform for Interactive analysis and Visualization Of Transcriptomics data
# Copyright (c) 2015-2018, Qin Zhu and Junhyong Kim, University of Pennsylvania.
# All Rights Reserved.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

################################# 10x data folder processor #####################################

GeneBCMatrix <- setClass("GeneBCMatrix",
                         contains="ExpressionSet",
                         slots=list(
                             barcode_filtered="logical",
                             summary="list",
                             subsampled="logical",
                             molecule_info="data.table"
                         ),
                         prototype = prototype( new("VersionedBiobase",
                                                    versions = c( classVersion("ExpressionSet"), GeneBCMatrix = "1.0.0" ) ))
)

newGeneBCMatrix <- function(mat, fd, pd, template=NULL) {
    res <- new("GeneBCMatrix", assayData=assayDataNew( "environment", exprs=mat ), featureData=new("AnnotatedDataFrame", data=fd), phenoData=new("AnnotatedDataFrame", data=pd), molecule_info=data.table())
    
    if (!is.null(template)) {
        for (slot_name in setdiff(slotNames(res), c('assayData', 'featureData', 'phenoData'))) {
            slot(res, slot_name) <- slot(template, slot_name)
        }
    }
    
    res
}


output$tenx_package_check_ui <- renderUI({
    if(input$file_format != "tenx") return()
    return(
        list(
            column(4),
            column(4,
                   selectInput("tenx_symbol_id", "Use Gene Symbol/ID", choices = list("Gene Symbol" = "symbol", "Gene ID" = "id"))
            )
        )
    )
})


#shinyFiles::shinyFileChoose(input, 'tenx_folder', session=session, roots=c(home='~'))




##### Single file data submission module #####

parse10xFiles = reactive({
    
    #load matrix file
    tryCatch({
        mat <- readMM(gzfile(input$matrix_file$datapath))
        print(mat)
        cat('Loaded matrix information\n')
    }, error = function(e){
        stop(sprintf("Could not load matrix file \n\t%s\n", input$matrix_file$name))
    })
    
    #load gene file
    tryCatch({
        gene_info <- read.delim(gzfile(input$gene_file$datapath), stringsAsFactors=FALSE, sep="\t", header=FALSE)
        if (dim(mat)[1] != length(gene_info[,1])) {
            stop(sprintf("Mismatch dimension between gene file: \n\t %s\n and matrix file: \n\t %s\n", input$gene_file$name,input$matrix_file$name))
        } else {
            rownames(mat) <- gene_info[,1]
            gene_symbols <- gene_info
            row.names(gene_symbols) = gene_symbols[, 1]
            colnames(gene_symbols) = c("id", "symbol")
            cat('Loaded gene information\n')
        }
    }, error = function(e){
        stop(sprintf("Could not load gene file: \n\t %s\n",input$gene_file$name))
    })
    
    #load barcode file
    tryCatch({
        barcodes <- read.delim(gzfile(input$barcode_file$datapath), stringsAsFactors=FALSE, sep="\t", header=FALSE)
        if (dim(mat)[2] != length(barcodes[,1])) {
            stop(sprintf("Mismatch dimension between barcode file: \n\t %s\n and matrix file: \n\t %s\n", input$barcode_file$name,input$matrix_file$name))
        } else {
            colnames(mat) <- barcodes[,1]
            pd = data.frame(id=barcodes[,1], row.names=barcodes[,1])
            colnames(pd) = c("barcode")
            cat('Loaded barcode information\n')
        }
    }, error = function(e){
        stop(sprintf("Could not load barcode file: \n\t %s\n", input$barcode_file$name))
    })
    
    #load matrix file
    tryCatch({
        summary <- read.csv(input$summary_file$datapath, as.is=TRUE, header=TRUE)
        res@summary <- summary
        cat('Loaded summary information\n')
    }, error = function(e){
        cat(sprintf("Could not find summary csv: \n\t %s.\nThis file is only necessary if you are performing depth-normalization (calling the equalize_gbms function) in R.\nIf this pipestance was produced by `cellranger aggr` with the default parameters, depth-normalization in R (via equalize_gbms) is not necessary.\n",input$summary_file$name))
    })
    
    # Build GeneBCMatrix object
    res <- newGeneBCMatrix(mat=mat, fd=gene_symbols, pd=pd)
    res@subsampled <- FALSE
    res@barcode_filtered <- TRUE
    return(res)
})

observeEvent(input$submit_tenx, {
    # if(!"cellrangerRkit" %in% installed.packages()) {
    #     session$sendCustomMessage(type = "showalert", "Please install Cell Ranger R Kit first.")
    #     return()
    # }
    if(is.null(input$matrix_file) | is.null(input$gene_file) | is.null(input$barcodes_file)){
        session$sendCustomMessage(type = "showalert", "Please specify all your 10x data files")
        return()
    }
    
    if (is.null(input$summary_file)) {
        warning("No summary file provided. Some funcions may be disabled without the metrics in summary csv.\n")
    }

    if(!is.null(r_data$glb.raw)) # This is not first time submission, then clean up previous session
    {
        r_data <- init_state(r_data)
        r_data <- clear_design(r_data)
    }

    # r_data$file_info$type <- "tenx"
    # cellranger_pipestance_path <- shinyFiles::parseDirPath(roots=c(home='~'), input$tenx_folder)
    # r_data$file_info$path <- cellranger_pipestance_path

    withProgress(message = 'Processing', value = 0, {
        error_I <- 0
        tryCatch({
            # gbm <- cellrangerRkit::load_cellranger_matrix(cellranger_pipestance_path)
            gbm = parse10xFiles()
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



