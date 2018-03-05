


#' PIVOT Data Control
#' @description
#' Set all stored values to NULL
#'
#' @export
init_state <- function(r_data) {
    ######### Base input reactive values ########

    r_data$file_info <- NULL

    r_data$history <- NULL # Data manipulation history (with all parameters)
    r_data$his_num <- NULL
    r_data$his_tbl <- NULL # Data manipulation history (as table)

    r_data$his_nodes <- NULL
    r_data$his_edges <- NULL

    r_data$reg_history <- NULL # Analysis register history (with parameters and links to reports)
    r_data$reg_tbl <- NULL # Analysis register table

    r_data$glb.raw <- NULL # This raw counts are combined or loaded raw counts (not filtered)
    r_data$raw <- NULL # This raw is different from data_ed$raw, this one is filtered raw cnts.
    r_data$df <- NULL # This is the filtered DESeq normalized data
    r_data$ercc <- NULL # Isolate ERCC raw counts
    r_data$ercc_wt_mol <- NULL # Include ERCC molecule and detection probability

    r_data$glb.meta <- NULL # META table
    r_data$meta <- NULL # META table of the activated dataset
    r_data$category <- NULL
    r_data$design_pv <- NULL # Temporary design input preview table

    r_data$feature_list <- NULL # This contains the filtered feature list used for analysis, every time the user choose a different feature set, the analyzer will grab that set of data using this key
    r_data$sample_name <- NULL # This has same value as sample_key once analyze btn is pressed.

    r_data$sceset <- NULL
    r_data$sample_stats <- NULL

    r_data$norm_param <- NULL

    r_data <- clear_results(r_data)
    return(r_data)
}

#' PIVOT Data Control
#'
#' @description
#' Clean all analysis results
#'
#' @export
clear_results <-function(r_data) {
    r_data$sceset <- NULL
    r_data$dds <- NULL
    r_data$deseq_results <- NULL
    r_data$deseq_params <- NULL
    r_data$deseq_group <- NULL

    r_data$edgeR <- NULL
    r_data$edgeR_fit <- NULL
    r_data$edgeR_test <- NULL
    r_data$edgeR_params <- NULL
    r_data$edgeR_group <- NULL
    r_data$edgeR_results <- NULL

    r_data$scde_ifm<-NULL
    r_data$scde_invalid <- NULL
    r_data$scde_ediff <- NULL
    r_data$scde_prior <- NULL
    r_data$scde_group <- NULL
    r_data$scde_sample <- NULL
    r_data$scde_batch <- NULL
    r_data$scde_gene <- NULL
    r_data$scde_ddo <- NULL
    r_data$scde_rw <- NULL
    r_data$scde_mrw <- NULL
    r_data$scde_results <- NULL

    r_data$kmeans <- NULL
    r_data$hc <- NULL
    r_data$community <- NULL

    r_data$pca <- NULL
    r_data$pca_var <- NULL
    r_data$pca_var_exp <- NULL

    r_data$plda <- NULL
    r_data$plda_flist <- NULL

    r_data$mds <- NULL
    r_data$nds <- NULL
    r_data$tsne <- NULL
    r_data$dfm <- NULL
    r_data$dpt <- NULL

    r_data$cooks <- NULL

    r_data$cfm <- NULL

    r_data$cor_ft_gene <- NULL
    r_data$coe_ft_target <- NULL
    r_data$coe_ft_tbl <- NULL

    r_data$mww_results <- NULL
    r_data$mww_group<- NULL
    r_data$mww_gene <- NULL

    r_data$cellset <- NULL # Monocle data object

    r_data$monocle_ok <- NULL
    r_data$monocle_results <- NULL
    r_data$monocle_gene_for_clust <- NULL
    r_data$monocle_gene_clusters <- NULL

    r_data$stringdb_link <- NULL
    r_data$stringdb_genetbl <- NULL
    r_data$string_meta <- NULL

    r_data$input_tfs <- NULL
    r_data$tf_list <- NULL
    r_data$tf_de_tbl <- NULL
    r_data$tf_de_tbl_vis <- NULL
    r_data$tf_tbl_net <- NULL
    r_data$tf_tbl1 <- NULL
    r_data$tf_tbl2 <- NULL
    r_data$tfs <- NULL
    r_data$tf_nets <- NULL
    r_data$tf_g1 <- NULL
    r_data$tf_g2 <- NULL
    r_data$tf_neighbor_order <- NULL

    r_data$gsea <- NULL

    r_data$reg_g <- NULL

    r_data$venn_list <- NULL
    return(r_data)
}

#' Get feature data
#'
#' @description
#' fData from SummarizedExperiment
#' @import SummarizedExperiment
#' @export
fInfo <- function(x){
    as.data.frame(SummarizedExperiment::rowData(x))
}

#' Get feature data
#'
#' @description
#' fData from Biobase
#' @import Biobase
#' @export
fData <- Biobase::fData

#' Get pheno data
#'
#' @description
#' pData from Biobase
#' @import Biobase
#' @export
pData <- Biobase::pData

#' PIVOT Data Control
#'
#' @description
#' Remove design information
#'
#' @export
clear_design <- function(r_data) {
    callModule(pivot_fileInput, "design", reset = TRUE)
    callModule(pivot_filePreview, "design_pv", NULL)

    if(!is.null(r_data$glb.raw)) {
        r_data$design_pv <- data.frame(Sample = colnames(r_data$glb.raw))
        r_data$glb.meta <- data.frame(sample = colnames(r_data$glb.raw))
        r_data$meta <- r_data$glb.meta[match(r_data$sample_name, r_data$glb.meta[,1]),, drop = F]
        r_data$category <- colnames(r_data$meta)
    } else {
        r_data$design_pv <- NULL
        r_data$glb.meta <- NULL
        r_data$meta <- NULL
        r_data$category <- NULL
    }
    r_data <- clear_results(r_data)
    return(r_data)
}

#' PIVOT Data Control
#'
#' @description
#' Meta data generation
#'
#' @export
init_meta <- function(r_data) {

    if(is.null(r_data$df)) return()

    # Generate sample statistics
    r_data$sample_stats <- data.frame(total_normalized_counts = colSums(r_data$df))

    if(r_data$norm_param$method != "None") {
        r_data$sample_stats$total_raw_reads <- colSums(r_data$raw)
        if(r_data$norm_param$method %in% c("DESeq", "Modified_DESeq") ) {
            r_data$sample_stats$deseq_size_factor <- r_data$norm_param$sizeFactor[r_data$sample_name,,drop = F]$size_factor
        } else if(r_data$norm_param$method %in% c("ERCC-RLM", "Census")) {
            r_data$sample_stats$t_estimate = r_data$norm_param$t_estimate[r_data$sample_name]
            r_data$sample_stats$expected_total_mRNAs = r_data$norm_param$expected_total_mRNAs[r_data$sample_name]
        }
    }

    r_data$sample_stats$num_genes_expressed <- colSums(r_data$raw > 0)

    # Design information
    r_data$meta <- r_data$glb.meta[match(r_data$sample_name, r_data$glb.meta[,1]),, drop = F]
    r_data$category <- colnames(r_data$meta)

    # Initiate sceset
    pd <- cbind(r_data$sample_stats, r_data$meta)
    fd <- data.frame(gene = r_data$feature_list, cap_name = toupper(r_data$feature_list))
    rownames(fd) <-rownames(r_data$df)
    r_data$sceset<-SingleCellExperiment::SingleCellExperiment(assays = list(counts = as.matrix(r_data$raw),
                                                                            normcounts = as.matrix(r_data$df),
                                                                            logcounts = as.matrix(log2(r_data$df + 1))),
                                                              colData = pd, rowData = fd)
    rowData(r_data$sceset)$feature_symbol <- rownames(r_data$df)
    SingleCellExperiment::isSpike(r_data$sceset, "ERCC") <- grepl("ERCC", SummarizedExperiment::rowData(r_data$sceset)$feature_symbol)
    r_data$sceset <- scater::calculateQCMetrics(r_data$sceset)
    return(r_data)
}

#' PIVOT Data Control
#'
#' @description
#' @import S4Vectors
#'
#' @export
metadata <- S4Vectors::metadata

#' PIVOT Data Control
#'
#' @description
#' Switch to selected dataset via data map module
#'
#' @export
switch_to_dataset <- function(r_data, s) {
    r_data <- clear_results(r_data)

    # Check if renormalization was performed
    parent <- search_subset_node(r_data$his_tbl, node = r_data$his_tbl$name[s])
    p_s <- which(r_data$his_tbl$name == parent)
    df <- r_data$history[[p_s]]$lists$df

    r_data$feature_list <- r_data$history[[s]]$lists[[1]]
    r_data$sample_name <- r_data$history[[s]]$lists[[2]]
    r_data$raw <- r_data$glb.raw[r_data$feature_list, r_data$sample_name]
    r_data$df <- df[r_data$feature_list, r_data$sample_name]
    r_data$norm_param <- r_data$history[[s]]$norm_param


    #r_data$df <- r_data$history[[s]]$lists[[3]] # This might take some memory, but for now it's the best solution

    is_activated <- rep("N", nrow(r_data$his_tbl))
    is_activated[s] <- "Y"
    r_data$his_tbl$is_activated <- is_activated

    neList<- generate_data_node(his_tbl = r_data$his_tbl, reg_tbl = r_data$reg_tbl)
    r_data$his_nodes <- neList$nodes
    r_data$his_edges <- neList$edges
    # Reset metadata
    r_data <- init_meta(r_data)
    return(r_data)
}


#' PIVOT Data Control
#'
#' @description
#' Create subset by filtering or subsetting
#'
#' @export
create_subset <- function(r_data, input, flist, slist, keep_filter = F, renorm = F, erccStds = NULL, actionType = c("Filter", "Subset"), actionText = "") {
    if(actionType == "Subset") {
        if(renorm) {
            error_I <- 0
            error_msg <- NULL
            tryCatch({
                result <- normalize_data(method = input$proc_method,
                                         params = list(
                                             gene_length = r_data$gene_len,
                                             control_gene = r_data$control_gene,
                                             ruvg_k = input$ruvg_k,
                                             ruvg_round = input$ruvg_round,
                                             deseq_threshold = input$deseq_threshold/100,
                                             expected_capture_rate = input$expected_capture_rate,
                                             ercc_added = input$norm_ercc_added,
                                             ercc_dilution = input$norm_ercc_ratio,
                                             ercc_mix_type = input$norm_ercc_mix_type,
                                             ercc_detection_threshold = input$ercc_detection_threshold,
                                             ercc_std = erccStds
                                         ),
                                         raw = r_data$glb.raw[flist,slist], ercc = r_data$ercc[, slist, drop = F])
            }, error = function(e){
                error_msg <<- e
                error_I <<- 1
            })

            if(error_I) {
                stop(paste0("Normalization failed with the following error message: ", error_msg))
            } else {
                r_data$df <- result$df
                r_data$norm_param <- result$norm_param
                norm_method <- paste(r_data$norm_param$method, "(renormalize)")
                actionText <- paste(actionText, "(with renormalization)")
            }
        } else { # Use original normalization parameters from input dataset
            r_data$df <- r_data$history[[1]]$lists$df[flist,slist]
            r_data$norm_param <- r_data$history[[1]]$norm_param
            norm_method <- r_data$history[[1]]$norm_param$method
        }
    } else if(actionType == "Filter") {
        norm_method <- r_data$norm_param$method
        if(keep_filter) { # keep filter is true
            r_data$df <- r_data$df[flist,slist]
        } else { # Clear previous filtering result, use parent sample subset
            # Check whether subset has been renomalized
            s <- which(r_data$his_tbl$is_activated == "Y")
            p_n <- search_subset_node(r_data$his_tbl, node = r_data$his_tbl$name[s])
            p_s <- which(r_data$his_tbl$name == p_n)
            p_df <- r_data$history[[p_s]]$lists$df
            r_data$df <- p_df[flist,slist]
        }
    } else { # Should not reach
        stop("actiontype required.")
    }

    r_data$raw <- r_data$glb.raw[flist,slist]
    r_data$feature_list <- flist
    r_data$sample_name <- slist

    r_data <- init_meta(r_data)

    if(actionType == "Filter") {
        if(keep_filter) {
            parent = r_data$his_tbl$name[which(r_data$his_tbl$is_activated == "Y")]
        } else { # The data is from (subset of) input data
            parent <- search_subset_node(r_data$his_tbl, node = r_data$his_tbl$name[which(r_data$his_tbl$is_activated == "Y")])
        }
    } else if(actionType == "Subset") {
        parent = r_data$history[[1]]$name
    } else { # Should not reach
        stop("Error")
    }

    if(actionType == "Subset") {
        save_df <- r_data$df
    } else {
        save_df <- NULL
    }

    r_data <- update_history(r_data, parent, actionType, actionText, lists = list(feature = r_data$feature_list, sample = r_data$sample_name, df = save_df), norm_method, r_data$norm_param)
    return(r_data)
}

