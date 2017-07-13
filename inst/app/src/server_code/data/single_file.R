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



################################# Single data file processor #####################################
###### loading module #####

tmpSingle <- reactiveValues()

observe({
    inFile<-callModule(pivot_fileInput, "single", return_df = F)
    tmpSingle$inFile <- inFile
    callModule(pivot_filePreview, "single_preview", inFile$df, height = "500px", search = T)
    isolate({
        if (is.null(inFile))
            return(NULL)
        if(!is.null(r_data$glb.raw)) {
            if(!identical(r_data$file_info$path, inFile$path)){ # New file
                r_data <- init_state(r_data)
                r_data <- clear_design(r_data)
            }
        }
    })
})


##### Single file data submission module #####
observeEvent(input$submit_single, {
    inFile <- tmpSingle$inFile
    if (is.null(inFile)){
        session$sendCustomMessage(type = "showalert", "Please specify your data input.")
        return()
    }
    df <- as.data.frame(inFile$df)

    if (ncol(df) < 3) {
        session$sendCustomMessage(type = "showalert", "Too few samples or the input format is incorrect!")
        return()
    }

    if(any(duplicated(df[,1]))){
        dup_ones <- df[,1][which(duplicated(df[,1]))]
        session$sendCustomMessage(type = "showalert", paste("Detect duplicated genes:", paste(dup_ones, collapse=", ")))
        return()
    }

    if(any(is.na(df))) {
        session$sendCustomMessage(type = "showalert", paste("Detect NA or Inf values."))
        return()
    }

    if(!is.null(r_data$glb.raw)) # This is not first time submission, then clean up previous session
    {
        r_data <- init_state(r_data)
        r_data <- clear_design(r_data)
    }


    # Convert tibble to data frame (for compatibility)
    firstcol <- as.character(df[,1])
    tmp_feature_name <- make.names(firstcol, unique = TRUE)
    sampleNm <- colnames(df)[-1]
    tmp_sample_name <- make.names(sampleNm, unique = TRUE)

    # Check if there are any difference between the new name and original names
    if(!identical(firstcol, tmp_feature_name)) {
        idx <- which(!firstcol%in%tmp_feature_name)
        cov_ones <- firstcol[idx]
        session$sendCustomMessage(type = "showalert", paste(length(idx), "feature names have been converted to syntactically valid names:",paste(cov_ones, collapse=", ")))
    }
    if(!identical(sampleNm, tmp_sample_name)) {
        idx <- which(!sampleNm%in%tmp_sample_name)
        cov_ones <- sampleNm[idx]
        session$sendCustomMessage(type = "showalert", paste(length(idx), "sample names have been converted to syntactically valid names:", paste(cov_ones, collapse=", ")))
    }

    df <- df[,-1,drop = F]
    rownames(df) <- tmp_feature_name
    colnames(df) <- tmp_sample_name

    # Reset data_ed
    r_data$glb.raw <- df
    r_data$file_info$type <- "single"
    r_data$file_info$path <- inFile$path
    r_data$file_info$name <- inFile$name

    withProgress(message = 'Processing', value = 0, {
        ### Feature exclusion
        # Exclude low count genes
        if(input$input_threshold_type == "mean")
            r_data$glb.raw <- r_data$glb.raw[rowMeans(r_data$glb.raw) > input$min_cnt_avg, ] # The default filter is 0.
        else if(input$input_threshold_type == "sum")
            r_data$glb.raw <- r_data$glb.raw[rowSums(r_data$glb.raw) > input$min_cnt_sum, ]
        else {
            session$sendCustomMessage(type = "showalert", "Unknown threshold type.")
            r_data <- init_state(r_data)
            r_data <- clear_design(r_data)
            return()
        }

        # Extract and exclude ERCC
        r_data$ercc <- r_data$glb.raw[grep("ERCC(-|[.])\\d{5}", rownames(r_data$glb.raw)),]
        if(grepl("ERCC", input$proc_method) && nrow(r_data$ercc) == 0) {
            session$sendCustomMessage(type = "showalert", "No ERCC detected.")
            r_data <- init_state(r_data)
            r_data <- clear_design(r_data)
            return()
        }
        if(input$exclude_ercc && nrow(r_data$ercc) > 0) {
            r_data$glb.raw<-r_data$glb.raw[-which(rownames(r_data$glb.raw) %in% rownames(r_data$ercc)), ]
        }

        r_data$sample_name <- colnames(r_data$glb.raw)
        r_data$feature_list <- rownames(r_data$glb.raw)
        incProgress(0.1, detail = "Perform data normalization...")
        error_I <- 0
        error_msg <- NULL

        tryCatch({
            result<-normalize_data(method = input$proc_method,
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
                                   raw = r_data$glb.raw, ercc = r_data$ercc)
        }, error = function(e){
            error_msg <<- e
            error_I <<- 1
        })
        if(error_I) {
            r_data <- init_state(r_data)
            r_data <- clear_design(r_data)
            session$sendCustomMessage(type = "showalert", paste0("Error detected: ", error_msg, "Please recheck format/try different normalization procedure."))
            return()
        }

        r_data$norm_param <- result$norm_param
        r_data$raw <- r_data$glb.raw
        r_data$df <- result$df
        incProgress(0.3, detail = "Adding metadata...")
        r_data$glb.meta <- data.frame(sample = r_data$sample_name)
        r_data <- init_meta(r_data)
        r_data <- update_history(r_data, NA, "Input", "Input count table", list(feature = r_data$feature_list, sample = r_data$sample_name, df = r_data$df), r_data$norm_param$method, r_data$norm_param)

        setProgress(1)
    })
})
