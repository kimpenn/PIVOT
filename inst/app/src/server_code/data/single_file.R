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

observe({
    if(is.null(input$file_single)) return()
    isolate({
        inFile <- input$file_single
        if (is.null(inFile))
            return(NULL)
        if(!is.null(r_data$glb.raw)) {
            if(!identical(r_data$file_path, inFile)){ # New file
                r_data <- init_state(r_data)
                r_data <- clear_design(r_data)
            }
        }
    })
})
# This shows the data when it first loaded
cnt_tbl_loaded <- reactive({
    inFile <- input$file_single
    if (is.null(inFile))
        return(NULL)
    error_I <- 0
    withProgress(message = 'Loading the file', value = 0, {
        incProgress(0.3)
        tryCatch({
            if(input$row_ct == "automatic")
                return(read.table(inFile$datapath, header=input$header_ct, sep=input$sep_ct, quote=input$quote_ct))
            else if(input$row_ct == "firstcol")
                return(read.table(inFile$datapath, header=input$header_ct, sep=input$sep_ct, quote=input$quote_ct, row.names = 1))
            else
                return(read.table(inFile$datapath, header=input$header_ct, sep=input$sep_ct, quote=input$quote_ct, row.names = NULL))
        },
        error = function(e){
            error_I <<- 1
        })
        if(error_I) {
            session$sendCustomMessage(type = "showalert", "File format not recogonized.")
            return()
        }
    })

})

output$cnt_tbl_original <- DT::renderDataTable({
    if(is.null(cnt_tbl_loaded())) return()
    validate(
      need(!is.null(cnt_tbl_loaded()) && ncol(cnt_tbl_loaded()) > 1, "Please correct your input format. Try options on the left panel until you see your data here."),
      errorClass = "myErrorClass1"
    )
    DT::datatable(cnt_tbl_loaded(), options = list(scrollX = TRUE, scrollY = "500px", lengthMenu = c(20, 50, 100)))
})

# Data in editting preview

output$data_inprocess <- DT::renderDataTable({
    if(is.null(r_data$df)) return ()
    DT::datatable(r_data$df, options = list(scrollX = TRUE, scrollY = "500px", lengthMenu = c(20, 50, 100)))
})


##### Single file data submission module #####
observeEvent(input$submit_single, {
    if (is.null(cnt_tbl_loaded())){
        session$sendCustomMessage(type = "showalert", "Give me something!")
        return()
    }
    if (ncol(cnt_tbl_loaded()) < 2) {
        session$sendCustomMessage(type = "showalert", "Too few samples or the input format is incorrect!")
        return()
    }

    if(!is.null(r_data$glb.raw)) # This is not first time submission, then clean up previous session
    {
        r_data <- init_state(r_data)
        r_data <- clear_design(r_data)
    }

    # Reset data_ed
    r_data$glb.raw <- cnt_tbl_loaded()
    r_data$input_type <- "single"
    r_data$file_path <- input$file_single
    # Make sure the names are good
    tmp_sample_name <- make.names(colnames(r_data$glb.raw), unique = TRUE)
    colnames(r_data$glb.raw) <- tmp_sample_name
    tmp_feature_name <- make.names(rownames(r_data$glb.raw), unique = TRUE)
    rownames(r_data$glb.raw) <- tmp_feature_name

    # Make sure the dataframe do not contain NAs
    if(sum(is.na(r_data$glb.raw))) {
        session$sendCustomMessage(type = "showalert", "NA value was found... Please check your input file again!")
        return()
    }

    withProgress(message = 'Processing', value = 0, {
        incProgress(0.3, detail = "Getting feature list...")
        # Filter larger than threshold features
        if(input$input_threshold_type == "mean")
            r_data$glb.raw <- r_data$glb.raw[rowMeans(r_data$glb.raw) > input$min_cnt_avg, ] # The default filter is 0.
        else if(input$input_threshold_type == "sum")
            r_data$glb.raw <- r_data$glb.raw[rowSums(r_data$glb.raw) > input$min_cnt_sum, ]
        else {
            session$sendCustomMessage(type = "showalert", "Unknown threshold type.")
            return()
        }
        r_data$sample_name <- colnames(r_data$glb.raw)
        r_data$feature_list <- rownames(r_data$glb.raw)

        incProgress(0.3, detail = "Perform data normalization...")

        error_I <- 0
        tryCatch({
            result<-normalize_data(method = input$proc_method,
                                   params = list(gene_length = r_data$gene_len, deseq_threshold = input$deseq_threshold/100),
                                   raw = r_data$glb.raw)
        }, error = function(e){
            error_I <<- 1
        })
        if(error_I) {
            r_data <- init_state(r_data)
            r_data <- clear_design(r_data)
            session$sendCustomMessage(type = "showalert", "Normalization failed! Please recheck.")
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
