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


############################# Folder and data file selection handler ###########################

shinyFiles::shinyDirChoose(input, 'data_folder', session=session, roots=c(home='~'))

output$data_folder_show <- renderPrint(
    if(is.null(input$data_folder))
        "No folder is selected"
    else
        shinyFiles::parseDirPath(roots=c(home='~'), input$data_folder)
)

observe({
    if(is.null(input$data_folder)) return()
    isolate({
        if(!is.null(r_data$glb.raw)) {
            if(!identical(r_data$file_info$path, input$data_folder$path)){
                r_data <- init_state(r_data)
                r_data <- clear_design(r_data)
            }
        }
    })
})

output$select_data <- renderUI({
    if(is.null(input$data_folder)) return()
    datadir <- shinyFiles::parseDirPath(roots=c(home='~'), input$data_folder)
    input$file_search # Respond to this
    isolate({
        dataFiles <- list.files(datadir, full.names = T)
        dataFiles_names <- list.files(datadir, full.names = F)

        filelis_all <- as.list(dataFiles)
        filelis_all <- setNames(filelis_all, dataFiles_names)

        filelis <- grep(input$file_search, filelis_all, value = "TRUE")

        if(length(filelis) <= 25) {
            inlen <- length(filelis)
        } else {
            inlen <- 25
        }

        selectInput("dataFiles",
                    label = NULL,
                    choices = filelis,
                    selected = filelis,
                    multiple = T,
                    selectize = F,
                    size = inlen
        )
    })

})



observeEvent(input$submit_dir, {
    #print(input$dataFiles)
    if(is.null(input$dataFiles))
    {
        session$sendCustomMessage(type = "showalert", "Give me something!")
        return()
    }

    if(length(input$dataFiles) < 2) {
        session$sendCustomMessage(type = "showalert", "Too few samples or the input format is incorrect!")
        return()
    }

    if(!is.null(r_data$glb.raw)) # This is not first time submission, then clean up previous session
    {
        r_data <- init_state(r_data)
        r_data <- clear_design(r_data)
    }

    withProgress(message = 'Processing', value = 0, {
        dfList <- list()
        r_data$file_info$type <- "dir"
        r_data$file_info$path <- input$data_folder$path

        n <- length(input$dataFiles)
        for(f in input$dataFiles) {
            dat <- read.table(f, header=T)
            sampleName <- sub("Sample_", "", sub("[.].*", "", basename(f)), ignore.case = T)
            names(dat) <- c("feature", sampleName)
            dfList[[length(dfList) + 1]] <- dat
            incProgress(0.5/n, detail = paste(sampleName, "added"))
        }

        incProgress(0.1, detail = "Bringing together samples...")
        allCountsRaw <- plyr::join_all(dfList, "feature")
        # Make sure the dataframe do not contain NAs
        if(sum(is.na(allCountsRaw))) {
            session$sendCustomMessage(type = "showalert", "NA value was produced... Maybe the samples are not from the same species? Please check again!")
            return()
        }

        rownames(allCountsRaw) <- allCountsRaw$feature
        colnames(allCountsRaw) <- make.names(colnames(allCountsRaw)) # Make sure the sample names are converted to the format accepted by R
        allCountsRaw <- allCountsRaw %>% dplyr::select(-feature)

        r_data$glb.raw <- allCountsRaw # global raw
        # Make sure the names are good
        tmp_sample_name <- make.names(colnames(r_data$glb.raw), unique = TRUE)
        colnames(r_data$glb.raw) <- tmp_sample_name
        tmp_feature_name <- make.names(rownames(r_data$glb.raw), unique = TRUE)
        rownames(r_data$glb.raw) <- tmp_feature_name

        ### Feature exclusion
        # Exclude low count genes
        if(input$input_threshold_type == "mean")
            r_data$glb.raw <- r_data$glb.raw[rowMeans(r_data$glb.raw) > input$min_cnt_avg, ] # The default filter is 0.
        else if(input$input_threshold_type == "sum")
            r_data$glb.raw <- r_data$glb.raw[rowSums(r_data$glb.raw) > input$min_cnt_sum, ]
        else {
            session$sendCustomMessage(type = "showalert", "Unknown threshold type.")
            return()
        }

        # Extract and exclude ERCC
        r_data$ercc <- r_data$glb.raw[grep("ERCC(-|[.])\\d{5}", rownames(r_data$glb.raw)),]
        if(input$exclude_ercc) {
            r_data$glb.raw<-r_data$glb.raw[-which(rownames(r_data$glb.raw) %in% rownames(r_data$ercc)), ]
        }

        r_data$sample_name <- colnames(r_data$glb.raw) # Get the sample_key
        r_data$feature_list <- rownames(r_data$glb.raw) # Get the feature_key

        incProgress(0.1, detail = "Perform data normalization...")

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
        incProgress(0.2, detail = "Adding metadata...")
        r_data$glb.meta <- data.frame(sample = r_data$sample_name)
        r_data <- init_meta(r_data)
        r_data <- update_history(r_data, NA, "Input", "Input counts directory", list(feature = r_data$feature_list, sample = r_data$sample_name, df = r_data$df), r_data$norm_param$method, r_data$norm_param)

        setProgress(1)
    })
})

