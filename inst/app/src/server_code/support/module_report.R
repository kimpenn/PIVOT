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

# Add report blocks to the ace editor

update_rmd <- function(session, rmd, id) {
    updateTabItems(session, "tabs", "report")

    if(is.null(rmd)) {
        rmd <- ""
    }

    file_path <- paste0("src/reports/", id, ".Rmd")

    new_block <- paste0(readLines(file_path), collapse = "\n")

    rmd <- paste0(rmd, new_block, sep = "\n")

    shinyAce::updateAceEditor(session, "report_rmd", value = rmd)

    return(rmd)
}

update_reg_history <- function(r_data, parent, action_type = "analysis", action, lists, norm_method, norm_params){
    if(is.null(r_data$reg_history)) {
        r_data$reg_history <- list()
    }

    name = action
    parent = r_data$his_tbl$name[which(r_data$his_tbl$is_activated == "Y")]

    r_data$reg_history[[length(r_data$reg_history) + 1]] <- list(
        name = as.character(name),
        time = lubridate::now(),
        parent_data = as.character(parent),
        action_type = action_type,
        action = action,
        lists = lists,
        norm_method = NA,
        norm_params = NA,
        feature_num = NA,
        sample_num = NA
    )
    reg_tbl <- plyr::ldply(r_data$reg_history, function(x){data.frame(x[c(1,2,3,4,5,7,9,10)])})
    reg_tbl$user_notes <- NA
    reg_tbl$is_activated <- NA
    r_data$reg_tbl <- reg_tbl
    neList<- generate_data_node(his_tbl = r_data$his_tbl, reg_tbl = r_data$reg_tbl)
    r_data$his_nodes <- neList$nodes
    r_data$his_edges <- neList$edges
}


observe({
    if(is.null(input$report_rmd)) return()
    r_data$rmd <- input$report_rmd
})



generate_block_report <- function(id, file = NULL, mode = "html_document") {
    file_path <- paste0("src/reports/", id, ".Rmd")
    print(file_path)
    new_block <- paste0(readLines(file_path), collapse = "\n")
    error_I <- 0
    tryCatch({
        fileConn<-file("www/tmp.rmd")
        writeLines(new_block, fileConn)
        close(fileConn)
        path <- rmarkdown::render(input = "www/tmp.rmd", output_format = mode, output_file = file)},
        error = function(e) {
            session$sendCustomMessage(type = "showalert", "PIVOT failed to generate your report.")
            error_I <<- 1
        }
    )
    if(error_I) {
        return()
    } else {
        return(path)
    }
}



####################################### OPTIMIZE THIS ############################################
# The code below handles report generation when user click the buttons on the enhanced box.
# It is copied multiple times for each modules. It is just sad that I can't use shiny modules for this...
# Mainly due to two reasons, the javascript called by updateTabItems require global namespace
# The callModule function written inside the report needs global namespace to work (can't work in both global and inside module).
# Maybe there's a way. Open to suggestions.

####### Data table show ######
observeEvent(input$data_table_report, {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "data_table")
})

observeEvent(input$data_table_reg, {
    id = "data_table"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Data table")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$data_table_html <- downloadHandler(
    filename = function() {
        id = "data_table"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "data_table"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### Spike-in Analysis ######
observeEvent(input$spike_in_report, {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "spike_in")
})

observeEvent(input$spike_in_reg, {
    id = "spike_in"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Spike-in Analysis")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$spike_in_html <- downloadHandler(
    filename = function() {
        id = "spike_in"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "spike_in"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Data distribution plot ######

observeEvent(input$"data_distribution_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "data_distribution")
})

observeEvent(input$"data_distribution_reg", {
    id = "data_distribution"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Data distribution plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$data_distribution_pdf <- downloadHandler(
    filename = function() {
        id = "data_distribution"
        paste0(id, ".pdf")
    },
    content = function(file) {
        id = "data_distribution"
        generate_block_report(id, file, mode = "pdf_document")
    }
)

output$data_distribution_html <- downloadHandler(
    filename = function() {
        id = "data_distribution"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "data_distribution"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Rank frequency plot ######

observeEvent(input$"rank_frequency_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "rank_frequency")
})

observeEvent(input$"rank_frequency_reg", {
    id = "rank_frequency"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Rank frequency plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$rank_frequency_pdf <- downloadHandler(
    filename = function() {
        id = "rank_frequency"
        paste0(id, ".pdf")
    },
    content = function(file) {
        id = "rank_frequency"
        generate_block_report(id, file, mode = "pdf_document")
    }
)

output$rank_frequency_html <- downloadHandler(
    filename = function() {
        id = "rank_frequency"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "rank_frequency"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### Mean variability plot ######

observeEvent(input$"mean_var_plot_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "mean_var_plot")
})

observeEvent(input$"mean_var_plot_reg", {
    id = "mean_var_plot"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Mean variability plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$mean_var_plot_pdf <- downloadHandler(
    filename = function() {
        id = "mean_var_plot"
        paste0(id, ".pdf")
    },
    content = function(file) {
        id = "mean_var_plot"
        generate_block_report(id, file, mode = "pdf_document")
    }
)

output$mean_var_plot_html <- downloadHandler(
    filename = function() {
        id = "mean_var_plot"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "mean_var_plot"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Mean standard-deviation plot ######

####### Mean variability plot ######

observeEvent(input$"mean_sd_plot_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "mean_sd_plot")
})

observeEvent(input$"mean_sd_plot_reg", {
    id = "mean_sd_plot"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Per-feature standard deviation plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$mean_sd_plot_pdf <- downloadHandler(
    filename = function() {
        id = "mean_sd_plot"
        paste0(id, ".pdf")
    },
    content = function(file) {
        id = "mean_sd_plot"
        generate_block_report(id, file, mode = "pdf_document")
    }
)

output$mean_sd_plot_html <- downloadHandler(
    filename = function() {
        id = "mean_sd_plot"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "mean_sd_plot"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### hierarchical cluster plot ######

observeEvent(input$"hierarchical_clust_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "hierarchical_clust")
})

observeEvent(input$"hierarchical_clust_reg", {
    id = "hierarchical_clust"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Hierarchical clustering")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$hierarchical_clust_pdf <- downloadHandler(
    filename = function() {
        id = "hierarchical_clust"
        paste0(id, ".pdf")
    },
    content = function(file) {
        id = "hierarchical_clust"
        generate_block_report(id, file, mode = "pdf_document")
    }
)

output$hierarchical_clust_html <- downloadHandler(
    filename = function() {
        id = "hierarchical_clust"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "hierarchical_clust"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### SC3 #######

# id = sc3_clust
observeEvent(input$"sc3_clust_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "sc3_clust")
})

observeEvent(input$"sc3_clust_reg", {
    id = "sc3_clust"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "SC3 Clustering")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$sc3_clust_html <- downloadHandler(
    filename = function() {
        id = "sc3_clust"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "sc3_clust"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### SCDE #######
# id = scde

observeEvent(input$"scde_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "scde")
})

observeEvent(input$"scde_reg", {
    id = "scde"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "SCDE")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$scde_html <- downloadHandler(
    filename = function() {
        id = "scde"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "scde"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### DESeq ######
# id = deseq

observeEvent(input$deseq_report, {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "deseq")
})

observeEvent(input$deseq_reg, {
    # First save a html output to tmp folder (how to get it go with state?)
    id = "deseq"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "DESeq results")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$deseq_html <- downloadHandler(
    filename = function() {
        id = "deseq"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "deseq"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### MWW DE Test #######

# id = mww_test_result

observeEvent(input$"mww_test_result_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "mww_test_result")
})

observeEvent(input$"mww_test_result_reg", {
    id = "mww_test_result"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Mann_Whitney U Test Result")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$mww_test_result_html <- downloadHandler(
    filename = function() {
        id = "mww_test_result"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "mww_test_result"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Monocle #######

#id = monocle_gene_var

observeEvent(input$"monocle_gene_var_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "monocle_gene_var")
})

observeEvent(input$"monocle_gene_var_reg", {
    id = "monocle_gene_var"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Monocle Cell Clustering/Ordering")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$monocle_gene_var_html <- downloadHandler(
    filename = function() {
        id = "monocle_gene_var"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "monocle_gene_var"
        generate_block_report(id, file, mode = "html_document")
    }
)

#id = monocle_state

observeEvent(input$"monocle_state_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "monocle_state")
})

observeEvent(input$"monocle_state_reg", {
    id = "monocle_state"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Monocle Cell Ordering/Clustering")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$monocle_state_html <- downloadHandler(
    filename = function() {
        id = "monocle_state"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "monocle_state"
        generate_block_report(id, file, mode = "html_document")
    }
)

#id = monocle_gene

observeEvent(input$"monocle_gene_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "monocle_gene")
})

observeEvent(input$"monocle_gene_reg", {
    id = "monocle_gene"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Monocle Gene Expression Pattern")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$monocle_gene_html <- downloadHandler(
    filename = function() {
        id = "monocle_gene"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "monocle_gene"
        generate_block_report(id, file, mode = "html_document")
    }
)


#id = monocle_de

observeEvent(input$"monocle_de_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "monocle_de")
})

observeEvent(input$"monocle_de_reg", {
    id = "monocle_de"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Monocle DE")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$monocle_de_html <- downloadHandler(
    filename = function() {
        id = "monocle_de"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "monocle_de"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### KMEANS clustering #######

# id = kmeans_clust
observeEvent(input$"kmeans_clust_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "kmeans_clust")
})

observeEvent(input$"kmeans_clust_reg", {
    id = "kmeans_clust"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "K-means Clustering")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$kmeans_clust_html <- downloadHandler(
    filename = function() {
        id = "kmeans_clust"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "kmeans_clust"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### Community Detection #######

# id = community_mst
observeEvent(input$"community_mst_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "community_mst")
})

observeEvent(input$"community_mst_reg", {
    id = "community_mst"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Community Detection with MST")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$community_mst_html <- downloadHandler(
    filename = function() {
        id = "community_mst"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "community_mst"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Pairwise Correlation #######
# id = pair_corr
observeEvent(input$"pair_corr_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "pair_corr")
})

observeEvent(input$"pair_corr_reg", {
    id = "pair_corr"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Pairwise Correlations Scatterplot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$pair_corr_html <- downloadHandler(
    filename = function() {
        id = "pair_corr"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "pair_corr"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Sample Corrrelation Heatmap #######
# id = sample_corr_heatmap
observeEvent(input$"sample_corr_heatmap_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "sample_corr_heatmap")
})

observeEvent(input$"sample_corr_heatmap_reg", {
    id = "sample_corr_heatmap"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Sample Correlation Heatmap")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$sample_corr_heatmap_html <- downloadHandler(
    filename = function() {
        id = "sample_corr_heatmap"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "sample_corr_heatmap"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### Feature Corrrelation Heatmap #######
# id = feature_corr
observeEvent(input$"feature_corr_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "feature_corr")
})

observeEvent(input$"feature_corr_reg", {
    id = "feature_corr"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Feature Correlation")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$feature_corr_html <- downloadHandler(
    filename = function() {
        id = "feature_corr"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "feature_corr"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### Feature Heatmap ######

observeEvent(input$"feature_heatmap_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "feature_heatmap")
})

observeEvent(input$"feature_heatmap_reg", {
    # First save a html output to tmp folder (how to get it go with state?)
    id = "feature_heatmap"
    tmp_path <- generate_block_report(id, mode = "html_document")
    rawHTML <- paste(readLines(tmp_path), collapse="\n")

    # Register the event in the reg_tbl
    r_data <- update_reg_history(r_data, lists = rawHTML, action = "Feature heatmap")
    session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
})

output$feature_heatmap_pdf <- downloadHandler(
    filename = function() {
        id = "feature_heatmap"
        paste0(id, ".pdf")
    },
    content = function(file) {
        id = "feature_heatmap"
        generate_block_report(id, file, mode = "pdf_document")
    }
)

output$feature_heatmap_html <- downloadHandler(
    filename = function() {
        id = "feature_heatmap"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "feature_heatmap"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### PCA #######

# id = pca
observeEvent(input$"pca_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "pca")
})

observeEvent(input$"pca_reg", {
    id = "pca"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "PCA")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$pca_html <- downloadHandler(
    filename = function() {
        id = "pca"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "pca"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### T-sne #######

# id = tsne
observeEvent(input$"tsne_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "tsne")
})

observeEvent(input$"tsne_reg", {
    id = "tsne"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "T-SNE")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$tsne_html <- downloadHandler(
    filename = function() {
        id = "tsne"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "tsne"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Metric MDS #######

# id = mds
observeEvent(input$"mds_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "mds")
})

observeEvent(input$"mds_reg", {
    id = "mds"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Metric MDS")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$mds_html <- downloadHandler(
    filename = function() {
        id = "mds"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "mds"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Nonmetric MDS #######

# id = nds
observeEvent(input$"nds_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "nds")
})

observeEvent(input$"nds_reg", {
    id = "nds"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Nonmetric MDS")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$nds_html <- downloadHandler(
    filename = function() {
        id = "nds"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "nds"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### Diffusion map #######

# id = dfm
observeEvent(input$"dfm_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "dfm")
})

observeEvent(input$"dfm_reg", {
    id = "dfm"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Diffusion Map")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$dfm_html <- downloadHandler(
    filename = function() {
        id = "dfm"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "dfm"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Penalized LDA ######
# id = plda
observeEvent(input$"plda_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "plda")
})

observeEvent(input$"plda_reg", {
    id = "plda"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "PenalizedLDA")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$plda_html <- downloadHandler(
    filename = function() {
        id = "plda"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "plda"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Network Analysis #######

# id = transnet
observeEvent(input$"transnet_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "transnet")
})

observeEvent(input$"transnet_reg", {
    id = "transnet"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "DE Table for Network Analysis")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$transnet_html <- downloadHandler(
    filename = function() {
        id = "transnet"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "transnet"
        generate_block_report(id, file, mode = "html_document")
    }
)

