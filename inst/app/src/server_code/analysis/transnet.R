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

species_list <- list("Homo sapiens (9606)" = "9606",
                     "Mus musculus (10090)" = "10090",
                     "Rattus norvegicus (10116)" = "10116",
                     "Danio rerio (7955)" = "7955",
                     "Caenorhabditis elegans (6239)" = "6239",
                     "Drosophila melanogaster (7227)" = "7227")

output$transnet_ui <- renderUI({

    if(is.null(r_data$df)) {
        return()
    }

    gene_choices <- list()

    if(!is.null(r_data$scde_results) && !("scde" %in% gene_choices)) {
        group_string <- paste("DE genes of Group", unique(r_data$scde_group)[1], "and Group", unique(r_data$scde_group)[2] ,"reported by SCDE")
        gene_choices <- c(gene_choices, setNames(list("scde"), group_string))
    }

    if(!is.null(r_data$deseq_results) && !("deseq" %in% gene_choices)) {
        group_string <- paste("DE genes of Group", r_data$deseq_group[1], "and Group",  r_data$deseq_group[2], "reported by DESeq")
        gene_choices <- c(gene_choices, setNames(list("deseq"), group_string))
    }

    if(!is.null(r_data$mww_results) && !("mww" %in% gene_choices)) {
        group_string <- paste("DE genes of Group", r_data$mww_group[1], "and Group",  r_data$mww_group[2], "reported by Mann–Whitney U test")
        gene_choices <- c(gene_choices, setNames(list("mww"), group_string))
    }

    if(length(gene_choices) == 0) {
        tf_de_select_ui <- tags$p("Please run DE analysis first.") # TODO: allow input of custom gene list
    } else {
        tf_de_select_ui <- selectInput("tf_de_select", "Choose DE results", choices = gene_choices)
    }

    list(
        fluidRow(
            column(12,
                   enhanced_box(
                       title = "TF Centered Gene Network Analysis with DE Results",
                       id = "transnet",
                       status = "primary",
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       register_analysis = T,
                       width = NULL,
                       tags$div(tags$b("STEP 1: Load DE table and compute gene score:"), class = "param_setting_title"),
                       fluidRow(
                           column(4,
                                  selectInput("transnet_species", "Choose species:",
                                              choices = species_list)
                           ),
                           column(5, tf_de_select_ui),
                           column(3, uiOutput("scde_lfc_ui"))
                       ),

                       tags$p("The built-in transcription factor list is a combined list of DBD predicted transcription factors and Regnetwork regulators (with micro-RNAs removed). "),

                       uiOutput("tf_loaded_ui"),
                       fluidRow(
                           column(6, checkboxInput("tf_custom_check", "Use a custom transcription factor list", value = F)),
                           column(6, uiOutput("tf_custom_btn_ui"))
                       ),
                       hr(),

                       wellPanel(
                           fluidRow(
                               column(3, selectInput("tf_de_subset", "Include genes:", choices = list("All genes" = "all", "Transcription factors" = "tf"), selected = "tf")),
                               column(3, selectInput("tf_de_rank_type", "Order genes by", choices = list("Score" = "score", "Adjusted P value" = "padj", "Absolute log fold change/Effect size" = "lfc"), selected = "score")),
                               column(3, selectInput("tf_de_direction", "Select direction:", choices = list("All direction (lfc = 0, >0, <0)" = "all", "Upregulation (lfc > 0)" = "up", "Downregulation (lfc < 0)" = "down"), selected = "all")),
                               column(3, numericInput("tf_de_padj", "Padj cutoff", value = 1, min = 0, max = 1, step = 0.001))
                           )
                       ),

                       wellPanel(
                           fluidRow(
                               column(3, selectInput("tf_score_type", "Gene score formula:",
                                                     choices = list("Absolute log fold change" = "lfc",
                                                                    "Absolute log fold change with Padj threshold" = "lfc_threshold",
                                                                    "Original Mogrify Score" = "mogrify",
                                                                    "Mogrify score with Padj threshold" = "mogrify_modified"),
                                                     selected = "mogrify_modified")),
                               column(6, uiOutput("tf_score_formula_ui")),
                               column(3, uiOutput("tf_score_pthresh_ui"))
                           )
                       ),

                       DT::dataTableOutput("tf_de_tbl_show"),

                       tags$p("Note: entries with NA adjusted-P values are removed from the original DE table."),

                       uiOutput("download_tf_de_tbl_ui")
                   ),
                   enhanced_box(
                       title = NULL,
                       status = "primary",
                       solidHeader = F,
                       width = NULL,
                       tags$div(tags$b("STEP 2: Network Mapping of Genes in the DE Table"), class = "param_setting_title"),
                       fluidRow(
                           column(4, uiOutput("tf_interactome_selection_ui")),
                           uiOutput("string_load_ui")
                       ),
                       uiOutput("loaded_interactome_info_ui"),
                       tags$p(),
                       uiOutput("net_vis_type_ui"),
                       uiOutput("net_vis")
                   ),

                   enhanced_box(
                       title = NULL,
                       status = "danger",
                       solidHeader = F,
                       width = NULL,
                       tags$div(tags$b("OPTIONAL: Network Based Transcription Factor Scoring and Ranking"), class = "param_setting_title"),
                       fluidRow(
                           column(4, selectInput("tf_influence_def", "Define Influence Score:",
                                                 choices = list("Weighted summation of p quantile of each neighborhood level" = "quantile",
                                                                "Weighted summation over all vertices in the local network" = "all"))),
                           column(4, tags$p(), uiOutput("tf_influence_def_pic")),
                           column(4, uiOutput("tf_influence_def_text"))
                       ),


                       uiOutput("tf_influence_parameters"),

                       actionButton("tf_sum_score", "Compute", class = "btn btn-info"),

                       hr(),

                       wellPanel(
                           fluidRow(
                               column(6,
                                      uiOutput("final_score_def_ui")
                               )

                           )
                       ),

                       fluidRow(
                           column(12,
                                  tags$b("Top Ranked TFs in Group1"),
                                  DT::dataTableOutput("tf_tbl1_show"),
                                  uiOutput("download_tf_tbl1_ui")
                           )
                       ),


                       fluidRow(
                           column(12,
                                  actionButton("tf_net_switch1", "Switch view"),
                                  downloadButton("tf_net_download1", "Download list of genes in the current graph")
                           )
                       ),
                       uiOutput("tf_net_text1"),
                       uiOutput("tf_gp1_net_show"),

                       hr(),

                       fluidRow(
                           column(12,
                                  tags$b("Top Ranked TFs in Group2"),
                                  DT::dataTableOutput("tf_tbl3_show"),
                                  uiOutput("download_tf_tbl2_ui")
                           )
                       ),


                       fluidRow(
                           column(12,
                                  actionButton("tf_net_switch2", "Switch view"),
                                  downloadButton("tf_net_download2", "Download list of genes in the current graph")
                           )
                       ),
                       uiOutput("tf_net_text2"),
                       uiOutput("tf_gp2_net_show")

                   )

            )

        )

    )

})


output$scde_lfc_ui <- renderUI({
    if(is.null(r_data$df) || is.null(input$tf_de_select)) return()

    if(input$tf_de_select == "scde") {
        selectInput("scde_lfc", "Choose SCDE LFC:",
                    choices = list("Maximum likelyhood estimate (mle)" = "mle", "Conservative estimate (ce)" = "ce"),
                    selected = "mle")
    }
})

output$final_score_def_ui <- renderUI({
    if(is.null(r_data$tf_de_tbl) ) return()
    if(!is.null(r_data$tf_neighbor_order) && r_data$tf_neighbor_order == 0) return()
    selectInput("final_score_def", "Define final score",
                choices = list(
                    "Influence score" = "influence_score",
                    "Influence score adjusted by total number of vertices" = "adj_v",
                    "Influence score adjusted by total number of activated vertices" = "adj_vth",
                    "Influence score adjusted by proportion of activated vertices" = "adj_prop",
                    "TF score" = "tf_score",
                    "TF score adjusted by total number of vertices" = "tf_adj_v",
                    "TF score adjusted by total number of activated vertices" = "tf_adj_vth",
                    "TF score adjusted by proportion of activated vertices" = "tf_adj_prop",
                    "Proportion of activated vertices" = "prop"
                ),
                selected = "adj_v"
    )
})

output$tf_score_formula_ui <- renderUI({
    if(input$tf_score_type == "mogrify") {
        return(withMathJax(HTML("$$Score = | Lfc | \\cdot (-\\log_{10} P_{adj}) $$")))
    }
    if(input$tf_score_type == "mogrify_modified") {
        return(withMathJax(HTML("$$Score =
                                \\begin{cases}
                                | Lfc | \\cdot (-\\log_{10} P_{adj}),  & \\text{if $P_{adj} > P_{threshold}$} \\\\
                                | Lfc | \\cdot (-\\log_{10} P_{threshold}),  & \\text{if $P_{adj} <= P_{threshold}$}
                                \\end{cases}$$")))
    }
    if(input$tf_score_type == "lfc") {
        return(withMathJax(HTML("$$Score = | Lfc |$$")))
    }
    if(input$tf_score_type == "lfc_threshold") {
        return(withMathJax(HTML("$$Score =
                                \\begin{cases}
                                | Lfc | ,  & \\text{if $P_{adj} <= P_{threshold}$} \\\\
                                0,  & \\text{if $P_{adj} > P_{threshold}$}
                                \\end{cases}$$")))
    }
})

output$tf_influence_def_pic <- renderUI({
    if(is.null(input$tf_influence_def)) return()
    if(input$tf_influence_def == "quantile") {
        img(src="influence_score_formula.png", height=50)
    } else if(input$tf_influence_def == "all"){
        img(src="influence_score_formula2.png", height=50)
    }

})

output$tf_influence_def_text <- renderUI({
    if(is.null(input$tf_influence_def)) return()
    if(input$tf_influence_def == "quantile") {
        tags$p("We use the p quantile (e.g., 90%) of the normalized score (Gr/Lr) of genes RL in the level L neighborhood to represent the influence of transcription factor x on that level.
                             Then, the influence score is summed over all levels in the local network, including level 0, the center transcription factor score. ")
    } else if(input$tf_influence_def == "all"){
        tags$p("The score is computed with the original mogrify formula but not accounting for the specificity (out degree of each vertices's parent).")
    }
})

output$tf_influence_parameters <- renderUI({
    if(is.null(r_data$tf_de_tbl)) return()
    default_thresh <- round(quantile(r_data$tf_de_tbl$gene_score, 0.75, na.rm = T), digits = 2)
    tf_acti_threshold_ui <- numericInput("tf_acti_threshold", "Vertex activation score threshold", min = 0, max = 100, step = 0.1, value = default_thresh)

    if(input$tf_influence_def == "quantile") {
        quantile_ui <-sliderInput("tf_quantile", "Quantile (p):", min = 0, max = 1, value = 0.9, step = 0.1)
    } else {
        quantile_ui <- NULL
    }

    list(
        fluidRow(
            column(3, sliderInput("tf_interactome_nb", "Neighborhood levels (Lmax):", min = 0, max = 2, value = 1, step = 1)),
            column(6, quantile_ui),
            column(3, uiOutput("tf_nbs_direction_ui"))
        ),
        fluidRow(
            column(3, tf_acti_threshold_ui),
            column(9, tags$p(), tags$p("Vertices with score above this threshold is considered 'activated' (related stats shown in the tables below). The default value is the 75 percentile of all gene scores."))
        )
    )
})

output$tf_score_pthresh_ui <- renderUI({
    if(input$tf_score_type == "mogrify_modified" || input$tf_score_type == "lfc_threshold") {
        numericInput("tf_score_pthresh", "P threshold", value = 0.1, min = 0, max = 1, step = 0.001)
    }
})


output$tf_nbs_direction_ui <- renderUI({
    if(is.null(input$tf_interactome_selection)) return()
    if(input$tf_interactome_selection == "reg"){
        selectInput("tf_nbs_direction", "Select edge directions:", choices = list("Include all direction" = "all", "Include outgoing edges" = "out", "Include incoming edges" = "in"), selected = "all")
    }
})

output$tf_custom_btn_ui <- renderUI({
    if(!input$tf_custom_check) return()
    content <- list(
        fluidRow(
            column(6,
                   wellPanel(
                       fileInput('tf_list_file', label = NULL, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                       checkboxInput('tf_header', 'Header', value = F),
                       radioButtons('tf_sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), selected = '\t'),
                       radioButtons('tf_quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"'),
                       actionButton("tf_list_submit", "Submit List", class = "btn btn-info")
                   )
            ),
            column(6,
                   tags$p("[feature name in 1st column]"),
                   DT::dataTableOutput('tf_list_tbl_show')
            )
        )
    )

    list(
        actionButton("tf_custom_btn", label = "Upload a custom TF list", class = "btn-warning"),
        shinyBS::bsModal(id = "tf_custom_modal", "Upload a custom transcription factor list", "tf_custom_btn", size = "large", content)
    )
})

generate_neighbor_graph <- function(cur_id, cur_mode = "all", cur_levs, influence_def, cur_quantile, g0, meta_tbl, type, de_tbl, only_graph = F) {
    result <- list()

    if(type == "stringdb") {
        cur_id <- as.character(meta_tbl$STRING_id[match(toupper(cur_id), meta_tbl$cap_name)])
    } else if(type == "reg") {
        cur_id <- as.character(meta_tbl$cap_name[match(toupper(cur_id), meta_tbl$cap_name)])
    }

    if(influence_def == "quantile") {
        agre_func <- function(x){quantile(x, cur_quantile, na.rm = T)}
    } else if (influence_def == "all") {
        agre_func <- function(x){sum(x, na.rm = T)}
    }


    if(is.na(cur_id)) {
        msg<-paste("TF", cur_id, "can't be mapped" )
        influence_score <- NA
        tf_nets <- NA
        stop(msg)
    }

    has_error <- 0
    tryCatch({
        nbs <- list()
        nbs[[1]] <- cur_id
        for(j in 2: (cur_levs + 1)){
            nbs[[j]] <- names(unlist(igraph::neighborhood(g0, j-1, cur_id, cur_mode))) # order 1 neighbors (including itself)
        }
    },
    error = function(e){
        has_error <<- 1
    }
    )

    if(has_error) {
        msg<-paste("TF", cur_id, "can't be mapped" )
        influence_score <- NA
        tf_nets <- NA
        stop(msg)
    }

    if(type == "stringdb")
        matched <-match(nbs[[length(nbs)]], meta_tbl$STRING_id) #  Find which neighbors are in the current dataset
    if(type == "reg")
        matched <-match(nbs[[length(nbs)]], meta_tbl$cap_name)

    nbs_tbl <- meta_tbl[matched[!is.na(matched)],] # extract the gene names


    if(type == "stringdb") {
        nbs_in <- nbs[[length(nbs)]][which(nbs[[length(nbs)]] %in% nbs_tbl$STRING_id)] # the filtered neighbors (in the current set, has string mapping)
        nbs_nets <- igraph::induced_subgraph(g0, nbs_in) # the subgraph centered on the current TF
        V(nbs_nets)$name <- as.character(nbs_tbl$gene[match(V(nbs_nets)$name, nbs_tbl$STRING_id)])
    }

    if(type == "reg") {
        nbs_in <- nbs[[length(nbs)]][which(nbs[[length(nbs)]] %in% nbs_tbl$cap_name)]
        nbs_nets <- igraph::induced_subgraph(g0, nbs_in) # the subgraph centered on the current TF
        V(nbs_nets)$name <- as.character(nbs_tbl$gene[match(V(nbs_nets)$name, nbs_tbl$cap_name)])
    }

    nbs_tbl$score <- de_tbl$gene_score[match(nbs_tbl$gene, de_tbl$gene)]

    vertex_order <- c("1")
    name_order <- c(cur_id)

    for(k in 2:length(nbs)) {
        vertex_order <- c(vertex_order, rep(as.character(k), length(nbs[[k]]) - length(nbs[[k-1]])))
        name_order <- c(name_order, nbs[[k]][which(!nbs[[k]] %in% nbs[[k-1]])])
    }

    if(type == "stringdb") {
        nbs_tbl$group <- vertex_order[match(nbs_tbl$STRING_id, name_order)]
    }
    if(type == "reg") {
        nbs_tbl$group <- vertex_order[match(nbs_tbl$cap_name, name_order)]
    }

    # Calculate the total score.

    tmp_tbl <- nbs_tbl %>% dplyr::select(gene, group, score)

    if(!only_graph) {
        num_vertices <- nrow(tmp_tbl)
        num_nonz_vertices <- sum(tmp_tbl$score[-1] > input$tf_acti_threshold, na.rm = T) # Do not account for the center TF
        prop_nonz <- round(num_nonz_vertices/(num_vertices-1), digits = 2)

        tmp_tbl$norm_score <- tmp_tbl$score/as.numeric(tmp_tbl$group)

        tmp_tbl1<-tmp_tbl %>% dplyr::filter(group != "1")

        if(nrow(tmp_tbl1) != 0) {
            tmp_tbl2<-aggregate(tmp_tbl1$norm_score, list(tmp_tbl1$group), agre_func)
            nb_score <- sum(tmp_tbl2$x, na.rm = T)
        }else {
            nb_score <- 0
        }
        influence_score <- tmp_tbl$norm_score[which(tmp_tbl$group == "1")] + nb_score
        result$num_nonz_vertices <- num_nonz_vertices
        result$prop_nonz <- prop_nonz
        result$influence_score <- influence_score
    }

    cols <- c("1"="#1f77b4", "2"="#ff7f0e", "3" = "#2ca02c", "4" = "#d62728")

    V(nbs_nets)$color <- cols[ nbs_tbl$group[match(V(nbs_nets)$name, nbs_tbl$gene) ] ]
    V(nbs_nets)$size <- nbs_tbl$score[match(V(nbs_nets)$name, nbs_tbl$gene)]
    V(nbs_nets)$size[is.na(V(nbs_nets)$size)] <- 0
    V(nbs_nets)$group <- nbs_tbl$group[match(V(nbs_nets)$name, nbs_tbl$gene)]

    result$tf_net <- nbs_nets
    return(result)
}


tmp_tf_list <- reactiveValues()
tmp_tf_list$tbl <- NULL

# process the upload TF list
observe({
    inFile <- input$tf_list_file
    error_I <- 0
    if (!is.null(inFile)) {
        tryCatch({
            tmp_tf_list$tbl <- read.csv(inFile$datapath, header=input$tf_header, sep=input$tf_sep, quote=input$tf_quote)
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

output$tf_list_tbl_show <- DT::renderDataTable({
    if(is.null(tmp_tf_list$tbl)) return()
    DT::datatable(tmp_tf_list$tbl, options = list(scrollX = TRUE, scrollY = "350px", searching = FALSE))
})

observeEvent(input$tf_list_submit, {
    if (is.null(tmp_tf_list$tbl) || nrow(tmp_tf_list$tbl) == 0)
    {
        session$sendCustomMessage(type = "showalert", "Give me something!")
        return(NULL)
    }

    tf_names <- make.names(as.character(unique(tmp_tf_list$tbl[,1])))

    cur_flist <- r_data$feature_list

    tf_list <- cur_flist[match(toupper(tf_names), toupper(cur_flist))]
    tf_list <- tf_list[!is.na(tf_list)]

    if(length(tf_list) != length(tf_names)) {
        message_gl <- paste0(length(tf_names) - length(tf_list)," genes in your file (", length(tf_names)," gene names) are not found in the current dataset.")
        session$sendCustomMessage(type = "showalert", message_gl)
    }

    if(length(tf_list) > 0) {
        r_data$tf_list <- tf_list
        r_data$input_tfs <- tf_names
    }
})


# Load TF list
observe({
    if(is.null(input$transnet_species) || is.null(r_data$df)) return()
    if(input$tf_custom_check) return()
    withProgress(message = 'Loading list...', value = 0.5, {
        if(input$transnet_species == "9606"){
            tf_tbl1 <- read.csv("src/built_in_files/hs_dbd_tf.csv")
            tf_tbl2 <- read.csv("src/built_in_files/hs_reg_tf.csv")
            list1 <- as.character(tf_tbl1$tf_gene)
            list2 <- as.character(tf_tbl2$tf_gene)
            input_tfs<- unique(c(list1, list2))
            r_data$input_tfs<-input_tfs[!grepl("hsa-.*", input_tfs)]
        } else if(input$transnet_species == "10090") {
            tf_tbl1 <- read.csv("src/built_in_files/mm_dbd_tf.csv")
            tf_tbl2 <- read.csv("src/built_in_files/mm_reg_tf.csv")
            r_data$reg_g = igraph::graph.data.frame(tf_tbl2,T)
            list1 <- as.character(tf_tbl1$tf_gene)
            list2 <- as.character(tf_tbl2$tf_gene)
            input_tfs<- unique(c(list1, list2))
            r_data$input_tfs<-input_tfs[!grepl("MMU-.*", input_tfs)]
        } else {
            r_data$reg_g <- NULL
            print("Add species alert")
            return()
        }

        r_data$tf_list <-r_data$input_tfs[which(toupper(r_data$input_tfs) %in% toupper(r_data$feature_list))] # change it to unfiltered list?

    })

})


output$tf_loaded_ui <- renderUI({
    if(!is.null(r_data$tf_list)){
        return(
            fluidRow(
                column(9, tags$b(
                    paste0("Total number of transcription factors found in the current dataset: ",
                           length(r_data$tf_list), " (", length(r_data$input_tfs) - length(r_data$tf_list),
                           " tfs are not found)."))),
                column(3, downloadButton("download_tfs", "Download TF list", class = "btn btn-success"))
            )

        )
    } else {
        return(tags$p(paste0("You need to manually upload a transcription factor list for the specified species.")))
    }
})

output$download_tfs <- downloadHandler(
    filename = "tf_list.csv",
    content = function(file) {
        if(is.null(r_data$tf_list)) return()
        write.csv(data.frame(tf_list = r_data$tf_list), file, row.names = F)
    }
)



output$tf_de_tbl_show <- DT::renderDataTable({
    if(is.null(r_data$df) || is.null(input$tf_de_select)) return()
    if(input$tf_de_select == "scde" && is.null(input$scde_lfc)) return()
    tryCatch({
    if(input$tf_de_select == "scde") {
        if(is.null(r_data$scde_results)) return()
        if(input$scde_lfc == "mle") {
            tbl <- r_data$scde_results %>% tibble::rownames_to_column("gene") %>%
                dplyr::select(gene, lfc_or_es = mle, pval = p.values, padj = p.values.adj)
        } else if(input$scde_lfc == "ce") {
            tbl <- r_data$scde_results %>% tibble::rownames_to_column("gene") %>%
                dplyr::select(gene, lfc_or_es = ce, pval = p.values, padj = p.values.adj)
        }

    } else if(input$tf_de_select == "deseq") {
        if(is.null(r_data$deseq_results)) return()
        tbl <- as.data.frame(r_data$deseq_results) %>% tibble::rownames_to_column("gene") %>%
            dplyr::select(gene, lfc_or_es = log2FoldChange, pval = pvalue, padj = padj)
    } else if(input$tf_de_select == "mww") {
        if(is.null(r_data$mww_results)) return()
        tbl <- r_data$mww_results %>% tibble::rownames_to_column("gene") %>%
            dplyr::select(gene, lfc_or_es = effectSize, pval = P.value, padj = adjustedP)
        tbl$lfc_or_es <- sign(tbl$lfc_or_es)*log(abs(tbl$lfc_or_es) + 1) # This is the correct transformation?
    }

    if(input$tf_score_type == "lfc") {
        tbl$gene_score = abs(tbl$lfc_or_es)
    } else if(input$tf_score_type == "mogrify") {
        tbl$gene_score = abs(tbl$lfc_or_es) * (-log10(tbl$padj))
    } else if(input$tf_score_type == "mogrify_modified") {
            tmp_padj <- ifelse(tbl$padj > input$tf_score_pthresh, -log10(tbl$padj), -log10(input$tf_score_pthresh))
            tbl$gene_score = abs(tbl$lfc_or_es) * tmp_padj
    } else if(input$tf_score_type == "lfc_threshold") {
        mulplier <- ifelse(tbl$padj > input$tf_score_pthresh,0,1)
        tbl$gene_score = abs(tbl$lfc_or_es) * mulplier
    }


    if(input$tf_de_rank_type == "score") {
        tbl <- tbl %>% dplyr::arrange(-gene_score)
    } else if(input$tf_de_rank_type == "padj") {
        tbl <- tbl %>% dplyr::arrange(padj)
    } else if(input$tf_de_rank_type == "lfc") {
        tbl <- tbl %>% dplyr::arrange(-abs(lfc_or_es))
    }

    r_data$tf_de_tbl <- tbl

    if(input$tf_de_subset == "tf"){
        if(is.null(r_data$tf_list)) return()
        tbl <- tbl[which(toupper(tbl$gene) %in% toupper(r_data$tf_list)),]
    }

    tbl <- tbl %>% dplyr::filter(padj <= input$tf_de_padj)

    if(input$tf_de_direction == "up") {
        tbl <- tbl[which(tbl$lfc_or_es > 0),]
    } else if(input$tf_de_direction == "down") {
        tbl <- tbl[which(tbl$lfc_or_es < 0),]
    }

    r_data$tf_de_tbl_vis <- tbl
    }, error = function(e){})

    DT::datatable(tbl, selection = 'single', options = list(scrollX = TRUE, scrollY = "300px", searching=T))
})

output$download_tf_de_tbl_ui <- renderUI({
    if(is.null(input$tf_de_select) || is.null(r_data$df) || is.null(r_data$tf_de_tbl_vis)) return()
    if(input$tf_de_select == "scde" && is.null(input$scde_lfc)) return()

    downloadButton("download_tf_de_tbl", "Download", class = "btn btn-success")
})

output$download_tf_de_tbl <- downloadHandler(
    filename = "de_tbl.csv",
    content = function(file) {
        if(is.null(r_data$tf_de_tbl_vis)) return()
        tbl <- r_data$tf_de_tbl_vis

        write.csv(tbl, file)
    }
)


output$tf_interactome_selection_ui <- renderUI({
    interactome_list <- list()
    loaded_species<-names(species_list[which(species_list == input$transnet_species)])
    interactome_list <- c(interactome_list,
                          setNames(list("stringdb"), paste0("STRING (", loaded_species, ")")))

    if(input$transnet_species %in% c("9606", "10090")) {
        interactome_list <- c(interactome_list, setNames(list("reg"), paste0("RegNetwork (", loaded_species, ")")))
    }

    if(length(interactome_list) > 0) {
        selectInput("tf_interactome_selection", "Network database:", choices = interactome_list)
    } else {
        tags$p("Please load a interactome first.")
    }
})

output$string_load_ui <- renderUI({
    if(is.null(input$tf_interactome_selection)) return()

    if(input$tf_interactome_selection == "stringdb") {
        list(
            column(4, numericInput("stringdb_threshold", "Score threshold", value = 400, min = 0, max = 1000, step = 50)),
            column(4, tags$br(), actionButton("loadStringSpecies", "Load STRING Database", class = "btn btn-info"))
        )
    }
})

observe({
    if(is.null(input$tf_interactome_selection)) return()
    if(input$tf_interactome_selection == "reg"){
        if(input$transnet_species == "9606"){
            tbl2 <- read.csv("src/built_in_files/hs_reg_tf.csv")
            r_data$reg_g = igraph::graph.data.frame(tbl2,T)
        } else if(input$transnet_species == "10090") {
            tbl2 <- read.csv("src/built_in_files/mm_reg_tf.csv")
            r_data$reg_g = igraph::graph.data.frame(tbl2,T)
        } else {
            r_data$reg_g <- NULL
            return()
        }
    }
})


# The functions swiped from STRINGdb, using the original stringdb package cause crash because of igraph incompatibility

downloadAbsentFile <- function(urlStr, oD = tempdir()){

    fileName = tail(strsplit(urlStr, "/")[[1]], 1)
    temp <- paste(oD,"/", fileName, sep="")
    if(! file.exists(temp) || file.info(temp)$size==0) download.file(urlStr,temp)
    if(file.info(temp)$size==0) {
        unlink(temp)
        temp=NULL
        cat(paste("ERROR: failed to download ", fileName,".\nPlease check your internet connection and/or try again. " ,
                  "\nThen, if you still display this error message please contact us.",sep=""))
    }
    return(temp)
}

stringdb_get_graph = function(species, version, score_threshold, backgroundV = NULL){
    '
    Description:
    Downloads and returns the STRING network (the network is set also in the graph variable of the STRING_db object).

    It makes use of the variables:
    "backgroundV"         vector containing STRING identifiers to be used as background
    (i.e. the STRING network loaded will contain only the proteins that are present also in this vector)
    "score_threshold"     STRING combined score threshold (the network loaded contains only interactions having a combined score greater than this threshold)
    Author(s):
    Andrea Franceschini
    '


    temp = downloadAbsentFile(paste("http://string.uzh.ch/permanent/string/", version, "/protein_links/",
                                    species, "__protein_links.tsv.gz", sep=""))
    PPI <- read.table(temp, sep = " ", header=TRUE, stringsAsFactors=FALSE, fill = TRUE)

    PPIselected = PPI
    if(length(score_threshold)!=0) PPIselected <- PPI[PPI$combined_score >= score_threshold,]
    if(!is.null(backgroundV)){
        PPIselected <- PPIselected[PPIselected$protein1 %in% backgroundV,]
        PPIselected <- PPIselected[PPIselected$protein2 %in% backgroundV,]
    }

    myg = igraph::graph.data.frame(PPIselected,FALSE)
    return(myg)
}



observeEvent(input$loadStringSpecies, {
    withProgress(message = 'Loading database...', value = 0.5, {
        tryCatch({
            r_data$stringdb <- STRINGdb::STRINGdb$new(version = "10", species = as.numeric(input$transnet_species), score_threshold=as.numeric(input$stringdb_threshold))
            r_data$stringdb_species <- input$transnet_species
            r_data$string_score <- as.numeric(input$stringdb_threshold)
            r_data$stringdb_g <- stringdb_get_graph(species=as.numeric(input$transnet_species), version="10", score_threshold=as.numeric(input$stringdb_threshold))
        }, error = function(e){
            session$sendCustomMessage(type = "showalert", "Stringdb loading failed.")
            return()
        })
        if(!is.null(r_data$string_meta)) r_data$string_meta <- NULL
        message_alert <- capture.output({
            if(!is.null(r_data$feature_meta$STRING_id)) {
                tbl <- r_data$feature_meta %>% dplyr::select(-STRING_id)
            } else {
                tbl <- r_data$feature_meta
            }
            string_meta <- r_data$stringdb$map(as.matrix(tbl), "cap_name", removeUnmappedRows = FALSE) # After this step, get more rows because of non-unique stringid
            #assign("test_sm", string_meta, env = .GlobalEnv)
            r_data$string_meta <- string_meta[!duplicated(string_meta$gene),] %>% dplyr::select(cap_name, gene, STRING_id) # remove duplicated(only keep first match), reduce size of the table
            !(colnames(df) %in% c("x","bar","foo"))
        })
        if(!is.null(message_alert) && length(message_alert) != 0) {
            session$sendCustomMessage(type = "showalert", message_alert)
        }
    })
})

output$loaded_interactome_info_ui <- renderUI({
    if(!is.null(r_data$stringdb) && !is.null(r_data$string_meta)) {
        loaded_species <- names(species_list[which(species_list == r_data$stringdb_species)])
        string_text <- tags$li(tags$b(paste("STRING: Current loaded species:", loaded_species, ". ", sum(!is.na(r_data$string_meta$STRING_id)), "out of", length(r_data$feature_list), "genes are successfully mapped.")))
    } else {string_text <- NULL}
    if(!is.null(r_data$reg_g)) {
        loaded_species <- names(species_list[which(species_list == input$transnet_species)])
        all_gene<-toupper(r_data$feature_list)
        reg_gene<-V(r_data$reg_g)$name
        reg_text <- tags$li(tags$b(paste("Regnetwork: Current loaded species:", loaded_species, ". ", sum(toupper(all_gene) %in% toupper(reg_gene)), "out of", length(r_data$feature_list), "genes are successfully mapped.")))
    } else {reg_text <- NULL}

    if(!is.null(string_text) || !is.null(reg_text)) {
        list(string_text, reg_text)
    } else {
        return(tags$p("No database is loaded. Please select a species and load its STRING database."))
    }
})

output$net_vis_type_ui <- renderUI({
    if(is.null(input$tf_interactome_selection)) return()
    if(input$tf_interactome_selection == "reg") return()
    selectInput("net_vis_type", "Select visualization method:", choices = list("igraph network" = "igraph", "String network (string-db.org)" = "string"), selected = "igraph")
})

output$net_vis <- renderUI({
    if(is.null(input$net_vis_type)) return()
    if(!is.null(r_data$tf_de_tbl_vis) && nrow(r_data$tf_de_tbl_vis) != 0) {
        if(nrow(r_data$tf_de_tbl_vis) > 1000){
            max_num = 1000
        } else {
            max_num = nrow(r_data$tf_de_tbl_vis)
        }

        tf_net_limit_ui <- sliderInput("tf_net_limit", "Top rank genes to be included", min = 2, max = max_num, value = 500, step = 1)
    } else {
        tf_net_limit_ui <- NULL
    }
    if(input$tf_interactome_selection == "reg" || input$net_vis_type == "igraph") {
        list(
            wellPanel(
                fluidRow(
                    column(12, tf_net_limit_ui)
                ),
                fluidRow(
                    column(4,
                           checkboxInput("tf_net_rm_isolates", "Remove isolated vertices", value = T),
                           checkboxInput("tf_net_keep_module", "Only keep largest connected module", value = T),
                           checkboxInput("tf_net_add_label", "Show gene names", value = T)
                    ),
                    column(4,
                           numericInput("tf_vscale_factor", "Vertex size scaling:", min = 0.1, max = 100, value = 1)
                    ),
                    column(4, selectInput("tf_net_layout", "Choose graph layout:", choices = list("auto" = "layout.auto", "fruchterman.reingold" = "layout.fruchterman.reingold", "kamada.kawai" = "layout.kamada.kawai", "mds" = "layout.mds", "random" = "layout.random", "circle" = "layout.circle", "sphere" = "layout.sphere")))
                )
            ) ,


            plotOutput("tf_net_all", width = "100%", height = "750px"),

            tags$p("The size of the vertices correlates with the absolute value of log fold change. Red and blue indicate direction.")
        )
    } else if(input$net_vis_type == "string") {
        list(
            wellPanel(
                uiOutput("stringdb_gene_range_ui"),
                uiOutput("plotStringNets_ui")
            ),

            htmlOutput("stringdb_vis")
        )
    }
})

output$plotStringNets_ui <- renderUI({
    if(!is.null(r_data$stringdb) && !is.null(r_data$tf_de_tbl_vis) && nrow(r_data$tf_de_tbl_vis) > 0) {
        actionButton("plotStringNets", "Generate Network Plot", class = "btn btn-info")
    } else {
        NULL
    }
})

output$stringdb_gene_range_ui <- renderUI({
    if(!is.null(r_data$tf_de_tbl_vis) && nrow(r_data$tf_de_tbl_vis) != 0) {
        if(nrow(r_data$tf_de_tbl_vis) > 350){
            max_num = 350
        } else {
            max_num = nrow(r_data$tf_de_tbl_vis)
        }

        stringdb_gene_range_ui <- sliderInput("stringdb_gene_range", label = "You can plot up to the top 350 genes", min = 1, max = max_num, value = max_num, step = 1)

    } else {
        stringdb_gene_range_ui <- NULL
    }

})

observeEvent(input$plotStringNets, {
    if(is.null(input$stringdb_gene_range) || is.null(r_data$stringdb)) return()
    withProgress(message = 'Generating graph...', value = 0.5, {
        tbl <- r_data$tf_de_tbl_vis
        message_alert <- capture.output({
            tbl <- r_data$stringdb$map(as.matrix(tbl[1:input$stringdb_gene_range,]), "gene", removeUnmappedRows = TRUE)
        })

        if(!is.null(message_alert) && length(message_alert) != 0) {
            session$sendCustomMessage(type = "showalert", message_alert)
        }

        if(nrow(tbl) == 0) {
            session$sendCustomMessage(type = "showalert", "Plotting failed because zero genes were mapped to STRING database.")
            return()
        }

        genes <- tbl$STRING_id

        link <- r_data$stringdb$get_link(genes, payload_id=NULL, required_score=r_data$string_score) # Set this to match

        r_data$stringdb_link<- paste0("<iframe src='", link, "', width='100%', height='750'>STRINGdb Visualization</iframe>")
    })
})


output$stringdb_vis <- renderUI({
    return(HTML(r_data$stringdb_link))
})

output$tf_net_all <- renderPlot({
    if(is.null(r_data$tf_de_tbl_vis) || nrow(r_data$tf_de_tbl_vis) ==0) return()
    if(is.null(input$tf_interactome_selection)) return()

    if(input$tf_interactome_selection == "stringdb") {
        if(is.null(r_data$stringdb_g)) return()
        g0 <- r_data$stringdb_g
    }
    if(input$tf_interactome_selection == "reg") g0 <- r_data$reg_g

    tbl <- r_data$tf_de_tbl_vis

    if(nrow(tbl) <=input$tf_net_limit)
        row_limit <- nrow(tbl)
    else
        row_limit <- input$tf_net_limit

    tbl <- tbl[1:row_limit,]

    if(input$tf_interactome_selection == "stringdb") {
        str_in <- r_data$string_meta$STRING_id[match(toupper(tbl$gene), r_data$string_meta$cap_name)]
        cap_id <- r_data$string_meta$cap_name[match(toupper(tbl$gene), r_data$string_meta$cap_name)]

        tbl_in<-as.character(str_in[which(str_in %in% V(g0)$name)])
        cap_id <-cap_id[which(str_in %in% V(g0)$name)]
        tbl <- tbl[match(cap_id, toupper(tbl$gene)), ]
    }

    if(input$tf_interactome_selection == "reg"){
        tbl_in <- r_data$feature_meta$cap_name[match(toupper(tbl$gene), r_data$feature_meta$cap_name)]
        tbl_in<-as.character(tbl_in[which(tbl_in %in% V(g0)$name)])
        tbl <- tbl[match(tbl_in, toupper(tbl$gene)), ]
    }


    tbl_nets <- igraph::induced_subgraph(g0, tbl_in)

    V(tbl_nets)$name <- as.character(tbl$gene[match(V(tbl_nets)$name, tbl_in)])

    range<-round(tbl$lfc_or_es[match(V(tbl_nets)$name, tbl$gene)])

    max_r <- max(range, na.rm = T)
    min_r <- min(range, na.rm = T)
    abs_max <- max(abs(max_r), abs(min_r))
    colfunc<-colorRampPalette(c("red","white","royalblue"))
    col=colfunc(abs_max * 2 + 1)
    names(col) <- as.character(seq(-abs_max, abs_max, by = 1))
    cols <- col[as.character(range)]

    lbl_size <- seq(0, 0.8, length = abs_max + 1)

    names(lbl_size) <- as.character(c(0:abs_max))

    V(tbl_nets)$label.cex = lbl_size[as.character(abs(range))] + 0.1 # give it a min so that no 0s

    V(tbl_nets)$color <- cols
    V(tbl_nets)$size <- abs(tbl$gene_score[match(V(tbl_nets)$name, tbl$gene)]) * input$tf_vscale_factor # gene_score, or LFC
    V(tbl_nets)$size[is.na(V(tbl_nets)$size)] <- 0

    if(input$tf_net_rm_isolates) {
        tbl_nets <- igraph::delete.vertices(tbl_nets, which(degree(tbl_nets) == 0))
    }

    if(input$tf_net_keep_module) {
        modules <- igraph::decompose.graph(tbl_nets)
        largest <- which.max(sapply(modules, vcount))
        tbl_nets <- modules[[largest]]
    }

    r_data$tf_tbl_net <- tbl_nets

    if(input$tf_net_add_label) {
        igraph::plot.igraph(tbl_nets, layout = get(input$tf_net_layout))
        legend("bottom", c(paste("Upregulated in Group1"), paste("Upregulated in Group2")), # Double check the direction
        pch = 16, # gives the legend appropriate symbols (lines)
        pt.cex = 2,
        horiz = T, x.intersp = 2,
        col=c("blue","red"),
        bty = "n") # gives the legend lines the correct color and width
    } else {
        igraph::plot.igraph(tbl_nets, vertex.label=NA, layout = get(input$tf_net_layout))
    }
})


observeEvent(input$tf_sum_score, {
    if(is.null(r_data$tf_de_tbl)){
        session$sendCustomMessage(type = "showalert", "Please run and load DE analysis first.")
        return()
    }
    if(is.null(r_data$tf_list)) {
        session$sendCustomMessage(type = "showalert", "Please load a transcription factor list first.")
        return()
    }

    if(input$tf_interactome_selection == "stringdb"){
        if(is.null(r_data$stringdb_g)) {
            session$sendCustomMessage(type = "showalert", "Please load STRING database first.")
            return()
        }
        g0 <- r_data$stringdb_g
    }
    if(input$tf_interactome_selection == "reg") g0 <- r_data$reg_g

    withProgress(message = 'Computing...', value = 0.1, {

        if(input$tf_interactome_nb == 0) {
            r_data$tf_g1 <- NULL
            r_data$tf_g2 <- NULL

            tfs <- r_data$tf_de_tbl[which(toupper(r_data$tf_de_tbl$gene) %in% toupper(r_data$tf_list)),]
            tfs$num_vertices <- 1

            if(nrow(tfs) == 0) {
                r_data$tf_tbl1 <- NULL
                r_data$tf_tbl2 <- NULL
                r_data$tf_neighbor_order <- NULL
                r_data$tfs <- NULL
                r_data$tf_g1 <- NULL
                r_data$tf_g2 <- NULL
                return()
            }

            r_data$tfs <- tfs

        } else {

            tbl <- r_data$tf_de_tbl

            if(input$tf_interactome_selection == "stringdb"){
                tbl$modified_gene <- tbl$gene
                tfs <- r_data$stringdb$map(as.matrix(tbl), "modified_gene", removeUnmappedRows = T)
            }
            else {
                tfs <- tbl
            }


            tfs <- tfs[which(toupper(tfs$gene) %in% toupper(r_data$tf_list)),] # calculate based on the data frame with only TFs (add rest genes scores to tf scores)

            if(nrow(tfs) == 0) {
                r_data$tf_tbl1 <- NULL
                r_data$tf_tbl2 <- NULL
                r_data$tf_neighbor_order <- NULL
                r_data$tfs <- NULL
                r_data$tf_g1 <- NULL
                r_data$tf_g2 <- NULL
                return()
            }

            influence_score <- list()
            num_vertices <- list()
            num_nonz_vertices <- list()
            prop_nonz <- list()

            cur_levs <- input$tf_interactome_nb
            for(i in 1: nrow(tfs)) {
                if(input$tf_interactome_selection == "reg") {
                    cur_mode = input$tf_nbs_direction
                    meta_tbl <- r_data$feature_meta
                } else {
                    cur_mode = "all"
                    meta_tbl <- r_data$string_meta
                }
                cur_id <- toupper(tfs$gene[[i]])

                has_error <- 0
                tryCatch({
                    result<-generate_neighbor_graph(cur_id, cur_mode, cur_levs, input$tf_influence_def, cur_quantile = input$tf_quantile, g0, meta_tbl, type = input$tf_interactome_selection,  r_data$tf_de_tbl)
                }, error = function(e){
                    has_error <<- 1
                })

                if(has_error) {
                    influence_score[[i]] <- NA
                    num_vertices[[i]] <- NA
                    num_nonz_vertices[[i]] <- NA
                    prop_nonz[[i]] <- NA
                    print(paste("TF", i, "can't be mapped"))
                } else {
                    influence_score[[i]] <- result$influence_score
                    num_vertices[[i]] <- vcount(result$tf_net)
                    num_nonz_vertices[[i]] <- result$num_nonz_vertices
                    prop_nonz[[i]] <- result$prop_nonz

                    print(paste("TF", i,"Done"))
                }
            }

            tfs$influence_score <- unlist(influence_score)

            tfs$num_vertices <- unlist(num_vertices)
            tfs$num_v_activated <- unlist(num_nonz_vertices)
            tfs$prop_activated <- unlist(prop_nonz)


            if(input$tf_interactome_selection == "stringdb") {
                r_data$tfs<-tfs %>% dplyr::select(-c(modified_gene, STRING_id))
            }

            if(input$tf_interactome_selection == "reg") {
                r_data$tfs<-tfs
            }
        }


        r_data$tfs$lfc_or_es <- as.numeric(as.character(r_data$tfs$lfc_or_es))

        r_data$tf_neighbor_order <- as.numeric(input$tf_interactome_nb)

    })

})

observe({
    if(!is.null(r_data$tfs)) {
        r_data$tfs$gene_score <- as.numeric(as.character(r_data$tfs$gene_score))
        if(r_data$tf_neighbor_order != 0 && !is.null(input$final_score_def)) {
            if(input$final_score_def == "influence_score"){
                r_data$tfs$final_score <- r_data$tfs$influence_score
            } else if(input$final_score_def == "adj_v"){
                r_data$tfs$final_score <- r_data$tfs$influence_score * log10(r_data$tfs$num_vertices)
            } else if(input$final_score_def == "adj_vth"){
                r_data$tfs$final_score <- r_data$tfs$influence_score * log10(r_data$tfs$num_v_activated)
            } else if(input$final_score_def == "adj_prop"){
                r_data$tfs$final_score <- r_data$tfs$influence_score * r_data$tfs$prop_activated
            } else if(input$final_score_def == "prop"){
                r_data$tfs$final_score <- r_data$tfs$prop_activated
            } else if(input$final_score_def == "tf_score"){
                r_data$tfs$final_score <- r_data$tfs$gene_score
            } else if(input$final_score_def == "tf_adj_v") {
                r_data$tfs$final_score <- r_data$tfs$gene_score * log10(r_data$tfs$num_vertices)
            } else if(input$final_score_def == "tf_adj_vth") {
                r_data$tfs$final_score <- r_data$tfs$gene_score * log10(r_data$tfs$num_v_activated)
            } else if(input$final_score_def == "tf_adj_prop") {
                r_data$tfs$final_score <- r_data$tfs$gene_score * r_data$tfs$prop_activated
            }

            tfs <- r_data$tfs %>%
                dplyr::filter(final_score > 0) %>%
                dplyr::arrange(-final_score)
        } else {
            tfs <- r_data$tfs %>%
                dplyr::filter(gene_score > 0) %>%
                dplyr::arrange(-gene_score)
        }


        # Graph of this net
        r_data$tf_tbl1 <- tfs %>%
            dplyr::filter(lfc_or_es > 0) %>%
            dplyr::select(-dplyr::one_of("pval", "padj", "lfc_or_es"))

        r_data$tf_tbl2 <- tfs %>%
            dplyr::filter(lfc_or_es < 0) %>%
            dplyr::select(-dplyr::one_of("pval", "padj", "lfc_or_es"))
    }
})

output$tf_tbl1_show <- DT::renderDataTable({
    if(!is.null(r_data$tf_tbl1)) {
        DT::datatable(r_data$tf_tbl1, selection = 'single', options = list(scrollX = TRUE, scrollY = "350px", searching=T, lengthMenu = c(20,50,100)))
    }
})

output$download_tf_tbl1_ui <- renderUI({
    if(is.null(r_data$tf_tbl1)) return()
    downloadButton("download_tf_tbl1", "Download", class = "btn btn-success")
})

output$download_tf_tbl1 <- downloadHandler(
    filename = "tf_tbl1.csv",
    content = function(file) {
        if(is.null(r_data$tf_tbl1)) return()

        write.csv(r_data$tf_tbl1, file)
    }
)


output$tf_net_text1 <- renderUI({
    if(is.null(r_data$tf_tbl1)) return()
    if(r_data$tf_neighbor_order == 0) return()
    s1 = as.numeric(input$tf_tbl1_show_row_last_clicked)

    if (length(s1)) {
        selected_gene1 <- r_data$tf_tbl1[s1, ]$gene
        return(tags$b(paste("Influence network of selected gene", selected_gene1)))
    } else {
        return()
    }
})

output$tf_net_download1 <- downloadHandler(
    filename = "genes_in_subgraph1.csv",
    content = function(file) {
        if(is.null(r_data$tf_g1)) return()

        write.csv(data.frame(gene = V(r_data$tf_g1)$name), file, row.names = F)
    }
)


output$tf_gp1_net_show <- renderUI({
    if(is.null(r_data$tf_tbl1)) return()
    if(r_data$tf_neighbor_order == 0) return()

    s = as.numeric(input$tf_tbl1_show_row_last_clicked)
    input$tf_net_switch1

    isolate({
        if (length(s)) {

            selected_gene <- r_data$tf_tbl1[s, ]$gene

            cur_levs <- r_data$tf_neighbor_order

            if(input$tf_interactome_selection == "reg") {
                cur_mode = input$tf_nbs_direction
                meta_tbl <- r_data$feature_meta
            } else {
                cur_mode = "all"
                meta_tbl <- r_data$string_meta
            }
            if(input$tf_interactome_selection == "stringdb") g0 <- r_data$stringdb_g
            if(input$tf_interactome_selection == "reg") g0 <- r_data$reg_g

            has_error <- 0
            tryCatch({
                result<-generate_neighbor_graph(selected_gene, cur_mode, cur_levs, input$tf_influence_def, cur_quantile = input$tf_quantile, g0, meta_tbl, type = input$tf_interactome_selection,  r_data$tf_de_tbl, only_graph = T)
            }, error = function(e){
                has_error <<- 1
            })

            if(has_error) {
                r_data$tf_g1 <- NULL
                return(tags$p("The gene cannot be mapped to the specified database."))
            }

            r_data$tf_g1 <- result$tf_net

            if(input$tf_net_switch1 %% 2 == 0){
                if(vcount(r_data$tf_g1) > 1000) {
                    return(tags$p("Doesn't support graphs with more than 1000 vertices."))
                } else {
                    plotOutput("tf_gp1_net")
                }
            }
            else {
                if(vcount(r_data$tf_g1) > 100) {
                    return(tags$p("Interactive view doesn't support graphs with more than 100 vertices."))
                } else {
                    networkD3::forceNetworkOutput("tf_gp1_forcenet")
                }
            }
        } else {
            return()
        }

    })
})


output$tf_gp1_net <- renderPlot({
    if(!is.null(r_data$tf_g1)){
        if(vcount(r_data$tf_g1) > 1000) {
            return()
        }
        igraph::plot.igraph(r_data$tf_g1, vertex.label = NA)
        legend("topleft",legend=c("Center TF", "Level 1 Neighbors", "Level 2 Neighbors")[1:(r_data$tf_neighbor_order+1)],
               col=c("#1f77b4", "#ff7f0e", "#2ca02c","#d62728")[1:(r_data$tf_neighbor_order+1)], pch=19, pt.cex = 2, bty ="n")
    }
})


output$tf_gp1_forcenet <- networkD3::renderForceNetwork({
    if(vcount(r_data$tf_g1) > 100 || vcount(r_data$tf_g1) < 2) {
        return()
    }
    g1 <- r_data$tf_g1
    d3_g1<-networkD3::igraph_to_networkD3(g1, group = V(g1)$group)
    d3_g1$nodes$size <- V(g1)$size
    networkD3::forceNetwork(Links = d3_g1$links, Nodes = d3_g1$nodes, Source = "source", Target = "target",
                 NodeID = "name", Nodesize = "size", Group = "group", legend = T,
                 radiusCalculation = "Math.sqrt(d.nodesize) * 2",
                 opacity = .8, zoom = T,  colourScale = networkD3::JS("d3.scaleOrdinal(d3.schemeCategory10)"))
})



output$tf_tbl3_show <- DT::renderDataTable({
    if(!is.null(r_data$tf_tbl2)) {
        DT::datatable(r_data$tf_tbl2, selection = 'single', options = list(scrollX = TRUE, scrollY = "350px", searching=T, lengthMenu = c(20,50,100)))
    }
})

output$download_tf_tbl2_ui <- renderUI({
    if(is.null(r_data$tf_tbl2)) return()
    downloadButton("download_tf_tbl2", "Download", class = "btn btn-success")
})

output$download_tf_tbl2 <- downloadHandler(
    filename = "tf_tbl2.csv",
    content = function(file) {
        if(is.null(r_data$tf_tbl2)) return()

        write.csv(r_data$tf_tbl2, file)
    }
)


output$tf_net_text2 <- renderUI({
    if(is.null(r_data$tf_tbl2)) return()
    if(r_data$tf_neighbor_order == 0) return()
    s2 = as.numeric(input$tf_tbl3_show_row_last_clicked)

    if (length(s2)) {
        selected_gene2 <- r_data$tf_tbl2[s2, ]$gene
        return(tags$b(paste("Influence network of selected gene", selected_gene2)))
    } else {
        return()
    }
})

output$tf_net_download2 <- downloadHandler(
    filename = "genes_in_subgraph2.csv",
    content = function(file) {
        if(is.null(r_data$tf_g2)) return()

        write.csv(data.frame(gene = V(r_data$tf_g2)$name), file, row.names = F)
    }
)


output$tf_gp2_net_show <- renderUI({
    if(is.null(r_data$tf_tbl1)) return()
    if(r_data$tf_neighbor_order == 0) return()

    s = as.numeric(input$tf_tbl3_show_row_last_clicked)
    input$tf_net_switch2

    isolate({
        if (length(s)) {

            selected_gene <- r_data$tf_tbl2[s, ]$gene

            cur_levs <- r_data$tf_neighbor_order

            if(input$tf_interactome_selection == "reg") {
                cur_mode = input$tf_nbs_direction
                meta_tbl <- r_data$feature_meta
            } else {
                cur_mode = "all"
                meta_tbl <- r_data$string_meta
            }
            if(input$tf_interactome_selection == "stringdb") g0 <- r_data$stringdb_g
            if(input$tf_interactome_selection == "reg") g0 <- r_data$reg_g

            has_error <- 0
            tryCatch({
                result<-generate_neighbor_graph(selected_gene, cur_mode, cur_levs, input$tf_influence_def, cur_quantile = input$tf_quantile, g0, meta_tbl, type = input$tf_interactome_selection,  r_data$tf_de_tbl, only_graph = T)
            }, error = function(e){
                has_error <<- 1
            })

            if(has_error) {
                r_data$tf_g2 <- NULL
                return(tags$p("The gene cannot be mapped to the specified database."))
            }

            r_data$tf_g2 <- result$tf_net

            if(input$tf_net_switch2 %% 2 == 0){
                if(vcount(r_data$tf_g2) > 1000) {
                    return(tags$p("Doesn't support graphs with more than 1000 vertices."))
                } else {
                    plotOutput("tf_gp2_net")
                }
            }
            else {
                if(vcount(r_data$tf_g2) > 100) {
                    return(tags$p("Interactive view doesn't support graphs with more than 100 vertices."))
                } else {
                    networkD3::forceNetworkOutput("tf_gp2_forcenet")
                }
            }
        } else {
            return()
        }

    })
})


output$tf_gp2_net <- renderPlot({
    if(!is.null(r_data$tf_g2)){
        if(vcount(r_data$tf_g2) > 1000) {
            return()
        }
        igraph::plot.igraph(r_data$tf_g2, vertex.label = NA)
        legend("topleft",legend=c("Center TF", "Level 1 Neighbors", "Level 2 Neighbors")[1:(r_data$tf_neighbor_order+1)],
               col=c("#1f77b4", "#ff7f0e", "#2ca02c","#d62728")[1:(r_data$tf_neighbor_order+1)], pch=19, pt.cex = 2, bty ="n")
    }
})


output$tf_gp2_forcenet <- networkD3::renderForceNetwork({
    if(vcount(r_data$tf_g2) > 100 || vcount(r_data$tf_g2) < 2) {
        return()
    }
    g2 <- r_data$tf_g2
    d3_g2<-networkD3::igraph_to_networkD3(g2, group = V(g2)$group)
    d3_g2$nodes$size <- V(g2)$size
    networkD3::forceNetwork(Links = d3_g2$links, Nodes = d3_g2$nodes, Source = "source", Target = "target",
                 NodeID = "name", Nodesize = "size", Group = "group", legend = T,
                 radiusCalculation = "Math.sqrt(d.nodesize) * 2",
                 opacity = .8, zoom = T,  colourScale = networkD3::JS("d3.scaleOrdinal(d3.schemeCategory10)"))
})




