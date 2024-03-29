
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


#' PIVOT generate relative frequency table given counts data
#'
#' @export
generateRelativeFreq <- function(raw){
    as.data.frame(apply(raw,2,function(col) col/sum(col)))
}

#' PIVOT data scale module
#'
#' @description
#' Controls which transformed data should be used as input for analysis modules, this is the UI part of the module.
#'
#' @export
pivot_dataScale_UI <- function(id, include = c("Counts (raw)", "Counts (normalized)", "Relative Frequency", "Log10 Counts", "Standardized Counts", "Log10 & Standardized", "Projection Matrix"), selected = "Counts (normalized)", width = 4, order = F) {
    ns<- NS(id)
    names(include) <- include
    proj_lists <- c("PCA", "t-SNE", "MDS", "Nonmetric-MDS", "DiffusionMap")
    names(proj_lists) <- proj_lists
    if("Projection Matrix" %in% include) {
        tagList(
            column(width = width/3, selectInput(ns("data_scale"), label = "Data Input",
                                              choices = as.list(include), selected = selected)),
            conditionalPanel(sprintf("input['%s'] == 'Projection Matrix'", ns("data_scale")),
                             column(width = width/3,
                                    selectInput(ns("proj_matrix"), label = "Choose Projection", choices = proj_lists)
                             ),
                             column(width = width/3,
                                    uiOutput(ns("pc_choice"))
                             )
            )
        )
    } else {
        if(order) {
            tagList(
                column(width = width/2, selectInput(ns("data_scale"), label = "Data Input",
                                                  choices = as.list(include), selected = selected)),
                column(width = width/2, selectInput(ns("data_order"), label = "Order features by",
                            choices = list("None" = "none", "Variance" = "variance", "Fano Factor" = "fano_factor", "Row Average" = "row_average", "Row Median" = "row_median"),
                            selected = "none"))
            )
        } else {
            tagList(
                column(width = width, selectInput(ns("data_scale"), label = "Data Input",
                                                  choices = as.list(include), selected = selected))
            )
        }
    }
}


#' PIVOT data scale module
#'
#' @description
#' Controls which transformed data should be used as input for analysis modules, this is the server part of the module.
#'
#' @import dplyr tibble
#' @export
pivot_dataScale <- function(input, output, session, r_data, order=F, keep_stats = FALSE) {

    output$pc_choice <- renderUI({
        req(input$data_scale == "Projection Matrix")
        if(input$proj_matrix == "PCA") {
            req(r_data$pca)
            pcs<-colnames(r_data$pca$x)
            names(pcs) <- pcs
            selectInput(session$ns("pca_pcs"), label = "Choose PC", choices = pcs, multiple = T)
        } else if(input$proj_matrix == "DiffusionMap") {
            req(r_data$dfm)
            dcs<-colnames(r_data$dfm@eigenvectors)
            names(dcs) <- dcs
            selectInput(session$ns("dfm_dcs"), label = "Choose DC", choices = dcs, multiple = T)
        } else {
            dims<-c("2D", "3D")
            names(dims) <- dims
            selectInput(session$ns("dims"), label = "Choose Dimension", choices = dims)
        }
    })

    data0 <- reactive({
        if(is.null(input$data_scale) || is.null(r_data$df)) return(NULL)
        if(input$data_scale != "Projection Matrix") {
            raw <- r_data$raw
            df <- r_data$df
            log <- log10(df + 1)
            nm <- as.data.frame(t(scale(t(df))))
            log_nm <- as.data.frame(t(scale(t(log10(df + 1)))))

            if(input$data_scale == "Counts (normalized)")
                new_df <- df
            else if(input$data_scale == "Log10 Counts")
                new_df <- log
            else if(input$data_scale == "Standardized Counts")
                new_df <- nm
            else if(input$data_scale == "Log10 & Standardized")
                new_df <- log_nm
            else if(input$data_scale == "Counts (raw)")
                new_df <- raw
            else if(input$data_scale == "Relative Frequency")
                new_df <- generateRelativeFreq(raw)

            if(order && !is.null(input$data_order)) {
                if(input$data_order == "none") {
                    new_df <- new_df %>% tibble::rownames_to_column("feature")
                } else if(input$data_order == "variance") {
                    new_df <- new_df %>%
                        tibble::rownames_to_column("feature") %>%
                        dplyr::mutate(variance = apply(new_df,1,var)) %>% # Compute variance of feature across sample
                        dplyr::arrange(desc(variance))
                } else if(input$data_order == "row_average") {
                    new_df <- new_df %>%
                        tibble::rownames_to_column("feature") %>%
                        dplyr::mutate(average = apply(new_df,1,mean)) %>%
                        dplyr::arrange(desc(average))
                } else if(input$data_order == "fano_factor"){
                    new_df <- new_df %>%
                        tibble::rownames_to_column("feature") %>%
                        dplyr::mutate(average = apply(new_df,1,mean)) %>%
                        dplyr::mutate(variance = apply(new_df,1,var)) %>% # Compute variance of feature across sample
                        dplyr::mutate(fano_factor = variance/average) %>%
                        dplyr::arrange(desc(fano_factor))
                } else if(input$data_order == "row_median") {
                    new_df <- new_df %>%
                        tibble::rownames_to_column("feature") %>%
                        dplyr::mutate(median = apply(new_df,1,median)) %>%
                        dplyr::arrange(desc(median))
                }

                rownames(new_df) <- new_df$feature
                new_df <- new_df %>% dplyr::select(-feature)
                if(!keep_stats) {
                    new_df <- new_df[,!(colnames(new_df)%in%c("variance", "average", "fano_factor", "median"))]
                }
            }
            return(new_df)
        } else {
            if(input$proj_matrix == "PCA") {
                if(is.null(r_data$pca)) {
                    session$sendCustomMessage(type = "showalert", "Please run PCA first.")
                    return()
                } else {
                    if(is.null(input$pca_pcs)) {
                        return(as.data.frame(t(r_data$pca$x)))
                    } else {
                        return(as.data.frame(t(r_data$pca$x[,input$pca_pcs, drop = F])))
                    }
                }
            } else if(input$proj_matrix == "t-SNE") {
                if(is.null(r_data$tsne)) {
                    session$sendCustomMessage(type = "showalert", "Please run t-SNE first.")
                    return()
                } else {
                    req(input$dims)
                    if(input$dims == "2D") {
                        return(t(r_data$tsne$tsne_2d))
                    } else {
                        return(t(r_data$tsne$tsne_3d))
                    }
                }
            } else if(input$proj_matrix == "MDS") {
                if(is.null(r_data$mds)) {
                    session$sendCustomMessage(type = "showalert", "Please run MDS first.")
                    return()
                } else {
                    req(input$dims)
                    if(input$dims == "2D") {
                        return(t(r_data$mds$mds_2d))
                    } else {
                        return(t(r_data$mds$mds_3d))
                    }
                }
            } else if(input$proj_matrix == "Nonmetric-MDS") {
                if(is.null(r_data$nds)) {
                    session$sendCustomMessage(type = "showalert", "Please run nonmetric-MDS first.")
                    return()
                } else {
                    req(input$dims)
                    if(input$dims == "2D") {
                        return(t(as.data.frame(r_data$nds$nds_2d$points)))
                    } else {
                        return(t(as.data.frame(r_data$nds$nds_3d$points)))
                    }
                }
            } else if(input$proj_matrix == "DiffusionMap") {
                print("test")
                if(is.null(r_data$dfm)) {
                    session$sendCustomMessage(type = "showalert", "Please run diffusion map first.")
                    return()
                } else {
                    if(is.null(input$dfm_dcs)) {
                        return(as.data.frame(t(r_data$dfm@eigenvectors)))
                    } else {
                        return(as.data.frame(t(r_data$dfm@eigenvectors[,input$dfm_dcs, drop = F])))
                    }
                }
            } else {
                return()
            }
        }
    })

    return(reactive(list(
        df = data0(),
        scale = input$data_scale,
        order = input$data_order
    )))
}

#' a wrapper module for scaling and range selection for heatmap
#'
#' @description
#' This is the UI part of the module.
#' @export
pivot_dataScaleRange_UI <- function(id, bound = 500, value = 100, include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts", "Standardized Counts", "Log10 & Standardized")) {
    ns<- NS(id)

    if (bound > 500) {
        max_bound = 500
    } else {
        max_bound = bound
    }

    tagList(
        fluidRow(
            pivot_dataScale_UI(ns("hm_scale"), include = include, selected = "Log10 Counts", width = 6, order = T),
            column(6, sliderInput(ns("feature_range"), label = "Rank Range", min = 1, max = max_bound, value = c(1, value), step = 1))
        ),
        fluidRow(
            column(3,tags$b("Manually input a range:")),
            column(3, uiOutput(ns("min_rank_ui"))),
            column(3, uiOutput(ns("max_rank_ui"))),
            column(3, actionButton(ns("update_range"), "Update Range", class = "btn btn-info"))
        )
    )
}

#' a wrapper module for scaling and range selection for heatmap
#'
#' @description
#' This is the server part of the module.
#' @export
pivot_dataScaleRange <- function(input, output, session, r_data, keep_stats = FALSE) {

    output$min_rank_ui <- renderUI({
        req(r_data$df, input$feature_range)
        feature_num = nrow(r_data$df)
        numericInput_1(session$ns("min_rank"), label = "Min:", value = input$feature_range[1], min = 1, max = feature_num - 1, step = 1)
    })

    output$max_rank_ui <- renderUI({
        req(r_data$df, input$feature_range)
        feature_num = nrow(r_data$df)
        numericInput_1(session$ns("max_rank"), label = "Max:", value = input$feature_range[2], min = 2, max = feature_num, step = 1)
    })

    observeEvent(input$update_range, {
        req(r_data$df, input$min_rank, input$max_rank)
        feature_num = nrow(r_data$df)
        if(input$min_rank <= feature_num & input$max_rank <= feature_num) {
            if(input$max_rank - input$min_rank + 1 < 2) {
                session$sendCustomMessage(type = "showalert", "Max must be larger than min by at least 1!")
                return()
            }
            if(input$max_rank - input$min_rank + 1 > 10000) {
                session$sendCustomMessage(type = "showalert", "You specified more than 10000 features, please choose less.")
                return()
            }
            updateSliderInput(session, "feature_range",
                              label = "Rank Range",
                              min = input$min_rank, max = input$max_rank, value = c(input$min_rank, input$max_rank), step = 1)
        } else {
            session$sendCustomMessage(type = "showalert", "You don't have that many features in the data set.")
            return()
        }
    })

    hmapList <- callModule(pivot_dataScale, "hm_scale", r_data, keep_stats = keep_stats, order = T)
    flist <- callModule(pivot_featureInputModal, "ft_hmap", r_data = r_data)

    data0 <- reactive({
        hm_data <- hmapList()$df
        if(is.null(hm_data) || is.null(input$feature_range)) return()
        if(input$feature_range[2] > nrow(hm_data)) {
            return(NULL)
        }
        rs<-hm_data[input$feature_range[1]:input$feature_range[2],]
        if(nrow(rs) == 0) {
            return(NULL)
        } else {
            return(rs)
        }
    })

    return(reactive({
        if(is.null(data0())){
            return()
        } else {
            list(
                df = data0(),
                order = hmapList()$order,
                range = c(input$feature_range[1],input$feature_range[2])
            )
        }
    }))
}




#' shiny module for choosing feature set generated by DE or custom input
#'
#' @description
#' This is the ui part of the module.
#' @export
pivot_featureList_UI <- function(id, include = c("custom", "deseq", "edgeR", "scde", "monocle", "mwu"), selected = "custom", width = 8) {
    ns<- NS(id)
    map_include <- list(
        "Custom gene list" = "custom",
        "DE genes (DESeq)" = "deseq",
        "DE genes (edgeR)" = "edgeR",
        "DE genes (SCDE)" = "scde",
        "DE genes (monocle)" = "monocle",
        "DE genes (Mann-Whitney)" = "mwu"
    )
    options <- map_include[which(map_include %in% include)]
    tagList(
        column(width/2,
               selectInput(ns("feature_input"), "Feature Set", choices = options, selected = selected)
        ),
        conditionalPanel(
            sprintf("input['%s'] == 'custom'", ns("feature_input")),
            column(width/2,
                   tags$br(),
                   pivot_featureInputModal_UI(ns("custom_modal"), "Input custom gene list")
            )
        ),
        conditionalPanel(
            sprintf("input['%s'] != 'custom'", ns("feature_input")),
            column(width/4,
                   numericInput(ns("feature_alpha"), "Padj", value = 0.1, min = 0, max = 0.5, step = 0.001)
            ),
            column(width/4,
                   selectInput(ns("feature_lfc"), "LFC", choices = list("All DEG" = "all", "LFC>0" = "plus", "LFC<0" = "minus"))
            )
        )
    )
}


#' shiny module for choosing feature set generated by DE or custom input
#'
#' @description
#' This is the server part of the module.
#' @export
pivot_featureList <- function(input, output, session, r_data) {

    feature_tbl <- reactive({
        req(input$feature_input)
        if(input$feature_input == "scde") {
            req(input$feature_alpha)
            if(is.null(r_data$scde_results)) return()
            tbl<-r_data$scde_results %>% tibble::rownames_to_column("gene") %>%
                dplyr::select(gene, lfc_or_es = mle, pval = p.values, padj = p.values.adj) %>% dplyr::filter(padj <= input$feature_alpha)
        } else if(input$feature_input == "deseq") {
            req(input$feature_alpha)
            if(is.null(r_data$deseq_results)) return()
            tbl <- as.data.frame(r_data$deseq_results) %>% tibble::rownames_to_column("gene") %>%
                dplyr::select(gene, lfc_or_es = log2FoldChange, pval = pvalue, padj = padj) %>% dplyr::filter(padj <= input$feature_alpha)
        } else if(input$feature_input == "mwu") {
            req(input$feature_alpha)
            if(is.null(r_data$mww_results)) return()
            tbl <- r_data$mww_results %>% tibble::rownames_to_column("gene") %>%
                dplyr::select(gene, lfc_or_es = effectSize, pval = P.value, padj = adjustedP) %>% dplyr::filter(padj <= input$feature_alpha)
        } else if(input$feature_input == "edgeR") {
            req(input$feature_alpha)
            if(is.null(r_data$edgeR_results)) return()
            tbl <- r_data$edgeR_results$table %>% tibble::rownames_to_column("gene") %>%
                dplyr::select(gene, lfc_or_es = logFC, pval = PValue, padj = FDR) %>% dplyr::filter(padj <= input$feature_alpha)
        } else if(input$feature_input == "monocle") {
            req(input$feature_alpha)
            if(is.null(r_data$monocle_results)) return()
            tbl <- r_data$monocle_results %>% tibble::rownames_to_column("gene") %>%
                dplyr::select(gene, pval = pval, padj = qval) %>% dplyr::filter(padj <= input$feature_alpha)
        } else if(input$feature_input == "custom") {
            res <- callModule(pivot_featureInputModal, "custom_modal", r_data = r_data)
            tbl <- data.frame(gene = res())
        }
        if(!is.null(tbl$lfc_or_es)) {
            if(input$feature_lfc == "plus") {
                tbl <-tbl %>% dplyr::filter(lfc_or_es > 0)
            } else if(input$feature_lfc == "minus") {
                tbl <- tbl %>% dplyr::filter(lfc_or_es < 0)
            }
        }
        tbl
    })
    return(feature_tbl)
}





