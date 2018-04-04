
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


# Modules for group input of DE analysis


#' Choose the design model for DE analysis
#'
#' @description
#' This module allow user choose which model, e.g., "~ condition", "~condition + batch"  or "timecourse", should be used for DE analysis,
#' and which meta data column should be used as the covariates.
#'
#' @export
pivot_deGroupBy_UI <- function(id, meta, width = 12, reduced = c("no","yes","maybe"), model = c("condition", "condition_batch", "custom")) {
    if(is.null(meta) || ncol(meta) < 2) return()
    ns<- NS(id)
    categories = colnames(meta)[-1]
    names(categories) <- categories
    options <- as.list(categories)

    models <- list()
    if("condition" %in% model) {
        models <- c(models, "~ Condition" = "condition")
    }
    if("condition_batch" %in% model) {
        models <- c(models, "Full: ~ Condition + Batch, Reduced: ~ Batch" = "condition_batch")
    }
    if("custom" %in% model) {
        models <- c(models, "Use custom design formula" = "custom")
    }

    if(reduced == "yes") {
        reduced_ui <- textInput(ns("reduced_formula"), "Reduced Model Formula (e.g., ~A+B)", placeholder = "Required")
    } else if(reduced == "maybe") {
        reduced_ui <- textInput(ns("reduced_formula"), "Reduced Model Formula (e.g., ~A+B)", placeholder = "Required for certain test.")
    } else {
        reduced_ui <- NULL
    }

    list(
        column(width/3,
               selectInput(ns("design_set"), "Experiment Design",
                           choices = as.list(
                               models
                           )
               )
        ),
        column(width/3,
               conditionalPanel(sprintf("input['%s'] == 'condition' || input['%s'] == 'condition_batch'", ns("design_set"), ns("design_set")),
                                selectInput(ns("condition"), label = "Condition:", choices = options),
                                uiOutput(ns("condition_text"))
               ),
               conditionalPanel(sprintf("input['%s'] == 'custom'", ns("design_set")),
                                tagList(
                                    tags$br(),
                                    actionButton(ns("formula_modal_btn"), label = "Custom Formula", class = "btn-warning"),
                                    shinyBS::bsModal(id = ns("formula_modal"), "Custom Formula", ns("formula_modal_btn"),
                                                     tags$p(paste("Valid terms: ", paste(c("0", "1", categories), collapse = "; "))),
                                                     textInput(ns("full_formula"), "Full Model Formula (e.g., ~A+B+A:B)", placeholder = "Required"),
                                                     reduced_ui,
                                                     actionButton(ns("custom_formula_submit"), "Submit Design", class = "btn-info")
                                    )
                                )
               )
        ),
        column(width/3,
               conditionalPanel(sprintf("input['%s'] == 'condition_batch'", ns("design_set")),
                                selectInput(ns("batch"), label = "Batch:", choices = options),
                                uiOutput(ns("batch_text"))
               ),
               conditionalPanel(sprintf("input['%s'] == 'custom'", ns("design_set")),
                                uiOutput(ns("custom_formula_text"))
               )
        )
    )
}

#' Choose the design model for DE analysis
#'
#' @description
#' This is the server part of the module
#'
#' @export
pivot_deGroupBy <- function(input, output, session, meta, reduced = c("no","yes","maybe")) {
    sanity <- reactiveValues()
    sanity$condition <- 1
    sanity$batch <- 1

    cformula <- reactiveValues()
    cformula$full <- NULL
    cformula$reduced <- NULL

    output$condition_text <- renderUI({
        req(input$design_set, input$condition)

        groups = meta[,input$condition]
        if(any(is.na(groups)) || any(groups == "")) {
            sanity$condition <- 0
            return(tags$p("'NA' or '' detected. Please specify a valid group name or remove these samples."))
        }
        if(length(unique(groups)) < 2) {
            sanity$condition <- 0
            return(tags$p("Only one group in this category."))
        }

        # All conditions checks pass
        sanity$condition <- 1

        group_uniq <- as.character(unique(groups))
        return(tags$p(paste0(length(group_uniq), " groups (",  paste(group_uniq, collapse = " "), ") found in the current category.")))
    })

    # Function from DESeq2 (checkFullRank)
    checkRank <- function (modelMatrix)
    {
        if (qr(modelMatrix)$rank < ncol(modelMatrix)) {
            if (any(apply(modelMatrix, 2, function(col) all(col ==
                                                            0)))) {
                msg <- ("the model matrix is not full rank, so the model cannot be fit as specified.\n  Levels or combinations of levels without any samples have resulted in\n  column(s) of zeros in the model matrix.\n")
            }
            else {
                msg <- ("the model matrix is not full rank, so the model cannot be fit as specified.\n  One or more variables or interaction terms in the design formula are linear\n  combinations of the others and must be removed.\n")
            }
            return(FALSE)
        } else {
            return(TRUE)
        }
    }

    observeEvent(input$custom_formula_submit, {
        req(input$full_formula)

        if(!is.null(cformula$full)) { #Reset
            cformula$full <- NULL
            cformula$reduced <- NULL
        }

        if(is.null(input$reduced_formula) || nchar(input$reduced_formula) == 0) {
            if(reduced == "yes") {
                session$sendCustomMessage(type = "showalert", "Reduced formula is required.")
                return()
            }
        }

        categories = colnames(meta)[-1]
        names(categories) <- categories
        valid_terms <- categories

        error_I <- 0
        tryCatch({
            full_terms <- all.vars(as.formula(input$full_formula))
            reduced_terms <- NULL
            if(!is.null(input$reduced_formula) && nchar(input$reduced_formula) != 0) {
                reduced_terms <- all.vars(as.formula(input$reduced_formula))
            }
        },
        error = function(e) {
            session$sendCustomMessage(type = "showalert", "Your input cannot be recognized as valid R formula.")
            error_I <<- 1
        })

        if(error_I) {
            return()
        }

        if(!all(full_terms %in% valid_terms)){
            session$sendCustomMessage(type = "showalert", "Your full model formula contain invalid terms.")
            return()
        }

        if(!is.null(reduced_terms)) {
            if(!all(reduced_terms %in% valid_terms)) {
                session$sendCustomMessage(type = "showalert", "Your reduced model formula contain invalid terms.")
                return()
            }
        }

        cformula$full <- as.formula(input$full_formula)
        if(!is.null(reduced_terms)) {
            cformula$reduced <- as.formula(input$reduced_formula)
        }
    })

    output$custom_formula_text <- renderUI({
        if(is.null(cformula$full)) return()
        if(!is.null(cformula$reduced)) {
            tagList(
                tags$b("Full formula:"),
                tags$p(Reduce(paste, deparse(cformula$full))),
                tags$b("Reduced formula:"),
                tags$p(Reduce(paste, deparse(cformula$reduced)))
            )
        } else {
            tagList(
                tags$b("Full formula:"),
                tags$p(Reduce(paste, deparse(cformula$full)))
            )
        }
    })

    output$batch_text <- renderUI({
        req(input$design_set, input$condition, input$batch)

        batches = meta[,input$batch]
        groups = meta[, input$condition]
        if(sanity$condition == 0) {
            return(tags$p("Please correct condition input first."))
        }
        if(input$batch == input$condition) {
            sanity$batch <- 0
            return(tags$p("Batch cannot be the same as condition."))
        }
        if(any(is.na(batches)) || any(batches == "")) {
            sanity$batch <- 0
            return(tags$p("'NA' or '' detected. Please specify a valid group name or remove these samples."))
        }
        if(length(unique(batches)) < 2) {
            sanity$batch <- 0
            return(tags$p("Only one group in this category."))
        }
        # Detect perfect confounding
        df <- data.frame(group=groups, batch=batches)
        mm <- model.matrix(~batch+group, df)
        if(!checkRank(mm)){
            sanity$batch <- 0
            return(tags$p("The model matrix is not full rank. Please check design again."))
        }

        # All batch checks pass, reset to ok
        sanity$batch <- 1

        batch_uniq <- as.character(unique(batches))
        return(tags$p(paste0(length(batch_uniq), " batches (",  paste(batch_uniq, collapse = " "), ") found in the current category.")))
    })


    returnList<-reactive({
        req(input$design_set)
        if(input$design_set == "condition") {
            if(sanity$condition == 0) return()
            return(list(
                design = "condition",
                model = list(
                    full = as.formula(paste0("~", input$condition)),
                    reduced = formula(~1)
                )
            ))
        } else if(input$design_set == "condition_batch") {
            if(sanity$condition == 0 || sanity$batch == 0) return()
            return(list(
                design = "condition_batch",
                model = list(
                    full = as.formula(paste0("~", input$condition, "+", input$batch)),
                    reduced = as.formula(paste0("~", input$batch))
                )
            ))
        } else if(input$design_set == "custom") {
            if(is.null(cformula$full)) return()
            return(list(
                design = "custom",
                model = list(
                    full = cformula$full,
                    reduced = cformula$reduced
                )
            ))
        } else {
            return()
        }
    })
    return(returnList)

}
