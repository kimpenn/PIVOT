

# Modules for group input of DE analysis


#' Choose the design model for DE analysis
#'
#' @description
#' This module allow user choose which model, e.g., "~ condition", "~condition + batch"  or "timecourse", should be used for DE analysis,
#' and which meta data column should be used as the covariates.
#'
#' @export
pivot_deGroupBy_UI <- function(id, meta, width = 12, method = c("deseq", "scde", "mww"), model = c("condition", "condition_batch", "timecourse1", "timecourse2")) {
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
    if("timecourse2" %in% model) {
        models <- c(models, "Full: ~ Condition + Time, Reduced: ~ Time" = "timecourse2")
    }
    if("timecourse1" %in% model) {
        models <- c(models, "Full: ~ Condition + Time + Condition:Time, Reduced: ~ Condition + Time" = "timecourse1")
    }

    list(
        column(4,
               selectInput(ns("design_set"), "Experiment Design",
                           choices = as.list(
                               models
                           )
               )
        ),
        column(4,
               selectInput(ns("condition"), label = "Condition:", choices = options),
               uiOutput(ns("condition_text"))
        ),
        column(4,
               conditionalPanel(sprintf("input['%s'] == 'condition_batch'", ns("design_set")),
                                selectInput(ns("batch"), label = "Batch:", choices = options),
                                uiOutput(ns("batch_text"))
               ),
               conditionalPanel(sprintf("input['%s'] == 'timecourse1' || (input['%s'] == 'timecourse2')", ns("design_set"), ns("design_set")),
                                selectInput(ns("time"), label = "Time:", choices = options),
                                uiOutput(ns("time_text"))
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
pivot_deGroupBy <- function(input, output, session, r_data, method = c("deseq", "scde")) {
    sanity <- reactiveValues()
    sanity$condition <- 1
    sanity$batch <- 1
    sanity$time <- 1

    output$condition_text <- renderUI({
        req(input$design_set, input$condition)

        groups = r_data$meta[,input$condition]
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
                msg <- ("the model matrix is not full rank, so the model cannot be fit as specified.\n  Levels or combinations of levels without any samples have resulted in\n  column(s) of zeros in the model matrix.\n\n  Please read the vignette section 'Model matrix not full rank':\n\n  vignette('DESeq2')")
            }
            else {
                msg <- ("the model matrix is not full rank, so the model cannot be fit as specified.\n  One or more variables or interaction terms in the design formula are linear\n  combinations of the others and must be removed.\n\n  Please read the vignette section 'Model matrix not full rank':\n\n  vignette('DESeq2')")
            }
            return(FALSE)
        } else {
            return(TRUE)
        }
    }

    output$batch_text <- renderUI({
        req(input$design_set, input$condition, input$batch)

        batches = r_data$meta[,input$batch]
        groups = r_data$meta[, input$condition]
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

    output$time_text <- renderUI({
        req(input$design_set, input$condition,input$time)
        timecourse = r_data$meta[,input$time]
        groups = r_data$meta[, input$condition]
        if(sanity$condition == 0) {
            return(tags$p("Please correct condition input first."))
        }
        if(input$time == input$condition) {
            sanity$time <- 0
            return(tags$p("Time cannot be the same as condition."))
        }
        if(any(is.na(timecourse)) || any(timecourse == "")) {
            sanity$time <- 0
            return(tags$p("'NA' or '' detected. Please specify a valid group name or remove these samples."))
        }
        if(length(unique(timecourse)) < 2) {
            sanity$time <- 0
            return(tags$p("Only one group in this category."))
        }
        # Detect perfect confounding
        df <- data.frame(group=groups, time=timecourse)
        # Check rank
        if(input$design_set == "timecourse1") {
            mm <- model.matrix(~ group + time + group:time, df)
        } else if(input$design_set == "timecourse2") {
            mm <- model.matrix(~ group + time, df)
        }

        if(!checkRank(mm)){
            sanity$time <- 0
            return(tags$p("The model matrix is not full rank. Please check design again."))
        }

        # Pass test
        sanity$time <- 1
        time_uniq <- as.character(unique(timecourse))
        return(tags$p(paste0(length(time_uniq), " time points (",  paste(time_uniq, collapse = " "), ") found in the current category.")))
    })

    returnList<-reactive({
        req(input$design_set)
        if(input$design_set == "condition") {
            if(sanity$condition == 0) return()
            return(list(design = "condition", model = list(full = formula(~ group), reduced = formula(~1)), group_cate = input$condition))
        } else if(input$design_set == "condition_batch") {
            if(sanity$condition == 0 || sanity$batch == 0) return()
            return(list(design = "condition_batch", model = list(full = formula(~ batch + group), reduced = formula(~ batch)), group_cate = input$condition, batch_cate = input$batch))
        } else if(input$design_set == "timecourse1") {
            if(sanity$condition == 0 || sanity$time == 0) return()
            return(list(design = "timecourse1", model = list(full = formula(~ group + timecourse + group:timecourse), reduced = formula(~ group + timecourse)), group_cate = input$condition, time_cate = input$time))
        } else if(input$design_set == "timecourse2") {
            if(sanity$condition == 0 || sanity$time == 0) return()
            return(list(design = "timecourse2", model = list(full = formula(~ group + timecourse), reduced = formula(~ timecourse)), group_cate = input$condition, time_cate = input$time))
        } else {
            return()
        }
    })
    return(returnList)

}
