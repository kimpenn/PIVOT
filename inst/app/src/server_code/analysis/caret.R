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



######### caret #########

output$caret_ui <- renderUI({
    req(r_data$df)
    tagList(
        enhanced_box(
            width = 12,
            title = "Classification with Caret",
            id = "caret",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("General Settings:"), class = "param_setting_title"),
            fluidRow(
                pivot_dataScale_UI("caret", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts", "Standardized Counts", "Log10 & Standardized", "Projection Matrix"), selected = "Log10 Counts", width = 9),
                pivot_groupBy_UI("caret", r_data$category, append_sample = F, choose_color = F, width = 3)
            ),
            fluidRow(
                column(3, numericInput("caret_seed", label = "Set seed", value = 1, min = 1, max = 5000, step = 1)),
                column(3, uiOutput("caret_train_method_ui")),
                column(3, numericInput("caret_cv_fold", label = "Cross-validation fold", value = 6, min = 2, max = 20, step = 1)),
                column(3, numericInput("caret_cv_repeat", label = "Cross-validation repeat", value = 6, min = 2, max = 20, step = 1))
            ),
            tags$li("Note this module only performs basic classification task with caret. We'll add more parameter choices in future release."),
            tags$li("Please pay attention to the background R session, as some modeling methods require installation of new R packages. Select 1 (yes) if necessary.")
        ),
        enhanced_box(
            width = 12,
            title = NULL,
            status = "primary",
            solidHeader = F,
            tags$div(tags$b("Training"), class = "param_setting_title"),
            #tags$p("This module requires you have less than 500 features in the current dataset. Some modeling method require additional packages to be installed. In such cases, you will have to select yes (1) or no (2) in the background R session. "),
            fluidRow(
                column(4,
                       box(
                           width = NULL,
                           status = "danger",
                           uiOutput("caret_train_gp_ui"),
                           actionButton('caret_train_gp_btn', label = "Train", class = "btn btn-info btn_rightAlign"),
                           tags$b("Training Data"),
                           DT::dataTableOutput("caret_training_tbl")
                       )
                ),
                column(4,
                       box(title = "Modeling Result",
                           status = "warning",
                           width = NULL,
                           verbatimTextOutput("caret_model_result")
                       )
                ),
                column(4,
                       box(title = "Feature Coefficient",
                           status = "success",
                           width = NULL,
                           DT::dataTableOutput("caret_model_coef")
                       )
                )
            )
        ),
        enhanced_box(
            width = 12,
            title = NULL,
            status = "primary",
            solidHeader = F,
            tags$div(tags$b("Testing"), class = "param_setting_title"),
            fluidRow(
                column(4,
                       box(
                           width = NULL,
                           status = "danger",
                           uiOutput("caret_test_gp_ui"),
                           actionButton('caret_test_gp_btn', label = "Test", class = "btn btn-info"),
                           hr(),
                           tags$b("Testing Data"),
                           DT::dataTableOutput("caret_test_tbl")
                       )
                ),
                column(4,
                       box(title = "Test Result (Confusion Matrix)",
                           width = NULL,
                           status = "warning",
                           DT::dataTableOutput("caret_conf_tbl"),
                           plotOutput("caret_conf_plot")
                       )
                ),
                column(4,
                       box(title = "Test Result (Assignment)",
                           width = NULL,
                           status = "success",
                           DT::dataTableOutput("caret_test_result_tbl"),
                           downloadButton("download_caret_test_result", label = "Download", class = "btn btn-success")
                       )
                )
            )
        ),
        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, Chris Keefer, Allan Engelhardt, Tony Cooper, Zachary Mayer, Brenton Kenkel, the R Core Team, Michael
                        Benesty, Reynald Lescarbeau, Andrew Ziem, Luca Scrucca, Yuan Tang and Can Candan. (2015). caret: Classification and Regression Training. R package version 6.0-58.
                        http://CRAN.R-project.org/package=caret.", class = "citation")
                )
                ),
        img(src="caret.png",height = 200)
    )

})

caretList <- callModule(pivot_dataScale, "caret", r_data)

caret <- reactiveValues()
caret$data <- NULL
caret$category <- NULL
caret$train_sample <- NULL
caret$test_sample <- NULL

caret$model <- NULL
caret$cv_result <- NULL
caret$test_result <- NULL

# load method table
caret_model_tbl <- read.csv("src/built_in_files/caret_model_tbl.csv")
# training method
output$caret_train_method_ui <- renderUI({
    model_tbl <- caret_model_tbl %>%
        dplyr::filter(BB == 1) %>%
        dplyr::select(Model, method.Argument.Value)
    all_methods <- as.list(model_tbl$method.Argument.Value)
    names(all_methods) <- model_tbl$Model
    selectInput("caret_model_method", label = "Modeling method",  choices = all_methods, selected = "glm")
})

# Training and Cross-validation
output$caret_train_gp_ui <- renderUI({
    rsList <- callModule(pivot_groupBy, "caret", meta = r_data$meta)
    if(is.null(rsList$meta)) {
        return()
    }
    group <- rsList$meta[,1]
    caret$category <- rsList$group_by
    caret_train_gp_choices <- as.list(unique(group))
    names(caret_train_gp_choices) <- unique(group)
    selectizeInput('caret_train_gp', label = "Training Groups", choices = caret_train_gp_choices, multiple = TRUE, selected = caret_train_gp_choices)
})

observe({
    req(caret$category)
    caret$data <- caretList()$df
    req(caret$data)
    selected_train_group <- input$caret_train_gp
    sample_list <- r_data$sample_name
    group_list <- r_data$meta[,caret$category]
    names(group_list) <- sample_list

    train_group <- group_list[which(group_list %in% selected_train_group)]
    train_sample <- names(train_group)
    attr(train_sample, "group") <- train_group
    caret$train_sample <- train_sample
})

observeEvent(input$caret_train_gp_btn,{
    req(caret$data, caret$train_sample)
    if(nrow(caret$data) > 5000) {
        session$sendCustomMessage(type = "showalert", "Please filter features to less than 5000.")
        return()
    }

    error_I <- 0

    withProgress(message = 'Processing', value = 0.8, {
        set.seed(input$caret_seed)
        fitControl <- caret::trainControl(## 10-fold CV
            method = "repeatedcv",
            number = input$caret_cv_fold,
            ## repeated ten times
            repeats = input$caret_cv_repeat)
        tryCatch({
            caret$model <- caret::train(x = t(caret$data[, caret$train_sample]), y = attributes(caret$train_sample)$group, method = input$caret_model_method, trControl = fitControl)
        },
        error = function(e){
            session$sendCustomMessage(type = "showalert", paste("Modeling failed with the following error message: \n", e))
            error_I <<- 1
        }
        )
    })

    if(error_I) return()
    #caret$cv_result <- caret.cv(caret$train_data, caret$train_group, k = input$caret_k, prob = TRUE)
})

output$caret_training_tbl <- DT::renderDataTable({
    req(caret$data, caret$train_sample)
    DT::datatable(caret$data[, caret$train_sample], options = list(scrollX = TRUE, scrollY = "250px", lengthMenu = c(20, 50, 100), searching = F))
})

output$caret_model_result <- renderPrint({
    caret$model
})

output$caret_model_coef <- DT::renderDataTable({
    if(is.null(caret$model)) return ()
    error_I <- 0
    tryCatch({
        a <- as.data.frame(caret$model$finalModel$coefficients)
        a <- a %>% tibble::rownames_to_column()
        colnames(a) <- c("feature", "coefficients")
        tbl <- a[order(abs(a$coefficients), decreasing = T),]
    }, error = function(e) {
        error_I <<- 1
    })

    if(error_I) return(data.frame(error_message = c("This information is not available.")))

    return(tbl)
})


# Testing

output$caret_test_gp_ui <- renderUI({
    rsList <- callModule(pivot_groupBy, "caret", meta = r_data$meta)
    if(is.null(rsList$meta)) {
        return()
    }
    group <- rsList$meta[,1]
    caret$category <- rsList$group_by
    caret_test_gp_choices <- as.list(unique(group))
    names(caret_test_gp_choices) <- unique(group)
    selectizeInput('caret_test_gp', label = "Testing Groups", choices = caret_test_gp_choices, multiple = TRUE, selected = NULL)
})

observeEvent(input$caret_test_gp_btn, {
    req(caret$data)
    if(is.null(caret$model)) {
        session$sendCustomMessage(type = "showalert", "Please train the model first.")
        return ()
    }
    selected_test_group <- input$caret_test_gp
    sample_list <- r_data$sample_name
    group_list <- r_data$meta[,caret$category]
    names(group_list) <- sample_list
    test_group <- group_list[which(group_list %in% selected_test_group)]
    test_sample <- names(test_group)
    attr(test_sample, "group") <- test_group
    caret$test_sample <- test_sample
    withProgress(message = 'Processing', value = 0.8, {
        caret$test_result <- stats::predict(caret$model, newdata = t(caret$data[, caret$test_sample]))
    })
})

output$caret_test_tbl <- DT::renderDataTable({
    req(caret$data, caret$test_sample)
    DT::datatable(caret$data[, caret$test_sample], options = list(scrollX = TRUE, scrollY = "250px", lengthMenu = c(20, 50, 100), searching = F))
})

caret_result_tbl <- reactive({
    cbind(sample = caret$test_sample, actual_group = attributes(caret$test_sample)$group, assigned_group = as.character(caret$test_result))
})

output$caret_test_result_tbl <- DT::renderDataTable({
    if(is.null(caret$test_result)) return ()
    DT::datatable(caret_result_tbl(), rownames = F, options = list(scrollX = TRUE, scrollY = "450px", lengthMenu = c(20, 50, 100)))
})

output$download_caret_test_result <- downloadHandler(
    filename = "caret_test_result.csv",
    content = function(file) {
        write.csv(caret_result_tbl(), file, row.names = F)
    }
)

# output$caret_confusionmatrix <- renderPrint({
#     if(is.null(caret$test_result)) return ()
#     assign("caret", reactiveValuesToList(caret), env=.GlobalEnv)
#
#     error_I <- 0
#     tryCatch({
#         cm<-caret::confusionMatrix(caret$test_result, attributes(caret$test_sample)$group)
#     }, error = function(e) {
#         error_I <<- 1
#     })
#     if(error_I) return("Confusion matrix not available.")
#     return(cm)
# })

output$caret_conf_tbl <- DT::renderDataTable({
    req(caret$test_result, caret$test_sample)
    sample_gp <- attributes(caret$test_sample)$group
    names(sample_gp) <- caret$test_sample
    tbl <- as.data.frame.matrix(table(caret$test_result, sample_gp))
    colnam <- names(tbl)
    names(tbl) <- sapply(colnam, function(x) paste("Is", x))
    rownam <- rownames(tbl)
    rownames(tbl) <- sapply(rownam, function(x) paste("Classified as", x))
    DT::datatable(tbl, options = list(paging = FALSE, searching = FALSE))
})

output$caret_conf_plot <- renderPlot({
    req(caret$test_result, caret$test_sample)
    sample_gp <- attributes(caret$test_sample)$group
    names(sample_gp) <-caret$test_sample
    plot(as.factor(sample_gp), as.factor(caret$test_result), xlab="Group", ylab = "Classified Result")
})




