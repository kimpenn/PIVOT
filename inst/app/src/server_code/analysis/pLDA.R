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

################################ penalizedLDA UI #################################

output$plda_ui <- renderUI({
    if(is.null(r_data$glb.raw)) return()
    if(is.null(r_data$meta)) {
        return(tags$p("Group information is required for this module."))
    }
    list(
        enhanced_box(
            width = 12,
            title = "PenalizedLDA",
            id = "plda",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("penalizedLDA Settings:"), class = "param_setting_title"),
            fluidRow(
                column(4, pivot_dataScale_UI("plda", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts", "Standardized Counts", "Log10 & Standardized"), selected = "Log10 Counts")),
                column(4, selectInput("plda_select_feature", label = "Perform PenalizedLDA on", choices = list("All features" = "all", "Custom feature list" = "custom"))),
                column(4, numericInput("plda_L", "Lasso penality tuning parameter",
                                       min = 0.01,
                                       max = 1,
                                       step = 0.01,
                                       value = .14))
            ),
            fluidRow(
                column(5, uiOutput("plda_feature_upload_ui")),
                column(7, uiOutput("plda_feature_upload_text"))
            ),
            fluidRow(
                pivot_colorBy_UI("plda", meta = r_data$meta, multiple = F, width = 8),
                column(4, uiOutput("plda_K_ui"))
            ),
            actionButton("run_plda", "Run", class = "btn-info"),
            tags$hr(),
            tags$div(tags$b("Discriminant Vectors:"), class = "param_setting_title"),
            plotOutput("plda_feature_plot"),
            DT::dataTableOutput("plda_discrim_tbl"),
            downloadButton('download_plda_discrim', 'Download', class = "btn btn-success")
        ),
        pivot_dimScatter_UI("plda", type = "plda")
    )
})

output$plda_feature_upload_ui <- renderUI({
    if(input$plda_select_feature != "custom") return()
    content <- list(
        fluidRow(
            column(6,
                   wellPanel(
                       fileInput('plda_list_file', label = NULL, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                       checkboxInput('plda_header', 'Header', value = F),
                       radioButtons('plda_sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), selected = '\t'),
                       radioButtons('plda_quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"'),
                       actionButton("plda_list_submit", "Submit List", class = "btn btn-info")
                   ),
                   uiOutput("plda_feature_upload_text_inmodal")
            ),
            column(6,
                   tags$p("[feature name in 1st column]"),
                   DT::dataTableOutput('plda_list_tbl_show')
            )
        )
    )

    list(
        actionButton("plda_custom_btn", label = "Upload a custom feature list for penalized LDA", class = "btn-warning"),
        shinyBS::bsModal(id = "plda_custom_modal", "Upload a custom feature list", "plda_custom_btn", size = "large", content)
    )
})

output$plda_feature_upload_text <- renderUI({
    if(input$plda_select_feature != "custom") return()
    req(r_data$plda_flist)
    list(
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b(paste(length(r_data$plda_flist), "features have been successfully uploaded.")), style = "font-size:110%;")
    )
})

output$plda_K_ui <- renderUI({
    plda_minfo<-callModule(pivot_colorBy, "plda", meta = r_data$meta)
    numericInput("plda_K", "Number of discriminant vectors",
                 min = 1, max = length(unique(plda_minfo$meta[,1])) - 1, step = 1,
                 value = length(unique(plda_minfo$meta[,1])) - 1)
})


output$plda_1d_plt_dim_ui <- renderUI({
    req(r_data$plda)
    dims <- colnames(r_data$plda$discrim)[-1]
    names(dims) <- dims
    selectInput("plda_1d_plt_dim", "Discriminant vector", choices = dims)
})


plda_gene <- reactiveValues()
plda_gene$tbl <- NULL

# process the upload feature list
observe({
    inFile <- input$plda_list_file
    error_I <- 0
    if (!is.null(inFile)) {
        tryCatch({
            plda_gene$tbl <- read.csv(inFile$datapath, header=input$plda_header, sep=input$plda_sep, quote=input$plda_quote)
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

output$plda_list_tbl_show <- DT::renderDataTable({
    if(is.null(plda_gene$tbl)) return()
    DT::datatable(plda_gene$tbl, options = list(scrollX = TRUE, scrollY = "350px", searching = FALSE))
})

observeEvent(input$plda_list_submit, {

    # First process the marker feature file and get the list
    if (is.null(plda_gene$tbl) || nrow(plda_gene$tbl) == 0)
    {
        session$sendCustomMessage(type = "showalert", "Give me something!")
        return(NULL)
    }
    marker_names <- make.names(as.character(unique(plda_gene$tbl[,1])))

    cur_flist <- rownames(r_data$raw)


    flist <- cur_flist[match(toupper(marker_names), toupper(cur_flist))]
    flist <- flist[!is.na(flist)]
    if(length(flist) != length(marker_names)) {
        message_gl <- paste0(length(marker_names) - length(flist)," features in your feature list (", length(marker_names),") are not found in the current dataset.")
        session$sendCustomMessage(type = "showalert", message_gl)
    }
    r_data$plda_flist <- flist
})


observeEvent(input$run_plda, {
    if(is.null(r_data$meta)) return()
    plda_minfo<-callModule(pivot_colorBy, "plda", meta = r_data$meta)
    if(input$plda_K > length(unique(plda_minfo$meta[,1])) - 1) {
        session$sendCustomMessage(type = "showalert", "The number of discriminant vectors must be no greater than (number of classes - 1).")
        return()
    }

    rsList <- callModule(pivot_dataScale, "plda", r_data)
    plda_data <- rsList$df
    req(plda_data)
    if(input$plda_select_feature == "custom") {
        if(is.null(r_data$plda_flist)) {
            session$sendCustomMessage(type = "showalert", "Please upload your feature list.")
            return()
        }
        plda_data <- plda_data[r_data$plda_flist, ]
    } else {
        plda_data <- plda_data
    }

    error_I <- 0

    tryCatch({
        r_data$plda<-make_plda(plda_data, as.numeric(as.factor(plda_minfo$meta[,1])), lambda=input$plda_L,K=input$plda_K)
    },
    error = function(e) {
        session$sendCustomMessage(type = "showalert", "PenalizdeLDA failed. Please recheck your parameters.")
        r_data$plda <- NULL
        error_I <<- 1
    })
    if(error_I) return()
    callModule(pivot_dimScatter, "plda", type = "plda", obj = r_data$plda, minfo = plda_minfo)
})

output$plda_feature_plot <- renderPlot({
    req(r_data$plda)
    penalizedLDA::plot.penlda(r_data$plda$plda)
})


output$plda_discrim_tbl <- DT::renderDataTable({
    req(r_data$plda)
    DT::datatable(r_data$plda$discrim)
})

output$download_plda_discrim <-downloadHandler(
    filename = "discrim.csv",
    content = function(file) {
        req(r_data$plda)
        write.csv(r_data$plda$discrim, file)
    })


# 2d_plda select discriminant vector UI
output$plda_x_ui <- renderUI({
    req(r_data$plda)
    dims <- colnames(r_data$plda$discrim)[-1]
    names(dims) <- dims
    selectInput("plda_2d_x", "Discriminant vector on X axis", choices = dims)
})

output$plda_y_ui <- renderUI({
    req(r_data$plda)
    dims <- colnames(r_data$plda$discrim)[-1]
    if(length(dims) < 2) return()
    names(dims) <- dims
    selectInput("plda_2d_y", "Discriminant vector on Y axis", choices = dims, selected = dims[2])
})


output$plda_3d_proj_plt <- threejs::renderScatterplotThree({
    req(r_data$plda)
    xproj1 <- as.data.frame(r_data$plda$plda$xproj)
    if(ncol(xproj1) < 3) return()
    group_color <- get_color_vector(r_data$group, pal = group_pal$val, maxCol = length(unique(r_data$group)))

    threejs::scatterplot3js(xproj1[,1:3], color=group_color,size=2,labels=rownames(xproj1), renderer="canvas")
})



