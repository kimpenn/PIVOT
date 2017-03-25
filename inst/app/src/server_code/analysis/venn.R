
# Venn

output$venn_ui <- renderUI({

    fluidRow(
        column(12,
            box(
                title = "Venn Diagram",
                status = "primary",
                width = NULL,
                solidHeader = T,
                fluidRow(
                    column(6,
                           tags$b("Upload lists"),
                           fluidRow(
                               column(6,
                                      wellPanel(
                                          fileInput('venn_in_file', label = NULL, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                          checkboxInput('venn_header', 'Header', value = F),
                                          radioButtons('venn_sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), selected = '\t'),
                                          radioButtons('venn_quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"'),
                                          textInput('venn_list_name', "List Name", value = ""),
                                          actionButton("venn_list_submit", "Add List", class = "btn btn-info")
                                      )
                               ),
                               column(6,
                                      tags$p("[list in 1st column]"),
                                      DT::dataTableOutput('venn_list_tbl_show')
                               )
                           )
                    ),
                    column(6,
                           uiOutput("venn_lists"),
                           tags$br(),
                           actionButton("clear_venn", "Clear Input lists", class = "btn btn-danger")
                    )
                ),

                fluidRow(
                    column(6,
                           plotOutput("venn_show")
                    ),

                    uiOutput("venn_set_show_ui")
                )

            )
        )
    )

})

tmp_venn_list <- reactiveValues()
tmp_venn_list$tbl <- NULL

observe({
    inFile <- input$venn_in_file
    error_I <- 0
    if (!is.null(inFile)) {
        tryCatch({
            tmp_venn_list$tbl <- read.csv(inFile$datapath, header=input$venn_header, sep=input$venn_sep, quote=input$venn_quote)
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

output$venn_list_tbl_show <- DT::renderDataTable({
    if(is.null(tmp_venn_list$tbl)) return()
    DT::datatable(tmp_venn_list$tbl, options = list(scrollX = TRUE, scrollY = "350px", searching = FALSE))
})

observeEvent(input$venn_list_submit, {
    if (is.null(tmp_venn_list$tbl) || nrow(tmp_venn_list$tbl) == 0)
    {
        session$sendCustomMessage(type = "showalert", "No list detected.")
        return()
    }


    if(is.null(r_data$venn_list)) {
        r_data$venn_list <- list()
    }

    if (length(r_data$venn_list) >= 5)
    {
        session$sendCustomMessage(type = "showalert", "VennDiagram allows for a maximum of 5 sets.")
        return()
    }


    if(nchar(input$venn_list_name) == 0) {
        list_name <- paste("List", length(r_data$venn_list) + 1)
    } else {
        if(input$venn_list_name %in% names(r_data$venn_list)) {
            session$sendCustomMessage(type = "showalert", "Please use a different list name.")
            return()
        }
        list_name <- input$venn_list_name
    }

    r_data$venn_list[[list_name]] <- as.character(unique(tmp_venn_list$tbl[,1]))
})

output$venn_lists <- renderUI({
    if(!is.null(r_data$venn_list) && length(r_data$venn_list) > 0) {
        lists_text <- list()
        for(i in 1:length(r_data$venn_list)) {
            cur_len <- length(r_data$venn_list[[i]])
            if(cur_len > 20) {
                cur_list_show <- paste0(paste(r_data$venn_list[[i]][1:20], collapse = ", "), "... (", cur_len, " features)")
            } else {
                cur_list_show <- paste(r_data$venn_list[[i]], collapse = ", ")
            }

            lists_text[[i]] <- tags$li(paste0(names(r_data$venn_list[i]), ": ", cur_list_show))
        }
        return(lists_text)
    }
})


observeEvent(input$clear_venn, {
    r_data$venn_list <- list()
})


output$venn_show <- renderPlot({
    if(!is.null(r_data$venn_list) && length(r_data$venn_list) > 0) {
        futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
        vp <- VennDiagram::venn.diagram(r_data$venn_list, fill = 1:length(r_data$venn_list), alpha = 0.3, filename = NULL);
        grid::grid.draw(grid::gTree(children = vp, vp = grid::viewport(width = 0.9, height = 0.9)))
    }
})

output$venn_set_show_ui <- renderUI({
    if(!is.null(r_data$venn_list) && length(r_data$venn_list) > 0) {
        list(
            column(3,
                   DT::dataTableOutput("venn_intersect_tbl"),
                   downloadButton("download_venn_intersect", "Download", class = "btn btn-success")
            ),
            column(3,
                   DT::dataTableOutput("venn_union_tbl"),
                   downloadButton("download_venn_union", "Download", class = "btn btn-success")
            )
        )
    }
})

output$venn_intersect_tbl <- DT::renderDataTable({
    if(!is.null(r_data$venn_list) && length(r_data$venn_list) > 0) {
        DT::datatable(data.frame(intersect_list = Reduce(intersect, r_data$venn_list)),
                      options = list(scrollX = TRUE, scrollY = "350px"))
    }
})

output$venn_union_tbl <- DT::renderDataTable({
    if(!is.null(r_data$venn_list) && length(r_data$venn_list) > 0) {
        DT::datatable(data.frame(union_list = Reduce(union, r_data$venn_list)),
                      options = list(scrollX = TRUE, scrollY = "350px"))
    }
})


output$download_venn_intersect <- downloadHandler(
    filename = "intersect_list.csv",
    content = function(file) {
        if(!is.null(r_data$venn_list) && length(r_data$venn_list) > 0) {
            write.csv(data.frame(intersect_list = Reduce(intersect, r_data$venn_list)), file, row.names = F)
        }
    }
)

output$download_venn_union <- downloadHandler(
    filename = "union_list.csv",
    content = function(file) {
        if(!is.null(r_data$venn_list) && length(r_data$venn_list) > 0) {
            write.csv(data.frame(union_list = Reduce(union, r_data$venn_list)), file, row.names = F)
        }
    }
)

