



#' PIVOT file input module, UI
#'
#' @export
pivot_fileInput_UI <- function(id, format = "list"){
    ns<- NS(id)

    header <- numericInput(ns("skip_header"), label='Skip Rows', value=0)
    comment <- selectInput(ns("comment"), 'Comment', c("None"="none","#"="#", "%"="%", "//"="//"), selected = "none")
    seperator <- radioButtons(ns("file_sep"), 'Separator', c(Comma=',', Semicolon=';', Tab='\t', Space = ' '), selected = ',', inline = TRUE)
    colRow <- checkboxInput(ns("col_names"), 'First Row as Colnames', value = T)
    if(format == "list") {
        tagList(
            fileInput(ns('file_in'), 'Choose file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', ".xls", ".xlsx")),
            wellPanel(
                header,
                comment,
                seperator,
                colRow
            )
        )
    } else if(format == "compact"){
        tagList(
            fileInput(ns('file_in'), 'Choose file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', ".xls", ".xlsx")),
            wellPanel(
                fluidRow(
                    column(6, header),
                    column(6, comment)
                ),
                fluidRow(
                    column(6, seperator),
                    column(6, colRow)
                )
            )
        )
    } else {
        stop("Unrecognized argument.")
    }
}

#' PIVOT help modules, server
#'
#' @export
pivot_fileInput <- function (input, output, session, reset = FALSE, return_df = T) {
    if(reset) {
        shinyjs::reset("file_in")
        return(NULL)
    } else {
        userFile <- reactive({
            # If no file is selected, don't do anything
            validate(need(input$file_in, message = FALSE))
            input$file_in
        })

        # The user's data, parsed into a data frame
        result <- reactive({
            if(is.null(userFile())) return()
            ext = tools::file_ext(userFile()$name)
            if(ext %in% c("xls", "xlsx")) {
                # This is an excel file
                is_excel = T
            } else {
                is_excel = F
            }

            error_I <- 0
            tryCatch({
                if(is_excel) {
                    if(ext == "xlsx") {
                        df <- readxl::read_xlsx(path = userFile()$datapath, skip=input$skip_header,
                                                col_names=input$col_names, trim_ws = TRUE)
                    } else if(ext == "xls") {
                        df <- readxl::read_xls(path = userFile()$datapath, skip=input$skip_header,
                                                col_names=input$col_names, trim_ws = TRUE)
                    }

                } else {
                    df <- readr::read_delim(file = userFile()$datapath, skip=input$skip_header,
                                            col_names=input$col_names, delim=input$file_sep,
                                            comment=ifelse(input$comment=="none", "",input$comment),
                                            escape_double = FALSE, trim_ws = TRUE)
                }

                if(return_df) {
                    df <- as.data.frame(df)
                }
            },
            error = function(e){
                error_I <<- 1
            })
            if(error_I) {
                session$sendCustomMessage(type = "showalert", "File format not recogonized.")
                return()
            }
            return(list(
                df = df,
                path = userFile()$datapath,
                name = userFile()$name
            ))
        })

        # Return the reactive that yields the data frame
        return(
            result()
        )
    }
}


#' PIVOT file input preview module, UI
#' @description
#' Generate a DT table preview for input file
#' @export
pivot_filePreview_UI <- function(id){
    ns<- NS(id)
    tagList(
        DT::dataTableOutput(ns("tbl_preview"))
    )
}

#' PIVOT file input preview module, server
#'
#' @export
pivot_filePreview <- function (input, output, session, df, height = "350px", search = FALSE) {
    output$tbl_preview <- DT::renderDataTable({
        if(is.null(df)) return()
        DT::datatable(df, rownames=F, options = list(scrollX = TRUE, scrollY = height, lengthMenu = c(20, 50, 100), searching = search))
    })
}

#' a wrapper for feature input and preview in Modal
#' @description
#' This is the ui part of the module.
#' @export
pivot_featureInputModal_UI <- function(id, label = NULL) {
    ns<- NS(id)
    content <- list(
        fluidRow(
            column(6,
                   selectInput(ns("input_type"), "Input Method:", choices = list("Manually Input" = "manual", "Upload Feature List" = "upload")),
                   conditionalPanel(
                       sprintf("input['%s'] == 'manual'", ns("input_type")),
                       tags$p("Please enter features in the right text box. Names should be separated by an <ENTER> (one line for one feature).")
                   ),
                   conditionalPanel(
                       sprintf("input['%s'] == 'upload'", ns("input_type")),
                       pivot_fileInput_UI(ns("ft_file"))
                   ),

                   fluidRow(
                       column(6),
                       column(3,actionButton(ns("ft_submit"), "Submit List", class = "btn btn-info")),
                       column(3,uiOutput(ns("ft_submit_img")))
                   )
            ),
            column(6,
                   conditionalPanel(
                       sprintf("input['%s'] == 'manual'", ns("input_type")),
                       textareaInput(ns("manual_genes"), "Feature Manual Input:", placeholder = "Type features here (separated by an <ENTER>).", rows = 15)
                   ),
                   conditionalPanel(
                       sprintf("input['%s'] == 'upload'", ns("input_type")),
                       tags$p("[feature in 1st column, case insensitive]"),
                       pivot_filePreview_UI(ns("ft_preview"))
                   )
            )
        )
    )
    tagList(
        actionButton(ns("modal_btn"), label = label, class = "btn-info"),
        shinyBS::bsModal(id = ns("ft_modal"), "Upload a custom feature list", ns("modal_btn"), size = "large", content)
    )
}

#' a wrapper for feature input and preview in Modal
#' @description
#' This is the server part of the module.
#' @export
pivot_featureInputModal <- function(input, output, session, r_data, match_rdata = T) {
    observe({
        df <- callModule(pivot_fileInput, "ft_file")
        callModule(pivot_filePreview, "ft_preview", df$df)
    })


    flist<-reactive({
        input$ft_submit
        req(input$input_type)
        isolate({
            # First process the marker feature file and get the list
            if(input$input_type == "upload") {
                df <- callModule(pivot_fileInput, "ft_file")
                if(is.null(df)) return()
                df <- df$df
                marker_names <- make.names(as.character(unique(df[,1])))
            } else if(input$input_type == "manual") {
                if(input$manual_genes == "") {
                    return()
                }
                if(nchar(input$manual_genes) > 100000) {
                    session$sendCustomMessage(type = "showalert", "Exceed maximum number of characters (100000) allowed.")
                    return()
                }
                marker_names <- unlist(strsplit(input$manual_genes, split = "\n"))
            } else {
                return()
            }

            if(match_rdata) {
                cur_flist <- rownames(r_data$df)
                flist <- cur_flist[match(toupper(marker_names), toupper(cur_flist))]
                unmatch <- marker_names[which(!toupper(marker_names) %in% toupper(cur_flist))]
                flist <- flist[!is.na(flist)]
                if(length(unmatch)) {
                    message_gl <- paste0(length(unmatch)," features in your feature list (", length(marker_names),") are not found in the current dataset.")
                    session$sendCustomMessage(type = "showalert", message_gl)
                }
            } else {
                flist <- marker_names
            }


            if(length(flist) > 50000) {
                session$sendCustomMessage(type = "showalert", "Exceed maximum number of features (50000) allowed.")
                return()
            }

            return(flist)
        })
    })

    output$ft_submit_img <- renderUI({
        if(is.null(flist())) return()
        img(src = "button_ok.png", width = 35, height = 35)
    })


    return(flist())
}



#' Box for text input
#' @export
textareaInput <- function(inputId, label, value="", placeholder="", rows=2){
    tagList(
        div(strong(label), style="margin-top: 5px;"),
        tags$style(type="text/css", "textarea {width:100%; margin-top: 5px;}"),
        tags$textarea(id = inputId, placeholder = placeholder, rows = rows, value))
}


