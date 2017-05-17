



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
            fileInput(ns('file_in'), 'Choose file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            wellPanel(
                header,
                comment,
                seperator,
                colRow
            )
        )
    } else if(format == "compact"){
        tagList(
            fileInput(ns('file_in'), 'Choose file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
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
pivot_fileInput <- function (input, output, session, reset = FALSE) {
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
            error_I <- 0
            tryCatch({
                df <- readr::read_delim(file = userFile()$datapath, skip=input$skip_header,
                                        col_names=input$col_names, delim=input$file_sep,
                                        comment=ifelse(input$comment=="none", "",input$comment),
                                        escape_double = FALSE, trim_ws = TRUE)
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
                   pivot_fileInput_UI(ns("ft_file")),
                   actionButton(ns("ft_submit"), "Submit List", class = "btn btn-info")
            ),
            column(6,
                   tags$p("[feature name in 1st column]"),
                   pivot_filePreview_UI(ns("ft_preview"))
            )
        )
    )
    tagList(
        actionButton(ns("modal_btn"), label = label, class = "btn-warning"),
        shinyBS::bsModal(id = ns("ft_modal"), "Upload a custom gene list", ns("modal_btn"), size = "large", content)
    )
}

#' a wrapper for feature input and preview in Modal
#' @description
#' This is the server part of the module.
#' @export
pivot_featureInputModal <- function(input, output, session, r_data) {
    observe({
        df <- callModule(pivot_fileInput, "ft_file")
        callModule(pivot_filePreview, "ft_preview", df$df)
    })


    flist<-reactive({
        input$ft_submit
        isolate({
            # First process the marker feature file and get the list
            df <- callModule(pivot_fileInput, "ft_file")
            if(is.null(df)) return()
            df <- as.data.frame(df$df)

            marker_names <- make.names(as.character(unique(df[,1])))

            cur_flist <- rownames(r_data$df)

            flist <- cur_flist[match(toupper(marker_names), toupper(cur_flist))]
            flist <- flist[!is.na(flist)]
            if(length(flist) != length(marker_names)) {
                message_gl <- paste0(length(marker_names) - length(flist)," features in your gene list (", length(marker_names),") are not found in the current dataset.")
                session$sendCustomMessage(type = "showalert", message_gl)
            }
            return(flist)
        })
    })

    return(flist())
}





