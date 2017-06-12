



#' PIVOT help modules, UI
#'
#' @export
pivot_help_UI <- function(id, title, tooltip = T){
    ns<- NS(id)
    if(tooltip) {
        tip <- shinyBS::bsTooltip(
            ns("pivot_help"),
            title = title,
            options = list(container = "body")
        )
    } else {
        tip <- NULL
    }
    tagList(
        actionButton(ns("pivot_help"), label = NULL, icon = icon("question-circle"), class = "btn-help"),
        tip
    )
}

#' PIVOT help modules, server
#'
#' @export
pivot_help <- function (input, output, session, title, content, size = "m") {
    observeEvent(input$pivot_help, {
        showModal(modalDialog(
            title = title,
            size = size,
            content,
            easyClose = TRUE
        ))
    })
}

