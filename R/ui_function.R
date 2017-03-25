
#' function From Shiny
#'
#' @export
`%AND%` <- function(x, y) {
    if (!is.null(x) && !is.na(x))
        if (!is.null(y) && !is.na(y))
            return(y)
    return(NULL)
}

#' function From Shiny
#'
#' @export
formatNoSci <- function(x) {
    if (is.null(x)) return(NULL)
    format(x, scientific = FALSE, digits = 15)
}


#' PIVOT Data Input, UI function
#' custom numeric input based on shiny
#' @export
numericInput_1 <- function (inputId, label, value, min = NA, max = NA, step = NA,
                            width = NULL)
{
    inputTag <- tags$input(id = inputId, type = "number", class = "form-control",
                           value = formatNoSci(value), style = "display:inline-block; width:60%;")
    if (!is.na(min))
        inputTag$attribs$min = min
    if (!is.na(max))
        inputTag$attribs$max = max
    if (!is.na(step))
        inputTag$attribs$step = step
    div(class = "form-group shiny-input-container", style = if (!is.null(width))
        paste0("width: 100%;"), label %AND%
            tags$label(label, `for` = inputId, style = "display:inline_block"), inputTag)
}




#' PIVOT Data Input, UI function
#'
#' @export
tagAssert <- function(tag, type = NULL, class = NULL, allowUI = TRUE) {
    if (!inherits(tag, "shiny.tag")) {
        print(tag)
        stop("Expected an object with class 'shiny.tag'.")
    }

    # Skip dynamic output elements
    if (allowUI &&
        (hasCssClass(tag, "shiny-html-output") ||
         hasCssClass(tag, "shinydashboard-menu-output"))) {
        return()
    }

    if (!is.null(type) && tag$name != type) {
        stop("Expected tag to be of type ", type)
    }

    if (!is.null(class)) {
        if (is.null(tag$attribs$class)) {
            stop("Expected tag to have class '", class, "'")

        } else {
            tagClasses <- strsplit(tag$attribs$class, " ")[[1]]
            if (!(class %in% tagClasses)) {
                stop("Expected tag to have class '", class, "'")
            }
        }
    }
}

#' PIVOT Data Input, UI function
#'
#' @export
hasCssClass <- function(tag, class) {
    if (is.null(tag$attribs) || is.null(tag$attribs$class))
        return(FALSE)

    classes <- strsplit(tag$attribs$class, " +")[[1]]
    return(class %in% classes)
}

#' PIVOT Data Input, UI function
#'
#' @export
dropdownMenu <- function (..., type = c("messages", "notifications", "tasks", "system", "support"), top_msg = NULL, icon = NULL, .list = NULL)
{
    type <- match.arg(type)
    items <- c(list(...), .list)
    lapply(items, tagAssert, type = "li")
    dropdownClass <- ifelse(type %in% c("system","support"), paste0("dropdown ", "notifications", "-menu"), paste0("dropdown ", type, "-menu"))
    if (is.null(icon)) {
        icon <- switch(type, messages = shiny::icon("envelope"),
                       notifications = shiny::icon("list-alt"), tasks = shiny::icon("tasks"), system = shiny::icon("cog"), support = shiny::icon("question-circle"))
    }
    dropdown_style <- ifelse(type%in% c("messages","support"), "width:290px", "width:200px;")
    tags$li(class = dropdownClass, a(href = "#", class = "dropdown-toggle",`data-toggle` = "dropdown", icon, NULL), tags$ul(class = "dropdown-menu", style = dropdown_style, tags$li(class = "header", top_msg), tags$li(tags$ul(class = "menu", style = "list-style-type: none;max-height: 800px;", items))))
}

#' PIVOT Data Input, UI function
#'
#' @export
systemItem <- function (title = NULL, text, icon = NULL, status = "success", href = NULL)
{
    tagAssert(icon, type = "i")
    icon <- tagAppendAttributes(icon, class = paste0("text-", status))
    if(!is.null(title)) {
        title <- tags$b(title)
        text <- tags$p(text, style="text-indent: 2px;")
    }

    tags$li(a(icon, title, text, href = href))
}

#' PIVOT Data Input, UI function
#'
#' @export
custom_downloadbtn <- function (outputId, label = "Download", class = NULL, style = NULL)
{
    tags$a(id = outputId, class = paste("shiny-download-link", class), style = style, href = "", target = "_blank", label)
}

#' PIVOT Data Input, UI function
#'
#' @export
valueBoxOutput_custom <- function (outputId, width = 4, style = NULL)
{
    shiny::uiOutput(outputId, class = paste0("col-sm-", width), style = style)
}

#' PIVOT Data Input, UI function
#'
#' @export
infoBoxOutput_custom <- function (outputId, width = 4, style = NULL)
{
    shiny::uiOutput(outputId, class = paste0("col-sm-", width), style = style)
}


#' PIVOT Data Input, UI function
#'
#' @export
validateColor <- function(color) {
    validColors <- c("red", "yellow", "aqua", "blue", "light-blue", "green",
                     "navy", "teal", "olive", "lime", "orange", "fuchsia",
                     "purple", "maroon", "black")
    if (color %in% validColors) {
        return(TRUE)
    }

    stop("Invalid color: ", color, ". Valid colors are: ",
         paste(validColors, collapse = ", "), ".")
}

#' PIVOT Data Input, UI function
#'
#' @export
validateStatus <- function(status) {
    validStatuses <- c("primary", "success", "info", "warning", "danger")
    if (status %in% validStatuses) {
        return(TRUE)
    }

    stop("Invalid status: ", status, ". Valid statuses are: ",
         paste(validStatuses, collapse = ", "), ".")
}

#' PIVOT Data Input, UI function
#'
#' @export
"%OR%" <- function(a, b) if (!is.null(a)) a else b

#' PIVOT Data Input, UI function
#'
#' @export
enhanced_box <- function (..., title = NULL, id = NULL, footer = NULL, status = NULL, solidHeader = FALSE,
                          background = NULL, width = 6, height = NULL, collapsible = FALSE,
                          reportable = FALSE, get_pdf = FALSE, get_html = FALSE, register_analysis = FALSE,
                          collapsed = FALSE)
{
    boxClass <- "box"
    if (solidHeader || !is.null(background)) {
        boxClass <- paste(boxClass, "box-solid")
    }
    if (!is.null(status)) {
        boxClass <- paste0(boxClass, " box-", status)
    }
    if (collapsible && collapsed) {
        boxClass <- paste(boxClass, "collapsed-box")
    }
    if (!is.null(background)) {
        validateColor(background)
        boxClass <- paste0(boxClass, " bg-", background)
    }
    style <- NULL
    if (!is.null(height)) {
        style <- paste0("height: ", shinydashboard::validateCssUnit(height))
    }
    titleTag <- NULL
    if (!is.null(title)) {
        titleTag <- h3(class = "box-title", title)
    }

    collapseTag <- NULL
    if (collapsible) {
        buttonStatus <- status %OR% "default"
        collapseIcon <- if (collapsed)
            "plus"
        else "minus"

        collapseTag <- tags$button(class = paste0("btn btn-box-tool"),
                                   `data-widget` = "collapse",
                                   shiny::icon(collapseIcon))

    }

    reportTag <- NULL
    pdfTag <- NULL
    htmlTag <- NULL
    registerTag <- NULL

    if(get_pdf && !is.null(id)) {
        pdfTag <- tags$a(id = paste0(id, "_pdf"), class = "btn btn-box-tool shiny-download-link",
                         `data-toggle`="tooltip", `data-original-title`="PDF",
                         href = "", target = "_blank", icon("file-pdf-o"), NULL)
    }

    if(get_html && !is.null(id)) {
        htmlTag <- tags$a(id = paste0(id, "_html"), class = "btn btn-box-tool shiny-download-link",
                          `data-toggle`="tooltip", `data-original-title`="HTML",
                          href = "", target = "_blank", icon("file-picture-o"), NULL)
    }

    if(reportable && !is.null(id)) {
        reportTag <- tags$button(class = paste0("btn btn-box-tool action-button shiny-bound-input"),
                                 `data-toggle`="tooltip", `data-original-title`="Add to report",
                                 href=paste0(id,'_report'), id=paste0(id,'_report'), shiny::icon("clipboard"))
    }

    if(register_analysis && !is.null(id)) {
        registerTag <- tags$button(class = paste0("btn btn-box-tool action-button shiny-bound-input"),
                                   `data-toggle`="tooltip", `data-original-title`="Register results",
                                   href=paste0(id,'_reg'), id=paste0(id,'_reg'), shiny::icon("magnet"))
    }

    headerTag <- NULL
    if (!is.null(titleTag) || !is.null(collapseTag)) {
        boxtools <- div(class = "box-tools pull-right", registerTag, reportTag, pdfTag, htmlTag, collapseTag)
        headerTag <- div(class = "box-header", titleTag, boxtools)
    }
    div(class = if (!is.null(width))
        paste0("col-sm-", width), div(class = boxClass, style = if (!is.null(style))
            style, headerTag, div(class = "box-body", ...), if (!is.null(footer))
                div(class = "box-footer", footer)))
}






#' PIVOT Data Input, UI function
#' @description
#' Modal confirm button from Weicheng Zhu
#' source: https://github.com/mingsnu/shiny-confirm-dialog
#' Note: This was not used in PIVOT. I later switched to shiny modal.
#' @export
modalConfirmDialog = function(id, header = "Confirmation", body = "Are you sure?", footer = list(actionButton("confirmDlgOkBtn", "OK"))){
    div(id = id, class = "modal fade",
        div(class = "modal-dialog modal-sm",
            div(class = "modal-content",
                div(class = "modal-header",
                    tags$button(type = "button", class = "close", 'data-dismiss' = "modal", 'aria-hidden' = "true", HTML('&times;')),
                    tags$h4(class = "modal-title", header)
                ),
                div(class = "modal-body",
                    tags$p(body)
                ),
                div(class = "modal-footer",
                    tagList(footer)
                )
            )
        )
    )
}

#' PIVOT Data Input, UI function
#' @description
#' modal trigger button from Weicheng Zhu
#' source: https://github.com/mingsnu/shiny-confirm-dialog
#' Note: This was not used in PIVOT. I later switched to shiny modal.
#' @export
modalTriggerButton = function(inputId, target, label, icon = NULL, class = "btn action-button btn-danger btn_leftAlign"){
    if (!is.null(icon))
        buttonContent <- list(icon, label)
    else buttonContent <- label
    tags$button(id = inputId, type = "button", class = class, 'data-toggle' = "modal", 'data-target' = target,
                buttonContent)
}

#' PIVOT Data Input, UI function
#' @description
#' custom modal button which also serves as an action button
#' @import shiny
#' @export
modalActionButton <- function (inputId, label, icon = NULL, width = NULL, ...)
{
    value <- restoreInput(id = inputId, default = NULL)
    tags$button(id = inputId, style = if (!is.null(width))
        paste0("width: ", validateCssUnit(width), ";"), type = "button",
        class = "btn btn-default action-button", `data-val` = value,
        `data-dismiss` = "modal",  # This line is new
        list(icon, label), ...)
}

#' renderPlotly from plotly
#' @import htmlwidgets plotly
#' @export
render_Plotly <- function (expr, env = parent.frame(), quoted = FALSE)
{
    if (!quoted) {
        expr <- substitute(expr)
    }
    expr <- call("ggplotly", expr)
    shinyRenderWidget(expr, plotlyOutput, env, quoted = TRUE)
}
#' ggplotly from plotly
#' @import plotly
#' @export
ggplotly <- plotly::ggplotly

#' PIVOT Data Input, pipe function
#'
#' @export
`%>%` <- magrittr::`%>%`

#' check if a button was NOT pressed
#'
#' @export
not_pressed <- function(x) if(is.null(x) || x == 0) TRUE else FALSE


#' Checkbox tooltip, from K. Rohde
#' Source: http://stackoverflow.com/questions/36670065/tooltip-in-shiny-ui-for-help-text
#' @export
makeCheckboxTooltip <- function(checkboxValue, buttonLabel, Tooltip){
    tags$script(HTML(paste0("
                            $(document).ready(function() {
                            var inputElements = document.getElementsByTagName('input');
                            for(var i = 0; i < inputElements.length; i++){
                            var input = inputElements[i];

                            if(input.getAttribute('value') == '", checkboxValue, "'){
                            var buttonID = 'button_' + Math.floor(Math.random()*1000);

                            var button = document.createElement('button');
                            button.setAttribute('id', buttonID);
                            button.setAttribute('type', 'button');
                            button.setAttribute('class', 'btn action-button btn-inverse btn-xs');
                            button.appendChild(document.createTextNode('", buttonLabel, "'));

                            input.parentElement.parentElement.appendChild(button);
                            shinyBS.addTooltip(buttonID, \"tooltip\", {\"placement\": \"bottom\", \"trigger\": \"hover\", \"title\": \"", Tooltip, "\"})
                            };
                            }
                            });
                            ")))
                            }

