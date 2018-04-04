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

