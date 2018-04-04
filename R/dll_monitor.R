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


# DLL monitor

dllUI <- function(id){
    ns<- NS(id)
    tagList(
        uiOutput(ns("dll"), style = "background-color: #f2f2f2; text-align: center;")
    )
}

dllText <- function (input, output, session) {
    output$dll <- renderUI({
        num_dll <- length(getLoadedDLLs())
        if(num_dll < 35) {
            status <- "Great"
            bg_color <- "#71f442"
        } else if(num_dll < 60) {
            status <- "Good"
            bg_color <- "#92f442"
        } else if(num_dll < 80) {
            status <- "DLL cleanup recommended."
            bg_color <- "#f4d142"
        } else {
            status <- "DLL cleanup strongly recommended."
            bg_color <- "#ff4f4f"
        }
        text <- paste0("Current loaded DLLs: ", num_dll, "       Status: ", status)
        return(tags$pre(tags$b(text),style = paste("padding: 6px;background-color:", bg_color)))
    })
}
