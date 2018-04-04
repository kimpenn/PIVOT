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

##### Report module #####

# UI

output$report_ui <- renderUI({
    content <- 'For more details on using R Markdown see <http://rmarkdown.rstudio.com>.\nYou can embed an R code chunk like this:\n\n```{r}\nsummary(cars)\n```\n\nYou can also embed plots, for example:\n\n```{r, echo=FALSE}\nplot(cars)\n```\n\n'
    isolate({ # First render
        if(is.null(input$report_rmd)) {
            if(!is.null(r_data$rmd)) {
                content <- r_data$rmd
            }
        }
    })

    list(
        fluidRow(
            column(
                width = 12,
                box(width = NULL,
                    title = "R-markdown report",
                    status = "info",
                    solidHeader = T,
                    collapsible = T,
                    fluidRow(
                        column(3,
                               textInput("report_title", "Title: "),
                               textInput("report_author", "Author: "),
                               radioButtons("report_toc", "Table of Contents", choices = list("Yes" = "true", "No" = "false")),
                               radioButtons("report_ns", "Number sections", choices = list("Yes" = "true", "No" = "false")),
                               selectInput("report_theme", "Theme", choices = list("Default" = "default", "Cerulean" = "cerulean", "Journal" = "journal", "Flatly" = "flatly", "Readable" = "readable", "Spacelab" = "spacelab", "United" = "united", "Cosmo" = "cosmo")),
                               #uiOutput("report_choice"),
                               tags$p("Please use the paste-to-report button on the top-right of each module to add report blocks. PIVOT will capture the last state of each module and write it to report.")
                        ),
                        column(9,
                               shinyAce::aceEditor("report_rmd", mode="markdown", height = "600px", value = content)
                        )
                    ),

                    fluidRow(
                        column(4,
                               actionButton("evalRmd", "Update Preview", class = "btn btn-info"),
                               downloadButton("savePreview", "Save Preview", class = "btn btn-success")
                        ),
                        column(5, radioButtons("rmd_dl_format", label = "Choose Format:", c("HTML" = "html", "Slides" = "slides", "R Markdown" = "rmd"), inline = T)),
                        column(3, shinyBS::tipify(downloadButton("saveRmd", "Generate & Save", class = "btn btn-warning"), title = "This will regenerate the download file in the chosen format, you can also directly save the preview html.", placement = "bottom", options = list(container = "body")))
                    )
                ),

                box(
                    width = NULL,
                    title = "Report preview",
                    status = "primary",
                    solidHeader = T,
                    collapsible = T,
                    htmlOutput("reportPreview")
                )
            )
        )
    )

})

# This function turns R scripts into Rmd code blocks.
construct_rmd_block <- function(R_file_path, echo = FALSE, message = FALSE) {
    return(paste0("```{r, echo = ", echo, ", message = ", message, "}\n", paste0(readLines(R_file_path), collapse = "\n"), "\n```\n\n"))
}

# Header info
rmd_static_codes <- reactive({
    paste0("---\ntitle: '", input$report_title, "'\nauthor: '", input$report_author,
           "'\ndate: '", Sys.Date(), "'\noutput:\n  html_document:\n    toc: ",
           input$report_toc, "\n    number_sections: ", input$report_ns,
           "\n    theme: ", input$report_theme, "\n---\n\n")
})


# Preview module
output$reportPreview <- renderUI({
    input$evalRmd
    error_I <- 0
    withProgress(message = 'Processing', value = 0, {
        isolate({
            fileConn<-file("www/tmp.rmd")
            tmp_content <- paste0(rmd_static_codes(), input$report_rmd)
            writeLines(tmp_content, fileConn)
            close(fileConn)
            incProgress(0.5, detail = "Synthesizing report...")
            tryCatch({
                rmarkdown::render(input = "www/tmp.rmd", output_format = "html_document", output_file = "../www/rmd_preview.html")},
                error = function(e) {
                    error_I <<- 1
                }
            )
        })
        setProgress(1)
    })
    if(error_I) return("PIVOT failed to generate your report. Please make sure all specified modules have been run at least once and try again. Common errors: If the current dataset do not contain ERCC reads, don't add ERCC module. If you are adding SCDE, please make sure you have run the sub-modules, otherwise remove the unused code chunk.")
    return(HTML("<iframe src='rmd_preview.html', width='100%', height='800'></iframe>"))
})

# Download preview
output$savePreview <- downloadHandler(
    filename = "report.html",
    content = function(file) {
        file.copy("www/rmd_preview.html", file)
    }
)

# Generate and Download module
output$saveRmd <- downloadHandler(
    filename = function() {
        if(input$rmd_dl_format == "rmd") {
            "report.Rmd"
        } else {
            "report.html"
        }
    },
    content = function(file) {
        tmp_content <- paste0(rmd_static_codes(), input$report_rmd)
        error_I <- 0
        if(input$rmd_dl_format == "rmd") {
            tmp_content %>% cat(.,file=file,sep="\n")
        } else {
            fileConn<-file("www/tmp.rmd")
            writeLines(tmp_content, fileConn)
            close(fileConn)
            if(input$rmd_dl_format == "html") {
                tryCatch({
                    rmarkdown::render(input = "www/tmp.rmd", output_format = "html_document", output_file = file)},
                    error = function(e) {
                        session$sendCustomMessage(type = "showalert", "PIVOT failed to generate your report. Please make sure all specified modules have been run at least once and try again.")
                        error_I <- 1
                    }
                )
            } else if(input$rmd_dl_format == "slides") {
                tryCatch({
                    rmarkdown::render(input = "www/tmp.rmd", output_format = "ioslides_presentation", output_file = file)},
                    error = function(e) {
                        session$sendCustomMessage(type = "showalert", "PIVOT failed to generate your report. Please make sure all specified modules have been run at least once and try again.")
                        error_I <- 0
                    }
                )
            }
        }
        if(error_I) return(NULL)
    }
)

# Select code block module
output$report_choice <- renderUI({
    reportFiles <- list.files("src/reports/old", pattern="\\.(md|Rmd)$", full.names = T)
    reportFiles_names <- sub("[.]Rmd", "", list.files("src/reports/old", pattern="\\.(md|Rmd)$", full.names = F))

    filelis_all <- as.list(reportFiles)
    filelis_all <- setNames(filelis_all, reportFiles_names)

    selectInput("reportFiles",
                label = "Report modules",
                choices = filelis_all,
                selected = NULL,
                multiple = TRUE,
                selectize = T
    )
})

# update aceEditor module
observe({
    # loading rmd report from disk
    inFile <- input$reportFiles

    isolate({
        if(!is.null(inFile) && !is.na(inFile)) {
            rmdfile <- ""
            for(i in 1: length(inFile)) {
                rmdfile <- update_report(rmdfile, inFile[i])
            }
            shinyAce::updateAceEditor(session, "report_rmd", value = rmdfile)
        }
    })
})


# This function returns the Rmd modules specified by the user.
update_report <- function(cur_rmd, file_path) {
    m_name <- sub("[.]Rmd", "", sub("src/reports/old/", "", file_path))

    new_block <- paste0(readLines(file_path), collapse = "\n")

    return(paste0(cur_rmd, new_block, sep = "\n"))
}

