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




output$biomart_ui <- renderUI({
    box(
        title = "Biomart Gene Annotation",
        status = "primary",
        width = NULL,
        solidHeader = T,
        tags$div(tags$b("Load Ensembl Database:"), class = "param_setting_title"),
        fluidRow(
            #column(4, selectInput("bmart_mode", "Query mode", choices = list("Advanced Query" = "advanced", "Name/ID Conversion" = "simple"))),
            column(4, uiOutput("biomart_species_ui")),
            column(2,
                   tags$br(),
                   actionButton("bmart_load_db", "Load Database", class = "btn btn-info btn_alignLeft")
            ),
            column(2, tags$br(), uiOutput("bmart_loaded_png"))
        ),
        # conditionalPanel("input.bmart_mode == 'advanced'",
                         tags$p("Assume you have a list of HGNC gene symbol, and want to find corresponding entrez gene ids, then do the following:"),
                         tags$li("Select filter, hgnc_symbol in the filters table."),
                         tags$li("Upload HGNC gene symbol lists by clicking the hgnc_symbol button."),
                         tags$li("Choose hgnc_symbol, entrezgene in the attribute table, and press the query button."),
                         tags$li("See https://bioconductor.org/packages/release/bioc/vignettes/biomaRt/inst/doc/biomaRt.html for more examples."),
                         tags$hr(),
                         fluidRow(
                             column(6,
                                    tags$div(tags$b("Select Filters:"), class = "param_setting_title"),
                                    DT::dataTableOutput("bmart_filters")
                             ),
                             column(6,
                                    tags$div(tags$b("Select Attributes:"), class = "param_setting_title"),
                                    DT::dataTableOutput("bmart_attributes")
                             )
                         ),
                         tags$hr(),
                         fluidRow(
                             column(6,
                                    fluidRow(
                                        column(4,
                                               tags$b("Selected Filters:"),
                                               tags$br(),
                                               tags$br(),
                                               uiOutput("bmart_filter_btns")
                                        ),
                                        column(8,
                                               tags$b("Filter features:"),
                                               tags$br(),
                                               tags$br(),
                                               uiOutput("bmart_lists")
                                        )
                                    )
                             ),
                             column(6,
                                    tags$b("Requested Attributes:"),
                                    tags$br(),
                                    tags$br(),
                                    uiOutput("bmart_attribute_btns"),
                                    tags$hr(),
                                    tags$p("Press this button when you have input your filters and selected the attributes."),
                                    actionButton("bmart_query", "Query Biomart", class = "btn-primary"),
                                    tags$br(),
                                    tags$br(),
                                    tags$p("Press this button to reset all query."),
                                    actionButton("bmart_reset", "Reset All", class = "btn-danger")
                             )
                         ),
                         tags$br(),
                         tags$br(),
                         tags$div(tags$b("Query Result:"), class = "param_setting_title"),
                         DT::dataTableOutput("bmart_result"),
                         downloadButton("download_bmart_result", "Download", class = "btn-success btn_rightAlign")
        # ),
        # conditionalPanel("input.bmart_mode == 'nameid'",
        #     tags$p("ADD here")
        # )
    )
})


bmart <- reactiveValues()

output$biomart_species_ui <- renderUI({
    db <- biomaRt::useMart("ensembl")
    if(is.null(db)) {
        session$sendCustomMessage(type = "showalert", "Failed to load biomart database. Please make sure you have internet connection.")
    }
    species<-biomaRt::listDatasets(db)
    species_options <- species$dataset
    names(species_options) <- species$description
    selectInput("bmart_species","Choose Species:", choices = species_options, selected = "hsapiens_gene_ensembl")
})

observeEvent(input$bmart_load_db, {
    if(is.null(input$bmart_species)) return()
    withProgress(message = 'Loading database...', value = 0.3, {
        tryCatch({
            bmart$db = biomaRt::useMart("ensembl",dataset=input$bmart_species)
        }, error=function (e){
            showNotification("Request to BioMart web service failed. Please make sure you have internet connection.", type="error", duration=10)
            return()
        })
    })
})

output$bmart_loaded_png <- renderUI({
    req(bmart$db)
    img(src = "button_ok.png", width = 35, height = 35)
})

output$bmart_filters <- DT::renderDataTable({
    req(bmart$db)
    input$bmart_reset
    filters = biomaRt::listFilters(bmart$db)
    bmart$filters <- filters %>% dplyr::filter(!grepl("with_|transcript_count",name))
    DT::datatable(bmart$filters, options = list(scrollX = TRUE, scrollY = "350px", paging = FALSE))
})

output$bmart_attributes <- DT::renderDataTable({
    req(bmart$db)
    input$bmart_reset
    bmart$attributes = biomaRt::listAttributes(bmart$db)
    DT::datatable(bmart$attributes, options = list(scrollX = TRUE, scrollY = "350px", paging = FALSE))
})

output$bmart_filter_btns <- renderUI({
    req(bmart$filters)
    s = input$bmart_filters_rows_selected
    if (length(s)) {
        filters<-bmart$filters[s, 'name', drop = TRUE]
    } else {
        return()
    }
    if(length(filters) > 6) {
        return(tags$p("Please specify less filters."))
    }

    bmart$selected_filters <- filters

    lapply(filters, function(x){
        return(list(pivot_featureInputModal_UI(x, label = x), tags$br(), tags$br()))
    })
})


observe({
    req(bmart$selected_filters)
    flists <-lapply(bmart$selected_filters, function(x){
        res<-callModule(pivot_featureInputModal, x, r_data = r_data, match_rdata = F)
        return(res())
    })
    names(flists) <- bmart$selected_filters
    bmart$flists <- flists
})


output$bmart_lists <- renderUI({
    req(bmart$flists)
    lists_text <- list()
    for(i in 1:length(bmart$flists)) {
        cur_len <- length(bmart$flists[[i]])
        if(cur_len > 10) {
            cur_list_show <- paste0(paste(bmart$flists[[i]][1:10], collapse = ", "), "... (", cur_len, " features)")
        } else {
            cur_list_show <- paste(bmart$flists[[i]], collapse = ", ")
        }

        lists_text[[i]] <- tagList(
            tags$li(paste0(names(bmart$flists[i]), ": ", cur_list_show)),
            tags$br()
        )
    }
    return(lists_text)
})


output$bmart_attribute_btns <- renderUI({
    req(bmart$attributes)
    s = input$bmart_attributes_rows_selected
    if (length(s)) {
        attributes<-bmart$attributes[s, 'name', drop = TRUE]
    } else {
        return()
    }

    bmart$selected_attributes <- attributes

    lapply(attributes, function(x){
        return(actionButton(x, label = x, class = "btn-success"))
    })
})


# Biomart query

observeEvent(input$bmart_query, {
    if(any(sapply(bmart$flists, is.null))) {
        session$sendCustomMessage(type = "showalert", "You did not specify some of your filters. Please recheck.")
        return()
    }
    if(is.null(bmart$selected_attributes)) {
        session$sendCustomMessage(type = "showalert", "Please select at least one attribute.")
        return()
    }
    withProgress(message = 'Querying database...', value = 0.3, {
        bmart$result <-biomaRt::getBM(
            filters= bmart$selected_filters,
            attributes= bmart$selected_attributes,
            values= bmart$flists,
            mart= bmart$db)
    })
})


output$bmart_result <- DT::renderDataTable({
    req(bmart$result)
    DT::datatable(bmart$result, options = list(scrollX = TRUE, scrollY = "350px"))
})


output$download_bmart_result <- downloadHandler(
    filename = function() { paste0("biomart_query-",lubridate::now(),".csv") },
    content = function(file) {
        req(bmart$result)
        write.csv(bmart$result, file)
    }
)









