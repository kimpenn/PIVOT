



output$biomart_ui <- renderUI({
    box(
        title = "Biomart Gene Annotation",
        status = "primary",
        width = NULL,
        solidHeader = T,
        tags$div(tags$b("Load Ensembl Database:"), class = "param_setting_title"),
        fluidRow(
            column(3, uiOutput("biomart_species_ui")),
            column(2,
                   tags$br(),
                   actionButton("bmart_load_db", "Load Database", class = "btn btn-info btn_alignLeft")
            ),
            column(1, tags$br(), uiOutput("bmart_loaded_png"))
        ),
        fluidRow(
            column(6, DT::dataTableOutput("bmart_filters")),
            column(6, uiOutput("bmart_attributes_ui"))
        )

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
        bmart$db = biomaRt::useMart("ensembl",dataset=input$bmart_species)
    })
})

output$bmart_loaded_png <- renderUI({
    if(is.null(bmart$db)) return()
    img(src = "button_ok.png", width = 35, height = 35)
})

output$bmart_filters <- DT::renderDataTable({
    if(is.null(bmart$db)) return()
    filters = biomaRt::listFilters(ensembl)
    filters <- filters %>% dplyr::filter(!grepl("with_|transcript_count",name))
    DT::datatable(filters, options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE))
})
