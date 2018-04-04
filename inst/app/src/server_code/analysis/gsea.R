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


output$gsea_ui <- renderUI({

    list(
        enhanced_box(
            width = 12,
            title = "Enrichment Analysis",
            id = "gsea",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("Enrichment Analysis Settings:"), class = "param_setting_title"),
            fluidRow(
                column(4,
                       selectInput("gsea_type", "Analysis Type", choices = list("Gene Ontology Analysis" = "go", "KEGG Pathway Analysis" = "kegg"))
                ),
                column(4,
                       uiOutput("gsea_species_ui")
                ),
                column(4, tags$br(),
                       checkboxInput("gsea_id_convert",
                                     tags$span("Convert gene name to id", shinyBS::tipify(
                                         bsButton("gsea_id_convert_tooltip", label = NULL, icon = icon("question-circle"), style = "link", size = "extra-small"),
                                         title = "Check this if your input list is gene name. PIVOT will automatically convert it to entrez id. If you have already converted names to id in the Feature panel, uncheck this to proceed.",
                                         options = list(container = "body")
                                     )),
                                     value = T)
                    )
            ),
            fluidRow(
                pivot_featureList_UI("gsea", include = c("custom", "deseq", "edgeR", "scde", "monocle", "mwu"), selected = "custom", width = 8),
                uiOutput("gsea_feature_preview")
            ),
            fluidRow(
                column(4,
                       selectInput("gsea_bg_type", "Background Set", choices = list("All expressed genes" = "expressed", "All Entrez Gene IDs" = "all", "Custom background" = "custom"))
                ),
                uiOutput("gsea_custom_bg_ui"),
                uiOutput("gsea_bg_preview")
            ),
            fluidRow(
                column(4),
                column(4, numericInput("gsea_top_num", "Top GO term #", value = 100, min =1, step=1)),
                column(4, uiOutput("run_gsea_ui"))
            )
        ),
        enhanced_box(
            width = 12,
            title = NULL,
            status = "primary",
            solidHeader = T,
            tags$div(tags$b("Result Table:"),  class = "param_setting_title"),
            DT::dataTableOutput("gsea_result"),
            downloadButton("download_gsea_result","Download", class = "btn-success btn_rightAlign")
        ),
        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("R Ritchie, M.E., Phipson, B., Wu, D., Hu, Y., Law, C.W., Shi, W., and Smyth, G.K. (2015). limma powers differential
  expression analyses for RNA-sequencing and microarray studies. Nucleic Acids Research 43(7), e47.", class = "citation")
                )
        )
    )
})

output$gsea_feature_preview <- renderUI({
    if(is.null(gsea_feature_tbl()) || nrow(gsea_feature_tbl()) == 0) {
        column(4,
            tags$br(),
            tags$p("Feature set not detected.")
        )
    } else {
        modal_content <- list(
            DT::dataTableOutput("gsea_feature_pv_tbl")
        )
        column(4,
               tags$br(),
               actionButton("gsea_feature_pv_btn", paste0("Preview loaded feature set (", nrow(gsea_feature_tbl()), ")"), class = "btn-success"),
               shinyBS::bsModal(id = "gsea_feature_pv_modal", "Loaded features", "gsea_feature_pv_btn", size = "large", modal_content)
        )
    }
})

output$gsea_feature_pv_tbl <- DT::renderDataTable({
    req(gsea_feature_tbl())
    DT::datatable(gsea_feature_tbl())
})

gsea_bg_custom <- callModule(pivot_featureInputModal, "gsea_bg", r_data = r_data, match_rdata = F)
gsea_bg <- reactive({
    req(input$gsea_bg_type)
    if(input$gsea_bg_type == "custom") {
        req(gsea_bg_custom())
        bg <- gsea_bg_custom()
    } else if(input$gsea_bg_type == "expressed"){
        bg <- rownames(r_data$df)
    } else if(input$gsea_bg_type == "all") {
        bg <- NULL
    }
    return(bg)
})

output$gsea_custom_bg_ui <- renderUI({
    req(input$gsea_bg_type)
    if(input$gsea_bg_type == "custom") {
        column(4,
            tags$br(),
            pivot_featureInputModal_UI("gsea_bg", "Input custom background list")
        )
    }
})

output$gsea_bg_preview <- renderUI({
    if(!is.null(gsea_bg())) {
        modal_content <- list(
            DT::dataTableOutput("gsea_bg_pv_tbl")
        )
        column(4,
               tags$br(),
               actionButton("gsea_bg_pv_btn", paste0("Preview loaded background set (", length(gsea_bg()), ")"), class = "btn-success"),
               shinyBS::bsModal(id = "gsea_bg_pv_modal", "Loaded background", "gsea_bg_pv_btn", size = "large", modal_content)
        )
    }
})

output$gsea_bg_pv_tbl <- DT::renderDataTable({
    req(gsea_bg())
    DT::datatable(data.frame(background = gsea_bg()))
})

gsea_feature_tbl <- callModule(pivot_featureList, "gsea", r_data = r_data)

output$run_gsea_ui <- renderUI({
    req(gsea_feature_tbl())
    list(
        tags$br(),
        actionButton("run_gsea", "Run Analysis", class = "btn-primary btn_leftAlign")
    )
})

output$gsea_species_ui <- renderUI({
    db <- biomaRt::useMart("ensembl")
    if(is.null(db)) {
        session$sendCustomMessage(type = "showalert", "Failed to load biomart database. Please make sure you have internet connection.")
    }
    species<-biomaRt::listDatasets(db)
    species_options <- species$dataset
    names(species_options) <- species$description
    selectInput("gsea_species","Choose Species:", choices = species_options[which(species_options %in% c("hsapiens_gene_ensembl", "mmusculus_gene_ensembl"))],
                selected = "hsapiens_gene_ensembl")
})

observeEvent(input$run_gsea, {
    bglist <- gsea_bg()
    flist <- gsea_feature_tbl()$gene
    if(!length(flist)) {
        session$sendCustomMessage(type = "showalert", "Please input feature set.")
        return()
    }

    withProgress(message = 'Processing...', value = 0.2, {
        if(input$gsea_id_convert) {
            incProgress(amount = 0.2, detail = "Converting gene name to entrez gene ID...")
            bmart_db = biomaRt::useMart("ensembl",dataset=input$gsea_species)
            flist <- unique(biomaRt::getBM(
                filters = "external_gene_name",
                attributes="entrezgene",
                values= flist,
                mart= bmart_db))
            flist <- flist$entrezgene
            if(!is.null(bglist) && length(bglist)) {
                bglist <- unique(biomaRt::getBM(
                    filters = "external_gene_name",
                    attributes="entrezgene",
                    values= bglist,
                    mart= bmart_db))
                bglist <- bglist$entrezgene
            } else {
                bglist <- NULL
            }
        } else {
            flist <- gsub("^X", "", flist)
            bglist <-  gsub("^X", "", bglist)
        }

        if(input$gsea_species == "hsapiens_gene_ensembl") {
            species = "Hs"
        } else if(input$gsea_species == "mmusculus_gene_ensembl") {
            species = "Mm"
        }

        incProgress(amount = 0.2, detail = "Running enrichment analysis...")
        error_I <- 0
        if(input$gsea_type == "go") {
            tryCatch({
                r_data$gsea <- limma::topGO(limma::goana(de = flist, universe = bglist, species = species), number = input$gsea_top_num)
            }, error = function(e) {
                error_I <<- 1
                print(e)
            })
        } else if(input$gsea_type == "kegg") {
            tryCatch({
                r_data$gsea <- limma::topKEGG(limma::kegga(de = flist, universe = bglist, species = species), number = input$gsea_top_num)
            }, error = function(e) {
                error_I <<- 1
                print(e)
            })
        }
        if(error_I) {
            session$sendCustomMessage(type = "showalert", "Failed to run enrichment analysis, please try different background set.")
            return()
        }
    })
})


output$gsea_result <- DT::renderDataTable({
    req(r_data$gsea)
    DT::datatable(r_data$gsea)
})


output$download_gsea_result <- downloadHandler(
    filename = function() {
        "gsea_results.csv"
    },
    content = function(file) {
        tbl<-as.data.frame(r_data$gsea)
        write.csv(tbl, file)
    }
)

