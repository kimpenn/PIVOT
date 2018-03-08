# Copyright (c) 2015-2018 Qin Zhu and Junhyong Kim, University of Pennsylvania.
# All Rights Reserved.
#
# You may not use this file except in compliance with the Kim Lab License
# located at
#
#     http://kim.bio.upenn.edu/software/LICENSE
#
# Unless required by applicable law or agreed to in writing, this
# software is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License
# for the specific language governing permissions and limitations
# under the License.

main_info <- list(
    Module = "PIVOT.base",
    Description = "Main analysis module of PIVOT, including data input and management, DE, clustering, heatmap, dimension reduction, etc. ",
    Depend = c("PIVOT"),
    Citation = "Qin Zhu, Stephen A Fisher, Hannah Dueck, Sarah Middleton, Mugdha Khaladkar and Junhyong Kim. PIVOT: Platform for Interactive Analysis and Visualization of Transcriptomics Data (Preprint) bioRxiv 053348"
)

deseq_info <- list(
    Module = "DESeq2",
    Description = "Differential expression based on a model using the negative binomial distribution by Michael I Love, Wolfgang Huber and Simon Anders (2014).",
    Depend = c("DESeq2"),
    Citation = "Michael I Love, Wolfgang Huber and Simon Anders (2014): Moderated estimation of fold change and dispersion for RNA-Seq data with DESeq2. Genome Biology"
)

edgeR_info <- list(
    Module = "edgeR",
    Description = "Differential expression analysis of digital gene expression data by Yunshun Chen, Davis McCarthy, Matthew Ritchie, Mark Robinson, Gordon K. Smyth.",
    Depend = c("edgeR"),
    Citation = "Robinson MD, McCarthy DJ and Smyth GK (2010) and McCarthy DJ, Chen Y and Smyth GK (2012)."
)

scde_info <- list(
    Module = "scde",
    Description = "Single cell differential expression analysis by Kharchenko P and Fan J (2016).",
    Depend = c("scde"),
    Citation = "Kharchenko P and Fan J (2016). scde: Single Cell Differential Expression. R package version 2.2.0, http://pklab.med.harvard.edu/scde."
)

monocle_info <- list(
    Module = "monocle",
    Description = "Clustering, differential expression, and trajectory analysis for single- cell RNA-Seq by Cole Trapnell and Davide Cacchiarelli et al (2014).",
    Depend = c("HSMMSingleCell"),
    Citation = "Cole Trapnell and Davide Cacchiarelli et al (2014): The dynamics and regulators of cell fate decisions are revealed by pseudo-temporal ordering of single cells. Nature Biotechnology."
)

network_info <- list(
    Module = "PIVOT.network",
    Description = "Visualization of interactome/regulome network and transdifferentiation factor prediction with Mogrify-like method.",
    Depend = c("STRINGdb"),
    Citation = "Franceschini, A (2013). STRING v9.1: protein-protein interaction networks, with increased coverage and integration.
  In:'Nucleic Acids Res. 2013 Jan;41:D808-15. doi: 10.1093/nar/gks1094. Epub 2012 Nov 29'."
)

caret_info <- list(
    Module = "caret",
    Description = "The caret package is a set of functions that attempt to streamline the process for creating predictive models.",
    Depend = c("caret"),
    Citation = "Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, Chris Keefer, Allan Engelhardt, Tony Cooper,
  Zachary Mayer, Brenton Kenkel, the R Core Team, Michael Benesty, Reynald Lescarbeau, Andrew Ziem, Luca Scrucca, Yuan
    Tang, Can Candan and Tyler Hunt. (2017). caret: Classification and Regression Training. R package version 6.0-76.
    https://CRAN.R-project.org/package=caret"
)

toolkit_info <- list(
    Module = "PIVOT.toolkit",
    Description = "A set of tools for drawing Venn Diagram, converting gene name to gene id, etc.",
    Depend = c("PIVOT"), # For now we don't have many scripts for this module so it is included in PIVOT.analysis
    Citation = "Qin Zhu, Stephen A Fisher, Hannah Dueck, Sarah Middleton, Mugdha Khaladkar and Junhyong Kim. PIVOT: Platform for Interactive Analysis and Visualization of Transcriptomics Data (Preprint) bioRxiv 053348"
)

module_tbl <- as.data.frame(rbind(
    main_info, deseq_info, edgeR_info, scde_info, monocle_info, network_info, caret_info, toolkit_info
))

module_tbl <- cbind(ID = seq.int(nrow(module_tbl)), module_tbl)



#' PIVOT laucher
#'
#' Serves as a gateway for all analysis modules and allow users to monitor the R session state and clean when needed.
#' @examples
#' pivot_launcher()
#' @import shiny miniUI DT
#' @export
pivot_launcher <- function() {

    ui <- miniUI::miniPage(
        miniUI::gadgetTitleBar("PIVOT Launcher",
                       left = NULL,
                       right = NULL),
        miniUI::miniTabstripPanel(
            id = "tabstrip",
            miniUI::miniTabPanel("Main",
                                 icon = icon("flask"),
                                 miniUI::miniContentPanel(
                                     fillCol(flex = c(NA,NA, 1),
                                             dllUI("module1"),
                                             uiOutput("pkg_info_ui"),
                                             DT::dataTableOutput("workflow_tbl")
                                     )
                                 )
            ),
            miniUI::miniTabPanel("Info",
                                 icon = icon("map-pin"),
                                 miniUI::miniContentPanel(
                                     fillCol(flex = c(NA,1),
                                             dllUI("module2"),
                                             uiOutput("data_status")
                                     )
                                 )
            ),
            between = list(
                #tags$div(id = "max_dll_monitor", class = "shiny-html-output", style = "background-color: #f2f2f2; text-align: center;"),
                miniUI::miniButtonBlock(
                    actionButton("launch_module", label = "Set Module", class = "btn-primary", onclick = "setTimeout(function(){window.close();}, 100); "),
                    #actionButton("launch_module", "Launch Module", class = "btn-primary"),
                    actionButton("clean_session", label = "Clean DLLs", class = "btn-success", onclick = "setTimeout(function(){window.close();}, 100);) ")
                )
            )
        )
    )

    server <- function(input, output, session) {
        # For storing which rows have been excluded
        callModule(dllText,"module1")
        callModule(dllText,"module2")

        # Credits to Yihui Xie, https://github.com/rstudio/DT/issues/93
        shinyInput <- function(FUN, len, id, ...) {
            inputs <- character(len)
            for (i in seq_len(len)) {
                inputs[i] <- as.character(FUN(paste0(id, i), ...))
            }
            inputs
        }

        shinyInputVal <- function(FUN, len, id, cVal, ...)  {
            inputs <- character(len)
            for (i in seq_len(len)) {
                inputs[i] <- as.character(FUN(paste0(id, i), value = cVal[i], ...))
            }
            inputs
        }

        shinyValue = function(id, len) {
            unlist(lapply(seq_len(len), function(i) {
                value = input[[paste0(id, i)]]
                if (is.null(value)) NA else value
            }))
        }


        # Handle the Done button being pressed.
        observeEvent(input$done, {
            stopApp()
        })

        # Handle launch_module button

        observe({
            if(not_pressed(input$launch_module)) return()
            # quit R, unless you are running an interactive session

            if(interactive()) {
                isolate({
                    stopApp("Modules successfully set. Launch pivot by command `pivot()`.")
                })
            } else {
                stopApp("Modules successfully set. Launch pivot by command `pivot()`.")
                q("no")
            }
        })

        observe({
            if(not_pressed(input$clean_session)) return()
            # quit R, unless you are running an interactive session

            if(interactive()) {
                isolate({
                    stopApp("Performing DLL clean up... (Please use command clean_pivotSession() if new window does not show up.)")
                    sendToConsole("clean_pivotSession()", execute = T)
                })
            } else {
                stopApp()
                q("no")
            }
        })

        output$data_status <- renderUI({
            if(!exists("r_data") || is.null(r_data$glb.raw)) {
                return(list(
                    tags$b("Data Input Instructions:"),
                    tags$p("1. Select which modules you want to use for the analysis."),
                    tags$p("2. Press launch button.")
                ))
            } else {
                list(
                    uiOutput("input_file_info"),
                    DT::dataTableOutput("data_info")
                )
            }
        })

        ##### Data Info boxes

        output$data_info <- DT::renderDataTable({
            if(is.null(r_data$sample_stats)) return()
            DT::datatable(r_data$sample_stats, style = "bootstrap",
                          options = list(dom = 'tipr'))
        })

        output$input_file_info <- renderUI({
            if(is.null(r_data$file_info)) {
                file_path = NULL
            } else if(r_data$file_info$type == "single"){
                file_path = tags$li(paste("Input file:", r_data$file_info$name))
            }
            else if(r_data$file_info$type == "dir") {
                file_path = tags$li(paste("Input Directory:", r_data$file_info$path[[length(r_data$file_info$path)]]))
            }

            num_features = paste("Number of selected features:", nrow(r_data$raw))
            num_samples = paste("Number of selected samples:", ncol(r_data$raw))
            #num_cats = paste("Number of design categories:", ncol(r_data$glb.meta) - 1)
            return(list(
                file_path,
                tags$li(num_features),
                tags$li(num_samples)
                #tags$li(num_cats)
            ))
        })

        info_tbl <- reactive({
            info_tbl <- module_tbl
            packList <-rownames(installed.packages())

            cVal = c(T,T,F,F,F,F,F,T)

            info_tbl$Pick <- shinyInputVal(checkboxInput, nrow(info_tbl), 'pick_', cVal = cVal, label = NULL, width = '50px')

            info_tbl$Pick[which(!info_tbl$Depend %in% c(packList))] <- ""

            info_tbl$Status <- shinyInput(actionButton, nrow(info_tbl), 'button_', label = "Install", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')

            info_tbl$Status[which(info_tbl$Depend %in% c(packList))] <- "Installed"
            # Rearranging the columns
            return(info_tbl)
        })


        output$workflow_tbl <- DT::renderDataTable(
            info_tbl()[,c("ID", "Pick", "Module", "Status", "Description")], rownames = F, escape = FALSE, selection = 'none', options = list(
                dom = 't', paging = FALSE, ordering = FALSE,
                preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
            )
        )

        observeEvent(input$select_button, {
            selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
            selectedPkg <- info_tbl()$Depend[selectedRow]

            # Install code here, sort out later
            #stopApp("launching data module... (if new window does not show up, please use command launch_pivotData() or try again)")
            #sendToConsole("launch_pivotData()", execute = T)
            print(paste("Installing selected package:", selectedPkg, ". Please close the window and restart after the package being installed with pivot()"))
            source("https://bioconductor.org/biocLite.R")
            biocLite(selectedPkg, suppressUpdates = T, ask = F)
            print("Done!")
        })

        modules<-reactive({
            unlist(info_tbl()$Module[which(shinyValue('pick_', nrow(info_tbl())))])
        })

        output$pkg_info_ui <- renderUI({
            if("monocle" %in% modules()) {
                tags$p("Note: Monocle 2 has multiple dependecies. Please limit your module choice if you have not increased MAX_NUM_DLL.")
            } else {
                tags$p("Please select modules you want to use. Then use pivot() to launch the main program.")
            }
        })


        # Save selected module to global on exit
        saveModule <- function(session = session) {
            session$onSessionEnded(function() {
                isolate({
                    assign("r_module", modules(), envir = .GlobalEnv)
                })
            })
        }
        saveModule(session)
    }

    runGadget(ui, server, viewer = dialogViewer("PIVOT: Platform for Interactive Analysis and Visualization of Transcriptomics Data, v1.0.0"))
}


#' perform session clean
#'
#' @export
clean_pivotSession <- function(){
    detachAllPackages(); .rs.restartR(afterRestartCommand = 'pivot()')
}


#' Show available modules of PIVOT
#'
#' @export
pivot_module <- function() {
    print("Current available PIVOT modules: (Use pivot(#ID_vector)) such as pivot(c(1,3,5)) to launch)")
    module_tbl<-module_tbl[,c("ID", "Module"), drop = F]
    print.data.frame(module_tbl, row.names = F)
}

#' Launch PIVOT with default all seleceted modules
#'
#' @export
pivot <- function(s=NULL) {
    if(is.null(s)) {
        if(exists("r_module")) {
            print(paste("Launching previous selected module:", paste(r_module, collapse = ", ")))
        } else {
            mods = module_tbl$Module[match(1:8, module_tbl$ID)]
            print(paste("Launching default module:", paste(mods, collapse = ",")))
            assign("r_module", mods, envir = .GlobalEnv)
        }
        if("monocle" %in% r_module) {
            require(monocle)
        }
        pivot_main()
    } else if (is.numeric(s) && all(s%in%1:8)){
        mods = module_tbl$Module[match(s, module_tbl$ID)]
        assign("r_module", mods, envir = .GlobalEnv)
        if("monocle" %in% r_module) {
            require(monocle)
        }
        pivot_main()
    } else {
        print("Unknown argument. Use pivot_module() to discover modules currently implemented in PIVOT.")
        return()
    }
}




