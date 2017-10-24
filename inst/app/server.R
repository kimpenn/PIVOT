# Copyright (c) 2015,2016, Qin Zhu and Junhyong Kim, University of Pennsylvania.
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


shinyServer(function(input, output, session) {
    # load ERCC info (data files from ERCC product website)
    erccStds <- read.table("src/built_in_files/ercc_standard_mix_conc.txt", header=T, row.names=1)
    erccStds$ERCC_ID <- make.names(erccStds$ERCC_ID)
    rownames(erccStds) <- erccStds$ERCC_ID

    # load previous state if available
    ip_inputs <- paste0("r_state")
    ip_data <- paste0("r_data")
    ip_module <- paste0("r_module")
    if (exists("r_state") && exists("r_data")) {
        r_data <- do.call(reactiveValues, r_data)
        lapply(names(r_state),
               function(x) session$sendInputMessage(x, list(value = r_state[[x]]))
        )
        rm(r_data, r_state, envir = .GlobalEnv)
    } else {
        r_state <- list()
        r_data <- init_state(reactiveValues())
    }

    lapply(list.files("src/server_code", pattern = "\\.(r|R)$", recursive = TRUE, full.names = TRUE), function(x){source(file = x, local = TRUE)})

    if(!exists("r_module")) {
        r_module <- c('PIVOT.analysis')
    } else {
        r_module <- r_module
    }

    r_env <<- pryr::where("r_data")

    ################################################################################
    # function to save app state on refresh or crash
    # From Vincent Nijs, radiant package
    ################################################################################
    saveStateOnRefresh <- function(session = session) {
        session$onSessionEnded(function() {
            isolate({
                if(not_pressed(input$session_clear_sc) &&
                   not_pressed(input$exit_and_save) &&
                   not_pressed(input$return_btn_sc) &&
                   is.null(input$uploadState) &&
                   not_pressed(input$load_example)) {
                    assign(ip_inputs, lapply(reactiveValuesToList(input), unclass), envir = .GlobalEnv)
                    assign(ip_data, reactiveValuesToList(r_data), envir = .GlobalEnv)
                    assign(ip_module, r_module, envir = .GlobalEnv)
                    #assign(ip_dump, lubridate::now(), envir = .GlobalEnv)
                    try(rm(r_env, envir = .GlobalEnv), silent = TRUE)
                }
            })
        })
    }

    #######################################
    # Save state
    #######################################
    saveState <- function(filename) {
        isolate({
            #LiveInputs <- reactiveValuesToList(input)
            #r_state[names(LiveInputs)] <- LiveInputs
            r_state <- lapply(reactiveValuesToList(input), unclass)
            r_data <- reactiveValuesToList(r_data)
            r_module <- r_module
            save(r_state, r_data, r_module, file = filename)
        })
    }

    output$state_save_sc <- downloadHandler(
        filename = function() { paste0("PIVOTState-",Sys.Date(),".rda") },
        content = function(file) {
            saveState(file)
        }
    )

    #######################################
    # Load previous state
    #######################################
    observe({
        inFile <- input$uploadState
        if(!is.null(inFile)) {
            isolate({
                tmpEnv <- new.env()
                load(inFile$datapath, envir=tmpEnv)
                if (exists("r_data", envir=tmpEnv, inherits=FALSE)){
                    assign(ip_data, tmpEnv$r_data, envir=.GlobalEnv)
                }
                if (exists("r_state", envir=tmpEnv, inherits=FALSE)) {
                    assign(ip_inputs, tmpEnv$r_state, envir=.GlobalEnv)
                    lapply(names(r_state),
                           function(x) session$sendInputMessage(x, list(value = r_state[[x]]))
                    )
                }
                if (exists("r_module", envir=tmpEnv, inherits=FALSE)){
                    assign(ip_module, tmpEnv$r_module, envir=.GlobalEnv)
                }
                #assign(ip_dump, lubridate::now(), envir = .GlobalEnv)
                rm(tmpEnv)
            })
        }
    })

    output$refreshOnUpload <- renderUI({
        inFile <- input$uploadState
        if(!is.null(inFile)) {
            # Joe Cheng: https://groups.google.com/forum/#!topic/shiny-discuss/Olr8m0JwMTo
            tags$script("window.location.reload();")
        }
    })


    # Load example dataset
    observe({
        if(not_pressed(input$load_example)) return()
        isolate({
            withProgress(message = 'Loading data...', value = 0.5, {
                tmpEnv <- new.env()
                load("src/built_in_files/example_state_dueck.rda", envir=tmpEnv)
                if (exists("r_data", envir=tmpEnv, inherits=FALSE)){
                    assign(ip_data, tmpEnv$r_data, envir=.GlobalEnv)
                }
                if (exists("r_state", envir=tmpEnv, inherits=FALSE)) {
                    assign(ip_inputs, tmpEnv$r_state, envir=.GlobalEnv)
                    lapply(names(r_state),
                           function(x) session$sendInputMessage(x, list(value = r_state[[x]]))
                    )
                }
                if (exists("r_module", envir=tmpEnv, inherits=FALSE)){
                    assign(ip_module, tmpEnv$r_module, envir=.GlobalEnv)
                }
                #assign(ip_dump, lubridate::now(), envir = .GlobalEnv)
                rm(tmpEnv)
                setProgress(1)
            })
        })
    })


    output$refreshOnExampleLoad <- renderUI({
        if(not_pressed(input$load_example)) return()
        return(tags$script("window.location.reload();"))
    })



    # Close app observer
    observe({
        if(not_pressed(input$return_btn_sc) && not_pressed(input$exit_and_save)) return()
        # quit R, unless you are running an interactive session
        if(interactive()) {
            assign("r_state", lapply(reactiveValuesToList(input), unclass), envir = .GlobalEnv)
            assign("r_data", reactiveValuesToList(r_data), envir = .GlobalEnv)
            assign("r_module", r_module, envir = .GlobalEnv)
            try(rm(r_env, envir = .GlobalEnv), silent = TRUE) # removing the reference to the shiny environment
            if(not_pressed(input$exit_and_save)) {
                isolate({
                    stopApp("All change saved, returning to launcher...")
                    rstudioapi::sendToConsole("pivot_launcher()", execute = T)
                })
            } else {
                isolate({
                    stopApp("PIVOT closed. Relaunch using command 'pivot()'.")
                })
            }

        } else {
            stopApp()
            q("no")
        }
    })

    # Alert user if DLL exceeds maximum.

    observe({
        num_dll <- length(getLoadedDLLs())
        if(num_dll >= 95 & num_dll < 100) {
            showNotification(
                paste0("Current loaded DLLs: ", num_dll, ". Note there is an R limit (100 by default) for package loading.
                       You can solve this problem by restarting R session and launch PIVOT with less modules. Check PIVOT website for details."),
                duration = NULL,
                type = "warning"
            )
        } else if(num_dll >= 100) {
            showNotification(
                paste0("Current loaded DLLs: ", num_dll, ". Note you already reached maximum DLL with default R setting. Functions are expected to fail.
                       You can solve this problem by restarting R session and launch PIVOT with less modules. Check PIVOT website for details."),
                duration = NULL,
                action = a(href = "https://github.com/qinzhu/PIVOT"),
                type = "error"
            )
        }
    })

    saveStateOnRefresh(session)
})


