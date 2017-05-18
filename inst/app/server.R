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

    lapply(list.files("src/server_code", pattern = "\\.(r|R)$", recursive = TRUE, full.names = TRUE), function(x){source(file = x, local = TRUE)})

    ip_inputs <- paste0("r_state")
    ip_data <- paste0("r_data")
    ip_module <- paste0("r_module")

    if(!exists("r_module")) {
        r_module <- c('PIVOT.analysis')
    } else {
        r_module <- r_module
    }

    # load previous state if available
    if (exists("r_state") && exists("r_data")) {
        r_data <- do.call(reactiveValues, r_data)
        r_state <- r_state
        rm(r_data, r_state, envir = .GlobalEnv)
    } else if (exists(ip_inputs) && exists(ip_data)) {
        r_data <- do.call(reactiveValues, get(ip_data))
        r_state <- get(ip_inputs)
        rm(list = c(ip_inputs, ip_data), envir = .GlobalEnv)
    } else {
        r_state <- list()
        r_data <- init_state(reactiveValues())
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
                   is.null(input$uploadState)) {

                    assign(ip_inputs, reactiveValuesToList(input), envir = .GlobalEnv)
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
            LiveInputs <- reactiveValuesToList(input)
            r_state[names(LiveInputs)] <- LiveInputs
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
                if (exists("r_data", envir=tmpEnv, inherits=FALSE))
                    assign(ip_data, tmpEnv$r_data, envir=.GlobalEnv)
                if (exists("r_state", envir=tmpEnv, inherits=FALSE))
                    assign(ip_inputs, tmpEnv$r_state, envir=.GlobalEnv)
                if (exists("r_module", envir=tmpEnv, inherits=FALSE))
                    assign(ip_module, tmpEnv$r_module, envir=.GlobalEnv)
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

    observe({
        if(not_pressed(input$return_btn_sc) && not_pressed(input$exit_and_save)) return()
        # quit R, unless you are running an interactive session
        if(interactive()) {
            assign("r_state", reactiveValuesToList(input), envir = .GlobalEnv)
            assign("r_data", reactiveValuesToList(r_data), envir = .GlobalEnv)
            assign("r_module", r_module, envir = .GlobalEnv)
            try(rm(r_env, envir = .GlobalEnv), silent = TRUE) # removing the reference to the shiny environment
            if(not_pressed(input$exit_and_save)) {
                isolate({
                    stopApp("All change saved, returning to launcher...")
                    rstudioapi::sendToConsole("pivot()", execute = T)
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

    saveStateOnRefresh(session)

})


