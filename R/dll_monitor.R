

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
