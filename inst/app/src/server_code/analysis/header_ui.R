


# The feature info list in header
output$featureMenu <- renderMenu({
    if(is.null(r_data$raw)) {
        sample_num <- 0
    } else {
        sample_num <- ncol(r_data$raw)
    }
    dropdownMenu(type = "notifications",
                 top_msg = "Data Information",
                 notificationItem(
                     text = paste(nrow(r_data$raw), "selected features"),
                     icon("align-justify")
                 ),
                 notificationItem(
                     text = paste0(round(nrow(r_data$raw)/nrow(r_data$glb.raw), digits = 3) * 100, "% ", "of input non-zero features"),
                     icon("pie-chart"),
                     status = "success"
                 ),
                 notificationItem(
                     text = paste0(round(sum(r_data$raw)/sum(r_data$glb.raw), digits = 3) * 100, "% ", "of all reads"),
                     icon = icon("server"),
                     status = "success"
                 ),
                 notificationItem(
                     text = paste0(sample_num, " selected samples"),
                     icon = icon("cubes"),
                     status = "primary"
                 ),
                 notificationItem(
                     text = paste0(ncol(r_data$meta) - 1, " design categories"),
                     icon = icon("sitemap"),
                     status = "info"
                 )
    )
})

# tab switch control
observeEvent(input$report_save_sc, {
    updateTabItems(session, "tabs", "report")
})

observeEvent(input$data_load_sc, {
    updateTabItems(session, "tabs", "data")
    updateTabsetPanel(session, "input_tabset", "file_in")
})

observeEvent(input$group_load_sc, {
    updateTabItems(session, "tabs", "data")
    updateTabsetPanel(session, "input_tabset", "group_in")
})

observeEvent(input$feature_filter_sc, {
    updateTabItems(session, "tabs", "data")
    updateTabsetPanel(session, "input_tabset", "feature_in")
})

observeEvent(input$data_subset_sc, {
    updateTabItems(session, "tabs", "data")
    updateTabsetPanel(session, "input_tabset", "sample_in")
})

observeEvent(input$manual_sc, {
    updateTabItems(session, "tabs", "manual_file")
})

observeEvent(input$version_sc, {
    updateTabItems(session, "tabs", "about")
})

