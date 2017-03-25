
# Data management

output$data_man_ui <- renderUI({

    if(is.null(r_data$glb.raw)) return(tags$p("Please upload your data first."))

    ################### Data management module ###################

    content_rename <- list(
        textInput("dmap_rename_new", NULL, value = "", placeholder = "type new name here"),
        actionButton("dmap_rename_btn", "Rename", class = "btn-warning")
    )

    content_notes <- list(
        textInput("dmap_notes_content", NULL, value = "", placeholder = "type notes here"),
        actionButton("dmap_notes_btn", "Add", class = "btn-info")
    )

    list(
        tags$div(tags$b("Data Map"),
                 pivot_help_UI("data_map", "What is a 'Data Map'", tooltip= F),
                 class = "param_setting_title"),
        fluidRow(
            column(12,
                   visNetwork::visNetworkOutput("data_inter_net", width = "100%", height = "600px")
            )
        ),
        fluidRow(
            column(9,
                   actionButton("dmap_rename_modal_trigger", label = "Rename Node", class = "btn-warning btn_leftAlign"),
                   shinyBS::bsModal(id = "dmap_rename_modal", "Enter the new name:", "dmap_rename_modal_trigger", size = "small", content_rename),
                   actionButton("dmap_notes_modal_trigger", label = "Add Notes", class = "btn-info btn_leftAlign"),
                   shinyBS::bsModal(id = "dmap_notes_modal", "Add notes:", "dmap_notes_modal_trigger", size = "medium", content_notes),
                   uiOutput("dmap_delete_ui")
            ),
            column(3,
                   uiOutput("dmap_control_ui")
            )
        )
    )
})


# Observe the data changes, add logs each time the data has been modified

output$data_history <- DT::renderDataTable({
    if(is.null(r_data$history)) return()

    DT::datatable(r_data$his_tbl,selection = 'single', rownames = FALSE, options = list(
        scrollX = T, scrollY = "500px"))
})

output$download_his_tbl <- downloadHandler(
    filename = "data_history_table.csv",
    content = function(file) {
        if(is.null(r_data$history)) return()
        write.csv(r_data$his_tbl, file)
    }
)


output$data_man_net <- renderPlot({
    if(is.null(r_data$his_nodes)) return()
    tbl_nodes <- r_data$his_nodes
    tbl_edges <- r_data$his_edges
    total_feature_num <- nrow(r_data$glb.raw)
    total_sample_num <- ncol(r_data$glb.raw)
    feature_percent<-data.frame(num1 = tbl_nodes$feature_num, num2 = total_feature_num - tbl_nodes$feature_num)
    feature_percent<- as.list(as.data.frame(t(feature_percent)))

    g <- graph_from_data_frame(tbl_edges, directed=F, vertices=tbl_nodes)

    V(g)$color <- ifelse(is.na(tbl_nodes$is_activated), "purple", "yellow")
    V(g)$pie.color <- ifelse(tbl_nodes$is_activated == "Y", list(c("darkorange1", "gold")), list(topo.colors(2)))

    V(g)$size <- ifelse(is.na(tbl_nodes$sample_num), 70, 50 * tbl_nodes$sample_num[match(V(g)$name, tbl_nodes$name)] / total_sample_num)
    V(g)$label.cex <- ifelse(is.na(tbl_nodes$sample_num), 0.8, 1.2 * tbl_nodes$sample_num[match(V(g)$name, tbl_nodes$name)] / total_sample_num)

    E(g)$label.cex <- 0.5
    E(g)$width <- ifelse(is.na(tbl_edges$label), 1, 2)
    E(g)$weight <- ifelse(is.na(tbl_edges$label), 0.5, 2)
    E(g)$color <- ifelse(is.na(tbl_edges$label), "grey", "black")

    V(g)$shape <- ifelse(is.na(tbl_nodes$sample_num), "rectangle", "pie")

    V(g)$label.color <- "white"
    V(g)$pie <- feature_percent
    V(g)$label.family <- "sans"
    V(g)$frame.color <- "white"
    plot(g, layout = layout.fruchterman.reingold)
})

output$data_inter_net <- visNetwork::renderVisNetwork({
    if(is.null(r_data$his_nodes)) return()
    if(nrow(r_data$his_nodes) < 2) return() # If less than two nodes, not enough for a tree graph
    generate_data_map(r_data$his_nodes, r_data$his_edges)
})


output$dmap_control_ui<- renderUI({
    if(is.null(input$data_inter_net_selected) || is.null(r_data$his_nodes)) return()
    s <- which(r_data$his_nodes$name == input$data_inter_net_selected)
    if(!length(s)) return()
    if(is.na(r_data$his_nodes$is_activated[s])) {
        downloadButton("dmap_report", "Download Analysis Report", class = "btn-success btn_rightAlign")
    } else if(r_data$his_nodes$is_activated[s] == "N"){
        actionButton("dmap_switch", paste("Switch to Dataset:", input$data_inter_net_selected), class = "btn-primary btn_rightAlign")
    }
})

output$dmap_delete_ui<- renderUI({
    if(is.null(r_data$his_nodes)) return()
    s <- which(r_data$his_nodes$name == input$data_inter_net_selected)
    if(!length(s)) return()
    if(is.na(r_data$his_nodes$is_activated[s])) {
        actionButton("dmap_del_report", "Delete Analysis Report", class = "btn-danger btn_leftAlign")
    } else if(s != 1){
        actionButton("dmap_del_trigger", "Delete", class = "btn-danger btn_leftAlign")
    }
})


observeEvent(input$switch_data, {
    if(is.null(r_data$his_tbl)) return()
    s = input$data_history_row_last_clicked

    if (length(s) == 1) {
        s <- which(r_data$his_tbl$name == s)
        withProgress(message = 'Switching dataset, please wait...', value = 50, {
            r_data <- switch_to_dataset(r_data, s)
        })
    } else {
        return()
    }
})

# Graph: switch to selected dataset

observeEvent(input$dmap_switch, {
    if(is.null(r_data$his_tbl)) return()
    s <- which(r_data$his_tbl$name == input$data_inter_net_selected)
    if (length(s) == 1) {
        withProgress(message = 'Switching dataset, please wait...', value = 50, {
            r_data <- switch_to_dataset(r_data, s)
        })
    } else {
        return()
    }
})

# Graph: download html report of selected dataset

output$dmap_report <- downloadHandler(
    filename = function(){
        paste0(input$data_inter_net_selected, ".html")
    },
    content = function(file) {
        if(is.null(r_data$reg_tbl)) return()
        tbl <- r_data$reg_tbl
        tbl$id <- paste(as.character(tbl$name), tbl$time)

        s <- which(tbl$id == input$data_inter_net_selected)
        if (length(s)) {
            rawHTML <- r_data$reg_history[[s]]$lists
            writeLines(rawHTML, file)
        }
    }
)


###### Rename nodes ######
observeEvent(input$dmap_rename_btn, {
    if(is.null(input$data_inter_net_selected) || input$data_inter_net_selected == "") {
        session$sendCustomMessage(type = "showalert", "Please select a node on the data map.")
        return()
    }

    if(is.null(input$dmap_rename_new) || input$dmap_rename_new == "" || nchar(trimws(input$dmap_rename_new)) == 0) {
        session$sendCustomMessage(type = "showalert", "Please enter a valid name.")
        return()
    }

    new_name <- trimws(input$dmap_rename_new)

    s1 <- which(r_data$his_tbl$name == input$data_inter_net_selected)

    if(length(s1)) {        # update dataset history
        if(new_name %in% as.character(r_data$his_tbl$name)){ # For data, duplicated name are not allowed
            session$sendCustomMessage(type = "showalert", "Name has been taken by existing datasets.")
            return()
        }
        r_data$history[[s1]]$name <- new_name
        r_data$his_tbl$name[s1] <- new_name
        # Also need to update the parent relationship for BOTH data and analysis
        # For data
        p_idx <- which(r_data$his_tbl$parent_data == input$data_inter_net_selected)
        r_data$his_tbl$parent_data[p_idx] <- new_name
        for(idx in p_idx) {
            r_data$history[[idx]]$parent_data <- new_name
        }
        # For analysis
        p_idx <- which(r_data$reg_tbl$parent_data == input$data_inter_net_selected)
        r_data$reg_tbl$parent_data[p_idx] <- new_name
        for(idx in p_idx) {
            r_data$reg_history[[idx]]$parent_data <- new_name
        }
    } else {
        s2 <- which(paste(r_data$reg_tbl$name, r_data$reg_tbl$time) == input$data_inter_net_selected)
        if(length(s2)) {        # update analysis history
            # Analysis can take same names because the data node name used for linking is name+time
            r_data$reg_history[[s2]]$name <- new_name
            r_data$reg_tbl$name[s2] <- new_name
        }
    }
    neList<- generate_data_node(his_tbl = r_data$his_tbl, reg_tbl = r_data$reg_tbl)
    r_data$his_nodes <- neList$nodes
    r_data$his_edges <- neList$edges
    toggleModal(session, "dmap_rename_modal", toggle = "close")
})


###### Add Notes #######
observeEvent(input$dmap_notes_btn, {
    if(is.null(input$data_inter_net_selected) || input$data_inter_net_selected == "") {
        session$sendCustomMessage(type = "showalert", "Please select a node on the data map.")
        return()
    }

    if(is.null(input$dmap_notes_content)) {
        return()
    }

    new_notes <- trimws(input$dmap_notes_content)

    s1 <- which(r_data$his_tbl$name == input$data_inter_net_selected)

    if(length(s1)) {        # update dataset history
        r_data$history[[s1]]$user_notes <- new_notes
        r_data$his_tbl$user_notes[s1] <- new_notes
    } else {
        s2 <- which(paste(r_data$reg_tbl$name, r_data$reg_tbl$time) == input$data_inter_net_selected)
        if(length(s2)) {        # update analysis history
            r_data$reg_history[[s2]]$user_notes <- new_notes
            r_data$reg_tbl$user_notes[s2] <- new_notes
        }
    }
    neList<- generate_data_node(his_tbl = r_data$his_tbl, reg_tbl = r_data$reg_tbl)
    r_data$his_nodes <- neList$nodes
    r_data$his_edges <- neList$edges
    toggleModal(session, "dmap_notes_modal", toggle = "close")
})

###### Delete nodes ######
# Delete report

observeEvent(input$dmap_del_report, {
    if(is.null(r_data$reg_tbl)) return()
    tbl <- r_data$reg_tbl
    tbl$id <- paste(as.character(tbl$name), tbl$time)
    s <- which(tbl$id == input$data_inter_net_selected)
    if (length(s)) {
        # Remove this record from r_data$reg_history
        r_data$reg_history <- r_data$reg_history[-s]
        # Remove this record from r_data$reg_tbl
        r_data$reg_tbl <- r_data$reg_tbl[-s,]
    }
    neList<- generate_data_node(his_tbl = r_data$his_tbl, reg_tbl = r_data$reg_tbl)
    r_data$his_nodes <- neList$nodes
    r_data$his_edges <- neList$edges
})

# Delete data subset (data map)
observeEvent(input$dmap_del_subset, {
    if(is.null(r_data$his_tbl)) return()
    s <- which(r_data$his_tbl$name == input$data_inter_net_selected)
    if (length(s)) {
        session$sendCustomMessage(type = 'dialogContentUpdate',
                                  message = list(id = "deleteConfirmDlg",
                                                 message = paste0('Are you sure to delete this data subset?')))
    }
})


observeEvent(input$dmap_del_trigger, {
    content = list(
        tags$p("Note this will also remove any analysis reports linked to this subset.")
    )
    showModal(modalDialog(
        title = "Are you sure?",
        size = "s",
        content,
        easyClose = TRUE,
        footer = list(
            modalActionButton("dmap_delete_confirm", "Delete", class = "btn-danger btn_leftAlign"),
            modalButton("Cancel")
        )
    ))
})

observeEvent(input$dmap_delete_confirm, {
    if(is.null(r_data$his_tbl)) return()
    s <- which(r_data$his_tbl$name == input$data_inter_net_selected)
    if (length(s)) {
        if(!is.null(r_data$reg_tbl) && length(r_data$reg_tbl$parent_data) > 0) {
            a_ids <- which(r_data$reg_tbl$parent_data == input$data_inter_net_selected)
            if(length(a_ids)) {
                r_data$reg_history <- r_data$reg_history[-a_ids]
                r_data$reg_tbl <- r_data$reg_tbl[-a_ids,]
            }
        }

        # If the selected node is currently active, reset active node to input data
        if(r_data$his_tbl$is_activated[s] == "Y") {
            withProgress(message = 'Switching dataset, please wait...', value = 50, {
            r_data <- switch_to_dataset(r_data, 1)
            })
        }

        # Remove the current subset node
        r_data$history <- r_data$history[-s]
        r_data$his_tbl <- r_data$his_tbl[-s,]

        # Update node and edge table
        neList<- generate_data_node(his_tbl = r_data$his_tbl, reg_tbl = r_data$reg_tbl)
        r_data$his_nodes <- neList$nodes
        r_data$his_edges <- neList$edges
    }
})




