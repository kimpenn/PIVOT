
#' Generate data history node and edge table for data map
#'
#' @import dplyr
#' @export
generate_data_node <- function(his_tbl = NULL, reg_tbl = NULL) {
    if(is.null(his_tbl)) return()
    tbl <- his_tbl

    if(!is.null(reg_tbl)) {
        tbl <- rbind(tbl, reg_tbl)
    }

    tbl$label <- tbl$name
    tbl$name <- ifelse(is.na(tbl$is_activated), paste(as.character(tbl$name), tbl$time), as.character(tbl$name))

    tbl_nodes <- tbl %>% dplyr::select(name, time, feature_num, sample_num, is_activated, label, user_notes)
    tbl_nodes <- subset(tbl_nodes, !duplicated(tbl_nodes[,1]))

    tbl_edges <- apply(tbl, 1, function(row) {
        if(!is.na(row[3])){
            return(c("from" = row[3], "to" = row[1], "label" = ifelse(row[4] == "analysis", NA, row[5])))
        }
    })

    if(!is.null(tbl_edges)) {
        tbl_edges <- as.data.frame(t(as.data.frame(tbl_edges[!sapply(tbl_edges, is.null)])))
        colnames(tbl_edges) <- c("from", "to", "label")
    }

    return(list(
        nodes = tbl_nodes,
        edges = tbl_edges
    ))
}

#' PIVOT data map plotting function
#'
#' @import visNetwork dplyr
#' @export
generate_data_map <- function(vis_nodes, vis_edges) {
    title_content <- paste0("<p>",
                            ifelse(is.na(vis_nodes$is_activated),
                                   paste("Created at:", vis_nodes$time),
                                   paste(vis_nodes$feature_num, "features,", vis_nodes$sample_num, "samples. Created at:", vis_nodes$time)),
                            ifelse(vis_nodes$user_notes == "", "", paste0("<br>", vis_nodes$user_notes)), "</p>")
    vis_nodes <- vis_nodes %>%
        dplyr::mutate(group = ifelse(is.na(vis_nodes$is_activated), "Analysis", ifelse(vis_nodes$is_activated == "Y", "Dataset_active", "Dataset"))) %>%
        dplyr::mutate(title = title_content) %>%
        dplyr::select(id = name, label, group, title)

    if(!is.null(vis_edges)) {
        vis_edges <- vis_edges %>%
            dplyr::mutate(width = ifelse(is.na(vis_edges$label), 1, 5)) %>%
            dplyr::mutate(color = ifelse(is.na(vis_edges$label), "grey", "black")) %>%
            dplyr::select(from, to, title = label, width, color)
    }

    visplt <- visNetwork(vis_nodes, vis_edges) %>%
        visNodes(labelHighlightBold = T) %>%
        visGroups(groupname = "Dataset", shape = "icon", icon = list(code = "f200", size = 75)) %>%
        visGroups(groupname = "Dataset_active", shape = "icon", icon = list(code = "f200", size = 75, color = "orange")) %>%
        visGroups(groupname = "Analysis", shape = "icon", icon = list(code = "f080", size = 30, color = "red")) %>%
        addFontAwesome() %>%
        visOptions(nodesIdSelection = TRUE) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visEvents(doubleClick = "function(properties){alert('selected nodes: '+ properties.nodes);}")
    if(!is.null(vis_edges)) {
        visplt <- visplt %>% visLegend()
    }
    return(visplt)
}


#' PIVOT data management function
#' @description
#' Write new entries to data history
#' @export
update_history <- function(r_data, parent, action_type, action, lists, norm_method, norm_param){
    if(is.null(r_data$history)) {
        r_data$history <- list()
        r_data$his_num <- 0
    }

    if(is.na(parent)) {
        name = "Input Data"
    } else {
        r_data$his_num <- r_data$his_num + 1
        name = paste("Subset", r_data$his_num)
    }

    r_data$history[[length(r_data$history) + 1]] <- list(
        name = as.character(name),
        time = lubridate::now(),
        parent_data = as.character(parent),
        action_type = action_type,
        action = action,
        lists = lists,
        norm_method = as.character(norm_method),
        norm_param = norm_param,
        feature_num = length(r_data$feature_list),
        sample_num = length(r_data$sample_name),
        user_notes = ""
    )
    his_tbl <- plyr::ldply(r_data$history, function(x){data.frame(x[c(1,2,3,4,5,7,9,10,11)])})
    his_tbl$is_activated <- c(rep("N", nrow(his_tbl) - 1), "Y")
    his_tbl$name <- as.character(his_tbl$name) # change factor to string so good to replace
    his_tbl$parent_data <- as.character(his_tbl$parent_data) # same as above
    his_tbl$user_notes <- as.character(his_tbl$user_notes)
    r_data$his_tbl <- his_tbl
    # Update node and edge table
    neList<- generate_data_node(his_tbl = r_data$his_tbl, reg_tbl = r_data$reg_tbl)
    r_data$his_nodes <- neList$nodes
    r_data$his_edges <- neList$edges
}



#' PIVOT data management function
#' @description
#' This function is for determining which sample subset the filter operation is performed on, if not keep filtering.
#' @export
search_subset_node <- function(tbl, node) {
    cur_p = node # Current node

    act_t = tbl$action_type[which(as.character(tbl$name) == as.character(cur_p))]
    while(!act_t %in% c("Subset", "Input") ){
        cur_p = tbl$parent_data[which(as.character(tbl$name) == as.character(cur_p))]
        act_t = tbl$action_type[which(as.character(tbl$name) == as.character(cur_p))]
    }
    return(as.character(cur_p))
}

