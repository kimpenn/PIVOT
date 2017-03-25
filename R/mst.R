
#' Gnerate minimum spanning tree given distance matrix
#' @import igraph
#' @export
generate_mst <- function(dist_mtx, method = "walktrap", step = NULL, color_list = NULL) {
    if(is.null(method) || is.null(dist_mtx)) return(NULL)
    graph0<-igraph::graph.adjacency(dist_mtx, mode="undirected", weighted=TRUE, diag=FALSE, add.colnames='label')
    mst0 <-igraph::minimum.spanning.tree(graph0, weights = igraph::E(graph0)$weight)

    if(method == "walktrap") {
        community <- igraph::cluster_walktrap(mst0, steps = step, merges = TRUE, modularity = TRUE, membership = TRUE)
    }
    if(!is.null(color_list$meta)) {
        V(mst0)$color <- color_list$meta_color[,1]
        V(mst0)$group <- as.character(color_list$meta[,1])
    } else {
        V(mst0)$group <- rep("NA", nrow(dist_mtx))
    }
    V(mst0)$name <-  V(mst0)$label
    return(list(
        g = mst0,
        community = community
    ))
}
