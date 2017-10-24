
#' Gnerate minimum spanning tree given distance matrix
#' @import igraph
#' @export
generate_mst <- function(dist_mtx, method = "walktrap", step = NULL) {
    if(is.null(method) || is.null(dist_mtx)) return(NULL)
    graph0<-igraph::graph.adjacency(dist_mtx, mode="undirected", weighted=TRUE, diag=FALSE, add.colnames='label')
    mst0 <-igraph::minimum.spanning.tree(graph0, weights = igraph::E(graph0)$weight)

    if(method == "walktrap") {
        community <- igraph::cluster_walktrap(mst0, steps = step, merges = TRUE, modularity = TRUE, membership = TRUE)
    }
    V(mst0)$name <-  V(mst0)$label
    return(list(
        g = mst0,
        community = community
    ))
}
