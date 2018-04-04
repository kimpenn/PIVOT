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
