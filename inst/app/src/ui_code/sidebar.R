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



################################### Sidebar module ######################################
sidebarwell <- function (...)
{
    div(class = "well well_sidebar", ...)
}

sidebar <- dashboardSidebar(
    disable = F,
    # Some personalized css
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
    ),
    hr(),
    ##################### Menu Module ###################
    sidebarMenu(id="tabs",
                menuItem("Data Input",  icon = icon("table"), tabName="data_input"),
                menuItem("Basic Statistics", icon = icon("area-chart"),
                         menuSubItem("Data Table", tabName="table", icon = icon("angle-right")),
                         menuSubItem("Sample/Feature Stats", tabName="meta", icon = icon("angle-right")),
                         menuSubItem("Data Distribution", tabName = "data_distribution", icon = icon("angle-right")),
                         menuSubItem("Spike-in", tabName="ercc", icon = icon("angle-right"))
                ),
                menuItem("Differential Expression", icon = icon("eyedropper"),
                         menuSubItem("DESeq2", tabName = "deseq", icon = icon("angle-right")),
                         menuSubItem("edgeR", tabName = "edgeR", icon = icon("angle-right")),
                         menuSubItem("SCDE", tabName = "scde", icon = icon("angle-right")),
                         menuSubItem("Mann–Whitney U test", tabName = "mww", icon = icon("angle-right"))
                ),
                menuItem("Monocle", icon = icon("umbrella"),
                         menuSubItem("CellDataSet and DE", tabName = "monocle", icon = icon("angle-right")),
                         menuSubItem("Cell State Ordering ", tabName = "monocle_state", icon = icon("angle-right")),
                         menuSubItem("Gene Expression Pattern", tabName = "monocle_gene", icon = icon("angle-right"))
                ),
                menuItem("Clustering",  icon = icon("share-alt"),
                         menuSubItem("Hierachical", tabName = "hclust", icon = icon("angle-right")),
                         menuSubItem("K-means", tabName = "kmeans", icon = icon("angle-right")),
                         menuSubItem("SC3", tabName = "sc3", icon = icon("angle-right")),
                         menuSubItem("Community Detection", tabName = "community", icon = icon("angle-right"))
                ),
                menuItem("Correlation",  icon=icon("line-chart"),
                         menuSubItem("Pairwise Scatterplot", tabName = "pairwise",icon = icon("angle-right")),
                         menuSubItem("Sample Correlation Heatmap", tabName = "correlation_hm",icon = icon("angle-right")),
                         menuSubItem("Feature Correlation Heatmap", tabName = "cor_ft",icon = icon("angle-right"))
                ),
                menuItem("Heatmap", tabName = "heatmap", icon = icon("barcode")),
                menuItem("Dimension Reduction", icon = icon("yelp"),
                         menuSubItem("PCA", tabName = "pca", icon = icon("angle-right")),
                         menuSubItem("t-SNE", tabName = "tsne", icon = icon("angle-right")),
                         menuSubItem("MDS", tabName = "mds", icon = icon("angle-right")),
                         menuSubItem("DiffusionMap", tabName = "dfm", icon = icon("angle-right")),
                         menuSubItem("penalizedLDA", tabName = "plda", icon = icon("angle-right"))
                ),
                menuItem("Enrichment Analysis", tabName = "gsea", icon = icon("flask")),
                menuItem("Network Analysis", icon = icon("connectdevelop"), tabName = "transnet"),
                menuItem("Classification", tabName = "caret", icon = icon("delicious")),
                menuItem("Toolkit", icon = icon("wrench"),
                         menuSubItem("Venn Diagram", tabName = "venn", icon = icon("angle-right")),
                         menuSubItem("Gene Annotation", tabName = "biomart", icon = icon("angle-right"))
                ),
                menuItem("Report", tabName = "report", icon = icon("file-text")),
                menuItem("About", icon=icon("font"),
                         menuSubItem("User Manual", tabName = "manual_file", icon = icon("angle-right")),
                         menuItem("About PIVOT", tabName = "about", icon = icon("angle-right"))
                )
    ),



    br(),
    br()
    #column(1),
    #column(10,
    #       radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),inline = TRUE),
    #       downloadButton('genreport', 'featurerate')
    #)
        )
