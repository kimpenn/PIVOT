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


################################### Sidebar module ######################################
sidebarwell <- function (...)
{
    div(class = "well well_sidebar", ...)
}

if(!exists("r_module")) {
    r_module <- "PIVOT.analysis"
}

if('DESeq2' %in% r_module) {
    deseq_sub_ui <- menuSubItem("DESeq2", tabName = "deseq", icon = icon("angle-right"))
} else {
    deseq_sub_ui <- NULL
}

if('edgeR' %in% r_module) {
    edgeR_sub_ui <- menuSubItem("edgeR", tabName = "edgeR", icon = icon("angle-right"))
} else {
    edgeR_sub_ui <- NULL
}

if('scde' %in% r_module) {
    scde_sub_ui <- menuSubItem("SCDE", tabName = "scde", icon = icon("angle-right"))
} else {
    scde_sub_ui <- NULL
}

if('monocle' %in% r_module) {
    monocle_side_ui <- menuItem("Monocle", icon = icon("umbrella"),
                                menuSubItem("CellDataSet and DE", tabName = "monocle", icon = icon("angle-right")),
                                menuSubItem("Cell State Ordering ", tabName = "monocle_state", icon = icon("angle-right")),
                                menuSubItem("Gene Expression Pattern", tabName = "monocle_gene", icon = icon("angle-right"))
    )
} else {
    monocle_side_ui <- NULL
}

if('PIVOT.network' %in% r_module) {
    network_side_ui <- menuItem("Network Analysis", icon = icon("connectdevelop"), tabName = "transnet")
} else {
    network_side_ui <- NULL
}

if('PIVOT.toolkit' %in% r_module) {
    toolkit_side_ui <- menuItem("Toolkit", icon = icon("wrench"),
             menuSubItem("Venn Diagram", tabName = "venn", icon = icon("angle-right")),
             menuSubItem("Gene Annotation", tabName = "biomart", icon = icon("angle-right"))
    )
} else {
    toolkit_side_ui <- NULL
}

if('caret' %in% r_module) {
    caret_side_ui <- menuItem("Classification", tabName = "caret", icon = icon("delicious"))
} else {
    caret_side_ui <- NULL
}

if('PIVOT.analysis' %in% r_module) {
    side_ui <- list(
        menuItem("Basic Statistics", icon = icon("area-chart"),
                 menuSubItem("Data Table", tabName="table", icon = icon("angle-right")),
                 menuSubItem("Sample/Feature Stats", tabName="meta", icon = icon("angle-right")),
                 menuSubItem("Data Distribution", tabName = "data_distribution", icon = icon("angle-right")),
                 menuSubItem("Spike-in", tabName="ercc", icon = icon("angle-right"))
        ),
        menuItem("Differential Expression", icon = icon("eyedropper"),
                 deseq_sub_ui,
                 edgeR_sub_ui,
                 scde_sub_ui,
                 menuSubItem("Mann–Whitney U test", tabName = "mww", icon = icon("angle-right"))
        ),
        monocle_side_ui,
        menuItem("Clustering",  icon = icon("share-alt"),
                 menuSubItem("Hierachical", tabName = "hclust", icon = icon("angle-right")),
                 menuSubItem("K-means", tabName = "kmeans", icon = icon("angle-right")),
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
                 menuSubItem("penalizedLDA", tabName = "plda", icon = icon("angle-right"))
        ),
        menuItem("Enrichment Analysis", tabName = "gsea", icon = icon("flask")),
        network_side_ui,
        caret_side_ui,
        toolkit_side_ui
    )
} else {
    if(is.null(c(deseq_sub_ui, edgeR_sub_ui, scde_sub_ui))) {
        de_ui <- NULL
    } else {
        de_ui <- menuItem("Differential Expression", icon = icon("eyedropper"),
                                  deseq_sub_ui,
                                  edgeR_sub_ui,
                                  scde_sub_ui
        )
    }
    side_ui <- list(
        de_ui,
        monocle_side_ui,
        network_side_ui,
        caret_side_ui,
        toolkit_side_ui
    )
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
                side_ui,
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
