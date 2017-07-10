


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

################################ MDS UI #################################

output$mds_ui <- renderUI({
    list(
        enhanced_box(
            title = "Classical (Metric) Multidimensional scaling",
            status = "primary",
            id = "mds",
            width = NULL,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("General Settings:"), class = "param_setting_title"),
            fluidRow(
                pivot_dataScale_UI("mds", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts"), selected = "Log10 Counts", width = 3),
                column(3, selectInput("mds_dist", "Distance measure", choices = list("Euclidean" = "euclidean", "Maximum" = "maximum", "Manhattan" = "manhattan", "Canberra" = "canberra", "Binary" = "binary"), selected = "euclidean")),
                pivot_groupBy_UI("mds", r_data$category, append_none = T, multiple = F, width = 6)
            ),
            fluidRow(
                column(6,
                       tags$div(tags$b("2D Projection:"), class = "param_setting_title"),
                       pivot_Plot2d_UI("mds_plot2d", type = "mds")
                ),
                column(6,
                       tags$div(tags$b("3D Projection:"), class = "param_setting_title"),
                       pivot_Plot3d_UI("mds_plot3d", type = "mds", height = "500px")
                )
            )
        ),
        enhanced_box(
            title = "Nonmetric Multidimensional scaling",
            status = "info",
            id = "nds",
            width = NULL,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("General Settings:"), class = "param_setting_title"),
            fluidRow(
                pivot_dataScale_UI("nds", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts"), selected = "Log10 Counts", width = 3),
                column(3, selectInput("nds_dist", "Distance measure", choices = list("Euclidean" = "euclidean", "Maximum" = "maximum", "Manhattan" = "manhattan", "Canberra" = "canberra", "Binary" = "binary"), selected = "euclidean")),
                pivot_groupBy_UI("nds", r_data$category, append_none = T, multiple = F, width = 6)
            ),
            fluidRow(
                column(6,
                       tags$div(tags$b("2D Projection:"), class = "param_setting_title"),
                       textOutput("nds_stress_2d"),
                       pivot_Plot2d_UI("nds_plot2d", type = "nds")
                ),
                column(6,
                       tags$div(tags$b("3D Projection:"), class = "param_setting_title"),
                       textOutput("nds_stress_3d"),
                       pivot_Plot3d_UI("nds_plot3d", type = "nds", height = "500px")
                )
            )
        )
    )
})

# Metric MDS
mdsList <- callModule(pivot_dataScale, "mds", r_data)
observe({
    req(r_data$df, r_data$meta)
    mds_data <- mdsList()$df
    req(mds_data, input$mds_dist)

    tryCatch({
        mds_2d <- as.data.frame(cmdscale(dist(t(mds_data), method = input$mds_dist),eig=F, k=2))
        mds_3d <- as.data.frame(cmdscale(dist(t(mds_data), method = input$mds_dist),eig=F, k=3))
        r_data$mds <- list(mds_2d = mds_2d, mds_3d = mds_3d)
    },
    error = function(e) {
        session$sendCustomMessage(type = "showalert", "Metric MDS failed.")
        r_data$mds <- NULL
    })
})

mds_minfo<- reactive(callModule(pivot_groupBy, "mds", meta = r_data$meta))

mds_selected_sample <- reactiveValues()

observe({
    req(mds_minfo(), r_data$mds)
    drag <-reactive(plotly::event_data("plotly_selected", source = "mds_drag"))
    obj <- callModule(pivot_Plot2d, "mds_plot2d", type = "mds",  obj = NULL, proj = r_data$mds$mds_2d, minfo = mds_minfo(),
                      source = "mds_drag", event = drag, selected = mds_selected_sample)
    isolate({
        if(!is.null(obj$group)) {
            if(is.null(r_data$meta$mds_group)) {
                r_data$meta$mds_group <- rep("not specified", nrow(r_data$meta))
                r_data$category <- colnames(r_data$meta)
            }
            r_data$meta$mds_group[match(obj$group, r_data$meta$sample)] <- names(obj$group)[1]
        }
    })
})


observe({
    req(mds_minfo(), r_data$mds)
    callModule(pivot_Plot3d, "mds_plot3d", type = "mds", obj = NULL, proj = r_data$mds$mds_3d, minfo = mds_minfo())
})


# Nonmetric MDS
ndsList <- callModule(pivot_dataScale, "nds", r_data)

observe({
    req(r_data$df)
    nds_data <- ndsList()$df
    req(nds_data, input$nds_dist)

    tryCatch({
        nds_2d <- MASS::isoMDS(dist(t(nds_data), method = input$nds_dist), k=2)
        nds_3d <- MASS::isoMDS(dist(t(nds_data), method = input$nds_dist), k=3)
        r_data$nds <- list(nds_2d = nds_2d, nds_3d = nds_3d)
    },
    error = function(e) {
        session$sendCustomMessage(type = "showalert", "Nonmetric MDS failed.")
        r_data$nds <- NULL
    })
})

nds_minfo<- reactive(callModule(pivot_groupBy, "nds", meta = r_data$meta))

nds_selected_sample <- reactiveValues()

observe({
    req(nds_minfo(), r_data$nds)
    drag <-reactive(plotly::event_data("plotly_selected", source = "nds_drag"))
    obj <- callModule(pivot_Plot2d, "nds_plot2d", type = "nds", obj = NULL, proj = as.data.frame(r_data$nds$nds_2d$points), minfo = nds_minfo(),
                      source = "nds_drag", event = drag, selected = nds_selected_sample)
    isolate({
        if(!is.null(obj$group)) {
            if(is.null(r_data$meta$nonmetric_mds_group)) {
                r_data$meta$nonmetric_mds_group <- rep("not specified", nrow(r_data$meta))
                r_data$category <- colnames(r_data$meta)
            }
            r_data$meta$nonmetric_mds_group[match(obj$group, r_data$meta$sample)] <- names(obj$group)[1]
        }
    })
})

observe({
    req(nds_minfo(), r_data$nds)
    callModule(pivot_Plot3d, "nds_plot3d", type = "nds", obj = NULL, proj = as.data.frame(r_data$nds$nds_3d$points), minfo = nds_minfo())
})

output$nds_stress_2d <- renderText({
    req(nds_minfo(), r_data$nds)
    paste("Stress: ", r_data$nds$nds_2d$stress)
})

output$nds_stress_3d <- renderText({
    req(nds_minfo(), r_data$nds)
    paste("Stress: ", r_data$nds$nds_3d$stress)
})
