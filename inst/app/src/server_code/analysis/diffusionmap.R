


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

output$dfm_ui <- renderUI({
    list(
        enhanced_box(
            title = "Diffusion Map",
            status = "primary",
            id = "dfm",
            width = NULL,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("General Settings:"), class = "param_setting_title"),
            fluidRow(
                pivot_dataScale_UI("dfm", include = c("Counts (raw)", "Counts (normalized)", "Log10 Counts"), selected = "Log10 Counts", width = 4),
                column(4, selectInput("dfm_dist", "Distance measure", choices = list("euclidean"="euclidean", "cosine"="cosine", "rank correlation"="rankcor"), selected = "euclidean")),
                column(4, selectInput("dfm_sigma", "Sigma (Gaussian kernel)", choices = list("local"="local", "global"="global")))
            ),
            fluidRow(
                pivot_groupBy_UI("dfm", r_data$category, append_none = T, multiple = F, width = 8)
            ),
            fluidRow(column(12,actionButton("run_dfm", "Run", class = "btn-info btn_rightAlign"))),
            fluidRow(
                column(6,
                       tags$div(tags$b("2D Diffusion Map:"), class = "param_setting_title"),
                       pivot_Plot2d_UI("dfm_plot2d", type = "dfm")
                ),
                column(6,
                       tags$div(tags$b("3D Diffusion Map:"), class = "param_setting_title"),
                       pivot_Plot3d_UI("dfm_plot3d", type = "dfm", height = "500px")
                )
            )
        ),
        enhanced_box(
            width = NULL,
            title = NULL,
            status = "primary",
            solidHeader = T,
            tags$div(tags$b("Diffusion Pseudo Time (DPT):"), class = "param_setting_title"),
            tags$p("Diffusion Pseudo Time (DPT) is a pseudo time metric based on the transition probability of a diffusion process Haghverdi et al. (2016)."),
            fluidRow(
                column(6,
                       tags$div(tags$b("DPT:"), class = "param_setting_title"),
                       plotOutput("dpt_2d")
                ),
                column(6,
                       tags$div(tags$b("Branch labels:"), class = "param_setting_title"),
                       DT::dataTableOutput("dpt_branch")
                )
            )
        ),
        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Philipp Angerer et al. (2015): destiny: diffusion maps for large-scale single-cell data in R. Helmholtz-Zentrum München. ", class = "citation"),
                tags$li("Haghverdi, L., M. Büttner, F. A. Wolf, F. Buettner, and F. J. Theis 2016. Diffusion pseudotime robustly reconstructs lineage branching. Nature Methods.", class = "citation")
            )
        )
    )
})

dfmList <- callModule(pivot_dataScale, "dfm", r_data)
observeEvent(input$run_dfm,{
    req(r_data$df, r_data$meta)
    dfm_data <- dfmList()$df
    req(dfm_data, input$dfm_dist)

    tryCatch({
        assign("test1",dfm_data, env=.GlobalEnv)
        r_data$dfm <- destiny::DiffusionMap(t(dfm_data), verbose = T,
                               distance = input$dfm_dist, sigma=input$dfm_sigma)
        r_data$dpt <- destiny::DPT(r_data$dfm)
    },
    error = function(e) {
        showNotification("Failed to compute diffusion map. Note global sigma can only be used with euclidean distances.", type="error", duration=10)
        r_data$dfm <- NULL
        r_data$dpt <- NULL
    })
})

dfm_minfo<- reactive(callModule(pivot_groupBy, "dfm", meta = r_data$meta))

observe({
    req(dfm_minfo(), r_data$dfm)
    callModule(pivot_Plot2d, "dfm_plot2d", type = "dfm",  obj = NULL, proj = as.data.frame(r_data$dfm@eigenvectors), minfo = dfm_minfo(),
                      source = NULL, event = NULL, selected = NULL)
})

observe({
    req(dfm_minfo(), r_data$dfm)
    callModule(pivot_Plot3d, "dfm_plot3d", type = "dfm", obj = NULL, proj = as.data.frame(r_data$dfm@eigenvectors), minfo = dfm_minfo())
})

output$dpt_2d <- renderPlot({
    req(r_data$dpt)
    destiny::plot.DPT(r_data$dpt)
})

output$dpt_branch <- DT::renderDataTable({
    req(r_data$dpt)
    btbl <- as.data.frame(r_data$dpt@branch)
    rownames(btbl) <- r_data$sample_name
    DT::datatable(btbl)
})
