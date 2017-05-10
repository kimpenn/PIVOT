
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


output$distribution_ui <- renderUI({


    list(
        fluidRow(
            column(6,
                   enhanced_box(
                       title = "Data Distribution",
                       id = "data_distribution",
                       status = "primary",
                       width = 12,
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       get_pdf = T,
                       register_analysis= T,

                       tags$p("Log10-standardized data distribution (black line) compared to the log-normal ditribution (red line)."),
                       plotOutput("data_distribution_plt",  height = "450px")
                   )
            ),
            column(6,
                   enhanced_box(
                       title = "Per-feature Standard Deviation",
                       id = "mean_sd_plot",
                       status = "info",
                       width = 12,
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       get_pdf = T,
                       register_analysis= T,

                       tags$p("Per-feature standard deviation (taken across samples), against the rank of the mean, for the log transformed data."),
                       plotOutput("meansd_plt", height = "450px"),
                       tags$b("This plot is generated using vsn package. Citation:"),
                       tags$li("Wolfgang Huber, Anja von Heydebreck, Holger Sueltmann, Annemarie Poustka and Martin Vingron. Variance Stabilization Applied to Microarray Data
                               Calibration and to the Quantification of Differential Expression. Bioinformatics 18, S96-S104 (2002).", class = "citation")
                       )

            )
        ),
        enhanced_box(
            title = "Rank Frequency Plot",
            id = "rank_frequency",
            status = "warning",
            width = 12,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            get_pdf = T,
            register_analysis= T,
            tags$div(tags$b("Plot Settings:"), class = "param_setting_title"),
            fluidRow(
                column(4, checkboxInput("rf_logX", label = "Log Scale for X", value = T)),
                column(4, checkboxInput("rf_logY", label = "Log Scale for Y", value = T))
            ),
            plotOutput("rankfreq_plt", height = "500px")
        ),
        enhanced_box(
            title = "Mean Variability Plot",
            id = "mean_var_plot",
            status = "success",
            width = 12,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            tags$div(tags$b("Plot Settings:"), class = "param_setting_title"),
            fluidRow(
                column(2,
                       shinyBS::tipify(
                           numericInput("meanvar_bins", "Number of bins", min = 1, max = 50, step = 1, value = 20),
                           title = "Total number of bins to use in the scaled analysis", placement = "bottom", options = list(container = "body")
                       )
                ),
                column(2,
                       shinyBS::tipify(
                           numericInput("meanvar_y_cutoff1", "Dispersion bottom cutoff", min = 1, max = 10, step = 1, value = 2),
                           title = "E.g, Setting the cutoff to 2 identifies genes that are more than two standard deviations away from the average dispersion within a bin.", placement = "bottom", options = list(container = "body")
                       )
                ),
                column(2,
                       shinyBS::tipify(
                           numericInput("meanvar_y_cutoff2", "Dispersion top cutoff", min = 1, max = 20, step = 1, value = 12),
                           title = "Top cutoff on y-axis for identifying variable genes.", placement = "bottom", options = list(container = "body")
                       )
                ),
                column(2,
                       shinyBS::tipify(
                           numericInput("meanvar_x_cutoff1", "Expression bottom cutoff", min = 1, max = 10, step = 1, value = 1),
                           title = "Bottom cutoff on x-axis for identifying variable genes.", placement = "bottom", options = list(container = "body")
                       )
                ),
                column(2,
                       shinyBS::tipify(
                           numericInput("meanvar_x_cutoff2", "Expression top cutoff", min = 1, max = 20, step = 1, value = 8),
                           title = "Top cutoff on x-axis for identifying variable genes.", placement = "bottom", options = list(container = "body")
                       )
                )
            ),

            fluidRow(
                column(4,
                       tags$div(tags$b("Variably Expressed Genes:"), class = "param_setting_title"),
                       DT::dataTableOutput("mean_var_genes"),
                       downloadButton("download_mean_var_genes", "Download", class = "btn btn-success")
                ),
                column(8,
                       tags$div(tags$p("The X-axis is the mean expression level, and Y-axis is the log(Variance/mean). The results are plotted in log-space."),
                                pivot_help_UI("meanvar_help", "What is a mean variability plot")
                       ),
                       plotOutput("mean_var_plt", height = "600px")
                )
            ),
            tags$b("This plot is generated using Seurat package. Citation:"),
            tags$li("Rahul Satija (2015). Seurat: Seurat : R toolkit for single cell genomics. R package version 1.2.1. http://www.satijalab.org/seurat.", class = "citation")
        )
    )
})


output$data_distribution_plt <- renderPlot({
    req(r_data$df)
    withProgress(message = 'Generating distribution plot...', value = 0.5, {
    L <- log(r_data$df)
    # Standardize each gene, so that they are all on the same scale, Then melt # the data with plyr so we can plot it easily'
    melted_dens_df <- reshape2::melt(t(scale(t(L))))
    # Plot the distribution of the standardized gene expression values.
    stdlog_plot(melted_dens_df)
    })
})

############################## Rank Frequency Server End Module ##############################
# Original script

output$rankfreq_plt <- renderPlot({
    req(r_data$df)
    withProgress(message = 'Generating rank-frequency plot...', value = 0.5, {
    rank_freq(r_data$df, input$rf_logX, input$rf_logY)
    })
})


output$mean_var_plt <- renderPlot({
    req(r_data$df)
    if(input$meanvar_y_cutoff1 >= input$meanvar_y_cutoff2 || input$meanvar_x_cutoff1 >= input$meanvar_x_cutoff2) {
        session$sendCustomMessage(type = "showalert", "Bottom cutoff must be lower than top cutoff.")
        return()
    }
    withProgress(message = 'Generating mean variability plot...', value = 0.5, {
    r_data$meanvar <- meanVarPlot(log10(r_data$df+1),
                                   y.cutoff = input$meanvar_y_cutoff1, y.high.cutoff = input$meanvar_y_cutoff2,
                                   x.low.cutoff = input$meanvar_x_cutoff1, x.high.cutoff = input$meanvar_x_cutoff2,
                                   fxn.x = function(x){return(log(mean(exp(x) - 1) + 1))},
                                   fxn.y = function(x){return(log(var(exp(x) - 1)/mean(exp(x) - 1)))},
                                   num.bin = input$meanvar_bins)
    })
    # reprocess the table to add info

})

output$mean_var_genes <- DT::renderDataTable(({
    req(r_data$meanvar)
    DT::datatable(r_data$meanvar$var.tbl, rownames= FALSE, options = list(
        scrollX = T, scrollY = "400px", lengthMenu = c(20, 50, 100)
    ))
}))

output$download_mean_var_genes <- downloadHandler(
    filename = "seurat_var_genes.csv",
    content = function(file) {
        write.csv(r_data$meanvar$var.tbl, file, row.names = F)
    }
)

output$meansd_plt <- renderPlot({
    if(is.null(r_data$df)) return()
    withProgress(message = 'Generating mean-sd plot...', value = 0.5, {
    meanSdPlot(log10(as.matrix(r_data$df) + 1), bins = 100)
    })
})











