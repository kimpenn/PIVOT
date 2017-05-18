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

###########################################################################
# Script by Sarah Middleton & Hannah Dueck
# ERCC detection assessment

#----------------------------------------------------------------------------------------------
# Note on calculating absolute molecules:
#
#       1 attomole = 602214.15 molecules
#       0.9 uL of a 1:4,000,000 ERCC dilution spiked into our samples
#       number of molecules spiked in = (Orig Conc in attomoles/uL) * ((0.9*602214.15)/4000000)
#                                     = (Orig Conc in attomoles/uL) * 0.13545
#
#       9 nL of a 1:40,000 ERCC dilution in each well
#       number of molecules spiked in = (Orig Conc in attomoles/uL) * ((0.009*602214.15)/40000)
#                                     = (Orig Conc in attomoles/uL) * 0.13545
#
#       > Also: we appear to be using Mix 1 only
#----------------------------------------------------------------------------------------------


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# YOU NEED TO GET THE USER TO GIVE YOU THE AMOUNT OF ERCC ADDED AND THE ERCC DILUTION.
# IN OUR LAB THIS IS TYPICALLY 0.9uL OF ERCC ADDED AT A CONCENTRAION OF 4000000 (i.e. 1:4,000,000).
# SEE CALCULATIONS ABOVE.

output$ercc_ui <- renderUI({
    list(
        enhanced_box(
            width = 12,
            title = "Spike-in Analysis",
            id = "spike_in",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("General Settings:"), class = "param_setting_title"),
            fluidRow(
                column(3, uiOutput("ercc_scale_ui")),
                column(3, numericInput("ercc_added", label = "Amount of ERCC added (µL)", value = 0.9, min = 0, max = 100, step = .1)),
                column(3, numericInput("ercc_ratio", label = "ERCC dilution, 1 : ", value = 4000000, min = 0, max = 10e9, step = 100)),
                column(3, selectInput("ercc_mix_type", label = "Mix 1/2", choices = c("Mix 1" = "mix1", "Mix 2" = "mix2"), selected = "mix1"))
            ),
            fluidRow(
                column(4,
                       tags$div(tags$b("ERCC Standard Table:"), class = "param_setting_title"),
                       DT::dataTableOutput("ercc_std_tbl"),
                       downloadButton('download_ercc_std', 'Download', class = "btn btn-success btn_rightAlign")
                ),
                column(8,
                       tags$div(tags$b("ERCC Counts Detected:"), class = "param_setting_title"),
                       DT::dataTableOutput("ercc_tbl_preview"),
                       downloadButton('download_ercc_tbl', 'Download', class = "btn btn-success btn_rightAlign")
                )
            )
        ),
        tabBox(
            title = NULL,
            width = 12,
            tabPanel(tags$b("ERCC Distribution"),
                fluidRow(
                    column(8, plotOutput("ercc_distribution", height = "600px")),
                    column(4,
                           tags$div(tags$b("Coefficient of Variation:"), class = "param_setting_title"),
                           DT::dataTableOutput("ercc_cv_tbl"),
                           downloadButton('download_ercc_cv_tbl', 'Download',class = "btn btn-success")
                    )
                )
            ),
            tabPanel(tags$b("ERCC per Sample"),
                     fluidRow(
                         column(7,
                                fluidRow(
                                    column(3,
                                           selectInput("ercc_plot_stats", "Plot Stats",
                                                       choices = c("ERCC total counts" = "total_counts", "ERCC count fraction" = "count_fraction"),
                                                       selected = "total_counts")
                                    ),
                                    column(3,
                                           selectInput("ercc_plot_type", "Plot Type",
                                                       choices = c("Bar Plot" = "bar", "Histogram" = "histogram", "Density Plot" = "density"),
                                                       selected = "bar")
                                    ),
                                    column(3, uiOutput("ercc_info_group_ui")),
                                    column(3, uiOutput("ercc_bin_width_ui"))
                                ),
                                plotly::plotlyOutput("ercc_info_plot", height = "500px")
                         ),
                         column(5,
                                fluidRow(
                                    column(8, tags$p("Click on the bar plot to view the sample-wise linear regression.")),
                                    column(4, checkboxInput("ercc_lm_log", tags$b("Log Scale"), value = F))
                                ),

                                tags$p(),
                                plotly::plotlyOutput("ercc_lm_plot", height = "500px")
                         )
                     )

            ),
            tabPanel(tags$b("ERCC Detection Probability"),
                     fluidRow(
                         column(8, plotOutput("ercc_prob", height = "600px")),
                         column(4,
                                tags$div(tags$b("Binomial Logistic Regression Model Fit:"), class = "param_setting_title"),
                                verbatimTextOutput("ercc_glm_summary")
                         )
                     )

            )
        )
    )

})

output$ercc_scale_ui <- renderUI({
    if(length(grep("ERCC(-|[.])\\d{5}", rownames(r_data$glb.raw))) == 0) { # User specified exclude ERCC when input, then ERCCs are not normalized
        selectInput("ercc_scale", label = "Data Scale", choices = c("Counts (raw)" = "Counts (raw)"))
    } else {
        selectInput("ercc_scale", label = "Data Scale", choices = c("Counts (raw)" = "Counts (raw)", "Counts (Normalized)" = "Counts (Normalized)"), selected = "Counts (Normalized)")
    }
})

output$ercc_info_group_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2) return()
    categories = colnames(r_data$glb.meta)[-1]
    names(categories) <- categories
    options <- as.list(categories)
    options$None = "None"
    selectInput("ercc_info_group", label = "Color by", choices = options)
})


# load ERCC info (data files from ERCC product website)
erccStds <- read.table("src/built_in_files/ercc_standard_mix_conc.txt", header=T, row.names=1)
erccStds$ERCC_ID <- make.names(erccStds$ERCC_ID)
rownames(erccStds) <- erccStds$ERCC_ID

output$ercc_std_tbl <- DT::renderDataTable({
    DT::datatable(erccStds, rownames = F,
                  options = list(scrollX = TRUE, scrollY = "350px", lengthMenu = c(20, 50, 100)))
})

output$download_ercc_std <- downloadHandler(
    filename = function() {
        "ERCC_standard_table.csv"
    },
    content = function(file) {
        write.csv(erccStds, file, row.names = F)
    }
)


# Extract and match ERCC data
observe ({
    req(r_data$df, input$ercc_scale, input$ercc_mix_type, input$ercc_added, input$ercc_ratio)
    if(input$ercc_scale == "Counts (raw)") {
        tmp_ercc <- r_data$ercc[, r_data$sample_name] # The raw count of ERCC are isolated when user input the data
    } else {
        tmp_ercc <- as.data.frame(r_data$df[grep("ERCC(-|[.])\\d{5}", rownames(r_data$df)),])
    }

    if(input$ercc_mix_type == "mix1") {
        conc = erccStds$conc_attomoles_ul_Mix1
    } else {
        conc = erccStds$conc_attomoles_ul_Mix2
    }

    convert_ratio <- input$ercc_added * 602214.15/input$ercc_ratio
    erccStds$molecules <- conc * convert_ratio
    isolate({
        ercc_df<- tmp_ercc
        ercc_df$Matching_ID <- gsub("^.*(ERCC)", "\\1", rownames(ercc_df))
        ercc_df$PercentDetected <- apply(tmp_ercc, 1, function(x) sum(x > 0)/length(colnames(tmp_ercc)))
        ercc_df$Detected <- apply(tmp_ercc, 1, function(x) sum(x > 0))
        ercc_df$NotDetected <- apply(tmp_ercc, 1, function(x) sum(x == 0))
        ercc_df_merge <- merge(ercc_df, erccStds, by.x="Matching_ID", by.y = "ERCC_ID")
        if(nrow(ercc_df_merge) < 1) {
            r_data$ercc_wt_mol <- NULL
            return()
        }
        ercc_df_merge$Log10molecules <- log10(ercc_df_merge$molecules)
        r_data$ercc_wt_mol <- ercc_df_merge[with(ercc_df_merge, order(Log10molecules)), ]
        rownames(r_data$ercc_wt_mol) <- r_data$ercc_wt_mol$Matching_ID
    })
})

# ERCC data table

output$ercc_tbl_preview <- DT::renderDataTable({
    if(is.null(r_data$ercc_wt_mol)) return()
    df_wt_mol <- r_data$ercc_wt_mol %>% dplyr::select(molecules, dplyr::one_of(r_data$sample_name))
    DT::datatable(df_wt_mol, options = list(scrollX = TRUE, scrollY = "350px", lengthMenu = c(20, 50, 100)))
})

output$download_ercc_tbl <- downloadHandler(
    filename = function() {
        "ERCC_count_table.csv"
    },
    content = function(file) {
        if(is.null(r_data$ercc_wt_mol)) return()
        df_wt_mol <- r_data$ercc_wt_mol %>% dplyr::select(molecules, dplyr::one_of(r_data$sample_name))
        write.csv(df_wt_mol, file)
    }
)

# ERCC glm model summary

ercc_glm <- reactive({
    if(is.null(r_data$ercc_wt_mol)) return()
    glm(cbind(Detected, NotDetected) ~ Log10molecules, family=binomial(logit), data=r_data$ercc_wt_mol)
})

output$ercc_glm_summary <- renderPrint({
    if(is.null(r_data$ercc_wt_mol)) return()
    summary(ercc_glm())
})


# ERCC % detected vs expected plot

output$ercc_prob <- renderPlot({
    if(is.null(r_data$ercc_wt_mol)) return()
    ercc_detect_plot(r_data$ercc_wt_mol)
})


output$ercc_distribution <- renderPlot({
    if(is.null(r_data$ercc_wt_mol)) return()
    df_wt_mol <- r_data$ercc_wt_mol %>% dplyr::select(molecules, dplyr::one_of(r_data$sample_name))
    plotERCCDistribution(df_wt_mol)
})


output$ercc_cv_tbl <- DT::renderDataTable({
    if(is.null(r_data$ercc_wt_mol)) return()
    df_wt_mol <- r_data$ercc_wt_mol %>% dplyr::select(molecules, dplyr::one_of(r_data$sample_name))
    DT::datatable(erccCv(df_wt_mol))
})

output$download_ercc_cv_tbl <- downloadHandler(
    filename = function() {
        "ERCC_probeCVs.csv"
    },
    content = function(file) {
        if(is.null(r_data$ercc_wt_mol)) return()
        df_wt_mol <- r_data$ercc_wt_mol %>% dplyr::select(molecules, dplyr::one_of(r_data$sample_name))
        write.csv(erccCv(df_wt_mol), file)
    }
)


output$ercc_bin_width_ui <- renderUI({
    req(input$ercc_plot_type %in% c("histogram", "density"))
    sliderInput("ercc_step",.1, 2, value = .1, step = .1, label = "Band/Bin Width Adjustment")
})

# ERCC per sample plot

output$ercc_info_plot <- render_Plotly({
    req(input$ercc_plot_stats, input$ercc_plot_type)
    if(input$ercc_scale == "Counts (raw)") {
        tmp_ercc <- r_data$ercc[, r_data$sample_name]
    } else {
        tmp_ercc <- as.data.frame(r_data$df[grep("ERCC(-|[.])\\d{5}", rownames(r_data$df)),])
    }

    if(length(grep("ERCC(-|[.])\\d{5}", rownames(r_data$glb.raw))) == 0) { # ERCC excluded
        count_fraction = colSums(r_data$ercc[, r_data$sample_name]) / (colSums(r_data$raw) + colSums(r_data$ercc[, r_data$sample_name]))
    } else {
        count_fraction = colSums(r_data$ercc[, r_data$sample_name]) / colSums(r_data$raw)
    }

    tbl<-data.frame(total_counts = colSums(tmp_ercc),
                    count_fraction = count_fraction)
    tbl <- tbl %>% tibble::rownames_to_column("sample")
    if(!is.null(input$ercc_info_group) && input$ercc_info_group != "None") {
        tbl$Group <- r_data$glb.meta[,input$ercc_info_group][match(tbl$sample,r_data$glb.meta[,1])]
    } else {
        tbl$Group <- rep("sample", nrow(tbl))
    }
    if(input$ercc_plot_type == "bar") {
        plt1 <- tbl %>%
            plotly::plot_ly(x = ~sample, y = as.formula(paste0("~", input$ercc_plot_stats)), type = "bar", source = "selectERCCSample", color = as.character(tbl$Group)) %>%
            plotly::layout(margin = list(b=100))
    } else if(input$ercc_plot_type == "density") {
        dens<-tapply(tbl[,input$ercc_plot_stats], INDEX = tbl$Group, function(x){density(x,adjust = input$ercc_step)})
        df <- data.frame(
            x = unlist(lapply(dens, "[[", "x")),
            y = unlist(lapply(dens, "[[", "y")),
            Group = rep(names(dens[!sapply(dens, is.null)]), each = length(dens[[1]]$x))
        )
        plt1 <- plotly::plot_ly(df, x = ~x, y = ~y, color = ~Group, type  = "scatter", mode = "lines", fill = "tozeroy") %>%
            plotly::layout(xaxis = list(title = input$ercc_plot_stats))
    } else if (input$ercc_plot_type == "histogram") {
        start = min(tbl[,input$ercc_plot_stats])
        end = max(tbl[,input$ercc_plot_stats])
        plt1 <- plotly::plot_ly(tbl, x = as.formula(paste0("~", input$ercc_plot_stats)), type = "histogram",
                        xbins=list(start = start, end = end, size = (end - start)*input$ercc_step/2),
                        autobinx=F, color = as.character(tbl$Group), opacity = 0.8)
    }
    plt1
})

output$ercc_lm_plot <- render_Plotly({
    event.data <- plotly::event_data("plotly_click", source = "selectERCCSample")
    req(event.data, r_data$ercc_wt_mol)
    df_wt_mol <- r_data$ercc_wt_mol %>% dplyr::select(molecules, dplyr::one_of(r_data$sample_name))
    sample <- unlist(event.data[3])
    correlate_ercc(df_wt_mol, sample, log = input$ercc_lm_log)
})


correlate_ercc <- function(df, sample, log = F) {
    # Plots the relationship between the observed ERCC data and the expected ERCC
    # concentrations.

    # Args:
    #  observed: vector of summary statistic of observed ERCC counts
    #  expected: vector of ERCC concentrations
    #  description: optional string to add to title
    if(log) {
        tdf <- log(df + 1)
        tdf$molecules <- log(df$molecules)
        df <- tdf
    }

    ercc_fit <- lm(df[,sample] ~ df$molecules)
    plotly::plot_ly(df, x = ~molecules) %>%
        plotly::add_markers(y = as.formula(paste0("~", sample)), text = rownames(df), marker = list(size = 10, opacity = 0.5), name = "ERCCs") %>%
        plotly::add_lines(y = ~fitted(ercc_fit),
                  line = list(color = 'rgba(7, 164, 181, 1)'),
                  name = "Linear Fit") %>%
        plotly::layout(title = sprintf("%s: Y ~ %.2fX + %.2f ; R-squared: %.2f", sample,
                               ercc_fit$coefficients[2], ercc_fit$coefficients[1],
                               summary(ercc_fit)$r.squared),
                       font = list(size = 11))
}



