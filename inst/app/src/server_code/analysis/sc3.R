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


# SC3 clustering
output$sc3_ui <- renderUI({
    list(
        enhanced_box(
            title = "SC3 Clustering",
            id = "sc3_clust",
            status = "primary",
            width = 12,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("Clustering Settings:"), class = "param_setting_title"),
            fluidRow(
                column(3, tags$br(), actionButton("run_sc3_kest", "Estimate best K (optional)", class = "btn-info")),
                column(3, numericInput("sc3_kmin", "K range min", min=2, max = length(r_data$sample_name)-1, value = 2)),
                column(3, numericInput("sc3_kmax", "K range max", min=2, max = length(r_data$sample_name)-1, value = 2)),
                column(3, numericInput("sc3_ncore", "Number of cores:", value = 1, min=1, step=1))
            ),
            fluidRow(
                column(3, uiOutput("sc3_k_est")),
                column(6),
                column(3,tags$br(), actionButton("run_sc3", "Run SC3", class = "btn-primary btn_rightAlign"))
            )
        ),
        box(
            title = NULL,
            status = "primary",
            width = 12,
            fluidRow(
                column(6,
                       tags$div(tags$b("Average Silhouette"), class = "param_setting_title"),
                       plotOutput("sc3_avg_silw")
                ),
                column(6,
                       tags$div(tags$b("Silhouette Plot"), class = "param_setting_title"),
                       uiOutput("sc3_silw_k_ui"),
                       plotOutput("sc3_silw_plot")
                )
            )
        ),
        box(
            title = NULL,
            status = "primary",
            width = 12,
            tags$div(tags$b("Consensus Matrix"), class = "param_setting_title"),
            fluidRow(
                uiOutput("sc3_consensus_k_ui"),
                column(8, tags$br(),actionButton("sc3_consensus_btn", "Plot Consensus Matrix", class = "btn-primary btn_rightAlign"))
            ),
            plotOutput("sc3_consensus", height = "800px")
        ),
        box(
            title = NULL,
            status = "primary",
            width = 12,
            tags$div(tags$b("Clustering Results"), class = "param_setting_title"),
            DT::dataTableOutput("sc3_assignment"),
            downloadButton("sc3_download", "Download", class = "btn btn-success")
        )
    )
})

observeEvent(input$run_sc3_kest, {
    req(r_data$sceset)
    r_data$sceset <- SC3::sc3_estimate_k(r_data$sceset)
})

output$sc3_k_est <- renderUI({
    req(r_data$sceset@sc3)
    tagList(
        tags$br(),
        tags$b(paste0("k_estimation: ",r_data$sceset@sc3$k_estimation))
    )
})


observeEvent(input$run_sc3, {
    req(r_data$sceset, input$sc3_ncore)
    set.seed(1)
    if(input$sc3_kmin > input$sc3_kmax){
        showNotification("Min must be lower than max.", type="error", duration=10)
        return()
    }
    withProgress(message = 'Processing...', value = 0.5, {
        tryCatch({
            r_data$sc3_krange <- input$sc3_kmin:input$sc3_kmax
            r_data$sceset <- SC3::sc3(r_data$sceset, ks = r_data$sc3_krange, biology = TRUE, n_cores = input$sc3_ncore)
            r_data$meta <- cbind(r_data$meta, pData(r_data$sceset)[,paste0("sc3_", r_data$sc3_krange, "_clusters")])
            r_data$category <- colnames(r_data$meta)
        }, error=function(e) {
            showNotification("SC3 failed", type="error", duration=10)
            return()
        })
    })
})

output$sc3_avg_silw <- renderPlot({
    req(r_data$sc3_krange, r_data$sceset@sc3, r_data$sceset@sc3$consensus)
    krange <- r_data$sc3_krange
    silw<-sapply(krange, function(i) {
         mean(r_data$sceset@sc3$consensus[[as.character(i)]]$silhouette[,"sil_width"])
    })
    plot(krange, silw, type="n", xlab="k", ylab="Average Silhouette", ylim = c(0,1), xaxt="n")
    lines(krange, silw, type="b", lwd=1.5,lty=1, col="black", pch=18)
    axis(1, at = krange)
})

output$sc3_silw_k_ui <- renderUI({
    req(r_data$sc3_krange, r_data$sceset@sc3, r_data$sceset@sc3$consensus)
    krange <- r_data$sc3_krange
    numericInput("sc3_silw_k", "Choose k", min=min(krange), max = max(krange), value = min(krange))
})

output$sc3_silw_plot <- renderPlot({
    req(r_data$sc3_krange, r_data$sceset@sc3, input$sc3_silw_k)
    SC3::sc3_plot_silhouette(r_data$sceset, k = input$sc3_silw_k)
})

output$sc3_consensus_k_ui <- renderUI({
    req(r_data$sceset@sc3, r_data$sceset@sc3$consensus)
    krange <- r_data$sc3_krange
    column(4,numericInput("sc3_consensus_k", "Choose k", min=min(krange), max = max(krange), value = min(krange)))
})

output$sc3_consensus <- renderPlot({
    input$sc3_consensus_btn
    isolate({
        req(r_data$sc3_krange, r_data$sceset@sc3, input$sc3_consensus_k)
        SC3::sc3_plot_consensus(r_data$sceset, input$sc3_consensus_k, show_pdata = paste0("sc3_", r_data$sc3_krange, "_clusters"))
    })
})

output$sc3_assignment <- DT::renderDataTable({
    req(r_data$sc3_krange, r_data$sceset@sc3)
    p_data <- pData(r_data$sceset)
    if(all(paste0("sc3_", r_data$sc3_krange, "_clusters") %in% colnames(p_data))) {
        tbl <- p_data[,paste0("sc3_", r_data$sc3_krange, "_clusters")]
    } else {
        return()
    }
    DT::datatable(tbl, options = list(scrollX = TRUE, scrollY = "400px", lengthMenu = c(20, 50, 100)))
})

output$sc3_download <- downloadHandler(
    filename = function() {
        "sc3_assigment.csv"
    },
    content = function(file) {
        tbl <- pData(r_data$sceset)[,paste0("sc3_", r_data$sc3_krange, "_clusters")]
        write.csv(tbl, file, row.names = T)
    }
)




