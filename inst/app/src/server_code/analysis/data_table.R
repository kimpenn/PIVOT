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


################################ Data table module #################################

output$table_box_ui <- renderUI({
    if(input$tabs != 'table') return()

    list(
        enhanced_box(
            title = "Data Table",
            id = "data_table",
            status = "primary",
            width = NULL,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            tags$div(tags$b("General Settings:"), class = "param_setting_title"),
            fluidRow(
                pivot_dataScale_UI("table_scale", include = c("Counts (raw)", "Counts (normalized)", "Relative Frequency", "Log10 Counts", "Standardized Counts", "Log10 & Standardized"), width = 8, order = T)
            ),
            tags$p("Tip: Select row in the table to see the plot of that feature."),
            fluidRow(
                column(12,
                       DT::dataTableOutput("data_tbl")
                )
            ),
            downloadButton('downloadData', 'Download', class = "btn btn-success")
        ),

        enhanced_box(
            title = NULL,
            id = "data_table_gene_plot",
            status = "primary",
            width = NULL,
            solidHeader = F,
            pivot_featurePlot_UI("data_tbl_plt", meta = r_data$meta)
        )
    )

})

rs_table <- callModule(pivot_dataScale, "table_scale", r_data, order = T)

# Normalized Data
output$data_tbl <- DT::renderDataTable({
    req(rs_table())
    DT::datatable(rs_table()$df, selection = 'single', options = list(scrollX = TRUE, scrollY = "500px", lengthMenu = c(20, 50, 100)))
})


output$downloadData <- downloadHandler(
    filename = function() {
        paste0(rs_table()$scale, '.csv')
    },
    content = function(file) {
        req(rs_table())
        write.csv(rs_table()$df, file)
    }
)


observe({
    if(is.null(rs_table()$df)) return()
    s = input$data_tbl_row_last_clicked
    tbl<-as.data.frame(rs_table()$df)

    if (length(s)) {
        selected_gene <- rownames(tbl[s, , drop = FALSE])
    } else {
        return()
    }
    d <- as.data.frame(t(tbl[selected_gene,])) %>% tibble::rownames_to_column()
    colnames(d) <- c("sample", "expression_level")
    callModule(pivot_featurePlot, "data_tbl_plt", meta = r_data$meta, df = d, gene = selected_gene)
})





