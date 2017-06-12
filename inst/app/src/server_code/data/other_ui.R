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


# The image indicating file is submitted
output$data_submitted_img <- renderUI({
    if(is.null(r_data$raw)) return()
    img(src = "button_ok.png", width = 35, height = 35, align = "right")
})

output$input_submit_btn_ui <- renderUI({
    switch(input$file_format,
           dir = actionButton(inputId = "submit_dir", label = "Submit", class = "btn-primary btn_rightAlign"),
           single = actionButton(inputId = "submit_single", label = "Submit", class = "btn-primary btn_rightAlign"),
           customtable = actionButton(inputId = "submit_cs", label = "Submit", class = "btn-primary btn_rightAlign")
    )
})

output$input_threshold_ui <- renderUI({
    switch(input$input_threshold_type,
           mean = numericInput("min_cnt_avg", label = "Row Mean Threshold", value = 0, min = 0, max = 5000, step = 1),
           sum = numericInput("min_cnt_sum", label = "Row Sum Threshold", value = 0, min = 0, max = 5000, step = 1)
    )
})

output$data_pv_ui <- renderUI({
    ########## Show data preview #########
    if(input$input_tabset %in% c("file_in")) {
        if(!is.null(r_data$glb.raw)){
            enhanced_box(width = NULL,
                title = "Dataset for Analysis",
                status = "custom3",
                solidHeader = T,
                fluidRow(
                    column(12,
                           DT::dataTableOutput("data_inprocess")
                    )
                )
            )
        } else if(input$file_format == 'single' && is.null(r_data$glb.raw)) {
            ########## Show loaded cnt module (single file input) #########
            box(width = NULL,
                title = "Loaded File Preview",
                status = "warning",
                solidHeader = T,
                fluidRow(
                    column(12,
                           pivot_filePreview_UI("single_preview")
                    )
                )
            )
        }
    } else if(input$input_tabset == "feature_in") {
        enhanced_box(width = NULL,
            title = "Feature Statistics",
            status = "custom4",
            solidHeader = T,
            fluidRow(
                column(12,
                       DT::dataTableOutput("input_feature_stats_tbl"),
                       downloadButton("download_feature_stats_tbl", "Download", class = "btn btn-success btn_rightAlign")
                )
            )
        )
    } else if(input$input_tabset == "sample_in") {
        enhanced_box(width = NULL,
            title = "Sample Statistics",
            status = "custom5",
            solidHeader = T,
            fluidRow(
                column(12,
                       DT::dataTableOutput("input_sample_stats_tbl"),
                       downloadButton("download_input_sample_stats_tbl", "Download", class = "btn btn-success btn_rightAlign")
                )
            )
        )
    } else if(input$input_tabset == "group_in") {
        enhanced_box(width = NULL,
            title = "Design Table",
            status = "custom2",
            solidHeader = T,
            fluidRow(
                column(12,
                       DT::dataTableOutput("input_design_tbl"),
                       downloadButton("download_design_tbl", "Download", class = "btn-success btn_rightAlign"),
                       actionButton('clear_group_1', label = "Clear design", icon = icon("times"), class = "btn-danger btn_rightAlign")
                )
            )
        )
    } else if(input$input_tabset == "data_man") {
        enhanced_box(
            width = NULL,
            title =  "Data History",
            status = "info",
            solidHeader = T,
            fluidRow(
                column(12,
                       DT::dataTableOutput("data_history"),
                       downloadButton("download_his_tbl", "Download", class = "btn btn-success btn_rightAlign")
                )
            )
        )
    } else {
        return()
    }
})

# Data in editting preview
output$data_inprocess <- DT::renderDataTable({
    if(is.null(r_data$df)) return ()
    DT::datatable(r_data$df, options = list(scrollX = TRUE, scrollY = "500px", lengthMenu = c(20, 50, 100)))
})

output$download_feature_stats_tbl <- downloadHandler(
    filename = "feature_stats_tbl.csv",
    content = function(file) {
        ftbl <-r_data$feature_meta
        ftbl <- ftbl[, -which(names(ftbl) %in% c("use_for_ordering", "cap_name", "STRING_id"))]
        write.csv(ftbl, file)
    }
)

# A copy of the above table to be put in feature filter tab
output$input_feature_stats_tbl <- DT::renderDataTable({
    if(is.null(r_data$feature_meta)) return()
    ftbl <-r_data$feature_meta
    ftbl <- ftbl[, -which(names(ftbl) %in% c("use_for_ordering", "cap_name", "STRING_id"))]
    DT::datatable(ftbl, selection = 'single', rownames = FALSE, options = list(
        scrollX = T, scrollY = "500px", lengthMenu = c(20, 50, 100)
    )
    )
})









