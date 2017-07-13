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



##################### Quick View Module ######################

output$input_file_box <- renderInfoBox({
    if(is.null(r_data$file_info)) return(
        valueBox(value = "", "Input", icon = icon("credit-card"), color = "olive")
    ) else {
        if(r_data$file_info$type == "single"){
            valueBox(value = r_data$file_info$name, "Input file", icon = icon("credit-card"), color = "olive")
        }
        else if(r_data$file_info$type == "dir") {
            valueBox(value = r_data$file_info$path[[length(r_data$file_info$path)]], "Input folder", icon = icon("credit-card"), color = "olive")
        }
    }
})

# show how many feature is left after filtratrion
output$feature_number_box <- renderValueBox({
    if(is.null(r_data$raw)) {
        valueBox(
            0, "selected features", icon = icon("align-justify"), color = "yellow"
        )
    } else {
        valueBox(
            nrow(r_data$raw), "selected features", icon = icon("align-justify"), color = "yellow"
        )
    }
})

output$feature_percent_box <- renderValueBox({
    valueBox(
        paste0(round(nrow(r_data$raw)/nrow(r_data$glb.raw), digits = 3) * 100, "%"), "of input non-zero features", icon = icon("pie-chart"), color = "maroon"
    )
})

output$read_percent_box <- renderValueBox({
    valueBox(
        paste0(round(sum(r_data$raw)/sum(r_data$glb.raw), digits = 3) * 100, "%"), "of all reads", icon = icon("server"), color = "teal"
    )
})

output$sample_number_box <- renderValueBox({
    if(is.null(r_data$raw)) {
        valueBox(
            0, "selected samples", icon = icon("cubes"), color = "green"
        )
    } else {
        valueBox(
            ncol(r_data$raw), "selected samples", icon = icon("cubes"), color = "green"
        )
    }
})


output$cat_number_box <- renderValueBox({
    valueBox(
        ncol(r_data$glb.meta) - 1, "design categories", icon = icon("sitemap"), color = "aqua"
    )
})



