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



##################### Quick View Module ######################

output$input_file_box <- renderInfoBox({
    if(is.null(r_data$file_info)) return(
        valueBox(value = "", "Input", icon = icon("credit-card"), color = "olive")
    ) else {
        if(r_data$file_info$type == "single"){
            valueBox(value = r_data$file_info$name, "Input file", icon = icon("credit-card"), color = "olive")
        }
        else if(r_data$file_info$type %in% c("dir","tenx")) {
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



