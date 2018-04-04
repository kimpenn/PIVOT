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



# The feature info list in header
output$featureMenu <- renderMenu({
    if(is.null(r_data$raw)) {
        sample_num <- 0
    } else {
        sample_num <- ncol(r_data$raw)
    }
    dropdownMenu(type = "notifications",
                 top_msg = "Data Information",
                 notificationItem(
                     text = paste(nrow(r_data$raw), "selected features"),
                     icon("align-justify")
                 ),
                 notificationItem(
                     text = paste0(round(nrow(r_data$raw)/nrow(r_data$glb.raw), digits = 3) * 100, "% ", "of input non-zero features"),
                     icon("pie-chart"),
                     status = "success"
                 ),
                 notificationItem(
                     text = paste0(round(sum(r_data$raw)/sum(r_data$glb.raw), digits = 3) * 100, "% ", "of all reads"),
                     icon = icon("server"),
                     status = "success"
                 ),
                 notificationItem(
                     text = paste0(sample_num, " selected samples"),
                     icon = icon("cubes"),
                     status = "primary"
                 ),
                 notificationItem(
                     text = paste0(ncol(r_data$glb.meta) - 1, " design categories"),
                     icon = icon("sitemap"),
                     status = "info"
                 )
    )
})

# tab switch control
observeEvent(input$report_save_sc, {
    updateTabItems(session, "tabs", "report")
})

observeEvent(input$data_load_sc, {
    updateTabItems(session, "tabs", "data")
    updateTabsetPanel(session, "input_tabset", "file_in")
})

observeEvent(input$group_load_sc, {
    updateTabItems(session, "tabs", "data")
    updateTabsetPanel(session, "input_tabset", "group_in")
})

observeEvent(input$feature_filter_sc, {
    updateTabItems(session, "tabs", "data")
    updateTabsetPanel(session, "input_tabset", "feature_in")
})

observeEvent(input$data_subset_sc, {
    updateTabItems(session, "tabs", "data")
    updateTabsetPanel(session, "input_tabset", "sample_in")
})

observeEvent(input$manual_sc, {
    updateTabItems(session, "tabs", "manual_file")
})

observeEvent(input$version_sc, {
    updateTabItems(session, "tabs", "about")
})

