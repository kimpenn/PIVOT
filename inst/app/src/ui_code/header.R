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




dbHeader <- pivotboardHeader(
    title = "PIVOT",
    dropdownMenu(type = "support",
                 top_msg = "PIVOT Support",
                 systemItem(
                     title = "Load Example",
                     text = actionButton("load_example","An example dataset from Dueck, Hannah, et al.", class = "btn_no_border"),
                     icon = icon("database")
                 ),
                 systemItem(
                     title = "Technical Support",
                     text = actionButton("bug_Sc","General questions, bug reports.", class = "btn_no_border"),
                     icon = icon("wrench"),
                     href = "mailto:qinzhu@outlook.com"
                 ),
                 systemItem(
                     title = "PIVOT Manual",
                     text = actionButton("manual_sc", "Check out manuals of PIVOT.",  class = "btn_no_border"),
                     icon = icon("graduation-cap")
                 ),
                 systemItem(
                     title = "Version Info",
                     text = actionButton("version_sc","PIVOT, version: 1.0.0",  class = "btn_no_border"),
                     icon = icon("compass")
                 ),
                 systemItem(
                     title = "Kim Lab",
                     text = actionButton("lab_site_sc","Junhyong Kim Lab, University of Pennsylvania",  class = "btn_no_border"),
                     icon = icon("graduation-cap"),
                     href = "http://kim.bio.upenn.edu/"
                 )
    ),

    dropdownMenuOutput("featureMenu"),
    dropdownMenu(type = "system",
                 top_msg = "System Control",
                 tags$li(tags$span(
                     icon("save", class = "text-primary"),
                     custom_downloadbtn("state_save_sc","Save State", class = "btn btn-default btn_no_border shiny-bound-input", style = "margin-left:8px"),
                     style ="padding:10px;border-bottom:1px solid #f4f4f4;background-color:transparent; display:block;"
                 )
                 ),
                 systemItem(
                     text = actionButton("report_save_sc","Save Report", class = "btn_no_border"),
                     icon = icon("pencil"),
                     status = "warning"
                 ),
                 systemItem(
                     text = actionButton("session_clear_sc","Clear Session", class = "btn_no_border", onclick = "window.location.reload();"),
                     icon = icon("desktop"),
                     status = "danger"
                 ),
                 tags$li(tags$span(
                     icon("desktop", class = "text-primary"),
                     tags$a(id = "new_session", href = "./", target = "_blank", "New Session", class = "btn btn-default btn_no_border shiny-bound-input", style = "margin-left:5px")
                 ), style ="padding:10px;border-bottom:1px solid #f4f4f4;background-color:transparent; display:block;"
                 ),
                 # systemItem(
                 #     text = actionButton("return_btn_sc","Return to Launcher", class = "btn_no_border", onclick = "setTimeout(function(){window.close();}, 100); "),
                 #     icon = icon("sign-out"),
                 #     status = "return"
                 # ),
                 systemItem(
                     text = actionButton("exit_and_save","EXIT PIVOT", class = "btn_no_border", onclick = "setTimeout(function(){window.close();}, 100); "),
                     icon = icon("power-off"),
                     status = "danger"
                 )
    )
)


