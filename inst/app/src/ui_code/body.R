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

#################################### Body Module ########################################

body <- dashboardBody(
    tabItems(
        ########### Show Data Table Module #########
        tabItem(tabName = "data_input",
                fluidRow(
                    column(width = 8,
                           tabBox( width = NULL,
                                   id = "input_tabset",
                                   ##################### File input module ###################
                                   tabPanel(
                                       tags$b("FILE"),
                                       value = "file_in",
                                       tags$div(tags$b("Input Method and Normalization Setting:"), class = "param_setting_title"),
                                       fluidRow(
                                           column(6,
                                                  selectInput("file_format", label = "Input file type", choices = list("Counts Directory" = "dir", "Counts Table" = "single", "PIVOT State" = "state"),selected = "single")
                                           ),
                                           column(6,
                                                  uiOutput("proc_method_ui")
                                           )
                                       ),
                                       uiOutput("deseq_threshold_ui"),
                                       uiOutput("norm_text_ui"),
                                       fluidRow(
                                           column(6,
                                                  uiOutput("gene_length_ui")
                                           ),
                                           column(6,
                                                  uiOutput("norm_details_ui")
                                           )
                                       ),
                                       tags$p(),
                                       ##### count file input module #####
                                       conditionalPanel(
                                           "input.file_format == 'dir'",
                                           tags$div(
                                               tags$b("Counts Directory Input:"),
                                               pivot_help_UI("directory", "How to input a count folder"),
                                               class = "param_setting_title"
                                           ),
                                           fluidRow(
                                               column(6,
                                                      shinyFiles::shinyDirButton('data_folder', 'Select Data Folder', 'Please select a folder', FALSE, class = "btn-info")
                                               ),
                                               column(6, verbatimTextOutput("data_folder_show"))
                                           ),
                                           textInput("file_search", label = "File filter", value = ""),
                                           shinyBS::bsTooltip(
                                               "file_search",
                                               title = "Hint: Use keywords (e.g. exon.cnt) to only include wanted count files.",
                                               options = list(container = "body")
                                           ),
                                           uiOutput("select_data")
                                       ),

                                       ##### Single file module #####
                                       conditionalPanel(
                                           "input.file_format == 'single'",
                                           tags$div(
                                               tags$b("Data Table Input:"),
                                               pivot_help_UI("single", title = "How to input a count table"),
                                               class = "param_setting_title"
                                           ),
                                           fileInput('file_single', 'Choose counts file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                           fluidRow(
                                               column(9,
                                                      wellPanel(
                                                          checkboxInput('header_ct', 'Header', TRUE),
                                                          radioButtons('sep_ct', 'Separator', c(Comma=',', Semicolon=';', Tab='\t', Space = ' '), selected = ',', inline = TRUE),
                                                          fluidRow(
                                                              column(6, selectInput('row_ct', 'Row names', choices = list("automatic" = "automatic", "first column" = "firstcol", "numbers" = "numbers"), selected = "firstcol"))
                                                          ),
                                                          radioButtons('quote_ct', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"', inline = TRUE)
                                                      )
                                               )
                                           )
                                       ),

                                       # Threshold
                                       conditionalPanel(condition = "input.file_format == 'dir' || input.file_format == 'single'",
                                                        tags$div(tags$b("Additional Input Settings:"), class = "param_setting_title"),
                                                        fluidRow(
                                                            column(6, radioButtons("input_threshold_type", "Choose pre-filtering type:", choices = c("Row Mean" = "mean", "Row Sum" = "sum"), inline = T)),
                                                            column(6, uiOutput("input_threshold_ui"))
                                                        )

                                                        # ERCC isolation, no longer in PIVOT.data module
                                                        # fluidRow(
                                                        #     column(12,
                                                        #            checkboxInput("ercc_isolation",
                                                        #                          label = tags$span(
                                                        #                              "Only analyze ERCC in ERCC module.",
                                                        #                              shinyBS::tipify(
                                                        #                                  bsButton("ercc_isolation_tooltip", label = NULL, icon = icon("question-circle"), style = "link", size = "extra-small"),
                                                        #                                  title = "ERCC will not appear in modules like heatmap, PCA, etc. This exclusion of ERCC happens after DESeq normalization.",
                                                        #                                  options = list(container = "body")
                                                        #                              )
                                                        #                          ),
                                                        #                          value = T)
                                                        #     )
                                                        # )
                                       ),

                                       # upload state
                                       conditionalPanel(condition = "input.file_format == 'state'",
                                                        tags$div(tags$b("PIVOT State Upload:"), class = "param_setting_title"),
                                                        fileInput('uploadState', 'Load PIVOT state:',  accept = ".rda"),
                                                        uiOutput("refreshOnUpload"),
                                                        tags$li("Only valid PIVOT .rda file can be accepted."),
                                                        tags$li("You can save state using the system panel at top right."),
                                                        tags$li("Session will immediately switch to the loaded state.")
                                       ),

                                       # Submit button
                                       fluidRow(
                                           column(12,
                                                  uiOutput("data_submitted_img"),
                                                  uiOutput("input_submit_btn_ui")
                                           )
                                       )
                                   ),


                                   ####################### Group info module #####################
                                   tabPanel(
                                       tags$b("DESIGN"),
                                       value = "group_in",
                                       conditionalPanel(
                                           condition = "output.data_submitted_img",
                                           fluidRow(
                                               column(12,
                                                      tags$div(
                                                          tags$b("Input method:"),
                                                          pivot_help_UI("design", title = "What is a design table"),
                                                          class = "param_setting_title"
                                                      )
                                               )
                                           ),
                                           fluidRow(
                                               column(6,
                                                      selectInput("add_group_way", label = NULL, choices = list("Manually add design info" = "manual", "Upload a design info file" = "upload"))
                                               )
                                           )
                                       ),
                                       conditionalPanel(condition = "!output.data_submitted_img",
                                                        tags$p("Please submit your data first.")
                                       ),
                                       tags$p(),
                                       uiOutput("design_ui")
                                   ),

                                   #################### feature filtering module ###################
                                   tabPanel(
                                       tags$b("FEATURE"),
                                       value = "feature_in",
                                       uiOutput("filter_ui")
                                   ),

                                   #################### Data subsetting module ###################
                                   tabPanel(
                                       tags$b("SAMPLE"),
                                       value = "sample_in",
                                       uiOutput("subset_ui")
                                   ),

                                   tabPanel(
                                       tags$b("DATA MAP"),
                                       value = "data_man",
                                       uiOutput("data_man_ui")
                                   )
                           )
                    ),

                    column(width = 4,
                           uiOutput("data_pv_ui")
                           #uiOutput("return_btn_window")
                    )
                ),

                fluidRow(
                    box(
                        width = 12,
                        title = "Quick View of Chosen Data",
                        status = "info",
                        solidHeader = T,
                        fluidRow(
                            infoBoxOutput_custom("input_file_box", width = 2, style = "padding-right:2px; padding-left: 10px;"),
                            valueBoxOutput_custom("feature_number_box", width = 2, style = "padding-right:2px; padding-left: 2px;"),
                            valueBoxOutput_custom("feature_percent_box", width = 2, style = "padding-right:2px; padding-left: 2px;"),
                            valueBoxOutput_custom("read_percent_box", width = 2, style = "padding-right:2px; padding-left: 2px;"),
                            valueBoxOutput_custom("sample_number_box", width = 2, style = "padding-right:2px; padding-left: 2px;"),
                            valueBoxOutput_custom("cat_number_box", width = 2, style = "padding-right:10px; padding-left: 2px;")
                        )
                    )
                ),
                hr(),
                br()
        ),


        tabItem(tabName = "table",
                uiOutput("table_box_ui"),
                hr(),
                br()
        ),

        tabItem(tabName = "meta",
                uiOutput("meta_ui"),
                hr(),
                br()
        ),

        tabItem(tabName = "dmap",
                uiOutput("dmap_ui"),
                hr(),
                br()
        ),

        tabItem(tabName = "data_distribution",
                uiOutput("distribution_ui"),
                hr(),
                br()
        ),

        ################################# Analysis Output Module ##############################

        # ERCC
        tabItem(tabName = "ercc",
                uiOutput("ercc_ui"),
                hr(),
                br()
        ),

        tabItem(tabName = "deseq",
                uiOutput("deseq_ui"),
                hr(),
                br()
        ),

        tabItem(tabName = "scde",
                uiOutput("scde_ui"),
                hr(),
                br()
        ),

        tabItem(tabName = "mww",
                uiOutput("mww_ui"),
                hr(),
                br()
        ),

        tabItem(tabName = "monocle",
                uiOutput("monocle_ui"),
                hr(),
                br()
        ),

        tabItem(tabName = "monocle_state",
                uiOutput("monocle_state_ui"),
                hr(),
                br()
        ),

        tabItem(tabName = "monocle_gene",
                uiOutput("monocle_gene_ui"),
                hr(),
                br()
        ),

        # hierarchical clustering
        tabItem(tabName = "hclust",
                uiOutput("hclust_ui"),
                hr(),
                br()
        ),

        # k-means
        tabItem(tabName = "kmeans",
                uiOutput("kmeans_ui"),
                hr(),
                br()
        ),

        # Community detection
        tabItem(tabName = "community",
                uiOutput("mst_ui")
        ),

        # correlation scatterplot
        tabItem(tabName = "pairwise",
                uiOutput("pairwise_box"),

                hr(),
                br()
        ),

        # Sample correlation heatmap
        tabItem(tabName = "correlation_hm",
                uiOutput("cor_sp_ui"),
                hr(),
                br()
        ),

        # Sample correlation heatmap
        tabItem(tabName = "cor_ft",
                uiOutput("cor_ft_ui"),
                hr(),
                br()
        ),

        # feature heatmap
        tabItem(tabName = "heatmap",
                uiOutput("hm_ui"),
                hr(),
                br()
        ),

        # PCA
        tabItem(tabName = "pca",
                uiOutput("pca_ui"),
                hr(),
                br()
        ),

        # T-SNE
        tabItem(tabName = "tsne",
                uiOutput("tsne_ui"),
                hr(),
                br()
        ),

        # penalizedLDA
        tabItem(tabName = "plda",
                uiOutput("plda_ui"),
                hr(),
                br()
        ),

        # Network visulization
        tabItem(tabName = "network",
                uiOutput("network_ui"),
                hr(),
                br()
        ),

        # Transdifferentiation Calculator
        tabItem(tabName = "transnet",
                uiOutput("transnet_ui"),
                hr(),
                br()
        ),

        tabItem(tabName = "venn",
                uiOutput("venn_ui"),
                hr(),
                br()
        ),

        tabItem(tabName = "report",
                uiOutput("report_ui")

        ),


        tabItem(tabName = "manual_file",
                includeMarkdown("./manual/manual_file.Rmd")
        ),


        tabItem(tabName = "about",
                includeMarkdown("./manual/about.Rmd"),
                fluidRow(
                    HTML("
                               <div class = 'kimlab_footer'>
                                   <div class='kimlab_container'>
                                   <div class='kim_footer-3'>
                                   <a href='http://www.sas.upenn.edu'><img src='http://kim.bio.upenn.edu/wiki.media/images/penn-logo.png' class='kim_footer-logo'/></a><br/><br/>
                                   <p>&copy; 2015 J. Kim | All rights reserved</p>
                                   </div>
                                   <div class='kim_footer-3'>
                                   <address>
                                   <a href='http://www.bio.upenn.edu'><strong>Biology Department</strong></a><br/>
                                   <a href='http://www.upenn.edu'><strong>University of Pennsylvania</strong></a><br/>
                                   103I Lynch Laboratory<br/>
                                   433 S University Avenue<br/>
                                   Philadelphia, PA 19104 USA<br/>
                                   </address>
                                   </div>
                                   <div class='kim_footer-3'>
                                   <p>
                                   <strong>off:</strong>
                                   <a href='tel:+12157465187'>(215) 746-5187</a><br/>
                                   <strong>lab:</strong> <a href='tel:+12158988395'>(215) 898-8395</a><br/>
                                   </p><br/>
                                   <p>
                                   <strong>email:</strong>
                                   <a href='mailto:junhyong@sas.upenn.edu'>junhyong@sas.upenn.edu</a>
                                   </p>
                                   </div>
                                   </div>
                                   </div>"
                    )
                )
        )
    ),

    ################### JAVAScript ##################
    # Alert module
    singleton(tags$script(type="text/javascript", "
        $(document).ready(function() {
        Shiny.addCustomMessageHandler('showalert', function(message) {
          alert(message);
        });
        });
    ")),
    singleton(tags$script(type="text/javascript", "
        $(document).ready(function() {
                          Shiny.addCustomMessageHandler('closeModal', function(message) {
                                $('#modal').modal('hide');
                          });
                          });
                          ")),
    tags$script(
        HTML('
             Shiny.addCustomMessageHandler(
             type = "jsCode"
             ,function(message) {
             Shiny.onInputChange("deleteConfirmChoice",eval(message.value));
             })
             ')
    ),
    tags$head(
        tags$script(src = "js/session.js"),
        tags$script(src = "js/custom.js")
    )
)

