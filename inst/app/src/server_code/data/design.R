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



output$design_ui <- renderUI({
    if(is.null(r_data$glb.raw)) return()
    if(input$add_group_way == "manual") { # manual add design

            list(
                fluidRow(
                    column(5,
                           tags$div(tags$b("Add Category:"), class = "param_setting_title"),
                           fluidRow(
                               column(6, textInput("man_cate_name", "Name:", placeholder = "e.g., Group/Batch/Condition")),
                               column(6, selectInput("man_cate_type", "Type", choices = list("categorical" = "categorical", "numerical"="numerical")))
                           ),
                           actionButton('man_add_cate', label = "Add Category", class = "btn btn-info"),
                           tags$p(),
                           uiOutput("man_choose_cate_ui"),
                           tags$p(),
                           uiOutput("man_cate_add_group_ui")
                    ),
                    column(7,
                           tags$div(tags$b("Design Table Preview"), class = "param_setting_title"),
                           DT::dataTableOutput("group_tbl_man_show"),
                           tags$p(),
                           uiOutput("grp_submitted_img1"),
                           uiOutput("man_design_submit_ui")
                    )
                )
            )

    } else if(input$add_group_way == "upload"){ # upload design table
            list(
                fluidRow(
                    column(5,
                           tags$div(tags$b("Design Table Upload:"),
                                    pivot_help_UI("design_input", "File format requirement"),
                                    class = "param_setting_title"),
                           pivot_fileInput_UI("design"),
                           uiOutput("design_upload_cate_type_ui"),
                           checkboxInput("sample_reorder", "Reorder samples according to the design table.", value = F)
                    ),
                    column(7,
                           tags$div(tags$b("Design Table Preview"), class = "param_setting_title"),
                           pivot_filePreview_UI("design_pv"),
                           br(),
                           uiOutput("grp_submitted_img2"),
                           actionButton('submit_design_upload', label = "submit design", class = "btn-primary btn_rightAlign")
                    )
                )
            )
    } else { # should not reach here
        print("Debug required.")
        return()
    }
})


observe({
    df <- callModule(pivot_fileInput, "design")
    callModule(pivot_filePreview, "design_pv", df$df)
})


########################### Manual Input ###########################

custom_merge <- function(df, df1, by, col) {
    sp <- df[,by]
    gp <- df[,col]
    names(gp) <- as.character(sp)
    gp[as.character(df1[,by])] <- df1[,col]
    df[,col] <- gp
    return(df)
}

output$man_choose_cate_ui <- renderUI({
    categories = colnames(r_data$design_pv)[-1]
    if(length(categories) < 1) return()
    names(categories) = categories
    list(
        tags$div(tags$b("Edit Category:"), class = "param_setting_title"),
        selectInput("man_choose_cate", "Choose category", choices = as.list(categories)),
        actionButton('man_del_cate', label = "Delete Category", class = "btn-danger")
    )
})

output$manual_add_group_ui <- renderUI({
    req(input$man_choose_cate)
    sample_name <- colnames(r_data$glb.raw)
    group_options <- as.list(sample_name)
    names(group_options) <- sample_name

    if(is.character(r_data$design_pv[[input$man_choose_cate]])) {
        val_or_group <- textInput("man_group_name", label = "Group name:", value = "group_1")
    } else if (is.numeric(r_data$design_pv[[input$man_choose_cate]])) {
        val_or_group <- numericInput("man_group_value", label = "Group value:", value = 0)
    } else {
        return()
    }
    list(
        val_or_group,
        selectizeInput('man_sample_in_group', label = "Add samples to this group", choices = group_options, multiple = TRUE),
        verbatimTextOutput("man_group_show"),
        actionButton('man_add_group', label = "Add Group", class = "btn-info")
    )
})

output$man_cate_add_group_ui <- renderUI({
    if(is.null(input$man_choose_cate)) return()
    if(!input$man_choose_cate %in% colnames(r_data$design_pv)) return()
    list(
        tags$div(tags$b(paste("Add Values to Category"),input$man_choose_cate), class = "param_setting_title"),
        uiOutput("manual_add_group_ui")
    )
})

output$man_design_submit_ui <- renderUI({
    not_filled <- sum(is.na(r_data$design_pv))
    if(!not_filled) {
        actionButton("man_submit_design", "Submit Design", class = "btn-primary btn_rightAlign")
    } else {
        list(
            modalTriggerButton("man_submit_trigger", "#submitConfirmDlg", paste("Submit Design"), class = "btn action-button btn-primary btn_rightAlign"),
            modalConfirmDialog(id="submitConfirmDlg",
                        header = "Alert: Empty cells detected",
                        body = "Some cells are still empty, these cells will be replaced with NA values. It may affect some analysis/plots.",
                        footer=list(
                            modalTriggerButton("submitConfirmDlgBtn", "#submitConfirmDlg", "Proceed", class = "btn action-button btn-primary"),
                            tags$button(type = "button", class = "btn btn-danger", 'data-dismiss' = "modal", "Cancel")
                        )
            )
        )
    }
})


# Initialization
observe({
    # Init df
    if(is.null(r_data$glb.raw)) return()
    if(is.null(r_data$design_pv)) {
        r_data$design_pv <- data.frame(Sample = colnames(r_data$glb.raw))
        r_data$design_pv$Group <- as.character(rep(NA, nrow(r_data$design_pv)))
    }
})

observe({
    if(is.null(r_data$glb.raw) || is.null(input$man_choose_cate) || is.null(r_data$design_pv)) return ()
    if(!input$man_choose_cate %in% colnames(r_data$design_pv)) return()
    isolate({
        left_sample <- r_data$design_pv$Sample[which(is.na(r_data$design_pv[,input$man_choose_cate]))]
        group_options <- as.list(left_sample)
        names(group_options) <- left_sample
        updateSelectInput(session, "man_sample_in_group",
                          label = "Add samples to this group:",
                          choices = group_options, selected = NULL)
    })
})

current_group <- reactive({
    req(input$man_sample_in_group)
    cur_cate <- input$man_choose_cate
    df<-data.frame(Sample = input$man_sample_in_group)
    if(is.character(r_data$design_pv[[input$man_choose_cate]])) {
        df[[cur_cate]] <- rep(input$man_group_name, nrow(df))
    } else if (is.numeric(r_data$design_pv[[input$man_choose_cate]])) {
        df[[cur_cate]] <- rep(input$man_group_value, nrow(df))
    } else {
        return()
    }
    return(df)
})

output$man_group_show <- renderPrint({
    current_group()
})

observeEvent(input$man_add_cate, {
    if(is.null(input$man_cate_name) || input$man_cate_name == ""){
        session$sendCustomMessage(type = "showalert", "Please specify a category name.")
        return()
    }
    if(input$man_cate_name %in% colnames(r_data$design_pv)) {
        session$sendCustomMessage(type = "showalert", "Category already present in the table.")
        return()
    }
    if(input$man_cate_name %in% c("None", "none")) {
        session$sendCustomMessage(type = "showalert", "'None' or 'none' cannot be used as category name.")
        return()
    }
    if(input$man_cate_type == "categorical") {
        r_data$design_pv[[input$man_cate_name]] <- as.character(rep(NA, nrow(r_data$design_pv)))
    } else {
        r_data$design_pv[[input$man_cate_name]] <- as.numeric(rep(NA, nrow(r_data$design_pv)))
    }
})

observeEvent(input$man_del_cate, {
    if(is.null(input$man_choose_cate)) return()
    r_data$design_pv[,input$man_choose_cate] <- NULL
})

observeEvent(input$man_add_group, {
    if(is.null(input$man_sample_in_group)){
        session$sendCustomMessage(type = "showalert", "Please add samples to this group.")
        return()
    }
    r_data$design_pv <- custom_merge(r_data$design_pv, current_group(), by = "Sample", col = input$man_choose_cate)
})

output$group_tbl_man_show <- DT::renderDataTable({
    DT::datatable(r_data$design_pv, options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE, searching = FALSE))
})

observeEvent(input$clear_group_1, {
    withProgress(message = 'Processing', value = 0, {
        incProgress(0.3, detail = "Removing design info...")
        r_data <- clear_design(r_data)
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data)
        setProgress(1)
    })
})

# submit group (manual module)
observeEvent(input$man_submit_design, {
    if(ncol(r_data$design_pv) < 2) {
        session$sendCustomMessage(type = "showalert", "Please add at least one category.")
        return()
    }
    r_data <- clear_results(r_data)
    withProgress(message = 'Processing', value = 0, {
        incProgress(0.3, detail = "Adding design info...")
        df_tmp <- r_data$design_pv
        # Other sanity checks? Like cols contain NA, empty cols(only spaces), cols contain only one category?
        r_data$glb.meta <- df_tmp

        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data)
        setProgress(1)
    })
})

observeEvent(input$submitConfirmDlgBtn, {
    if(ncol(r_data$design_pv) < 2) {
        session$sendCustomMessage(type = "showalert", "Please add at least one category.")
        return()
    }
    r_data <- clear_results(r_data)
    withProgress(message = 'Processing', value = 0, {
        incProgress(0.3, detail = "Adding design info...")
        df_tmp <- r_data$design_pv
        # Other sanity checks? Like cols contain NA, empty cols(only spaces), cols contain only one category?
        r_data$glb.meta <- df_tmp
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data)
        setProgress(1)
    })
})





####################### Upload design #######################
output$design_upload_cate_type_ui <- renderUI({
    design_upload <- callModule(pivot_fileInput, "design")
    if(is.null(design_upload$df)) {
        return()
    }
    df_tmp <- as.data.frame(design_upload$df)[,-1,drop=F]
    cur_type<-lapply(df_tmp, typeof)
    content <- list(
        lapply(1:length(cur_type), function(i) {
            list(selectInput(paste0("design_cate", i),
                              names(cur_type)[i],
                              choices = list("categorical" = "categorical", "numerical" = "numerical"),
                              selected=ifelse(cur_type[i] %in% c("character","logical","raw"), "categorical", "numerical")))

        })
    )
    list(
        actionButton("design_cate_type_btn", label = "Set design variable type", class = "btn-info"),
        shinyBS::bsModal(id = "design_cate_type_modal", "Set design variable type", "design_cate_type_btn", size = "small", content)
    )
})


# Upload group info handler
observeEvent(input$submit_design_upload, {
    design_upload <- callModule(pivot_fileInput, "design")
    if(is.null(design_upload$df)) {
        return()
    }

    df_tmp <- as.data.frame(design_upload$df)

    # Set column type

    for(i in 2:ncol(df_tmp)){
        if(input[[paste0("design_cate",i-1)]] == "categorical"){
            df_tmp[[i]] <- as.character(df_tmp[[i]])
        } else {
            df_tmp[[i]] <- as.numeric(df_tmp[[i]])
        }
    }

    # assign('df_tmp', df_tmp, env = .GlobalEnv)
    # Take first column as sample column
    sample_col <- make.names(df_tmp[,1])
    matched_sp <- match(colnames(r_data$glb.raw), sample_col) # If contain NA, some sample are not found in sample_col
    if(any(is.na(matched_sp)))
    {
        notFound <- colnames(r_data$glb.raw)[which(!colnames(r_data$glb.raw) %in% sample_col)]
        session$sendCustomMessage(type = "showalert",
                                  paste("Please provide a meta table for ALL your input samples. Following samples are not found:",
                                  paste(notFound, collapse=",")))
        return()
    }

    if(any(colnames(df_tmp) %in% c("None", "none"))) {
        session$sendCustomMessage(type = "showalert", "'None' or 'none' cannot be used as category name.")
        return()
    }
    r_data <- clear_results(r_data)
    withProgress(message = 'Processing', value = 0, {
        incProgress(0.3, detail = "Adding design info...")

        r_data$glb.meta <- df_tmp[matched_sp,] # Remove any meta info for non-existing sample in the dataset

        if(input$sample_reorder) {
            sp_ordered<-colnames(r_data$glb.raw)[match(sample_col, colnames(r_data$glb.raw))]
            sp_ordered <- sp_ordered[!is.na(sp_ordered)]

            r_data$glb.raw <- r_data$glb.raw[sp_ordered]
            r_data$glb.meta <- r_data$glb.meta[match(sp_ordered, r_data$glb.meta[,1]), ]
            r_data$sample_name <- sp_ordered[which(sp_ordered %in% r_data$sample_name)]
            r_data$raw <- r_data$raw[r_data$sample_name]
            r_data$df <- r_data$df[r_data$sample_name]
        }

        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data)
        setProgress(1)
    })
})

# Group submitted indicator
# The image indicating file is submitted
output$grp_submitted_img1 <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) == 1) return()
    img(src = "button_ok.png", width = 35, height = 35, align = "right")
})

output$grp_submitted_img2 <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) == 1) return()
    img(src = "button_ok.png", width = 35, height = 35, align = "right")
})

output$input_design_tbl <- DT::renderDataTable({
    if(is.null(r_data$glb.meta)) return()
    DT::datatable(r_data$glb.meta, selection = 'single',
                  options = list(
                      scrollX = T, scrollY = "500px", lengthMenu = c(20, 50, 100)
                  )
    )
})

output$download_design_tbl <- downloadHandler(
    filename = "design_tbl.csv",
    content = function(file) {
        write.csv(r_data$glb.meta, file, row.names = F)
    }
)



