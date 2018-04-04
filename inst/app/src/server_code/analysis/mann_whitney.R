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


# Mann–Whitney U test

# Function From Hannah
compute_mww<-function(counts, group1, group2) {
    testRecords <- apply ( counts , 1 , function ( geneA ) {
        group1.counts <- as.numeric(as.character(geneA [ group1 ]))
        group2.counts <- as.numeric(as.character(geneA [ group2 ]))
        testResult <- tryCatch({
            wilcox.test ( group1.counts , group2.counts , alternative = 'two.sided' , paired = FALSE, conf.int = T , conf.level = 0.95 )
        }, error = function(e) { # So here basically set those failed computations (because of tie) to NULL and later to NA in the table, the warnings are ignored here
            return(NULL)
        }
        )

        if(!is.null(testResult)) {
            c ("Statistic" = testResult$statistic [[ 1 ]] , "P.value" = testResult$p.value [[ 1 ]], "group1.n" = length ( group1 ) , "group2.n" = length ( group2 ) ,
               "effectSize" = as.numeric(testResult$estimate) , "effectSize.CI.lower" = testResult$conf.int [ 1 ] , "effectSize.CI.upper" = testResult$conf.int [ 2 ] )
        } else {
            c ("Statistic" = NA , "P.value" = NA, "group1.n" = NA , "group2.n" = NA ,
               "effectSize" = NA , "effectSize.CI.lower" = NA , "effectSize.CI.upper" = NA )
        }

    })

    return(as.data.frame(t(testRecords)))
}


output$mww_ui <- renderUI({
    if(is.null(r_data$df)) return()
    if(is.null(r_data$group) || length(unique(r_data$group)) < 2) return({
        tags$li("This module requires group (condition) information.")
    })

    groups <- as.list(as.character(unique(r_data$group)))
    names(groups) <- unique(r_data$group)

    mww_group_ui <- list(
        wellPanel(
            fluidRow(
                column(3, selectInput("mww_group1", "Group 1", choices = groups, selected = as.character(groups[[1]]))),
                column(1, tags$b("vs")),
                column(3, selectInput("mww_group2", "Group 2", choices = c(groups, Combined_rest = "combined_rest"), selected = as.character(groups[[2]])))
            )
        ),
        actionButton("perform_mww", "Perform Test", class = "btn-info"),
        hr(),
        wellPanel(
            fluidRow(
                column(4, selectInput("mww_p_method", "P adjustment method", choices = list("Bonferroni correction" = "bonferroni", "False discovery rate" = "fdr", "Holm (1979)" = "holm", "Hochberg (1988)" = "hochberg","Hommel (1988)" = "hommel", "Benjamini & Yekutieli (2001)" = "BY", "None" = "none"), selected = "fdr")),
                column(4, numericInput("mww_alpha", "Adjusted-P cutoff", value = 0.1, min = 0, max = 0.5, step = 0.001)),
                column(4, checkboxInput("mww_cuttbl", "Only show significant genes", value = T))
            )
        )
    )
    if(!is.null(r_data$batch)) {
        batch_ui <- selectInput("mww_gene_plt_type", "Plot by", choices = list("Group" = "group", "Batch" = "batch"))
    } else {
        batch_ui <- NULL
    }

    list(
        enhanced_box(
            width = 12,
            title = "Mann–Whitney U test",
            id = "mww_test_result",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

           tags$p("The Mann–Whitney U test is also called the Mann–Whitney–Wilcoxon (MWW), Wilcoxon rank-sum test (WRS), or Wilcoxon–Mann–Whitney test.", class = "citation"),
           a("https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test", href = "https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test"),
           hr(),
           mww_group_ui,
           uiOutput("tbl_caption_ui", class = "table-cap"),
           DT::dataTableOutput("mww_result_tbl"),
           uiOutput("download_mww_result_ui"),
           hr(),
           uiOutput("mww_summary")
       ),
       enhanced_box(
           width = 12,
           title = "Plot of Selected Gene",
           id = "mww_gene_plot",
           status = "danger",
           solidHeader = T,
           collapsible = T,
           reportable = T,
           get_html = T,
           register_analysis= T,

           wellPanel(
               fluidRow(
                   column(4, checkboxInput("mww_gene_plt_all", "Show counts in all groups", value = F)),
                   column(4, selectInput("mww_gene_plt_style", "Plot type", choices = list("Plot points" = "points", "Box plot" = "box", "Violin plot" = "violin"), selected = "points")),
                   column(4, batch_ui)
               )
           ),
           plotOutput("mww_gene_plt", height = "600px")
       )
    )
})

output$tbl_caption_ui <- renderUI({
    if(is.null(r_data$mww_group)){
         return(NULL)
    } else {
        tags$b(paste("Mann–Whitney U test Results of", r_data$mww_group[1], "vs", r_data$mww_group[2]))
    }

})

output$download_mww_result_ui <- renderUI({
    if(is.null(r_data$mww_results)) return()
    if(input$mww_cuttbl) {
        tbl <- subset(r_data$mww_results, adjustedP <= input$mww_alpha)
    } else {
        tbl <- r_data$mww_results
    }
    if(nrow(tbl) == 0) return()
    download_mww_result_ui <- downloadButton("download_mww_result","Download", class = "btn btn-success")
})

output$download_mww_result <- downloadHandler(
    filename = function() {
        "mann_whitney_results.csv"
    },
    content = function(file) {
        if(input$mww_cuttbl) {
            tbl <- subset(r_data$mww_results, adjustedP <= input$mww_alpha)
        } else {
            tbl <- r_data$mww_results
        }

        tbl<-tbl[order(tbl$P.value),]
        write.csv(tbl, file)
    }
)

observeEvent(input$perform_mww, {
    if(is.null(r_data$group)) return()
    if(is.null(input$mww_group1)) return()
    if(input$mww_group1 == input$mww_group2){
        session$sendCustomMessage(type = "showalert", "Groups must be different.")
        return()
    }
    withProgress(message = 'Processing...', value = 0.8, {
        sp1 <- names(r_data$group[r_data$group == input$mww_group1])

        if(input$mww_group2 == "combined_rest") {
            sp2 <- names(r_data$group[r_data$group != input$mww_group1])
        } else {
            sp2 <- names(r_data$group[r_data$group == input$mww_group2])
        }

        suppressWarnings(tbl <- compute_mww(r_data$df, sp1, sp2))
        tbl$adjustedP <- p.adjust ( tbl$P.value , input$mww_p_method )
        first3col<-c("Statistic", "P.value", "adjustedP")
        r_data$mww_results <- tbl[c(first3col, setdiff(names(tbl), first3col))] # reorder
        r_data$mww_group <- c(input$mww_group1, input$mww_group2)
    })
})


observe({
    input$mww_p_method
    isolate({
        if(!is.null(r_data$mww_results)) {
            r_data$mww_results$adjustedP <- p.adjust ( r_data$mww_results$P.value , input$mww_p_method)
        }
    })
})



output$mww_result_tbl <- DT::renderDataTable({
    if(is.null(r_data$mww_results)) return()
    if(input$mww_cuttbl) {
        tbl <- subset(r_data$mww_results, adjustedP <= input$mww_alpha)
    } else {
        tbl <- r_data$mww_results
    }
    if(nrow(tbl) == 0) return()
    DT::datatable(tbl, selection = 'single', options = list(scrollX = TRUE, scrollY = "250px", searching=T, order = list(list(2, 'asc')) , orderClasses = T))
})

output$mww_summary <- renderUI({
    tags$li(paste0("Total number of significant genes: ", sum(r_data$mww_results$adjustedP < input$mww_alpha, na.rm = T), "."))
})



# Gene plot

output$mww_gene_plt <- renderPlot({

    s = input$mww_result_tbl_row_last_clicked
    tbl<-as.data.frame(r_data$mww_results)

    if (length(s)) {
        r_data$mww_gene <- rownames(tbl[s, , drop = FALSE])
    } else {
        return()
    }

    if(is.null(input$mww_gene_plt_type) || input$mww_gene_plt_type == "group") {
        v1<-r_data$df[r_data$mww_gene,]
        d<-data.frame(t(v1), group = as.character(r_data$group))
        colnames(d) <- c("expression_level", "group")

        if(!is.null(input$mww_gene_plt_all) && input$mww_gene_plt_all) {
        } else {
            if(r_data$mww_group[2] != "combined_rest") {
                d <- subset(d, group %in% c(r_data$mww_group[1], r_data$mww_group[2]))
            }
        }

        feature_plot(d, r_data$mww_gene, plot_group = "group",  style = input$mww_gene_plt_style, legend_pos = "top")

    } else if(input$mww_gene_plt_type == "batch") {
        v1<-r_data$df[r_data$mww_gene,]
        d<-data.frame(t(v1), group = as.character(r_data$batch))
        colnames(d) <- c("expression_level", "batch")

        feature_plot(d, r_data$mww_gene, plot_group = "batch", style = input$mww_gene_plt_style, legend_pos = "top")
    }

})





