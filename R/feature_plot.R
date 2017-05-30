

#' Generate color set for plots
#'
#' @export
get_brewer_set <- function(palette = c("sequential", "diverging", "qualitative")) {
    match.arg(palette,
              several.ok = TRUE)

    sequential_palette <- c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys',
                            'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                            'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd')
    names(sequential_palette) <- sequential_palette
    diverging_palette <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
    names(diverging_palette) <- diverging_palette
    qualitative_palette <- c('Accent','Dark2','Paired', 'Pastel1', 'Pastel2','Set1', 'Set2', 'Set3')
    names(qualitative_palette) <- qualitative_palette
    return_palette = list()
    if("qualitative" %in% palette) {
        return_palette <- c(return_palette, as.list(qualitative_palette))
    }
    if("diverging" %in% palette) {
        return_palette <- c(return_palette, as.list(diverging_palette))
    }
    if("sequential" %in% palette) {
        return_palette <- c(return_palette, as.list(sequential_palette))
    }

    return(return_palette)
}

#' Get a vector of colors for heatmap
#' @import RColorBrewer viridis
#' @export
get_heatmap_color <- function(palette) {
    if(palette %in% get_brewer_set("sequential")) {
        RColorBrewer::brewer.pal(9,palette)
    } else if(palette %in% list("viridis" = "viridis", "magma" = "magma", "plasma" = "plasma", "inferno" = "inferno")) {
        viridis(n = 100, option = palette)
    }
}

#' Get a vector of colors with RColorBrewer for group labels
#' @import RColorBrewer
#' @export
get_color_vector <- function(labels, pal="Set1", maxCol=9)
{
    unq <- unique(labels)
    hmcol <- RColorBrewer::brewer.pal(maxCol, pal)

    colv <- rep(NA, length(labels))

    if (length(unq) > 9)
    {
        cp <- colorRampPalette(hmcol)
        hmcol <- cp(length(unq))
    }

    for (i in 1:length(unq))
    {
        colv[labels == unq[i]] <- hmcol[i]
    }

    return(colv)
}



#' Plot feature expression with ggplot
#'
#' @import ggplot2 plotly
#' @export
feature_plot <- function(df, selected_gene, plot_by = "sample", meta = NULL, palette = "Set1", style = "box", log_scale = F, legend_pos = "top", textSize = 15, pointSize = 3){

    if(is.null(df) || nrow(df) == 0) {
        return()
    }

    if (is.null(meta)) {
        g1 <- ggplot(df, aes(x=sample, y=expression_level)) +
            theme(text = element_text(size=textSize), plot.title = element_text(hjust = 0.5)) +
            ggtitle(paste("Expression level of gene", selected_gene, "across samples"))
        if(style == "points") {
            g1 <- g1 + geom_point(size = pointSize)
        } else if(style == "bar") {
            g1 <- g1 + geom_bar(stat = "identity")
        }
    } else {
        if(is.null(palette)) return(NULL)
        df <- cbind(df, meta)
        g1 <- ggplot(df, aes_string(x=plot_by, y="expression_level"))
        if(style == "bar") {
            g1 <- ggplot(df, aes_string(x="sample", y="expression_level"))
            g1 <- g1 + geom_bar(stat = "identity", aes_string(fill = plot_by))
        } else if(style %in% c("points", "box", "violin")) {
            g1 <- g1 + geom_point(position=position_jitter(w=0.1,h=0), size = pointSize, aes_string(colour = plot_by, group = plot_by))
            if(style == "box") {
                g1 <- g1 + geom_boxplot(aes_string(fill = plot_by, alpha = 0.2))
            } else if(style == "violin") {
                g1 <- g1 + geom_violin(aes_string(fill = plot_by, alpha = 0.2), trim = F)
            }
        }
        g1 <- g1 + scale_color_brewer(palette = palette) +
            scale_fill_brewer(palette = palette) +
            ggtitle(paste0("Expression level of gene ", selected_gene))  +
            theme(text = element_text(size=textSize), legend.position=legend_pos, plot.title = element_text(hjust = 0.5)) +
            guides(fill=FALSE, alpha = F)
    }

    if(log_scale) {
        g1 <- g1 + scale_y_log10(breaks=c(25,100,400))
    }

    return(g1 + theme_minimal())
}


#' Choose which meta column to be used as coloring group
#'
#' @description
#' This is the UI part of the module
#'
#' @export
pivot_colorBy_UI <- function(id, meta, append_sample = F, append_none = F, multiple = F, choose_color = T, bset ="qualitative", width = 8) {
    if(is.null(meta) || ncol(meta) < 2) return()
    ns<- NS(id)
    categories = colnames(meta)[-1]
    names(categories) <- categories
    options <- as.list(categories)
    if(append_sample) {
        options <- c("sample" = "sample", options)
    }
    if(append_none) {
        options <- c(options, "none" = "none")
    }
    if(choose_color) {
        list(
            column(width/2, selectInput(ns("group_by"), label = "Group by", choices = options, multiple = multiple)),
            column(width/2, selectInput(ns("color_set"), label = "Group color", choices = get_brewer_set(bset), multiple = multiple))
        )
    } else {
        list(
            column(width, selectInput(ns("group_by"), label = "Group by", choices = options, multiple = multiple))
        )
    }
}

#' Choose which meta column to be used as coloring group
#'
#' @description
#' This is the server part of the module
#'
#' @export
pivot_colorBy <- function(input, output, session, meta) {
    if(is.null(input$group_by) || any(input$group_by %in% c("sample", "none"))) {
        meta <- NULL
    } else {
        req(input$group_by %in% colnames(meta))
        meta <- meta[, input$group_by, drop = F]
    }

    # extract meta data

    if(!is.null(meta)) {
        if(is.null(input$color_set) || length(input$group_by) != length(input$color_set)) {
            meta_color <- as.data.frame(apply(meta, 2, function(x){get_color_vector(x, pal = "Set1", maxCol = length(unique(x)))}))
        } else {
            meta_color <- NULL
            for(idx in 1:ncol(meta)) {
                col_color<-get_color_vector(meta[,idx], pal = input$color_set[idx], maxCol = length(unique(meta[,idx])))
                meta_color<-cbind(meta_color,col_color)
            }
        }
    } else {
        meta_color <- NULL
    }

    return(list(
        meta = meta,
        meta_color = meta_color,
        group_by = input$group_by,
        color_set = input$color_set
    ))
}

#' The feature plot module of PIVOT
#'
#' @description
#' This is the UI part of the module, includes scale option, color group option, color set option, plot style option and the plot.
#' @import plotly
#' @export
pivot_featurePlot_UI <- function(id, meta, ids = NULL) {
    ns<- NS(id)
    if(!is.null(meta)) {
        color_ui <- pivot_colorBy_UI(ns("tbl_plt"), meta = meta, append_sample = T, bset = c("qualitative","diverging"), width = 6)
        style_ui <- column(3, selectInput(ns("plt_style"), "Plot type", choices = list("Plot points" = "points", "Bar plot" = "bar", "Box plot" = "box", "Violin plot" = "violin")))
    } else {
        color_ui <-  NULL
        style_ui <- NULL
    }
    if(!is.null(ids)) {
        plotGroup_ui <- checkboxInput(ns("plt_all"), "Show expression for all samples", value = F)
    } else {
        plotGroup_ui <- NULL
    }

    tagList(
        tags$div(tags$b("Expression Plot:"), class = "param_setting_title"),
        fluidRow(
            column(3, selectInput(ns("plt_scale"), "Plot Scale", choices = list("Normalized Expression" = FALSE, "Log10" = TRUE))),
            color_ui,
            style_ui
        ),
        fluidRow(
            column(6),
            column(3, plotGroup_ui),
            column(3, checkboxInput(ns("plt_interactive"), tags$p("Interactive Plot"), value = F))
        ),
        conditionalPanel(sprintf("input['%s']", ns("plt_interactive")),
                plotly::plotlyOutput(ns("ft_stats_plt_interactive"))
        ),
        conditionalPanel(sprintf("!input['%s']", ns("plt_interactive")),
                plotOutput(ns("ft_stats_plt"))
        )
    )
}

#' The feature plot module of PIVOT
#'
#' @description
#' This is the server part of the module
#' @import plotly
#' @export
pivot_featurePlot <- function(input, output, session, meta, df, gene, ids = NULL) {
    plotObj <- reactive({
        rsList <- callModule(pivot_colorBy, "tbl_plt", meta = meta)
        meta <- rsList$meta
        if(!is.null(input$plt_all)) {
            req(ids)
            if(!input$plt_all) {
                idxs <- which(df[,1] %in% ids)
                df<-df[idxs,, drop = F]
                meta <- meta[idxs,, drop = F]
            }
        }
        return(list(df = df, meta = meta, group_by = rsList$group_by, color_set = rsList$color_set))
    })

    g0 <- feature_plot(plotObj()$df, gene, plot_by = plotObj()$group_by, plotObj()$meta, palette = plotObj()$color_set, style = input$plt_style, log_scale = input$plt_scale, legend_pos = "right")
    output$ft_stats_plt <- renderPlot({
        req(plotObj())
        #assign("t1",plotObj(),env = .GlobalEnv)
        g0
    })
    output$ft_stats_plt_interactive <- plotly::renderPlotly({
        req(plotObj())
        g<-feature_plot(plotObj()$df, gene, plot_by = plotObj()$group_by, plotObj()$meta, palette = plotObj()$color_set, style = input$plt_style, log_scale = input$plt_scale, legend_pos = "right", textSize = 12, pointSize = 2)
        plotly::ggplotly(g)
    })

    return(g0)
}




