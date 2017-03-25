

#' a wrapper module for plot in 1D, 2D and 3D with different packages
#'
#' @description
#' This is the UI part of the module.
#' @import plotly threejs
#' @export
pivot_dimScatter_UI <- function(id, type = c("pca", "tsne", "plda")) {
    ns<- NS(id)
    tagList(
        fluidRow(
            column(6,
                   enhanced_box(
                       width = NULL,
                       title = "1D projection",
                       id = ns("box_1d"),
                       status = "primary",
                       solidHeader = T,
                       pivot_Plot1d_UI(ns("plot1d"), type = type)
                   )
            ),
            column(6,
                   enhanced_box(
                       width = NULL,
                       title = "2D projection",
                       id = ns("box_2d"),
                       status = "warning",
                       solidHeader = T,
                       pivot_Plot2d_UI(ns("plot2d"), type = type)
                   )
            )
        ),
        fluidRow(
            column(8,
                   enhanced_box(
                       width = NULL,
                       title = "3D projection",
                       id = ns("box_3d"),
                       status = "danger",
                       solidHeader = T,
                       pivot_Plot3d_UI(ns("plot3d"), type = type)
                   )
            )
        )
    )
}

#' a wrapper module for scatter plot in 1D, 2D and 3D with different packages
#'
#' @description
#' This is the server part of the module.
#' @import plotly threejs
#' @export
pivot_dimScatter <- function(input, output, session, type = c("pca", "tsne", "plda"), obj = NULL, minfo = NULL) {
    if(type == "pca") {
        proj = as.data.frame(obj$x)
        plot1d <- callModule(pivot_Plot1d, "plot1d", type = type, obj, proj, minfo = minfo)
        plot2d <- callModule(pivot_Plot2d, "plot2d", type = type, obj, proj, minfo = minfo)
        plot3d <- callModule(pivot_Plot3d, "plot3d", type = type, obj, proj, minfo = minfo)
    } else if(type == "tsne"){
        plot1d <- callModule(pivot_Plot1d, "plot1d", type = type, obj = NULL, proj = data.frame(X1 = obj$tsne_1d$Y), minfo = minfo)
        plot2d <- callModule(pivot_Plot2d, "plot2d", type = type, obj = NULL, proj = data.frame(obj$tsne_2d$Y), minfo = minfo)
        plot3d <- callModule(pivot_Plot3d, "plot3d", type = type, obj = NULL, proj = data.frame(obj$tsne_3d$Y), minfo = minfo)
    } else if(type == "plda") {
        proj <- obj$proj
        plot1d <- callModule(pivot_Plot1d, "plot1d", type = type, obj = NULL, proj, minfo = minfo)
        plot2d <- callModule(pivot_Plot2d, "plot2d", type = type, obj = NULL, proj, minfo = minfo)
        plot3d <- callModule(pivot_Plot3d, "plot3d", type = type, obj = NULL, proj, minfo = minfo)
    }
    return(list(plot1d = plot1d, plot2d = plot2d, plot3d = plot3d))
}



#' a wrapper module for plot in 1D
#'
#' @description
#' This is the UI part of the module.
#' @import plotly
#' @export
pivot_Plot1d_UI <- function(id, type) {
    ns<- NS(id)
    tagList(
        plotlyOutput(ns("plotly1d")),
        fluidRow(
            column(4, selectInput(ns("plot1d_plt_type"), "Choose plot type", choices = list("density" = "density", "histogram" = "hist"), selected = "density")),
            column(4, uiOutput(ns("plot1d_pc_ui"))),
            column(4, sliderInput(ns("plot1d_step"),.1, 2, value = .1, step = .1, label = "Band/Bin Width Adjustment"))
        )
    )
}

#' a wrapper module for plot in 1D
#'
#' @description
#' This is the server part of the module.
#' @import plotly
#' @export
pivot_Plot1d <- function(input, output, session, type = NULL, obj = NULL, proj = NULL, minfo = NULL) {
    output$plot1d_pc_ui <- renderUI({
        if(is.null(proj)) return()
        if(type == "tsne") return()
        if(type == "pca") {
            dname = "PC"
        } else if(type == "plda") {
            dname = "V"
        }
        PCs <- colnames(proj)
        PC_options <- as.list(PCs)
        names(PC_options) <- PCs
        selectInput(session$ns("plot1d_pc"), label = paste(dname, "on X axis"), choices = PC_options, selected = PC_options[[1]])
    })

    # 1d Plotly
    plot1d <- reactive ({
        req(proj)
        req(input$plot1d_step)
        if(type == "pca") {
            req(input$plot1d_pc)
            d1 <- input$plot1d_pc
        } else if(type == "tsne") {
            d1 <- "X1"
        } else if(type == "plda") {
            d1 <- "V1"
        }
        if(!is.null(minfo$meta)){
            group = minfo$meta[,1]
            pal = unique(minfo$meta_color[,1])
            pal <- setNames(pal, unique(group))
        } else {
            group = rep("sample", nrow(proj))
            pal = NULL
        }

        if(input$plot1d_plt_type == "density") {
            df <- data.frame(x <- proj)
            df$group <- group
            dens<-tapply(df[,d1], INDEX = group, function(x){density(x,adjust = input$plot1d_step)})
            df <- data.frame(
                x = unlist(lapply(dens, "[[", "x")),
                y = unlist(lapply(dens, "[[", "y")),
                Group = rep(names(dens[!sapply(dens, is.null)]), each = length(dens[[1]]$x))
            )
            plot1d <- plotly::plot_ly(df, x = ~x, y = ~y, color = ~Group, type  = "scatter", mode = "lines", fill = "tozeroy", colors = pal)
        } else {
            plot1d <- plotly::plot_ly(proj, x = as.formula(paste0("~", d1)), type = "histogram",
                                      xbins=list(start = min(proj), end = max(proj), size = (max(proj) - min(proj))*input$plot1d_step/2), autobinx=F,
                                      color = minfo$meta[,1], colors = pal)
        }
        plot1d
    })
    output$plotly1d <- plotly::renderPlotly({
        req(plot1d())
        plot1d()
    })

    return(plot1d())
}


#' a wrapper module for plot in 2D
#'
#' @description
#' This is the UI part of the module.
#' @import plotly
#' @export
pivot_Plot2d_UI <- function(id, type) {
    ns<- NS(id)
    if(type == "pca") {
        biplot_select <- selectInput(ns("plot2d_package"), "Plotting package", choices = list("plotly" = "plotly", "ggbiplot" = "ggbiplot"), selected = "plotly")
    } else {
        biplot_select <- selectInput(ns("plot2d_package"), "Plotting package", choices = list("plotly" = "plotly"), selected = "plotly")
    }
    tagList(
        conditionalPanel(sprintf("input['%s'] == 'plotly'", ns("plot2d_package")),
                         plotly::plotlyOutput(ns("plotly2d"))
        ),
        conditionalPanel(sprintf("input['%s'] == 'ggbiplot'", ns("plot2d_package")),
                         plotOutput(ns("biplot"), width = "600px", height = "600px")
        ),
        fluidRow(
            column(4, biplot_select),
            column(4, uiOutput(ns("plot2d_x_ui"))),
            column(4, uiOutput(ns("plot2d_y_ui")))
        )
    )
}

#' a wrapper module for plot in 2D
#'
#' @description
#' This is the server part of the module.
#' @import plotly ggbiplot
#' @export
pivot_Plot2d <- function(input, output, session, type = NULL, obj = NULL, proj = NULL, minfo = NULL) {

    # 2d_pca select principal component UI
    output$plot2d_x_ui <- renderUI({
        if(is.null(proj)) return()
        if(type == "tsne") return()
        if(type == "pca") {
            dname = "PC"
        } else if(type == "plda") {
            dname = "V"
        }
        PCs <- colnames(proj)
        PC_options <- as.list(PCs)
        names(PC_options) <- PCs
        selectInput(session$ns("plot2d_x"), label = paste(dname, "on X axis"), choices = PC_options, selected = PC_options[[1]])
    })

    output$plot2d_y_ui <- renderUI({
        if(is.null(proj)) return()
        if(type == "tsne") return()
        if(type == "pca") {
            dname = "PC"
        } else if(type == "plda") {
            dname = "V"
        }
        if(ncol(proj) < 2) return()
        PCs <- colnames(proj)
        PC_options <- as.list(PCs)
        names(PC_options) <- PCs
        selectInput(session$ns("plot2d_y"), label = paste(dname, "on X axis"), choices = PC_options, selected = PC_options[[2]])
    })

    # 2d Plotly

    plot2d <- reactive({
        if(is.null(proj)) return()
        if(ncol(proj) < 2) return()
        if(type %in% c("pca", "plda")) {
            if(is.null(input$plot2d_y)) return()
            d1 <- input$plot2d_x
            d2 <- input$plot2d_y
        } else if(type == "tsne") {
            d1 <- "X1"
            d2 <- "X2"
        }
        if(!is.null(minfo$meta)){
            group = minfo$meta[,1]
            pal = unique(minfo$meta_color[,1])
            pal <- setNames(pal, unique(group))
        } else {
            group = NULL
            pal = NULL
        }
        plotly::plot_ly(proj, x = as.formula(paste0("~", d1)) , y = as.formula(paste0("~", d2)), text = rownames(proj),
                        type = "scatter", mode = "markers", color = minfo$meta[,1], colors = pal, marker = list(size = 10))
    })

    output$plotly2d <- plotly::renderPlotly({
        req(plot2d())
        plot2d()
    })

    # 2d ggbiplot
    output$biplot <- renderPlot({
        if(is.null(obj)) return()
        if(!is.null(minfo$meta)){
            groups <- factor(minfo$meta[,1], levels = unique(minfo$meta[,1]))
            ggbiplot::ggbiplot(obj, groups = groups, ellipse = TRUE, circle = TRUE, varname.size = 3, labels.size=3) +
                geom_point(aes(colour=groups), size = 3) +
                scale_colour_manual(values = unique(minfo$meta_color[,1])) +
                theme(legend.direction = 'horizontal', legend.position = 'top')
        } else {
            ggbiplot::ggbiplot(obj, ellipse = TRUE, circle = TRUE, varname.size = 3, labels.size=3) +
                geom_point(size = 3)
        }
    })

    return(plot2d())
}



#' a wrapper module for plot in 3D
#'
#' @description
#' This is the UI part of the module.
#' @import plotly
#' @export
pivot_Plot3d_UI <- function(id, type) {
    ns<- NS(id)
    tagList(
        conditionalPanel(sprintf("input['%s'] == 'plotly'", ns("plot3d_package")),
                         plotly::plotlyOutput(ns("plotly3d"), height = "600px")
        ),
        conditionalPanel(sprintf("input['%s'] == 'threejs'", ns("plot3d_package")),
                         threejs::scatterplotThreeOutput(ns("threejs3d"), height = "600px")
        ),
        fluidRow(
            column(6, selectInput(ns("plot3d_package"), "Plotting package", choices = list("plotly" = "plotly", "threejs" = "threejs"), selected = "plotly"))
        )
    )
}

#' a wrapper module for plot in 3D
#'
#' @description
#' This is the server part of the module.
#' @import plotly
#' @export
pivot_Plot3d <- function(input, output, session, type = NULL, obj = NULL, proj = NULL, minfo = NULL) {
    # 3d_pca threejs
    output$threejs3d <- threejs::renderScatterplotThree({
        if(is.null(proj)) return()
        if(ncol(proj) < 3) return()
        if(!is.null(minfo$meta)){
            threejs::scatterplot3js(proj[,1:3], color = minfo$meta_color[,1], labels = rownames(proj), label.margin="150px 150px 150px 150px", size = 1.2, renderer = "canvas")
        } else{
            threejs::scatterplot3js(proj[,1:3], color = "grey", labels = rownames(proj), label.margin="150px 150px 150px 150px", size = 1.2, renderer = "canvas")
        }
    })

    plot3d <- reactive({
        req(proj)
        if(ncol(proj) < 3) return()
        assign("pj1",proj, env = .GlobalEnv)

        if(type == "pca") {
            dname = "PC"
        } else if(type == "tsne"){
            dname = "X"
        } else if(type == "plda") {
            dname = "V"
        }

        if(!is.null(minfo$meta)){
            group = minfo$meta[,1]
            pal = unique(minfo$meta_color[,1])
            pal <- setNames(pal, unique(group))
        } else {
            group = NULL
            pal = NULL
        }

        plot_ly(proj[,1:3], x = as.formula(paste0("~", dname, "1")), y = as.formula(paste0("~", dname, "2")), z = as.formula(paste0("~", dname, "3")), text = rownames(proj), color = minfo$meta[,1], colors = pal) %>%
            add_markers() %>%
            layout(scene = list(xaxis = list(title = paste0(dname, "1")),
                                yaxis = list(title =  paste0(dname, "2")),
                                zaxis = list(title =  paste0(dname, "3"))))
    })

    # 3d_pca plotly
    output$plotly3d <- threejs::renderScatterplotThree({
        req(plot3d())
        plot3d()
    })

    return(plot3d())
}


#' Penalized LDA wrapper
#'
#' @import penalizedLDA dplyr tibble
#' @export
make_plda <- function(df, group, lambda, K) {
    result <- list()

    plda1 <- penalizedLDA::PenalizedLDA(t(df), y = as.numeric(as.factor(group)), lambda = lambda, K = K)
    proj1 <- plda1$xproj
    proj1 <- as.data.frame(proj1)
    rownames(proj1) <- colnames(df)

    discrim1<-as.data.frame(plda1$discrim)
    rownames(discrim1) <- rownames(df)
    discrim1 = subset(discrim1, rowSums(discrim1)!=0, drop = F)
    discrim1<-discrim1 %>% tibble::rownames_to_column("feature") %>% dplyr::arrange(-abs(V1))

    result$plda <- plda1
    result$proj <- proj1
    result$discrim <- discrim1
    return(result)
}


