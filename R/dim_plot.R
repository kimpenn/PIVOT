
#' a wrapper module for plot in 1D
#'
#' @description
#' This is the UI part of the module.
#' @import plotly
#' @export
pivot_Plot1d_UI <- function(id, type) {
    ns<- NS(id)
    tagList(
        plotly::plotlyOutput(ns("plotly1d")),
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
    if(is.null(proj)) return()
    output$plot1d_pc_ui <- renderUI({
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
        req(input$plot1d_step)
        if(type %in% c("pca", "plda")) {
            req(input$plot1d_pc)
            d1 <- input$plot1d_pc
        } else if(type == "tsne") {
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
            error_I <- 0
            # density require each group has two points, so add error handler here
            tryCatch({
                dens<-tapply(df[,d1], INDEX = group, function(x){density(x,adjust = input$plot1d_step)})
                df <- data.frame(
                    x = unlist(lapply(dens, "[[", "x")),
                    y = unlist(lapply(dens, "[[", "y")),
                    Group = rep(names(dens[!sapply(dens, is.null)]), each = length(dens[[1]]$x))
                )
            },
            error = function(e){
                error_I <<- 1
            }
            )
            if(error_I) {
                return()
            }

            plot1d <- plotly::plot_ly(df, x = ~x, y = ~y, color = ~Group, type  = "scatter", mode = "lines", fill = "tozeroy", colors = pal)
        } else {
            plot1d <- plotly::plot_ly(proj, x = as.formula(paste0("~", d1)), type = "histogram",
                                      xbins=list(start = min(proj), end = max(proj), size = (max(proj) - min(proj))*input$plot1d_step/2), autobinx=F,
                                      color = minfo$meta[,1], colors = pal)
        }
        #assign("p1d", plot1d, env = .GlobalEnv)
        plot1d
    })
    output$plotly1d <- plotly::renderPlotly({
        req(plot1d())
        plot1d()
    })

    return(isolate(plot1d()))
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
        options <- list("plotly" = "plotly", "ggbiplot" = "ggbiplot")
    } else {
        options <- list("plotly" = "plotly")
    }
    biplot_select <- selectInput(ns("plot2d_package"), "Plotting package", choices = options, selected = "plotly")
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
        ),
        uiOutput(ns("selected_sample_ui"))
    )
}

#' a wrapper module for plot in 2D
#'
#' @description
#' This is the server part of the module.
#' @import plotly ggplot2
#' @export
pivot_Plot2d <- function(input, output, session, type = NULL, obj = NULL, proj = NULL, minfo = NULL, source = NULL, event = NULL, selected = NULL) {
    if(is.null(proj)) return()
    # 2d_pca select principal component UI
    output$plot2d_x_ui <- renderUI({
        if(type %in% c("tsne", "mds", "nds")) return()
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
        if(type %in% c("tsne", "mds", "nds")) return()
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
        if(ncol(proj) < 2) return()
        if(type == "pca") {
            dname = "PC"
        } else {
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

        x = as.formula(paste0("~", dname, "1"))
        y = as.formula(paste0("~", dname, "2"))

        if(!is.null(input$plot2d_x)){
            x = as.formula(paste0("~", input$plot2d_x))
            y = as.formula(paste0("~", input$plot2d_y))
        }

        plotly::plot_ly(proj, x = x, y = y, text = row.names(proj), source = source, key = row.names(proj),
                        type = "scatter", mode = "markers", color = minfo$meta[,1], colors = pal, marker = list(size = 10)) %>%
            plotly::layout(dragmode = "select")

        # Use ggplotly instead because of wrong key binding
        # x = paste0(dname, "1")
        # y = paste0(dname, "2")
        #
        # if(!is.null(input$plot2d_x)){
        #     x = input$plot2d_x
        #     y = input$plot2d_y
        # }
        #
        # proj<-proj %>% tibble::rownames_to_column()
        # plt1 <- ggplot2::ggplot(proj, ggplot2::aes_string(x=x, y=y, text = "rowname", key = "rowname")) + ggplot2::theme_minimal()
        #
        # if(is.null(group)) {
        #     plt1 <- plt1 + ggplot2::geom_point(shape=19, alpha=0.8, size = 2, color = 'steelblue')
        # } else {
        #     plt1 <- plt1 +
        #         ggplot2::geom_point(shape=19, alpha=0.8, size = 2, ggplot2::aes(color = group)) +
        #         ggplot2::scale_color_manual(values = pal)
        # }
        # #assign("plt1", plt1, env =.GlobalEnv)
        # tryCatch({
        #     plotly::ggplotly(source = source) %>% plotly::layout(dragmode = "select")
        # }, error = function(e) {
        #     print(e)
        #     return()
        # })

    })

    output$plotly2d <- plotly::renderPlotly({
        req(plot2d())
        plot2d()
    })


    output$selected_sample_ui <- renderUI({
        if(is.null(event)) return()
        selected_samples <- event()$key
        if(length(selected_samples)) {
            sp_text<-paste(selected_samples, collapse = ", ")
            return(tagList(
                fluidRow(
                    column(4, tags$b("Selected samples:"), tags$p(sp_text)),
                    column(4, textInput(session$ns("graph_group"), "New group name:", placeholder = paste(type, "group 1"))),
                    column(4, tags$br(), actionButton(session$ns("add_group"), "Set Group", class = "btn btn-info"))
                )
            ))
        }
    })

    observeEvent(input$add_group, {
        if(is.null(event)) return()
        sps <- event()$key
        if(nchar(input$graph_group) < 1){
            session$sendCustomMessage(type = "showalert", "Please specify a group name.")
            return()
        }
        if(length(sps) && !is.null(selected)) {
            names(sps) <- rep(input$graph_group, length(sps))
            selected$group <- sps
        }
    })

    # 2d ggbiplot
    output$biplot <- renderPlot({
        if(is.null(obj)) return()
        if(!"ggbiplot" %in% rownames(installed.packages())) {
            session$sendCustomMessage(type = "showalert", "Please install ggbiplot manually from https://github.com/vqv/ggbiplot")
            return()
        }
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
    if(is.null(selected)) {
        return(list(plot = isolate(plot2d())))
    } else {
        return(list(plot = isolate(plot2d()), group = selected$group))
    }
}



#' a wrapper module for plot in 3D
#'
#' @description
#' This is the UI part of the module.
#' @import plotly
#' @export
pivot_Plot3d_UI <- function(id, type, height = "600px") {
    ns<- NS(id)
    tagList(
        conditionalPanel(sprintf("input['%s'] == 'plotly'", ns("plot3d_package")),
                         plotly::plotlyOutput(ns("plotly3d"), height = height)
        ),
        conditionalPanel(sprintf("input['%s'] == 'threejs'", ns("plot3d_package")),
                         threejs::scatterplotThreeOutput(ns("threejs3d"), height = height)
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
    if(is.null(proj)) return()
    # 3d_pca threejs
    output$threejs3d <- threejs::renderScatterplotThree({
        if(ncol(proj) < 3) return()
        if(!is.null(minfo$meta)){
            threejs::scatterplot3js(proj[,1:3], color = minfo$meta_color[,1], labels = rownames(proj), label.margin="150px 150px 150px 150px", size = 1.2, renderer = "canvas")
        } else{
            threejs::scatterplot3js(proj[,1:3], color = "grey", labels = rownames(proj), label.margin="150px 150px 150px 150px", size = 1.2, renderer = "canvas")
        }
    })

    plot3d <- reactive({
        if(ncol(proj) < 3) return()
        #assign("pj1",proj, env = .GlobalEnv)

        if(type == "pca") {
            dname = "PC"
        } else {
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

        plotly::plot_ly(proj[,1:3], x = as.formula(paste0("~", dname, "1")), y = as.formula(paste0("~", dname, "2")), z = as.formula(paste0("~", dname, "3")), text = rownames(proj), color = minfo$meta[,1], colors = pal) %>%
            plotly::add_markers() %>%
            plotly::layout(scene = list(xaxis = list(title = paste0(dname, "1")),
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


