
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
            column(4, sliderInput(ns("plot1d_step"),.1, 2, value = .1, step = .1, label = "Band/Bin Width Adjustment")),
            uiOutput(ns("plot1d_pc_ui"))
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
    if(type == "pca") {
        dname = "PC"
    } else if(type != "dfm") {
        dname = "V"
    } else if(type == "dfm") {
        dname = "DC"
    }
    output$plot1d_pc_ui <- renderUI({
        PCs <- colnames(proj)
        PC_options <- as.list(PCs)
        names(PC_options) <- PCs
        column(4, selectInput(session$ns("plot1d_pc"), label = "X axis", choices = PC_options, selected = PC_options[[1]]))
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
        options <- list("plotly" = "plotly", "ggplot2"="ggplot2", "ggbiplot" = "ggbiplot")
    } else {
        options <- list("plotly" = "plotly", "ggplot2"="ggplot2")
    }
    biplot_select <- selectInput(ns("plot2d_package"), "Plotting package", choices = options, selected = "plotly")
    tagList(
        conditionalPanel(sprintf("input['%s'] == 'plotly'", ns("plot2d_package")),
                         plotly::plotlyOutput(ns("plotly2d"))
        ),
        conditionalPanel(sprintf("input['%s'] != 'plotly'", ns("plot2d_package")),
                         uiOutput(ns("ggplot2d_ui"))
        ),
        fluidRow(
            column(3, biplot_select),
            column(3, numericInput(ns("psize_2d"), "Marker Size", value = 1, min =0.1, step =0.1)),
            uiOutput(ns("plot2d_x_ui")),
            uiOutput(ns("plot2d_y_ui"))
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

    if(type == "pca") {
        dname = "PC"
    } else if(type != "dfm") {
        dname = "V"
    } else if(type == "dfm") {
        dname = "DC"
    }

    if(!is.null(minfo$meta)){
        group = minfo$meta[,1]
        pal = unique(minfo$meta_color[,1])
        pal <- setNames(pal, unique(group))
    } else {
        group = NULL
        pal = NULL
    }

    # 2d_pca select principal component UI
    output$plot2d_x_ui <- renderUI({
        if(type %in% c("tsne", "mds", "nds")) return()
        PCs <- colnames(proj)
        PC_options <- as.list(PCs)
        names(PC_options) <- PCs
        column(3, selectInput(session$ns("plot2d_x"), label = "X axis", choices = PC_options, selected = PC_options[[1]]))
    })

    output$plot2d_y_ui <- renderUI({
        if(type %in% c("tsne", "mds", "nds")) return()
        if(ncol(proj) < 2) return()
        PCs <- colnames(proj)
        PC_options <- as.list(PCs)
        names(PC_options) <- PCs
        column(3, selectInput(session$ns("plot2d_y"), label = "X axis", choices = PC_options, selected = PC_options[[2]]))
    })

    # 2d plot object
    plot2d <- reactive({
        if(ncol(proj) < 2) return()
        req(input$plot2d_package, input$psize_2d)
        if(input$plot2d_package == "plotly") {
            x = as.formula(paste0("~", dname, "1"))
            y = as.formula(paste0("~", dname, "2"))

            if(!is.null(input$plot2d_x)){
                x = as.formula(paste0("~", input$plot2d_x))
                y = as.formula(paste0("~", input$plot2d_y))
            }
            pp<-plotly::plot_ly(proj, x = x, y = y, text = row.names(proj), source = source, key = row.names(proj),
                            type = "scatter", mode = "markers", color = minfo$meta[,1], colors = pal, marker = list(size = 10*input$psize_2d)) %>%
                plotly::layout(dragmode = "select")
        } else if(input$plot2d_package == "ggplot2") {
            x = paste0(dname, "1")
            y = paste0(dname, "2")

            if(!is.null(input$plot2d_x)){
                x = paste0(input$plot2d_x)
                y = paste0(input$plot2d_y)
            }
            pp<-ggplot2::ggplot(proj, ggplot2::aes_string(x=x, y=y)) +
                ggplot2::theme_bw()
            if(is.null(group)){
                pp<-pp +
                    ggplot2::geom_point(alpha=0.8, size = 3*input$psize_2d, color = 'steelblue')
            } else {
                pp <- pp +
                    ggplot2::geom_point(alpha=0.8, size = 3*input$psize_2d, ggplot2::aes(color = group)) +
                    ggplot2::scale_color_manual(values = pal)
            }
        } else if(input$plot2d_package == "ggbiplot") {
            if(!"ggbiplot" %in% rownames(installed.packages())) {
                session$sendCustomMessage(type = "showalert", "Please install ggbiplot manually from https://github.com/vqv/ggbiplot")
                return()
            }
            if(!is.null(group)){
                pp <-ggbiplot::ggbiplot(obj, groups = group, ellipse = TRUE, circle = TRUE, varname.size = 3, labels.size=3) +
                    geom_point(aes(colour=group), size = 3*input$psize_2d) +
                    scale_colour_manual(values = unique(minfo$meta_color[,1])) +
                    theme(legend.direction = 'horizontal', legend.position = 'top')
            } else {
                pp <-ggbiplot::ggbiplot(obj, ellipse = TRUE, circle = TRUE, varname.size = 3, labels.size=3) +
                    geom_point(size = 3*input$psize_2d)
            }
        }
        return(pp)
    })

    output$plotly2d <- plotly::renderPlotly({
        req(plot2d())
        plot2d()
    })

    output$selected_sample_ui <- renderUI({
        req(event, input$plot2d_package == "plotly")
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
            showNotification("Please specify a group name.", type="error", duration=10)
            return()
        }
        if(length(sps) && !is.null(selected)) {
            names(sps) <- rep(input$graph_group, length(sps))
            selected$group <- sps
        }
    })

    # 2d ggbiplot
    output$ggplot2d_ui <- renderUI({
        if(input$plot2d_package == "ggplot2") {
            plotOutput(session$ns("ggpp"), width = "100%", height = "450px")
        } else if(input$plot2d_package == "ggbiplot") {
            plotOutput(session$ns("biplot"), width = "100%", height = "450px")
        }
    })

    output$ggpp <- renderPlot({
        req(plot2d())
        plot2d()
    })

    output$biplot <- renderPlot({
        req(plot2d())
        plot2d()
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
            column(4, selectInput(ns("plot3d_package"), "Plotting package", choices = list("plotly" = "plotly", "threejs" = "threejs"), selected = "plotly")),
            column(4, numericInput(ns("psize_3d"), "Marker Size", value = 1, min =0.1, step =0.1))
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
            threejs::scatterplot3js(as.matrix(proj[,1:3]), color = minfo$meta_color[,1], labels = rownames(proj), label.margin="150px 150px 150px 150px", size = 0.5*input$psize_3d)
        } else{
            threejs::scatterplot3js(as.matrix(proj[,1:3]), color = rep("grey",nrow(proj)), labels = rownames(proj), label.margin="150px 150px 150px 150px", size = 0.5*input$psize_3d)
        }
    })

    plot3d <- reactive({
        if(ncol(proj) < 3) return()
        #assign("pj1",proj, env = .GlobalEnv)

        if(type == "pca") {
            dname = "PC"
        } else if(type != "dfm") {
            dname = "V"
        } else if(type == "dfm") {
            dname = "DC"
        }

        if(!is.null(minfo$meta)){
            group = minfo$meta[,1]
            pal = unique(minfo$meta_color[,1])
            pal <- setNames(pal, unique(group))
        } else {
            group = NULL
            pal = NULL
        }

        plotly::plot_ly(proj[,1:3],
                        x = as.formula(paste0("~", dname, "1")), y = as.formula(paste0("~", dname, "2")), z = as.formula(paste0("~", dname, "3")),
                        marker = list(size = 10*input$psize_3d),
                        text = rownames(proj), color = minfo$meta[,1], colors = pal) %>%
            plotly::layout(scene = list(xaxis = list(title = paste0(dname, "1")),
                                yaxis = list(title =  paste0(dname, "2")),
                                zaxis = list(title =  paste0(dname, "3"))))
    })

    # 3d_pca plotly
    output$plotly3d <- plotly::renderPlotly({
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


