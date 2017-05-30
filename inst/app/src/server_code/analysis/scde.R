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


# SCDE

output$scde_ui <- renderUI({
    if(is.null(r_data$meta) || ncol(r_data$meta) < 2){
        return(
            list(
                tags$li("This module requires design information input.")
            )
        )
    }

    list(
        enhanced_box(
            title = "SCDE Single-Cell Differential Expression Analysis",
            id = "scde",
            status = "primary",
            width = 12,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            tags$div(tags$b("STEP 1: Perform Error Modeling"), class = "param_setting_title"),
            fluidRow(
                pivot_deGroupBy_UI("scde", r_data$meta, width = 12, reduced = T, model = c("condition", "condition_batch"))
            ),
            fluidRow(
                column(6,
                       shinyBS::tipify(
                           checkboxInput("ifm_with_group", label = "Fit error model independently for each condition", value = F),
                           title = "If provided, the cross-fits and the expected expression magnitudes will be determined separately within each group",
                           placement = "right", options = list(container = "body")
                       )
                ),
                column(6,
                       uiOutput("run_scde_model_ui")
                )
            ),
            hr(),
            DT::dataTableOutput("scde_ifm"),
            uiOutput("scde_valid_cell"),
            div(tags$b("Probability of Transcript-detection Failures"), class = "table-cap"),
            plotOutput("scde_dropout_plt")
        ),

        enhanced_box(
            title = NULL,
            status = "primary",
            width = 12,
            solidHeader = F,
            tags$div(tags$b("STEP 2: Testing for Differential Expression"), class = "param_setting_title"),
            uiOutput("scde_ediff_ui")
        ),

        uiOutput("scde_gene_box"),

        uiOutput("scde_dist_box"),

        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Kharchenko, P. V., Silberstein, L., & Scadden, D. T. (2014). Bayesian approach to single-cell differential expression analysis. Nature methods, 11(7), 740-742.", class = "citation"),
                tags$li("Peter Kharchenko and Jean Fan (2015). scde: Single Cell Differential Expression. R package version 1.99.0. http://pklab.med.harvard.edu/scde/index.html", class = "citation"),
                tags$li("SCDE Website:", a("http://hms-dbmi.github.io/scde/diffexp.html", src = "http://hms-dbmi.github.io/scde/diffexp.html")),
                tags$li("SCDE is provided under the license located at https://github.com/hms-dbmi/scde/blob/master/license.txt. It is created by Jean Fan and Peter Kharchenko.
Harvard Medical School, Department of Biomedical Informatics (Regents). ")
            )
        )
    )
})

scdeModel <- callModule(pivot_deGroupBy, "scde", meta = r_data$meta)

output$run_scde_model_ui <- renderUI({
    req(scdeModel())
    actionButton("run_scde_model", "Perform Error Modeling", class = "btn btn-info")
})


observeEvent(input$run_scde_model, {
    req(scdeModel())

    if(!is.null(r_data$scde_ifm)) { # Reset results
        r_data$scde_ifm <- NULL
        r_data$scde_ediff <- NULL
        r_data$scde_results <- NULL
    }

    designVar <- all.vars(scdeModel()$model$full)
    cond <- designVar[1]
    if(scdeModel()$design == "condition_batch") {
        batch <- designVar[2]
    } else {
        batch = NULL
    }

    scde_tmp <- list()
    error_I <- 0
    withProgress(message = 'Processing...', value = 0.5, {
        # Future plan: Implement this to allow user to check individual model fits.

        tryCatch({
            if(!is.null(input$ifm_with_group) && input$ifm_with_group){
                scde_tmp$scde_ifm <- scde::scde.error.models(counts = r_data$raw, groups = r_data$meta[, cond], n.cores = 1, threshold.segmentation = TRUE, save.crossfit.plots = F, save.model.plots = F, verbose = 1)
            } else {
                scde_tmp$scde_ifm <- scde::scde.error.models(counts = r_data$raw, n.cores = 1, threshold.segmentation = TRUE, save.crossfit.plots = F, save.model.plots = F, verbose = 1)
            }
        },
            error = function(e) {
                error_I <<- 1
            }
        )

        if(error_I || nrow(scde_tmp$scde_ifm) == 0){
            session$sendCustomMessage(type = "showalert", "Error modeling failed.")
            return()
        }

        r_data$scde_ifm <- scde_tmp$scde_ifm[which(scde_tmp$scde_ifm$corr.a > 0), ]

        r_data$scde_params <- list(design = scdeModel()$design, condition = cond, batch = batch)

        r_data$scde_invalid <- (scde_tmp$scde_ifm)[which(scde_tmp$scde_ifm$corr.a <= 0),]
        r_data$scde_prior <- scde::scde.expression.prior(models = r_data$scde_ifm, counts = r_data$raw, length.out = 400, show.plot = F)
    })
})

output$scde_ifm <- DT::renderDataTable({
    req(r_data$scde_ifm)
    DT::datatable(r_data$scde_ifm,options = list(scrollX = TRUE, scrollY = "250px", searching=F, order = list(list(5, 'asc')) , orderClasses = T))
})

output$scde_valid_cell <- renderUI({
    req(r_data$scde_invalid)

    if(nrow(r_data$scde_invalid) == 0)
        valid_msg1 <- "Fits for all your cells are valid."
    else
        valid_msg1 <- paste("Fits for", paste(rownames(r_data$scde_invalid), collapse = ', '), "are abnormal, and these samples will be removed from SCDE analysis. (Please note that these samples are also removed from the above table)")
    list(
        hr(),
        tags$li(valid_msg1)
    )
})

output$scde_ediff_ui <- renderUI({

    req(r_data$scde_ifm)

    groups = unique(as.character(r_data$meta[,r_data$scde_params$condition]))
    names(groups) = groups

    if(!is.null(r_data$scde_batch)) { # Let the user choose which table to display if batch correction is applied
        scde_batch_ediff_choice_ui <- selectInput("scde_batch_ediff_choice", NULL, choices = list("Show Unadjusted Results" = "results", "Show Batch Adjusted Restuls" = "batch.adjusted", "Show Batch Effect" = "batch.effect"))
    } else {
        scde_batch_ediff_choice_ui <- NULL
    }

    list(
        fluidRow(
            column(2, tags$br(),tags$b("Pairwise comparison:")),
            column(3, selectInput("scde_group1", "Group 1", choices = as.list(groups), selected = groups[[1]])),
            column(1, tags$b("vs")),
            column(3, selectInput("scde_group2", "Group 2", choices = as.list(groups), selected = groups[[2]])),
            column(2, tags$br(), actionButton("run_scde_ediff", "Run DE Test", class = "btn btn-info"))
        ),
        hr(),
        fluidRow(
            column(4, numericInput_1("scde_alpha", "Adjusted-P cutoff", value = 0.05, min = 0, max = 0.5, step = 0.001)),
            column(4, checkboxInput("scde_cuttbl", "Only show significant genes", value = T)),
            column(4, scde_batch_ediff_choice_ui)
        ),

        uiOutput("ediff_cap_and_help"),
        DT::dataTableOutput("scde_ediff_tbl"),
        downloadButton("download_scde_ediff","Download", class = "btn-success btn_rightAlign"),
        uiOutput("scde_sig_genes")
    )
})

output$ediff_cap_and_help <- renderUI({
    if(is.null(r_data$scde_ediff)) return()
    cur_group<-unique(r_data$scde_group)

    tbl_caption <- paste("Differential Expression Test Results of", cur_group[1], "vs", cur_group[2])
    if(r_data$scde_params$design == "condition_batch") {
        req(input$scde_batch_ediff_choice)
        tbl_caption <- paste(tbl_caption, ifelse(input$scde_batch_ediff_choice == 'results', "(unadjusted)", ifelse(input$scde_batch_ediff_choice == 'batch.adjusted', "(batch adjusted)", "(batch effects)")))
    }

    table_info <- list(
        tags$li("lb, mle, ub: lower bound, maximum likelihood estimate, and upper bound of the 95;"),
        tags$li("ce conservative estimate of expression-fold change (equals to the min(abs(c(lb, ub))), or 0 if the CI crosses the 0;"),
        tags$li("Z uncorrected Z-score of expression difference; abs_Z, absolute Z-score;  "),
        tags$li("cZ expression difference Z-score corrected for multiple hypothesis testing using Holm procedure If batch correction has been performed (batch has been supplied)"),
        tags$li("result: default results dataframe, batch.adjusted: batch-corrected results, batch.effect: the differences explained by batch effects alone."),
        tags$li("The adjusted p-values are rescaled in order to control for false discovery rate (FDR).")
    )

    list(
        div(tags$b(tbl_caption), class = "table-cap"),
        a(id = "toggle_ediff_explain", icon("question-circle")),
        shinyBS::bsModal(id = "ediff_explain", title = "Table info", trigger = "toggle_ediff_explain", size = "large", table_info)
    )
})

output$scde_sig_genes <- renderUI({
    if(is.null(r_data$scde_ediff)) return()

    if(!is.null(r_data$scde_batch)) { # If batch correction applied, it returns a list containing 3 dataframes rather than a single dataframe
        req(input$scde_batch_ediff_choice)
        tbl <- r_data$scde_ediff[[input$scde_batch_ediff_choice]]
    } else {
        tbl <-r_data$scde_ediff
    }
    tbl <- tbl %>% tibble::rownames_to_column("feature") %>% dplyr::mutate(p.values = 2*pnorm(abs(tbl$Z),lower.tail=F)) %>% dplyr::mutate(p.values.adj = 2*pnorm(abs(tbl$cZ),lower.tail=F))
    if(input$scde_cuttbl) {
        tbl <- subset(tbl, p.values.adj <= input$scde_alpha)
    }
    rownames(tbl) <- tbl$feature
    tbl <- tbl %>% dplyr::select(-feature)
    r_data$scde_results <- tbl
    tags$li(paste0("Total number of significant genes: ", sum(tbl$p.values.adj < input$scde_alpha, na.rm = T), "."))
})


output$download_scde_ediff <- downloadHandler(
    filename = function() {
        "scde_results.csv"
    },
    content = function(file) {
        req(r_data$scde_results)
        write.csv(r_data$scde_results, file)
    }
)


observeEvent(input$run_scde_ediff, {
    req(r_data$scde_ifm)
    withProgress(message = 'Processing...', value = 0.8, {
        #  Get valid cell list
        valid.cells <-rownames(r_data$scde_ifm)

        # Specify group
        if(input$scde_group1 == input$scde_group2) {
            session$sendCustomMessage(type = "showalert", "Groups must be different.")
            return()
        } else {
            selected_group <- c(input$scde_group1,input$scde_group2)
        }

        groups <- r_data$meta[, r_data$scde_params$condition]
        names(groups) <- r_data$meta[,1]
        groups <- groups[valid.cells]
        r_data$scde_group <- factor(groups[groups %in% selected_group], levels=selected_group)
        r_data$scde_sample <- names(r_data$scde_group)

        # specify batch, if necessary
        if(r_data$scde_params$design == "condition_batch") {
            batches <- r_data$meta[, r_data$scde_params$batch]
            names(batches) <- r_data$meta[,1]
            batches <- batches[r_data$scde_sample]
            r_data$scde_batch <- factor(batches, levels=unique(batches))
        } else {
            r_data$scde_batch <- NULL
        }

        tryCatch({
            r_data$scde_ediff <- scde::scde.expression.difference(models = r_data$scde_ifm[r_data$scde_sample,], counts = r_data$raw, prior = r_data$scde_prior, groups = r_data$scde_group, batch = r_data$scde_batch, n.randomizations  =  100, n.cores  =  1, verbose  =  1)},
            error = function(e) {
                session$sendCustomMessage(type = "showalert", "Differential expression test failed.")
                return()
            }
        )

    })
})

output$scde_ediff_tbl <- DT::renderDataTable({
    req(r_data$scde_results)
    DT::datatable(r_data$scde_results, selection = 'single', options = list(scrollX = TRUE, scrollY = "210px", searching=T, order = list(list(8, 'asc')), orderClasses = T))
})

output$scde_gene_box <- renderUI({
    if(is.null(r_data$scde_results)) return()
    boxtitle <- paste("Plot of Selected Gene:", r_data$scde_gene)
    gene_plt_explain <- list(tags$li("The top and the bottom plots show expression posteriors derived from individual cells (colored lines) and joint posteriors (black lines)."),
                             tags$li("The middle plot shows posterior of the expression fold difference between the two cell groups, highlighting the 95% credible interval by the red shading."),
                             tags$li("In batch-adjusted plots, the grey lines are used to show posterior distributions based on the batch composition alone."),
                             tags$li("The thin black line shows log2 expression ratio posterior before correction. The red line shows the result after correction."))

    enhanced_box(
        title = NULL,
        status = "primary",
        width = 12,
        solidHeader = F,
        tags$div(tags$b(boxtitle), class = "param_setting_title"),
        fluidRow(
            column(12,
                   a(id = "scde_gene_plt_info", icon("question-circle")),
                   shinyBS::bsModal(id = "scde_gene_plt_help", title = "Plot info", trigger = "scde_gene_plt_info", size = "large",
                       gene_plt_explain
                   ),
                   plotOutput("scde_gene_plt", height ='600px')
            )
        )
    )
})

output$scde_gene_plt <- renderPlot({
    req(r_data$scde_results)
    tbl <- r_data$scde_results
    s = input$scde_ediff_tbl_row_last_clicked
    if (length(s)) {
        r_data$scde_gene <- rownames(tbl[s, , drop = FALSE])
    } else {
        return()
    }

    if(!length(r_data$scde_gene)) return()

    batch_show <- r_data$scde_batch # If no batch_info, this will be NULL. if batch info, then if the user choose batch.effect or batch.adjusted, show batch, otherwise set to NULL

    if(!is.null(r_data$scde_batch) && !is.null(input$scde_batch_ediff_choice) && (input$scde_batch_ediff_choice == 'results')) {
        batch_show <- NULL
    }

    scde::scde.test.gene.expression.difference(r_data$scde_gene, models = r_data$scde_ifm[r_data$scde_sample,], groups = r_data$scde_group, batch = batch_show, counts = r_data$raw, prior = r_data$scde_prior)
})


output$scde_dropout_plt <- renderPlot({
    if(is.null(r_data$scde_ifm)) return()
    # get failure probabilities on the expresison range
    o.fail.curves <- scde::scde.failure.probability(r_data$scde_ifm,magnitudes=log((10^r_data$scde_prior$x)-1))
    par(mfrow=c(1,1),mar = c(3.5,3.5,0.5,0.5), mgp = c(2.0,0.65,0), cex = 1);

    plot(c(),c(),xlim=range(r_data$scde_prior$x),ylim=c(0,1),xlab="expression magnitude (log10)",ylab="drop-out probability")

    sp <- colnames(o.fail.curves)
    nSamples <- length(sp)
    colorMin = 1
    colorMax = nSamples
    # colorRamp produces custom palettes, using values between 0-1
    colorFunction = colorRamp(c("black", "blue", "red"))
    zMin = colorMin
    zMax = colorMax
    zDiff = zMax - zMin
    z = zMin:zMax
    zScaled = (z - zMin) / zDiff
    # Apply colorRamp
    zMatrix = colorFunction(zScaled)
    # switch to hexadecimal representation and sort to be consistent with colorFactor.
    zColors = sort(rgb(zMatrix[,1], zMatrix[,2], zMatrix[,3], maxColorValue=255))

    # create an empty character vector that will be used to store the line colors. This is necessary to color the legend.
    colorsUsed = character(nSamples)

    for(i in 1:nSamples) {
        # using the sample name means the column order in colorFactorSorted file doesn't have to match the order in the counts file
        sample = sp[i]
        color = zColors[i]
        colorsUsed[i] = color
        lines(x=r_data$scde_prior$x,y=o.fail.curves[,sample], type="l", lty=i, lwd=1.5, cex=1, col=color)
    }
    legend("topright", legend=sp, col=colorsUsed, lty=1:nSamples, lwd=1.8, cex=0.8)
})



output$scde_dist_box <- renderUI({
    req(r_data$scde_ifm)
    enhanced_box(
        title = NULL,
        width = 12,
        status = "info",
        solidHeader = F,
        tags$div(tags$b("OPTIONAL: SCDE Adjusted Distance Measures"), class = "param_setting_title"),
        tags$li("If you want to use SCDE adjusted distance for other analysis such as clustering, please run this module."),
        fluidRow(
            column(4, selectInput("scde_dist_method", "Choose method:", choices = list("Direct drop-out" = "ddo", "Reciprocal weighting" = "rw", "Mode-relative weighting" = "mrw"), selected = "rw")),
            uiOutput("scde_dist_ddo_ui")
        ),
        tags$b("Method information:"),
        uiOutput("scde_dist_ddo_text"),
        uiOutput("scde_dist_rw_text"),
        uiOutput("scde_dist_mrw_text"),
        actionButton("run_scde_distance", "Compute Distance", class = "btn-info"),
        hr(),
        uiOutput("ddo_added_ui"),
        uiOutput("rw_added_ui"),
        uiOutput("mrw_added_ui")
    )
})

output$scde_dist_ddo_ui <- renderUI({
    if(input$scde_dist_method != 'ddo') return()
    column(4, numericInput("ddo_sim_rounds", "Sampling rounds", value = 100, min = 10, max = 1000, step = 10))
})

output$scde_dist_ddo_text <- renderUI({
    if(input$scde_dist_method != 'ddo') return()
    list(
        wellPanel(
            id = "scde_ddo_info",
            tags$li("According to the SCDE tutorial, ~500 or more should be used. (may take a long time if sample size is large)"),
            tags$li("Direct weighting downweights the contribution of a given gene to the cell-to-cell distance based on the probability that the given measurement is a drop-out event (i.e. belongs to the drop-out component). To estimate the adjusted distance, we will simulate the drop-out events, replacing them with NA values, and calculating correlation using the remaining points.")
        )
    )
})

output$scde_dist_rw_text <- renderUI({
    if(input$scde_dist_method != 'rw') return()
    list(
        wellPanel(
            id = "scde_rw_info",
            tags$li("The reciprocal weighting of the Pearson correlation will give increased weight to pairs of observations where a gene expressed (on average) at a level x1 observed in a cell c1 would not be likely to fail in a cell c2, and vice versa.")
        )
    )
})

output$scde_dist_mrw_text <- renderUI({
    if(input$scde_dist_method != 'mrw') return()
    list(
        wellPanel(
            id = "scde_mrw_info",
            tags$li("Please go to http://hms-dbmi.github.io/scde/diffexp.html if you are interested in details about this method."),
            tags$li("This method use 'a more reliable reference meganitude' to assess the drop-out likelihood.")
        )
    )
})


output$ddo_added_ui <- renderUI({
    if(is.null(r_data$scde_ddo)) return()
    list(
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b("Direct drop-out adjusted distance has been successfully loaded."), style = "font-size:110%;")
    )
})

output$rw_added_ui <- renderUI({
    if(is.null(r_data$scde_rw)) return()
    list(
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b("Reciprocal weighting adjusted distance has been successfully loaded."), style = "font-size:110%;")
    )
})

output$mrw_added_ui <- renderUI({
    if(is.null(r_data$scde_mrw)) return()
    list(
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b("Mode-relative weighting adjusted distance has been successfully loaded."), style = "font-size:110%;")
    )
})

observeEvent(input$run_scde_distance, {
    if(is.null(r_data$scde_ifm)){
        session$sendCustomMessage(type = "showalert", "Please perform error modeling first.")
        return()
    }
    withProgress(message = 'Processing...', value = 0.8, {
        cell.names <- rownames(r_data$scde_ifm)
        if(input$scde_dist_method == "ddo") {
            p.self.fail <- scde::scde.failure.probability(models=r_data$scde_ifm,counts = r_data$raw)
            # simulate drop-outs
            # note: using 10 sampling rounds for illustration here. ~500 or more should be used.
            n.simulations <- input$ddo_sim_rounds; k <- 0.9;
            names(cell.names) <- cell.names
            dl <- parallel::mclapply(1:n.simulations,function(i) {
                scd1 <- do.call(cbind,lapply(cell.names,function(nam) {
                    x <- r_data$raw[,nam];
                    # replace predicted drop outs with NAs
                    x[!as.logical(rbinom(length(x),1,1-p.self.fail[,nam]*k))] <- NA;
                    x;
                }))
                rownames(scd1) <- rownames(r_data$raw);
                # calculate correlation on the complete observation pairs
                cor(log10(scd1+1),use="pairwise.complete.obs");
            },mc.cores=1)
            # calculate average distance across sampling rounds
            r_data$scde_ddo <- as.dist(1-Reduce("+",dl)/length(dl))
        } else if(input$scde_dist_method =="rw") {
            o.fpm <- scde::scde.expression.magnitude(r_data$scde_ifm,counts=r_data$raw)
            k <- 0.95;
            rw.cor<-do.call(rbind,parallel::mclapply(cell.names,function(nam1) {
                unlist(lapply(cell.names,function(nam2) {
                    # reciprocal probabilities
                    f1 <- scde::scde.failure.probability(models=r_data$scde_ifm[nam1,,drop=F],magnitudes=o.fpm[,nam2])
                    f2 <- scde::scde.failure.probability(models=r_data$scde_ifm[nam2,,drop=F],magnitudes=o.fpm[,nam1])
                    # weight factor
                    pnf <- sqrt((1-f1)*(1-f2))*k +(1-k);
                    boot::corr(log10(cbind(r_data$raw[,nam1],r_data$raw[,nam2])+1),w=pnf)
                }))
            },mc.cores=1))
            rownames(rw.cor) <- cell.names
            colnames(rw.cor) <-cell.names
            r_data$scde_rw <- as.dist(1-rw.cor,upper=F)
        } else if(input$scde_dist_method =="mrw") {
            p.self.fail <- scde::scde.failure.probability(models=r_data$scde_ifm,counts = r_data$raw)

            jp <- scde::scde.posteriors(models=r_data$scde_ifm,r_data$raw,r_data$scde_prior,return.individual.posterior.modes=T,n.cores=1)
            # find joint posterior modes for each gene - a measure of MLE of group-average expression
            jp$jp.modes <- log(as.numeric(colnames(jp$jp)))[max.col(jp$jp)]
            p.mode.fail <- scde::scde.failure.probability(models=r_data$scde_ifm,magnitudes=jp$jp.modes)
            # weight matrix
            matw <- 1-sqrt(p.self.fail*sqrt(p.self.fail*p.mode.fail))
            # magnitude matrix (using individual posterior modes here)
            mat <- log10(exp(jp$modes)+1);

            # weighted distance
            mrw.cor<-do.call(rbind,parallel::mclapply(cell.names,function(nam1) {
                unlist(lapply(cell.names,function(nam2) {
                    boot::corr(cbind(mat[,nam1],mat[,nam2]),w=sqrt(sqrt(matw[,nam1]*matw[,nam2])))
                }))
            },mc.cores=1))
            rownames(mrw.cor) <- cell.names
            colnames(mrw.cor) <-cell.names

            r_data$scde_mrw <- as.dist(1-mrw.cor,upper=F);
        }
    })
})

