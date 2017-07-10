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


# Pairwise Scatterplot

output$pairwise_box <- renderUI({
    req(r_data$df)

    list(
        enhanced_box(
            width = 12,
            title = "Pairwise correlations",
            id = "pair_corr",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("General Settings:"), class = "param_setting_title"),

            fluidRow(
                column(6,
                    tags$li("Pairwise correlation is performed using log10 transformed raw counts with function from the SCDE package. "),
                    tags$li("If you have specified group infomation, the comparison will be performed group-wise. Otherwise all sample pairs will be compared.")
                ),
                pivot_groupBy_UI("paircor", r_data$category, append_none = T, choose_color = F, width = 4),
                column(2,  br(), actionButton("run_corr", "Run", class = "btn btn-info"))
            ),
            hr(),
            uiOutput("pair_corr_group_ui"),
            plotOutput("pair_corr_plt", height = "850px"),
            hr(),
            tags$p("This plot shows pairwise comparison between your samples.
            The x and y axis of each plot show log10 RPM estimates in the cell corresponding to a given column and row respectively.
            The set of smoothed scatter plots on the lower left shows the overall correspondence between the transcript abundances estimated in two given cells.
            The upper right corner shows three-component mixture model, separating genes that “drop-out” in one of the cells (green component shows drop/out events in the column cell, red component shows drop-out events in the row cell). The correlated component is shown in blue.
            The percent of genes within each component is shown in the legend.")
        ),
        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Kharchenko, P. V., Silberstein, L., & Scadden, D. T. (2014). Bayesian approach to single-cell differential expression analysis. Nature methods, 11(7), 740-742.", class = "citation"),
                tags$li("Peter Kharchenko and Jean Fan (2015). scde: Single Cell Differential Expression. R package version 1.99.0. http://pklab.med.harvard.edu/scde/index.html", class = "citation")
            )
        )
    )

})

paircorr <- reactiveValues()
paircorr$plt <- NULL

pair_corr <- function(df) {
    # get min and max count values for axis range.
    rangeMin = min(df)
    rangeMax = max(df)

    # featurerate color scale from black (0) to red (1) used to represent coorelations.
    colorFunction = colorRamp(c("black", "red"))
    # colorFunction() expects values from 0 to 1.
    zMatrix = colorFunction(seq(0,1,by=.01))
    # zColors goes from 1 to 100.
    zColors = sort(rgb(zMatrix[,1], zMatrix[,2], zMatrix[,3], maxColorValue=255))
    labelSize=1
    title="Pairwise Correlations"
    # Modified from R pairs() documentation
    panel.cor = function(x, y, digits=2, prefix="", cex.cor, ...) {
        usr = par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r = abs(cor(x, y))
        txt = format(c(r, 0.123456789), digits=digits)[1]
        txt = paste(prefix, txt, sep="")

        if (FALSE) {
            # color text based on r value and change size of text also based on r value (larger text for larger r value).
            if (missing(cex.cor)) cex.cor = labelSize/strwidth(txt)
            text(0.5, 0.5, txt, cex=cex.cor*r, col=zColors[r*100])
        } else {
            # color text based on r value (red is r=1).
            text(0.5, 0.5, txt, cex=labelSize, col=zColors[r*100])
        }
    }
    par(mar = c(0,0,0,0))
    pairs(df, pch=".", cex.labels=labelSize, main=title, upper.panel=panel.cor, ylim=c(rangeMin,rangeMax), xlim=c(rangeMin,rangeMax))
}


papply <- function(...,n.cores=detectCores()) {
    if(n.cores>1) {
        # bplapply implementation
        if(is.element("parallel", installed.packages()[,1])) {
            mclapply(...,mc.cores=n.cores)
        } else {
            # last resort
            bplapply(... , BPPARAM = MulticoreParam(workers = n.cores))
        }
    } else { # fall back on lapply
        lapply(...);
    }
}


calculate_crossfit_models <- function(counts, groups, min.count.threshold = 4, nrep = 1, verbose = 0, min.prior = 1e-5, n.cores = 12, save.plots = TRUE, zero.lambda = 0.1, old.cfm = NULL, threshold.segmentation = FALSE, threshold.prior = 1-1e-6, max.pairs = 1000, min.pairs.per.cell = 10) {
    names(groups) <- colnames(counts)
    # enumerate cross-fit pairs within each group
    cl <- do.call(cbind, tapply(colnames(counts), groups, function(ids) {
        cl <- combn(ids, 2)
        min.pairs.per.cell <- min(length(ids)*(length(ids)-1)/2, min.pairs.per.cell)
        if(verbose) {
            cat("number of pairs: ", ncol(cl), "\n")
        }
        if(ncol(cl) > max.pairs) {
            if(verbose) {
                cat("reducing to a random sample of ", max.pairs, " pairs\n")
            }

            # make sure there's at least min.pairs.per.cell pairs for each cell
            cl <- cl[, unique(c(sample(1:ncol(cl), max.pairs),
                                unlist(lapply(ids, function(id) sample(which(colSums(cl == id) > 0), min.pairs.per.cell)))))]
        }
        cl
    }))

    orl <- c()
    if(!is.null(old.cfm)) {
        # check which pairs have already been fitted in compared in old.cfm
        pn1 <- unlist(apply(cl, 2, function(ii) paste(ii, collapse = ".vs.")))
        pn2 <- unlist(apply(cl, 2, function(ii) paste(rev(ii), collapse = ".vs."))) ### %%% use rev() to revert element order
        vi <- (pn1 %in% names(old.cfm)) | (pn2 %in% names(old.cfm))
        cl <- cl[, !vi, drop = FALSE]
        orl <- old.cfm[names(old.cfm) %in% c(pn1, pn2)]
    }
    if(verbose) {
        cat("total number of pairs: ", ncol(cl), "\n")
    }

    if(dim(cl)[2] > 0) {
        if(verbose)  message(paste("cross-fitting", ncol(cl), "pairs:"))
        rl <- papply(seq_len(ncol(cl)), function(cii) {
            ii <- cl[, cii]
            df <- data.frame(c1 = counts[, ii[1]], c2 = counts[, ii[2]])
            vi <- which(rowSums(df) > 0, )
            if(!threshold.segmentation) { # NOT RUN HERE
                if(verbose) {
                    message("fitting pair [", paste(ii, collapse = " "), "]")
                }
                mo1 <- FLXMRglmCf(c1~1, family = "poisson", components = c(1), mu = log(zero.lambda))
                mo2 <- FLXMRnb2glmC(c1~1+I(log(c2+1)), components = c(2))
                mo3 <- FLXMRnb2glmC(c2~1+I(log(c1+1)), components = c(2))
                mo4 <- FLXMRglmCf(c2~1, family = "poisson", components = c(3), mu = log(zero.lambda))
                m1 <- mc.stepFlexmix(c1~1, data = df[vi, ], k = 3, model = list(mo1, mo2, mo3, mo4), control = list(verbose = verbose, minprior = min.prior), concomitant = FLXPmultinom(~I((log(c1+1)+log(c2+1))/2)+1), cluster = cbind(df$c1[vi]<= min.count.threshold, df$c1[vi] > min.count.threshold & df$c2[vi] > min.count.threshold, df$c2[vi]<= min.count.threshold), nrep = nrep)

                # reduce return size
                m1@posterior <- lapply(m1@posterior, function(m) {
                    rownames(m) <- NULL
                    return(m)
                })
                #rownames(m1@concomitant@x) <- NULL
                m1@concomitant@x <- matrix()
                m1@model <- lapply(m1@model, function(mod) {
                    mod@x <- matrix()
                    mod@y <- matrix()
                    #rownames(mod@x) <- NULL
                    #rownames(mod@y) <- NULL
                    return(mod)
                })

                #parent.env(environment(m1@components[[1]][[1]]@logLik)) <- globalenv()
                #parent.env(environment(m1@components[[1]][[2]]@logLik)) <- globalenv()
                #parent.env(environment(m1@components[[2]][[1]]@logLik)) <- globalenv()
                #parent.env(environment(m1@components[[2]][[2]]@logLik)) <- globalenv()

                names(vi) <- NULL
                pm <- posterior(m1)[, c(1, 3)]
                rownames(pm) <- NULL
                cl <- clusters(m1)
                names(cl) <- NULL
                gc()
            } else {
                # use min.count.threshold to quickly segment the points
                cl <- rep(2, length(vi))
                cl[df[vi, 1]<min.count.threshold] <- 1
                cl[df[vi, 2]<min.count.threshold] <- 3
                cl[df[vi, 1]<min.count.threshold & df[vi, 2]<min.count.threshold] <- 0
                names(cl) <- NULL
                pm <- cbind(ifelse(cl == 1, threshold.prior, 1-threshold.prior), ifelse(cl == 3, threshold.prior, 1-threshold.prior))
                rownames(pm) <- NULL
            }
            rli <- list(ii = ii, clusters = cl, posterior = pm, vi = vi)
            #message("return object size for pair [", paste(ii, collapse = " "), "] is ", round(object.size(rli)/(1024^2), 3), " MB")
            return(rli)
        }, n.cores = round(n.cores/nrep))
        #, mc.preschedule = threshold.segmentation) # mclapply function has preschedule
        names(rl) <- apply(cl, 2, paste, collapse = ".vs.")
        # clean up invalid entries
        rl <- rl[!unlist(lapply(rl, is.null))]
        rl <- rl[unlist(lapply(rl, is.list))]
        #names(rl) <- unlist(lapply(rl, function(d) paste(d$ii, collapse = ".vs.")))
    } else {
        rl <- c()
    }

    if(!is.null(old.cfm)) rl <- c(rl, orl)

    return(rl)
}

observeEvent(input$run_corr, {
    req(r_data$raw)
    withProgress(message = 'Performing pairwise correlation analysis...', value = 0.5, {
        gList <- callModule(pivot_groupBy, "paircor", meta = r_data$meta)
        if(!is.null(gList$meta)) {
            groups <- gList$meta[,1]
        } else {
            groups <- rep("samples", ncol(r_data$raw))
        }

        tryCatch({
            cfm<-calculate_crossfit_models(r_data$raw, groups, n.cores = 1, threshold.segmentation = TRUE, verbose = 1)
            r_data$pair_cor <- list(cfm = cfm, group_by = gList$group_by, group = groups)
        }, error = function(e) {
            return()
        })
    })
})


output$pair_corr_group_ui <- renderUI({
    req(r_data$pair_cor)
    gps <- unique(as.character(r_data$pair_cor$group))
    names(gps) <- gps
    list(
        selectInput("pair_corr_group", paste0("Current selected category: ", r_data$pair_cor$group_by, ". Plot Group: "), choices = as.list(gps))
    )

})

output$pair_corr_plt <- renderPlot({
    req(input$pair_corr_group)
    req(r_data$pair_cor)

    if(is.null(input$pair_corr_group)) {
        ids <- colnames(r_data$raw)
        groups <- rep("samples", ncol(r_data$raw))
    } else {
        ids <- r_data$sample_name[which(r_data$pair_cor$group == input$pair_corr_group)]
        groups <- r_data$pair_cor$group
    }

    withProgress(message = 'Generating Plots...', value = 0.5, {
        scde_pair_plot(r_data$raw, groups, r_data$pair_cor$cfm, ids)
    })
})



