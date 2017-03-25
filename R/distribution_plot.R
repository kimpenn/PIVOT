
#' Standardized log data distribution plot
#'
#' @description
#' @import ggplot2
#' @export
stdlog_plot <- function(df) {
    qplot(value, geom = "density", data = df) + stat_function(fun = dnorm, size = 0.5, color = "red") + xlab("Standardized log (normalized counts)") + ylab("Density")
}

#' Rank Frequency function
#'
#' @description
#'
#' @export
rank_freq <- function(df, logX = FALSE, logY = FALSE){
    sorted = apply(df, 2, function(x) {sort(x, decreasing=T)})
    sorted = sorted + 0.0000001
    ranks = 1:nrow(sorted)
    samples = colnames(sorted)
    nSamples = ncol(sorted)
    ymin = 1
    ymax = max(sorted)
    xmin = 1
    xmax = dim(sorted)[1]
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

    # configure which axis are log scale
    if (logX) {
        xAxis = "Rank (log)"
        logAxis = "x"
    } else {
        xAxis = "Rank"
        logAxis = ""
    }
    if (logY) {
        yAxis = "Frequency (log)"
        logAxis = paste(logAxis, "y", sep="")
    } else {
        yAxis = "Frequency"
    }
    title="Rank-Frequency"
    # set up an empty plot. We add the lines below.
    plot(ranks, sorted[ranks,1], xlim=c(xmin, xmax), ylim=c(ymin, ymax), log=logAxis, ylab=yAxis, xlab=xAxis, col='black', type='n', cex=0.4, main=title)

    for(i in 1:nSamples) {
        # using the sample name means the column order in colorFactorSorted file doesn't have to match the order in the counts file
        sample = samples[i]
        color = zColors[i]
        colorsUsed[i] = color
        lines(ranks, sorted[ranks,sample], type="l", lty=i, lwd=1.5, cex=1, col=color)
    }

    # add sample legend
    legend("topright", legend=samples, col=colorsUsed, lty=1:nSamples, lwd=1.8, cex=0.8)
}



#' mean.var.plot support function
#'
#' @description
#' From Seurat package.
#' @export
meanNormFunction=function(data,myfuncX,myfuncY,nBin=20) {
    data_x=apply(data,1,myfuncX)
    data_y=apply(data,1,myfuncY)
    data_x_bin=cut(data_x,nBin)
    names(data_x_bin)=names(data_x)
    mean_y=tapply(data_y,data_x_bin,mean)
    sd_y=tapply(data_y,data_x_bin,sd)
    return((data_y-mean_y[as.numeric(data_x_bin)])/sd_y[as.numeric(data_x_bin)])
}

#' mean.var.plot function
#'
#' @description
#' From Seurat package, with modification
#' @import dplyr tibble
#' @export
meanVarPlot <- function(df, fxn.x, fxn.y,do.plot=TRUE,set.var.genes=TRUE,do.text=TRUE,
                          x.low.cutoff=4,x.high.cutoff=8,y.cutoff=1,y.high.cutoff=12,cex.use=0.5,cex.text.use=0.5,do.spike=FALSE,
                          pch.use=16, col.use="black", spike.col.use="red",plot.both=FALSE,do.contour=TRUE,
                          contour.lwd=3, contour.col="white", contour.lty=2,num.bin=20) {
    # Original Seurat
    result <- list()

    data=df
    data.x=apply(data,1,fxn.x); data.y=apply(data,1,fxn.y); data.x[is.na(data.x)]=0
    data.norm.y=meanNormFunction(data,fxn.x,fxn.y,num.bin)
    data.norm.y[is.na(data.norm.y)]=0
    names(data.norm.y)=names(data.x)
    pass.cutoff=names(data.x)[which(((data.x>x.low.cutoff) & (data.x<x.high.cutoff)) & (data.norm.y>y.cutoff) & (data.norm.y < y.high.cutoff))]
    mv.df=data.frame(data.x,data.y,data.norm.y)
    rownames(mv.df)=rownames(data)
    result$mean.var=mv.df

    # Code added here to add a reformatted table
    tbl<-mv.df %>% tibble::rownames_to_column() %>% dplyr::arrange(-data.y)
    # Note this line needs to be changed if user specify different fxn.x and fxn.y, only applies to default
    colnames(tbl) <- c("Gene", "Mean", "Dispersion", "Dispersion Z-score")
    tbl <- tbl[match(pass.cutoff, tbl$Gene),]
    result$var.tbl <- tbl

    # Original Seurat
    if (do.spike) spike.genes=rownames(subr(data,"^ERCC"))
    if (do.plot) {
        if (plot.both) {
            par(mfrow=c(1,2))
            smoothScatter(data.x,data.y,pch=pch.use,cex=cex.use,col=col.use,xlab="Average expression",ylab="Dispersion",nrpoints=Inf)

            if (do.contour) {
                data.kde=MASS::kde2d(data.x,data.y)
                contour(data.kde,add=TRUE,lwd=contour.lwd,col=contour.col,lty=contour.lty)
            }
            if (do.spike) points(data.x[spike.genes],data.y[spike.genes],pch=16,cex=cex.use,col=spike.col.use)
            if(do.text) text(data.x[pass.cutoff],data.y[pass.cutoff],pass.cutoff,cex=cex.text.use)
        }
        smoothScatter(data.x,data.norm.y,pch=pch.use,cex=cex.use,col=col.use,xlab="Average expression",ylab="Dispersion",nrpoints=Inf)
        if (do.contour) {
            data.kde=MASS::kde2d(data.x,data.norm.y)
            contour(data.kde,add=TRUE,lwd=contour.lwd,col=contour.col,lty=contour.lty)
        }
        if (do.spike) points(data.x[spike.genes],data.norm.y[spike.genes],pch=16,cex=cex.use,col=spike.col.use,nrpoints=Inf)
        if(do.text) text(data.x[pass.cutoff],data.norm.y[pass.cutoff],pass.cutoff,cex=cex.text.use)
    }
    if (set.var.genes) {
        result$var.genes=pass.cutoff
        return(result)
        if (!set.var.genes) return(pass.cutoff)
    }
}



#' meanSDplot support function
#'
#' @description
#' meanSDplot support function from VSN package
#' @export
rowV <- function (x, mean, ...)
{
    sqr = function(x) x * x
    n = rowSums(!is.na(x))
    n[n < 1] = NA
    if (missing(mean))
        mean = rowMeans(x, ...)
    return(rowSums(sqr(x - mean), ...)/(n - 1))
}

#' meanSDplot
#'
#' @description
#' meanSDplot function from VSN package
#' @import ggplot2
#' @export
meanSdPlot <- function(x, ranks=TRUE, xlab = ifelse(ranks, "rank(mean)", "mean"),
                       ylab = "sd", pch, plot = TRUE, bins = 50, ...) {

    stopifnot(is.logical(ranks), length(ranks)==1, !is.na(ranks))

    n = nrow(x)
    if (n == 0L) {
        warning("In 'meanSdPlot': matrix has 0 rows, there is nothing to be done.")
        return()
    }
    if (!missing(pch)) {
        warning("In 'meanSdPlot': 'pch' is ignored.")
    }

    px   = rowMeans(x, na.rm=TRUE)
    py   = sqrt(rowV(x, mean=px, na.rm=TRUE))
    rpx  = rank(px, na.last=FALSE, ties.method = "random")

    ## run median with centers at dm, 2*dm, 3*dm,... and width 2*dm
    dm        = 0.025
    midpoints = seq(dm, 1-dm, by = dm)
    within    = function(x, x1, x2) { x>=x1 & x<=x2 }
    mediwind  = function(mp) median(py[within(rpx/n, mp-2*dm, mp+2*dm)], na.rm=TRUE)
    rq.sds    = sapply(midpoints, mediwind)

    res = if(ranks) {
        list(rank=midpoints*n, sd=rq.sds, px=rpx, py=py)
    } else {
        list(quantile=quantile(px, probs=midpoints, na.rm=TRUE), sd=rq.sds, px=px, py=py)
    }

    fmt = function() function(x) format(round(x, 0), nsmall = 0L, scientific = FALSE)

    res$gg = ggplot(data.frame(px = res$px, py = res$py),
                    aes(x = px, y = py)) + xlab(xlab) + ylab(ylab) +
        stat_binhex(bins = bins, ...) +
        scale_fill_gradient(name = "count", trans = "log", labels = fmt()) +
        geom_line(aes(x = x, y = y), data = data.frame(x = res[[1]], y = res$sd), color = "red")

    if (plot) print(res$gg)

    return(invisible(res))
}

