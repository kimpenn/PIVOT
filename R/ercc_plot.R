


#' Plot ERCC detected vs expected with ggplot
#'
#' @import ggplot2
#' @export
ercc_detect_plot <- function(ercc_df){
    glm.out = glm(PercentDetected ~ Log10molecules, family=binomial(logit), data=ercc_df)
    y_pred <-predict(glm.out, newdata=data.frame(Log10molecules = 0), type = "response")
    ggplot(ercc_df, aes(molecules, PercentDetected)) + scale_x_log10() +
        xlab("Expected Number Molecules") + ylab("Fraction of samples detecting gene (>= 1 read)") +
        theme_classic() + geom_hline(yintercept=y_pred, color="gray") +
        geom_vline(xintercept=1, color="gray", linetype="longdash") +
        stat_smooth(method="glm", method.args = list(family = "binomial"), color="dodgerblue2", fill="lightgray") +
        geom_point(size=2)
}

#' Plot ERCC distribution and perform linear regression
#'
#' @export
plotERCCDistribution <- function (counts) {
    molecules <- sapply ( counts[ , 'molecules' ] , function ( x ) { round ( x , digits = 2 ) } )
    names <- sapply ( rownames ( counts) , function ( x ) { strsplit ( x , 'ERCC.00' ) [[ 1 ]] [ 2 ] } )
    names <- paste ( names , molecules , sep = ', ' )

    tord <- order ( molecules )

    moleculeCts <- table ( molecules )
    transcriptBins <- list()
    transcriptBinXs <- list()
    spacing <- c(1)
    index <- 1

    for ( i in c ( 1 : length ( moleculeCts ) ) ) {
        start <- spacing [ length ( spacing ) ] + 2
        xs <- c ( start : ( start + moleculeCts [ i ] - 1 ) )
        spacing <- c ( spacing , xs )
        transcriptBinXs [[ i ]] <- c ( min ( xs ) , max ( xs ) ) - 1
        transcriptBins [[ i ]] <- c ( index : ( index + length ( xs ) - 1 ) )
        index <- index + length ( xs )
    }
    spacing <- spacing [ c ( 2 : length ( spacing ) ) ]
    spacing <- spacing - 1

    samples <- setdiff ( colnames ( counts ) , 'molecules' )

    # Plot:
    x1mar <- 0.5
    x2mar <- 0.05
    y1mar <- 1.3
    y2mar <- 0.3

    ymax_log <- ceiling(log10(max(counts[, samples])))

    if(ymax_log%%2) {
        ymax_log <- ymax_log+1
    }
    ymax <- 10^ymax_log


    par(mai=c(y1mar, x1mar, y2mar , x2mar ) , xaxs= 'i', yaxs = 'i')
    plot ( 1 , 1 ,  xlim = c ( 0 , max ( spacing ) + 1 ), ylim = c ( 0.8, ymax) , axes = FALSE , type = 'n', yaxt='n', xaxt='n', ylab='', xlab='' , main = "Distribution of ERCCs" , log = 'y' )
    mtext (  'Counts + 1'  , side = 2 , line = 2.5 )
    axis ( 2 , at = 10^seq(0,ymax_log,2 ) ,labels = 10^seq(0,ymax_log,2 ) ,cex.axis = 1 )
    axis ( 1 , at = spacing ,labels = names [ tord ] ,cex.axis = 1 , las = 2)


    ys <- do.call ( c , lapply ( samples , function ( x ) { counts [ , x ]  } ) )
    xs <- do.call ( c , lapply ( samples , function ( x ) { counts [ , 'molecules' ] } ) )
    keep <- ys > 0
    fit <- lm ( ys [ keep ] ~ 0 + xs [ keep ] )
    inflationFactor <- fit$coefficients [ 1 ]

    text ( 1 , ymax/2 , labels = paste ('Counts = ', round ( inflationFactor ) , ' x molecules', sep = '' ) , adj = c ( 0 , 0.5 ))

    # Plot expectation
    for ( index in c ( 1 : length ( molecules ) ) ) {
        polygon ( c ( spacing [ index ] - 0.5 , spacing [ index ] + 0.5 , spacing [ index ] + 0.5 , spacing [ index ] - 0.5 ) ,
                  c ( rep ( ( qpois ( 0.025, molecules [ tord [ index ] ] , lower.tail = TRUE )*inflationFactor ) + 1, 2 ) ,  rep ( ( qpois ( 0.025, molecules [ tord [ index ] ] , lower.tail = FALSE )*inflationFactor ) + 1, 2 ) ) , col = 'gray90' , border = NA  )
        lines ( c ( spacing [ index ] - 0.5 , spacing [ index ] + 0.5 ) , rep ( ( molecules [ tord [ index ] ] )*inflationFactor + 1, 2 ) , lwd = 1, col = 'gray65' )
    }

    # Plot group averages
    for ( i in c ( 1 : length ( transcriptBins ) ) ) {
        transcriptBinYs <- do.call ( c , lapply ( samples , function ( x ) { as.numeric ( counts [ tord [ transcriptBins [[ i ]] ] , x ] ) } ) )
        transcriptBinAverage <- mean ( transcriptBinYs + 1 )
        lines ( c ( transcriptBinXs [[ i ]] [ 1 ] - 0.7 ,  transcriptBinXs [[ i ]] [ 2 ] + 0.7 ) ,  rep ( transcriptBinAverage , 2 ), col = 'red' , lwd = 1 , lty = 1 )
        sem <- sd ( transcriptBinYs ) / sqrt ( length ( transcriptBinYs ) )
        upperCI <- transcriptBinAverage + ( 2*sem )
        lines ( c ( transcriptBinXs [[ i ]] [ 1 ] - 0.7 ,  transcriptBinXs [[ i ]] [ 2 ] + 0.7 ) ,  rep ( upperCI , 2 ), col = 'red' , lwd = 0.25 , lty = 1 )
        lowerCI <-  transcriptBinAverage - ( 2*sem )
        if ( lowerCI < 1 ) {
            lowerCI <- 1
        }
        lines ( c ( transcriptBinXs [[ i ]] [ 1 ] - 0.7 ,  transcriptBinXs [[ i ]] [ 2 ] + 0.7 ) ,  rep ( lowerCI , 2 ), col = 'red' , lwd = 0.25 , lty = 1 )
        lines ( rep ( transcriptBinXs [[ i ]] [ 1 ] - 0.7  , 2 ) , c ( lowerCI , upperCI ), col = 'red' , lwd = 0.25 )
        lines ( rep ( transcriptBinXs [[ i ]] [ 2 ] + 0.7  , 2 ) , c ( lowerCI , upperCI ), col = 'red' , lwd = 0.25 )
    }

    # Plot individual samples
    for ( index in c ( 1 : length ( molecules ) ) ) {
        lines ( rep ( spacing [ index ] , 2 ) , range ( counts [ tord [ index ] , samples ] + 1 ) , lwd = 0.5  )
        points ( jitter ( rep ( spacing [ index ] , length ( samples ) ) , 0.1 ), counts[ tord [ index ] , samples ] + 1 , pch = 20 , bg = 'gray65' , fg = 'black', cex = 0.35 )
    }

    #legend ( "topleft" , legend = samples , pch = seq ( 21 , 25 , 1 )  )

}

#' Compute coefficient of variation
#'
#' @export
erccCv <- function (counts) {

    molecules <- sapply ( counts[ , 'molecules' ] , function ( x ) { round ( x , digits = 2 ) } )
    samples <- setdiff ( colnames ( counts ) , 'molecules' )
    counts <- counts [ , samples ]
    CVs <- apply ( counts , 1, function ( y ) {  sd ( as.numeric ( y ) ) / mean ( as.numeric ( y ) ) } )
    CVs [ rowSums ( counts ) == 0 ] <- 0

    # And combine with number of molecules
    return(cbind ( "molecules"= molecules , "cv" = CVs ))
}


