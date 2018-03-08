# Copyright (c) 2015-2018 Qin Zhu and Junhyong Kim, University of Pennsylvania.
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

#' Data normalization master function
#'
#' @export
normalize_data <- function(method, params = NULL, raw, ercc = NULL) {
    error_I <- 0
    # Gene lengths verification
    if(grepl("RPKM", method) || method == "TPM") {
        if(is.null(params$gene_length)) {
            session$sendCustomMessage(type = "showalert", "Please upload your gene lengths first.")
            stop("Gene length required")
        }
        glen <- params$gene_length[match(toupper(rownames(raw)), toupper(names(params$gene_length)))]
        if(any(is.na(glen))) {
            session$sendCustomMessage(type = "showalert", "Some of your features does not have matched lengths, please recheck the length file.")
            stop("Gene length info incomplete.")
        }
    }

    # Different normalization procedures
    if(method == "DESeq") {
        tryCatch({
            samplesAll <- data.frame(row.names=colnames(raw), celltype=rep("nt",length(colnames(raw))))
            dds <- DESeq2::DESeqDataSetFromMatrix(countData = round(raw), colData=samplesAll, design = ~ 1)
            dds <- DESeq2::estimateSizeFactors(dds)
            sizeFactorRefAll <- data.frame(size_factor = SummarizedExperiment::colData(dds)$sizeFactor)
            norm_param <- list(method = method, sizeFactor = sizeFactorRefAll)
            df <- as.data.frame(DESeq2::counts(dds, normalized=T))
        },
        error = function(e){
            error_I <<- 1
        }
        )
    } else if(method == "Modified_DESeq") {
        if(is.null(params$deseq_threshold)) {
            stop("DESeq threshold required")
        }
        tryCatch({
            suppressWarnings({
                samplesAll <- data.frame(row.names=colnames(raw), celltype=rep("nt",length(colnames(raw))))
                dds <- DESeq2::DESeqDataSetFromMatrix(countData = round(raw), colData=samplesAll, design = ~ 1)
                sf_list <- estimateSizeFactorsForMatrix_MK(raw, threshold = params$deseq_threshold)
                DESeq2::sizeFactors(dds) <- sf_list$sf
                norm_param <- list(method = method, sizeFactor = data.frame(size_factor = sf_list$sf), numGenes = sf_list$numGenes, deseq_threshold = params$deseq_threshold)
                df <- as.data.frame(DESeq2::counts(dds, normalized=T))
            })
        },
        error = function(e){
            error_I <<- 1
        }
        )
    } else if (method %in% c("TMM", "upperquartile", "CPM", "TMM-RPKM", "upperquartile-RPKM")) { # this is done using the edgeR function
        y <- edgeR::DGEList(counts=raw)
        if(method == "TMM-RPKM") {
            y <- edgeR::calcNormFactors(y, method = "TMM")
            df <- as.data.frame(edgeR::rpkm(y, gene.length = glen, normalized.lib.sizes=TRUE))
        } else if(method == "upperquartile-RPKM") {
            y <- edgeR::calcNormFactors(y, method = "upperquartile")
            df <- as.data.frame(edgeR::rpkm(y, gene.length = glen, normalized.lib.sizes=TRUE))
        } else if(method == "CPM") {
            print("CPM normalization:")
            y <- edgeR::calcNormFactors(y, method = "none")
            df <- as.data.frame(edgeR::cpm(y, normalized.lib.sizes=TRUE)) # Counts per million
        } else {
            y <- edgeR::calcNormFactors(y, method = method)
            df <- as.data.frame(edgeR::cpm(y, normalized.lib.sizes=TRUE)) # Counts per million with upperquartile/TMM normalization
        }
        sf <- y$samples %>% dplyr::select(-group)
        sf$effective.lib.size <- 1e-06 * sf$lib.size * sf$norm.factors

        if(grepl("RPKM", method)) { # for RPKM, also need to add gene length vector
            norm_param <- list(method = method, sizeFactor = sf, gene_length = glen)
        } else {
            norm_param <- list(method = method, sizeFactor = sf)
        }
    } else if(method == "RPKM") {
        y <- edgeR::DGEList(counts=raw)
        df <- as.data.frame(edgeR::rpkm(y, gene.length = glen, normalized.lib.sizes=FALSE))
        norm_param <- list(method = method, sizeFactor = y$samples %>% dplyr::select(-group))
    } else if(method == "TPM") {
        y <- edgeR::DGEList(counts=raw)
        df <- as.data.frame(apply(raw, 2, function(col) {countToTpm(counts = col, effLen = glen)}))
        norm_param <- list(method = method, sizeFactor = y$samples %>% dplyr::select(-group))
    } else if(method == "Census") {
        if(is.null(params$expected_capture_rate)) {
            stop("Expected capture rate required.")
        }
        tryCatch({
            rpc <- relative2abs_modified(as.matrix(raw), expected_capture_rate = params$expected_capture_rate, return_all = T)
            norm_param <- list(method = method, t_estimate = rpc$t_estimate, expected_total_mRNAs = rpc$expected_total_mRNAs)
            df <- as.data.frame(rpc$norm_cds)
        },
        error = function(e){
            error_I <<- 1
        }
        )
    } else if(method == "ERCC-RLM") {
        if(any(sapply(list(params$expected_capture_rate, params$ercc_added,
                       params$ercc_dilution, params$ercc_mix_type,
                       params$ercc_detection_threshold, params$ercc_std), is.null))) {
            stop("Missing required parameters")
        }

        tryCatch({
            rpc <- relative2abs_modified(
                as.matrix(raw), expected_capture_rate = params$expected_capture_rate, return_all = T,
                ERCC_controls = as.matrix(ercc), ERCC_annotation = params$ercc_std, volume = params$ercc_added * 1e3, dilution = params$ercc_dilution,
                mixture_type = as.numeric(params$ercc_mix_type), detection_threshold = params$ercc_detection_threshold
            )
            norm_param <- list(method = method, k_b_solution = rpc$k_b_solution, ercc_added = params$ercc_added, ercc_dilution = params$ercc_dilution,
                               ercc_mix_type = as.numeric(params$ercc_mix_type), ercc_detection_threshold = params$ercc_detection_threshold)
            df <- as.data.frame(rpc$norm_cds)
            colnames(df) <- colnames(raw)
            rownames(df) <- rownames(raw)
        },
        error = function(e){
            error_I <<- 1
        }
        )
    } else if(method == "RUVg") {
        if(is.null(params$control_gene)) {
            stop("Contorl genes required.")
        }
        if(is.null(params$ruvg_k) || is.null(params$ruvg_round)) {
            stop("Missing required parameters.")
        }
        # samplesAll <- data.frame(row.names=colnames(raw), celltype=rep("nt",length(colnames(raw))))
        # set <- EDASeq::newSeqExpressionSet(as.matrix(raw), phenoData = data.frame(samplesAll, row.names=colnames(raw)))
        cgene = params$control_gene[which(params$control_gene %in% rownames(raw))]
        if(length(cgene) <= 2) {
            stop("Too few control genes.")
        }
        counts <- BiocGenerics::counts # Import the counts function
        set <- RUVSeq::RUVg(as.matrix(raw), cgene, k=params$ruvg_k, round = params$ruvg_round)
        df <- as.data.frame(set$normalizedCounts)
        norm_param <- list(method = method)
    } else if(method == "none") {
        norm_param <- list(method = method)
        df <- raw
    } else {
        stop("Not recognized normalization method.")
    }
    if(error_I) {
        stop("Error occured during normalization.")
    } else {
        return(list(df = df, norm_param = norm_param))
    }
}


#' Mugdhak's modified DESeq script
#'
#' @export
estimateSizeFactorsForMatrix_MK <- function( counts, locfunc = median, geoMeans, controlGenes, threshold = 0.7)
{
    if (missing(geoMeans)) {
        i = 0
        numGenes = 0
        loggeomeans = c()

        s_th <- threshold * ncol(counts)

        loggeomeans <- apply(counts, 1, function(crow) {
            if(length(crow[crow!=0]) < s_th){
                return(-Inf)
            } else {
                mean(log(crow[crow!=0]))
            }
        })

    } else {
        if (length(geoMeans) != nrow(counts)) {
            stop("geoMeans should be as long as the number of rows of counts")
        }
        loggeomeans <- log(geoMeans)
    }

    numGenes <- sum(is.finite(loggeomeans))

    if (all(is.infinite(loggeomeans))) {
        stop("every gene contains at least one zero, cannot compute log geometric means")
    }
    sf <- if (missing(controlGenes)) {
        apply(counts, 2, function(cnts) {
            exp(locfunc((log(cnts) - loggeomeans)[is.finite(loggeomeans) & cnts > 0]))
        })
    } else {
        if (!is.numeric(controlGenes) | is.logical(controlGenes)) {
            stop("controlGenes should be either a numeric or logical vector")
        }
        loggeomeansSub <- loggeomeans[controlGenes]
        apply(counts[controlGenes,], 2, function(cnts) {
            exp(locfunc((log(cnts) - loggeomeansSub)[is.finite(loggeomeansSub) & cnts > 0]))
        })
    }
    return(list(sf = sf, numGenes = numGenes, threshold = threshold))
}

#' Groupwise modified DESeq script
#'
#' @export
estimateSizeFactorsForMatrix_groupwise <- function( counts, group, locfunc = median, geoMeans, controlGenes, threshold = 0.7)
{
    if (missing(geoMeans)) {
        i = 0
        numGenes = 0
        loggeomeans = c()


        gp_num <- length(unique(group))

        det_rate<-as.data.frame(t(apply(counts, 1, function(crow) {table(group[which(crow > 0)])/table(group)})))

        not_to_keep <- which(rowSums(det_rate >= threshold) != gp_num)

        loggeomeans <- apply(counts, 1, function(crow) { mean(log(crow[crow!=0])) })

        loggeomeans[not_to_keep] <- -Inf

    } else {
        if (length(geoMeans) != nrow(counts)) {
            stop("geoMeans should be as long as the number of rows of counts")
        }
        loggeomeans <- log(geoMeans)
    }

    numGenes <- sum(is.finite(loggeomeans))

    if (all(is.infinite(loggeomeans))) {
        stop("every gene contains at least one zero, cannot compute log geometric means")
    }
    sf <- if (missing(controlGenes)) {
        apply(counts, 2, function(cnts) {
            exp(locfunc((log(cnts) - loggeomeans)[is.finite(loggeomeans) & cnts > 0]))
        })
    } else {
        if (!is.numeric(controlGenes) | is.logical(controlGenes)) {
            stop("controlGenes should be either a numeric or logical vector")
        }
        loggeomeansSub <- loggeomeans[controlGenes]
        apply(counts[controlGenes,], 2, function(cnts) {
            exp(locfunc((log(cnts) - loggeomeansSub)[is.finite(loggeomeansSub) & cnts > 0]))
        })
    }
    return(list(sf = sf, numGenes = numGenes, threshold = threshold))
}



#' Function from https://haroldpimentel.wordpress.com/2014/05/08/what-the-fpkm-a-review-rna-seq-expression-units/
#'
#' @description Calculating TPM for each sample.
#' @export
countToTpm <- function(counts, effLen)
{
    rate <- log(counts) - log(effLen)
    denom <- log(sum(exp(rate)))
    exp(rate - denom + log(1e6))
}



