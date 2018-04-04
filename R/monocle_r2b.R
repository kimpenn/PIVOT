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

# Function from monocle: Fuse gaussian kernel to calculate the mode of transcript counts
#' @importFrom stats density
dmode <- function(x, breaks="Sturges") {
    if (length(x) < 2) return (0);
    den <- density(x, kernel=c("gaussian"))
    ( den$x[den$y==max(den$y)] )
}


#' Function from monocle: Calibrate_per_cell_total_proposal
#' @param relative_exprs_matrix The matrix of relative TPM expression values
#' @param t_estimate the TPM value that corresponds to 1 cDNA copy per cell
#' @param expected_capture_rate The fraction of mRNAs captured as cDNAs
#' @param method the formula to estimate the total mRNAs (num_genes corresponds to the second formula while tpm_fraction corresponds to the first formula, see the anouncement on Trapnell lab website for the Census paper)
#' @importFrom stats ecdf
calibrate_per_cell_total_proposal <- function (relative_exprs_matrix, t_estimate, expected_capture_rate)
{
    split_relative_exprs <- split(relative_exprs_matrix, rep(1:ncol(relative_exprs_matrix),
                                                             each = nrow(relative_exprs_matrix)))
    proposed_totals <- unlist(lapply(1:length(split_relative_exprs),
                                     function(ind) {
                                         x <- split_relative_exprs[[ind]]
                                         x <- x[x > 0.1]
                                         P <- ecdf(x)
                                         num_single_copy_genes <- sum(x <= t_estimate[ind])
                                         frac_x <- P(t_estimate[ind])
                                         num_single_copy_genes/frac_x/expected_capture_rate
                                     }))
    return(proposed_totals)
}

#' Function from monocle: Find the most commonly occuring relative expression value in each cell
#'
#' Converting relative expression values to mRNA copies per cell requires
#' knowing the most commonly occuring relative expression value in each cell
#' This value typically corresponds to an RPC value of 1. This function
#' finds the most commonly occuring (log-transformed) relative expression value
#' for each column in the provided expression matrix.
#'
#' @param relative_expr_matrix a matrix of relative expression values for
#' values with each row and column representing genes/isoforms and cells,
#' respectively. Row and column names should be included.
#' Expression values should not be log-transformed.
#' @param relative_expr_thresh Relative expression values below this threshold
#' are considered zero.
#' @return an vector of most abundant relative_expr value corresponding to the
#' RPC 1.
#' @details This function estimates the most abundant relative expression value
#' (t^*) using a gaussian kernel density function. It can also optionally
#' output the t^* based on a two gaussian mixture model
#' based on the smsn.mixture from mixsmsn package
#' @export
#' @examples
#' \dontrun{
#' HSMM_fpkm_matrix <- exprs(HSMM)
#' t_estimate = monocle_estimate_t(HSMM_fpkm_matrix)
#'}

monocle_estimate_t <- function(relative_expr_matrix, relative_expr_thresh = 0.1) {
    #apply each column
    unlist(apply(relative_expr_matrix, 2, function(relative_expr) 10^mean(dmode(log10(relative_expr[relative_expr > relative_expr_thresh]))))) #avoid multiple output
}


#' Function from monocle (modified): Transform relative expression values into absolute transcript counts.
#'
#' Transform a relative expression matrix to absolute transcript matrix based on the inferred linear regression parameters from most abundant isoform relative expression value.
#' This function takes a relative expression matrix and a vector of estimated most abundant expression value from the isoform-level matrix and transform it into absolute transcript number.
#' It is based on the observation that the recovery efficient of the single-cell RNA-seq is relative low and that most expressed isoforms of gene in a single cell therefore only sequenced one copy so that the
#' most abundant isoform log10-FPKM (t^*) will corresponding to 1 copy transcript. It is also based on the fact that the spikein regression parameters k/b for each cell will fall on a line because of the
#' intrinsic properties of spikein experiments. We also assume that if we perform the same spikein experiments as Treutlein et al. did, the regression parameters should also fall on a line in the same way. The
#' function takes the the vector t^* and the detection limit as input, then it uses the t^* and the m/c value corresponding to the detection limit to calculate two parameters vectors k^* and b^* (corresponding to each cell)
#' which correspond to the slope and intercept for the linear conversion function between log10 FPKM and log10 transcript counts. The function will then apply a linear transformation
#' to convert the FPKM to estimated absolute transcript counts based on the the k^* and b^*. The default m/c values used in the algoritm are 3.652201, 2.263576, respectively.
#'
#' @param relative_expr_matrix the expression matrix for single cell RNA-seq with each row and column representing genes/isoforms and cells. Row and column names should be included
#' @param t_estimate an vector for the estimated most abundant FPKM value of isoform for a single cell. Estimators based on gene-level relative expression can also give good approximation but estimators
#' based on isoform FPKM will give better results in general
#' @param modelFormulaStr modelformula used to grouping cells for transcript counts recovery. Default is "~ 1", which means to recover the transcript counts from all cells.
#' @param ERCC_controls the FPKM matrix for each ERCC spike-in transcript in the cells if user wants to perform the transformation based on their spike-in data. Note that the row and column names should match up with the ERCC_annotation and relative_exprs_matrix respectively.
#' @param ERCC_annotation the ERCC_annotation matrix from illumina USE GUIDE which will be ued for calculating the ERCC transcript copy number for performing the transformation.
#' @param volume the approximate volume of the lysis chamber (nanoliters). Default is 10
#' @param dilution the dilution of the spikein transcript in the lysis reaction mix. Default is 40, 000. The number of spike-in transcripts per single-cell lysis reaction was calculated from
#' @param mixture_type the type of spikein transcripts from the spikein mixture added in the experiments. By default, it is mixture 1. Note that m/c we inferred are also based on mixture 1.
#' @param detection_threshold the lowest concentration of spikein transcript considered for the regression. Default is 800 which will ensure (almost) all included spike transcripts expressed in all the cells. Also note that the value of c is based on this concentration.
#' @param expected_capture_rate the expected fraction of RNA molecules in the lysate that will be captured as cDNAs during reverse transcription
#' @param return_all parameter for the intended return results. If setting TRUE, matrix of m, c, k^*, b^* as well as the transformed absolute cds will be returned
#' in a list format
#' @param cores number of cores to perform the recovery. The recovery algorithm is very efficient so multiple cores only needed when we have very huge number of cells or genes.
#' @param method the formula to estimate the total mRNAs (num_genes corresponds to the second formula while tpm_fraction corresponds to the first formula, see the anouncement on Trapnell lab website for the Census paper)
#' @param verbose a logical flag to determine whether or not we should print all the optimization details
#' @return an matrix of absolute count for isoforms or genes after the transformation.
#' @export
#' @importFrom parallel mcmapply mclapply detectCores
#' @examples
#' \dontrun{
#' HSMM_relative_expr_matrix <- exprs(HSMM)
#' HSMM_abs_matrix <- relative2abs(HSMM_relative_expr_matrix,
#'    t_estimate = monocle_estimate_t(HSMM_relative_expr_matrix))
#'}

relative2abs_modified <- function (relative_expr_matrix, t_estimate = monocle_estimate_t(relative_expr_matrix),
                          modelFormulaStr = "~1", ERCC_controls = NULL, ERCC_annotation = NULL,
                          volume = 10, dilution = 40000, mixture_type = 1, detection_threshold = 800,
                          expected_capture_rate = 0.25, verbose = FALSE, return_all = FALSE,
                          cores = 1)
{
    parameters <- c(t_estimate, volume, dilution, detection_threshold)
    if (any(c(!is.finite(parameters), is.null(parameters))))
        stop("Your input parameters should not contain either null or infinite numbers")
    if (detection_threshold < 0.01430512 | detection_threshold >
        7500)
        stop("concentration detection limit should be between 0.01430512 and 7500")
    if (mixture_type == 1)
        mixture_name = "conc_attomoles_ul_Mix1"
    else mixture_name = "conc_attomoles_ul_Mix2"
    Cell <- NULL
    if (!is.null(ERCC_controls) | !is.null(ERCC_annotation)) {
        if (is.null(ERCC_controls) | is.null(ERCC_annotation))
            stop("If you want to transform the data to copy number with your spikein data, please provide both of ERCC_controls and ERCC_annotation data frame...")
        valid_ids <- which(ERCC_annotation[, mixture_name] >=
                               detection_threshold)
        if (verbose)
            message("Performing robust linear regression for each cell based on the spikein data...")
        molModels <- apply(ERCC_controls, 2, function(cell_exprs,
                                                      input.ERCC.annotation, valid_ids) {
            spike_df <- input.ERCC.annotation
            spike_df <- cbind(spike_df, cell_exprs[row.names(spike_df)])
            colnames(spike_df)[length(colnames(spike_df))] <- "FPKM"
            spike_df$numMolecules <- spike_df[, mixture_name] *
                (volume * 10^(-3) * 1/dilution * 10^(-18) * 6.02214129 *
                     10^(23))
            spike_df$rounded_numMolecules <- round(spike_df$numMolecules)
            if (is.null(valid_ids))
                spike_df <- subset(spike_df, FPKM >= 1e-10)
            else {
                spike_df <- spike_df[valid_ids, ]
                spike_df <- subset(spike_df, FPKM >= 1e-10)
            }
            spike_df$log_fpkm <- log10(spike_df$FPKM)
            spike_df$log_numMolecules <- log10(spike_df$numMolecules)
            molModel <- tryCatch({
                molModel <- MASS::rlm(log_numMolecules ~ log_fpkm,
                                      data = spike_df)
                molModel
            }, error = function(e) {
                print(e)
                NULL
            })
            molModel
        }, ERCC_annotation, valid_ids)
        if (verbose)
            message("Apply the fitted robust linear regression model to recovery the absolute copy number for all transcripts each cell...")
        norm_fpkms <- mcmapply(function(cell_exprs, molModel) {
            tryCatch({
                norm_df <- data.frame(log_fpkm = log10(cell_exprs))
                res <- 10^predict(molModel, type = "response",
                                  newdata = norm_df)
            }, error = function(e) {
                rep(NA, length(cell_exprs))
            })
        }, split(as.matrix(relative_expr_matrix), rep(1:ncol(relative_expr_matrix),
                                                      each = nrow(relative_expr_matrix))), molModels, mc.cores = cores)
        k_b_solution <- data.frame(b = unlist(lapply(molModels,
                                                     FUN = function(x) {
                                                         intercept = x$coefficients[1]
                                                     })), k = unlist(lapply(molModels, FUN = function(x) {
                                                         slope = x$coefficients[2]
                                                     })))
        kb_model <- MASS::rlm(b ~ k, data = k_b_solution)
        kb_slope <- kb_model$coefficients[2]
        kb_intercept <- kb_model$coefficients[1]
        if (return_all == T) {
            return(list(norm_cds = norm_fpkms, kb_slope = kb_slope,
                        kb_intercept = kb_intercept, k_b_solution = k_b_solution))
        }
        norm_fpkms
    }
    else {
        names(t_estimate) <- colnames(relative_expr_matrix)
        expected_total_mRNAs <- calibrate_per_cell_total_proposal(relative_expr_matrix,
                                                                  t_estimate, expected_capture_rate)
        expr_probs <- t(t(relative_expr_matrix)/colSums(relative_expr_matrix))
        census_transcript_counts <- t(t(expr_probs) * expected_total_mRNAs)
        if (return_all == T) {
            return(list(norm_cds = census_transcript_counts,
                        t_estimate = t_estimate, expected_total_mRNAs = expected_total_mRNAs))
        }
        return(census_transcript_counts)
    }
}
