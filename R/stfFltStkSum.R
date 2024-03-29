#' @name stfFltStkSum
#' @title Data.frame containing short-term forecast summary of catch-related
#'   variables per stock and fleet combination
#'
#' @description The \code{stfFltStkSum} data.frame is an output of
#'   `FLBEIA::fltStkSum()`. Provides example data for use in
#'   `plot_effortFltStk`.
#'
#' \itemize{
#'   \item scenario - advice scenario
#'   \item year - advice year
#'   \item fleet - fleet names
#'   \item stock - stock names used in mixed fishery model
#'   \item Advice_name - stock code labels
#'   \item Limitation - by fleet, identifies limiting stocks
#'     (levels: choke, interm., least). Describes the most-, intermediate-,
#'     and least-limiting quotas based on quota uptake ratios (quotaUpt).
#'   \item quotaEffort - effort required to take up quota in advice year
#'   \item sqE_effort - status quo effort from last data year
#'   \item rel_effort - ratio of quota_effort / sqE_effort
#' }
#'
#' @docType data
#' @format data.frame
#' @source WGMIXFISH-Advice 2021, North Sea case study
#'   (https://github.com/ices-taf/2021_NrS_MixedFisheriesAdvice)
#' @usage data(stfFltStkSum)
#' @keywords datasets
#' @examples
#'
#' data(stfFltStkSum)
#' head(stfFltStkSum)
#'
#'
NULL


