#' @name stfFltSum
#' @title Data.frame containing short-term forecast summary of catch-related
#'   variables per fleet
#'
#' @description The \code{stfFltSum} data.frame is an output of
#'   `FLBEIA::fltSum()`. Provides example data for use in
#'   `plot_effortFltStk`.
#'
#' \itemize{
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
#' @source WGMIXFISH-Advice 2021, North Sea case study.
#'   (https://github.com/ices-taf/2021_NrS_MixedFisheriesAdvice)
#' @usage data(stfFltSum)
#' @keywords datasets
#' @examples
#'
#' data(stfFltSum)
#' head(stfFltSum)
#'
#'
NULL


