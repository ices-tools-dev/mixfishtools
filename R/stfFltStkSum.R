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
#'   \item iter - iteration number
#'   \item catch -
#'   \item landings -
#'   \item discards -
#'   \item discRat -
#'   \item price -
#'   \item tacshare - fraction of the total stock quota for a given fleet
#'   \item quota - advised catch quota
#'   \item quotaUptake - effort required to take up quota
#'   \item choke - (logical) is stock the limiting one for the fleet
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


