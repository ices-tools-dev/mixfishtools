#' @name stfMtStkSum
#' @title Data.frame containing short-term forecast summary of catch-related
#'   variables per stock, fleet, and metier combination
#'
#' @description The \code{stfMtStkSum} data.frame is an output of
#'   `FLBEIA::mtStkSum()`. Provides example data for use in
#'   `plot_catchComp`.
#'
#' \itemize{
#'   \item scenario - advice scenario
#'   \item year - advice year
#'   \item fleet - fleet names
#'   \item metier - metier names
#'   \item stock - stock names used in mixed fishery model
#'   \item iter - iteration number
#'   \item catch -
#'   \item landings -
#'   \item discards -
#'   \item discRat -
#'   \item price -
#' }
#'
#' @docType data
#' @format data.frame
#' @source WGMIXFISH-Advice 2021, North Sea case study
#'   (https://github.com/ices-taf/2021_NrS_MixedFisheriesAdvice)
#' @usage data(stfMtStkSum)
#' @keywords datasets
#' @examples
#'
#' data(stfMtStkSum)
#' head(stfMtStkSum)
#'
#'
NULL

