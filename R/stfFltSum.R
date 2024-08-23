#' @name stfFltSum
#' @title Data.frame containing short-term forecast summary of catch-related
#'   variables per fleet
#'
#' @description The \code{stfFltSum} data.frame is an output of
#'   `FLBEIA::fltSum()`. Provides example data for use in
#'   `plot_effortFltStk`.
#'
#' \itemize{
#'   \item scenario - scenario name
#'   \item year - year
#'   \item fleet - fleet names
#'   \item iter - iteration number
#'   \item catch -
#'   \item landings -
#'   \item discards -
#'   \item capacity -
#'   \item effort -
#'   \item fcosts -
#'   \item vcosts -
#'   \item costs -
#'   \item grossValue -
#'   \item nVessels -
#'   \item discRat -
#'   \item grossSurplus -
#'   \item price -
#'   \item salaries -
#'   \item gva -
#'   \item profitability -
#'   \item fep -
#'   \item netProfit -
#'   \item quotaUptake - effort required to take up quota
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


