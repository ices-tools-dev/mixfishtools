#' @name effortFltStk
#' @title Look-up reference table for stocks and associated attributes
#'
#' @description The \code{effortFltStk} dataframe lists stock names and
#'   corresponding colors for consistency across plots. To be used as a look-up
#'   table in converting between variable stock names and printed ones.
#'
#' \itemize{
#'   \item 1) fleet - fleet names
#'   \item 2) stock - stock names used in mixed fishery model
#'   \item 3) Advice_name - stock code labels
#'   \item 4) Limitation - by fleet, identifies limiting stocks
#'     (levels: choke, interm., least)
#'   \item 5) quotaEffort - effort required to take up quota in advice year
#'   \item 6) sqE_effort - status quo effort from last data year
#' }
#'
#' @docType data
#' @format bla bla
#' @source WGMIXFISH-Advice 2021, North Sea case study.
#'   \url{[https://github.com/ices-taf/2021_NrS_MixedFisheriesAdvice]}
#' @usage data(effortFltStk)
#' @keywords datasets
#' @examples
#'
#' data(effortFltStk)
#' head(effortFltStk)
#'
#'
NULL


