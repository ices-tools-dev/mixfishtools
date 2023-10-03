#' Headline advice plot
#'
#' @description Plot summarizing over- and under-quota catches by stock and
#'   scenario. Dashed line displays quota by stock. Colored background further
#'   emphasizes over- and under-quota catches. Used as the headline plot in
#'   WGMIXFISH-ADVICE.
#'
#' @param data data.frame Contains catch (`catch`) by scenario (`scenario`) and
#'   stock (`stock`).
#' @param adv data.frame Contains advice (`advice`) by stock (`stock`). Optional
#'   upper (`upper`) and lower (`lower`) advice limits can be included.
#' @param ofwhich logical. If TRUE an of which limit will be plotted.
#'   Requires a 'catch_ofwhich' column in data and an 'advice_ofwhich' column in adv.
#' @param xlab character X-axis label (Default: `xlab = "Scenario"`)
#' @param ylab character Y-axis label (Default: `ylab = "Catch [t]"`)
#'
#' @return plot output of class ggplot
#' @import ggplot2
#' @export
#'
#' @examples
#'
#' # make example data
#' data(stfFltStkSum)
#' head(stfFltStkSum)
#'
#' # subset data to advice year and restrictive stocks
#' advYr <- 2022 # advice year
#' restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
#'   "SOL-NS", "TUR", "WHG-NS", "WIT")
#' stfFltStkSum <- subset(stfFltStkSum, year == advYr & stock %in% restr.stks)
#'
#' # data for plotting (catch by scenario and stock)
#' catchScenStk <- aggregate(catch ~ scenario + stock, data = stfFltStkSum, FUN = sum)
#'
#' # re-order scenarios (sq_E, max, min, ... )
#' catchScenStk$scenario <- factor(catchScenStk$scenario,
#'   levels = c("sq_E", "max", "min", "cod-ns"),
#'   labels = c("sq_E", "max", "min", "cod-ns"))
#' head(catchScenStk)
#'
#' catchRange <- rbind(
#'   data.frame(stock = "COD-NS", advice = 14276, lower = 9701, upper = 14276),
#'   data.frame(stock = "HAD", advice = 128708, lower = 111702, upper = 128708),
#'   data.frame(stock = "PLE-EC", advice = 6365, lower = 4594, upper = 6365),
#'   data.frame(stock = "PLE-NS", advice = 142507, lower = 101854, upper = 195622),
#'   data.frame(stock = "POK", advice = 49614, lower = 30204, upper = 49614),
#'   data.frame(stock = "SOL-EC", advice = 1810, lower = 1068, upper = 2069),
#'   data.frame(stock = "SOL-NS", advice = 15330, lower = 9523, upper = 21805),
#'   data.frame(stock = "TUR", advice = 3609, lower = 2634, upper = 4564),
#'   data.frame(stock = "WHG-NS", advice = 88426, lower = 70169, upper = 91703),
#'   data.frame(stock = "WIT", advice = 1206, lower = 875, upper = 1206)
#' )
#'
#' # plot without range
#' p <- plot_catchScenStk(data = catchScenStk, adv = catchRange[,1:2])
#' print(p)
#'
#' # plot with range
#' p <- plot_catchScenStk(data = catchScenStk, adv = catchRange)
#' print(p)
#'
#' # export plot
#' # png("catchScenStk1.png", width = 6, height = 5, units = "in", res = 400)
#' # print(p); dev.off()
#'
plot_catchScenStk <- function(data, adv, ofwhich = FALSE,
  xlab = "Scenario", ylab = "Catch [t]"){

  adv$scenario <- 1 # dummy variable to allow plotting on facets

  # add dummy advice range values if missing
  if(!"upper" %in% names(adv)){
    adv$upper <- adv$advice
  }
  if(!"lower" %in% names(adv)){
    adv$lower <- adv$advice
  }

  p <- ggplot(data = data) + aes(x = scenario, y = catch) +
    facet_wrap(~ stock, scales = 'free_y') +
    geom_rect(stat = "identity", data = adv,
              mapping = aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = advice, y = 1),
              fill = 'green', alpha = 0.25) +
    geom_rect(stat = "identity", data = adv,
              mapping = aes(xmin = -Inf, xmax = Inf, ymin = advice, ymax = upper, y = 1),
              fill = 'yellow', alpha = 0.25) +
    geom_rect(stat = "identity", data = adv,
              mapping = aes(xmin = -Inf, xmax = Inf, ymin = upper, ymax = Inf, y = 1),
              fill = 'red', alpha = 0.25) +
    {if(ofwhich==T) {
      geom_rect_pattern(stat = "identity", data = adv,
                        mapping = aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = advice_ofwhich, y = 1),
                        fill = NA,
                        pattern = "circle",
                        pattern_colour = NA,
                        pattern_fill = "#85AD00") }} +
    geom_hline(data = adv, mapping = aes(yintercept = advice), lty = 1, col = "black") +
    geom_hline(data = adv, mapping = aes(yintercept = upper), lty = 3) +
    geom_hline(data = adv, mapping = aes(yintercept = lower), lty = 3) +
    {if(ofwhich==T){
      geom_hline(data = adv, mapping = aes(yintercept = advice_ofwhich), lty = 3, colour = "#85AD00") }} +
    geom_col(width = 0.5, fill = "grey35", color = "grey35") +
    {if(ofwhich==T){
      geom_col_pattern(aes(x = scenario, y = catch_ofwhich), pattern = "crosshatch",
                       width = 0.5, pattern_colour= NA, pattern_fill = "#85AD00") }} +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    xlab(label = xlab) +
    ylab(label = ylab) +
    theme(
      text = element_text(size = 10), legend.position="none",
      axis.text.x = element_text(angle = 90, vjust = 0, hjust=1))

  return(p)
}
