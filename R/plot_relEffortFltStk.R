#' Relative fleet effort to uptake stock quotas
#'
#' @description Plot of relative effort required to uptake each stock's quota
#'   by fleet. To be used in fishery overviews.
#'
#' @param data data.frame Contains information on relative effort (to status
#'   quo effort, `var`) required to uptake quotas by fleet (`fleet`) and
#'   stock (`scenario`).
#' @param limits vector Two value vector with lower and upper limits for fill
#'   colors (Default: `limits = c(-100,100)`)
#' @param xlab character X-axis label (Default: `xlab = "Stock"`)
#' @param ylab character Y-axis label (Default: `ylab = "Fleet"`)
#' @param fillLegendTitle character Fill legend title
#'   (Default: `fillLegendTitle = "Variation in effort"`)
#'
#' @details Users will need to provide the data and reference table objects to
#'   produce the plot.
#'   In the best case, effort associated with complete quota uptake by
#'   fleet may be derived from scenarios restricting fleet catch one
#'   stock at a time. In the following example, however, effort
#'   levels are derived by linearly extrapolating the quota uptake levels
#'   by the effort of the "min" scenario. This is strictly linear when quotas
#'   are based on partial F, as in FCube. In FLBEIA, quotas are based on catch
#'   (or landings), which may deviate from a linear relationship when a stock
#'   is close full exploitation (should not result from an ICES harvest control
#'   rule).
#'
#' @return plot output of class ggplot
#' @import ggplot2
#' @export
#'
#' @examples
#' # make data
#' data(refTable) # reference table with stock advice names, colors, order, etc.
#' data(stfFltSum) # summary of fleet-related variables (e.g. effort)
#' data(stfFltStkSum) # summary of fleet/stock-related catch variables
#'
#' ## get data from advice year
#' advYr <- 2022 # advice year
#' df <- subset(stfFltStkSum, scenario == "min" & year == advYr)
#'
#' eff <- subset(
#'   stfFltSum, scenario == "min" & year == advYr)[,c("fleet", "effort")]
#' sqEff <- subset(
#'   stfFltSum, scenario == "sq_E" & year == advYr)[,c("fleet", "effort")]
#' names(sqEff)[2] <- "sqEffort"
#' eff <- merge(x = eff, y = sqEff, all.x = TRUE)
#' df <- merge(x = df, y = eff, all.x = TRUE)
#' df$quotaEffort <- df$effort / df$quotaUpt
#' df$relEffort <- df$quotaEffort / df$sqEffort
#'
#' # df$scenario <- df$stock
#'
#' restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
#'   "SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9")
#' df <- subset(df, stock %in% restr.stks)
#'
#' # replace short stock names with ICES stock codes
#' df$stock <- refTable$stock[match(df$stock, refTable$stock_short)]
#'
#' # adjust stock order for the plot
#' df$stock <- factor(df$stock, levels = refTable$stock)
#'
#'
#' # convert to percentage change
#' df$var <- 100*(df$relEffort-1)
#'
#' # optional upper limit (e.g. 100)
#' df$var <- ifelse(df$var > 100, 100, df$var)
#'
#' # plot
#' p <- plot_relEffortFltStk(data = df)
#' print(p)
#'
#' # export plot
#' # png("relEffortFltStk1.png", width = 4, height = 6, units = "in", res = 400)
#' # print(p); dev.off()
#'
#'
plot_relEffortFltStk <- function(data,
  limits = c(-100,100),
  xlab = "Stock", ylab = "Fleet",
  fillLegendTitle = "Variation\n in effort"){

  p <- ggplot(data) +
    aes(x = stock, y = fleet,  fill = var) +
    geom_tile(color = "white", lwd = 1, linetype = 1) +
    scale_fill_gradient2(name = "Variation\n in effort",
      limits = limits,
      low = "red4",
      mid = "grey90",
      high = "blue4",
      midpoint = 0) +
    theme(panel.background = element_blank(),
      strip.background = element_rect(colour=NA, fill=NA),
      axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.3),
      text = element_text(size = 9)) +
    coord_fixed() +
    xlab(xlab) +
    ylab(ylab)

  return(p)
}


