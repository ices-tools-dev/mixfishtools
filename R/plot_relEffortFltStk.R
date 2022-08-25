#' Relative fleet effort to uptake stock quotas
#'
#' @description Plots the effort required for the uptake of each stock's quota
#'   by fleet. Most- (choke), intermediate- and least-limiting stocks are also
#'   denoted.
#'
#' @param data data.frame Contains information on relative effort (to status
#'   quo effort, `var`) required to uptake quotas by fleet (`fleet`) and
#'   stock (`scenario`).
#' @param limits vector Two value vector with lower and upper limits for fill
#'   colors (Default: `limits = c(-100,100)`)
#' @param xlab character X-axis label
#' @param ylab character Y-axis label
#' @param fillLegendTitle character Fill legend title
#'
#' @details Users will need to provide the data.table objects to
#'   produce the plot. These effort values are presented in terms
#'   of percent deviation from status quo effort.
#'   In the best case, effort for complete quota uptake by
#'   fleet may be derived from scenarios using each restrictive
#'   stock one at a time. In the following example, however, these effort
#'   levels are derived by linearly extrapolating the quota uptake levels
#'   by the effort of the "min" scenario. This is strictly linear when quotas
#'   are based on partial F, as in FCube. In FLBEIA, quotas are based on catch
#'   (or landings), which may deviate from a linear relationship at close to
#'   full exploitation (although not likely to result from an ICES harvest
#'   control rule).
#'
#' @return plot output of class ggplot
#' @export
#'
#' @examples
#'
#' data(stfFltSum) # summary of fleet-related variables (e.g. effort)
#' data(stfFltStkSum) # summary of fleet/stock-related catch variables
#'
#' # Prepare data
#'
#' # make data containing effort associated with tac.share uptake by
#' # fleet/stock in advice year (`quotaEffort`). Status quo effort also
#' # needed for reference (`sqEffort`).
#' # For FCube, calculation ofquotaEffort is straightforward given the linear
#' # relationship between F and effort. With FLBEIA, the following uses the
#' # assumption of a linear relationship between catch and effort
#' advYr <- 2022 # advice year
#' df <- subset(stfFltStkSum, scenario == "min" & year == advYr)
#' eff <- subset(
#'   stfFltSum, scenario == "min" & year == advYr)[,c("fleet", "effort")]
#' sqEff <- subset(
#'   stfFltSum, scenario == "sq_E" & year == advYr)[,c("fleet", "effort")]
#' names(sqEff)[2] <- "sqEffort"
#' eff <- merge(x = eff, y = sqEff, all.x = TRUE)
#' df <- merge(x = df, y = eff, all.x = TRUE)
#' df$quotaEffort <- df$effort / df$quotaUpt
#' df$relEffort <- df$quotaEffort / df$sqEffort
#' df$scenario <- df$stock
#' restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
#'   "SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9")
#' df <- subset(df, stock %in% restr.stks)
#'
#' # provide limits to max and min values
#' df$var <- ifelse(100*(df$relEffort-1) > 100, 100, 100*(df$relEffort-1))
#'
#' # plot
#' p <- plot_relEffortFltStk(data = df)
#' print(p)
#'
#' # png("relEffortFltStk1.png", width = 4, height = 6, units = "in", res = 400)
#' # print(p); dev.off()
#'
#'
plot_relEffortFltStk <- function(data,
  limits = c(-100,100),
  xlab = "Stock", ylab = "Fleet",
  fillLegendTitle = "Variation\n in effort"){

  p <- ggplot(data) +
    aes(x = scenario, y = fleet,  fill = var) +
    geom_tile(color = "white", lwd = 1, linetype = 1) +
    scale_fill_gradient2(name = "Variation\n in effort",
      limits = limits,
      low = "red4",
      mid = "grey90",
      high = "blue4",
      midpoint = 0) +
    # theme_hc() +
    theme(panel.background = element_blank(),
      strip.background = element_rect(colour=NA, fill=NA),
      axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.3),
      text = element_text(size = 9)) +
    coord_fixed() +
    xlab(xlab) +
    ylab(ylab)

  return(p)
}


