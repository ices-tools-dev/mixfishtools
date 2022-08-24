#' Fleet effort to uptake stock quotas
#'
#' @description Plots the effort required for the uptake of each stock's quota
#'   by fleet. Most- (choke), intermediate- and least-limiting stocks are also
#'   denoted.
#'
#' @param data data.frame Contains information on effort required to uptake
#'   quotas by fleet and stock, plus designation of each stock's limitation
#'   status to the fleet's fishing effort. Stock variable names (`Advice_name`) should
#'   match those of \code{\link[mixfishtools]{refTable}}.
#'   Other required variables include: `Limitation` - defines, by fleet, the
#'   most- (`choke`), least- (`least`), and intermediate-limiting (`interm.`)
#'   stocks; `effortQuota` - the effort, by fleet, required to take up the
#'   quota share of each stock; sqE_effort).
#' @param refTable data.frame Contains stock look-up information for consistent
#'   plotting of stocks. `Advice_name` defines the stock names corresponding to
#'   `data` object. `col` defines the color used to fill bars in plot.
#'   `order` defines the order of stocks in the plot facets.
#' @param xlab character X-axis label
#' @param ylab character Y-axis label
#' @param fillLegendTitle character Fill legend title
#' @param colLegendTitle character Color legend title
#'
#' @details Users will need to provide the data and reference table objects to
#'   produce the plot.
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
#' data(refTable) # reference table with stock advice names, colors, order, etc.
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
#'
#' # add "Limitation", showing which stocks are most- ("choke"),
#' # intermediate- ("interm.") or least-limiting in terms of effort for a
#' # given fleet. Choke and least-limiting stocks are determined from the
#' # restrictive stock with the highest and lowest quota uptakes in the "min"
#' # and "max" scenarios, respectively.
#' restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
#'   "SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9")
#' fls <- unique(df$fleet)
#' df2 <- vector("list", length(fls))
#' names(df2) <- fls
#' for(i in seq(fls)){
#'   tmp <- subset(df, fleet == fls[i])
#'   tmp$Limitation <- "interm." # initial setting to intermediate limitation
#'
#'   chokeStk <- subset(tmp, stock %in% restr.stks)
#'   chokeStk <- chokeStk$stock[which.max(chokeStk$quotaUpt)]
#'   tmp$Limitation[which(tmp$stock == chokeStk)] <- "choke"
#'
#'   leastLimStk <- subset(stfFltStkSum, scenario == "max" & year == advYr &
#'     fleet == fls[i] & stock %in% restr.stks)
#'   leastLimStk <- leastLimStk$stock[which.min(leastLimStk$quotaUpt)]
#'   tmp$Limitation[which(tmp$stock == leastLimStk)] <- "least"
#'
#'   df2[[i]] <- tmp
#' }
#' df2 <- do.call("rbind", df2)
#'
#' # add Advice_name corresponding to refTable
#' df2 <- merge(x = df2, y = refTable[,c("stock", "Advice_name")], all.x = TRUE)
#'
#' # plot
#' p <- plot_effortFltStk(data = df2, refTable = refTable)
#' # png("effortFltStk1.png", width = 8, height = 10, units = "in", res = 400)
#' # print(p); dev.off()
#'
#' # adjust ggplot2 settings
#' p <- p + theme(text = element_text(size = 12))
#' # png("effortFltStk2.png", width = 8, height = 10, units = "in", res = 400)
#' #  print(p); dev.off()
#'
plot_effortFltStk <- function(data, refTable,
  xlab = "Stock", ylab = "KW days ('000)",
  fillLegendTitle = "Effort stock", colLegendTitle = "Limiting stock"){

  stkFill <- data.frame(stock = unique(data$stock))
  stkFill <- merge(x = stkFill, y = refTable, all.x = TRUE)
  stkFill <- stkFill[order(stkFill$order),]
  stkColors <- stkFill$col
  names(stkColors) <- stkFill$Advice_name

  stkColorScale <- scale_colour_manual(
    name = "Effort stock", values = stkColors, aesthetics = c("fill"))

  data$Advice_name <- factor(data$Advice_name, levels = stkFill$Advice_name)

  p <- ggplot(data) +
    aes(x = Advice_name, y = quotaEffort,
      fill = Advice_name, color = Limitation, group = fleet) +
    facet_wrap(fleet~., scales = 'free_y', ncol = 3) +
    geom_bar(stat = 'identity', size = 1, alpha = 0.7) +
    geom_hline(data=data, aes(yintercept = sqEffort), lty=2) +
    scale_color_manual(values = c('red', NA, 'green'), na.value = NA) +
    xlab(xlab) +
    ylab(ylab) +
    labs(fill = fillLegendTitle, color = colLegendTitle) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3, size = 7),
      panel.grid = element_blank(),
      text = element_text(size = 9),
      strip.text = element_text(size = 9)) +
    guides(colour = guide_legend(order = 2),
      fill = guide_legend(order = 1)) +
    stkColorScale

  return(p)
}






