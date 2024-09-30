#' Plot fleet effort to uptake stock quotas
#'
#' @description Plot of effort required to uptake each stock's quota
#'   by fleet. Most- and least-limiting stocks are also
#'   denoted. Used in WGMIXFISH-ADVICE.
#'
#' @param data data.frame Contains information on effort required to uptake
#'   quotas by fleet and stock, plus designation of each stock's limitation
#'   status to the fleet's fishing effort. Stock variable names (`stock`)
#'   should match those of \code{\link[mixfishtools]{refTable}}.
#'   Other required variables include: `Limitation` - defines, by fleet, the
#'   most- (`most`), least- (`least`), and intermediate-limiting (`NA`)
#'   stocks; `quotaEffort` - the effort, by fleet, required to take up the
#'   quota share of each stock; `sqEffort` - status quo effort corresponding
#'   to most recent data year before forecast.
#' @param refTable data.frame Contains stock look-up information for consistent
#'   plotting of stocks. `stock` defines the stock names corresponding to
#'   `data` object. `col` defines the color used to fill bars in plot.
#'   `order` defines the order of stocks in the plot facets.
#' @param xlab character X-axis label (Default: `xlab = "Stock"`)
#' @param ylab character Y-axis label (Default: `ylab = "KW days ('000)"`)
#' @param fillLegendTitle character Fill legend title
#'   (Default: `fillLegendTitle = "Effort stock"`)
#' @param colLegendTitle character Color legend title
#'   (Default: `colLegendTitle = "Limiting stock"`)
#'
#' @details Users will need to provide the data and reference table objects to
#'   produce the plot.
#'   In the best case, effort associated with complete quota uptake by
#'   fleet (`data$quotaEffort`) may be derived from scenarios restricting fleet
#'   catch one stock at a time. In the following example, however, effort
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
#'
#' # example data for plot_effortFltStk --------------------------------------
#'
#' data(refTable) # reference table with stock advice names, colors, order, etc.
#' data(stfFltSum) # summary of fleet-related variables (e.g. effort)
#' data(stfFltStkSum) # summary of fleet/stock-related catch variables
#'
#' ## get data from advice year
#'
#' # catches by fleet and stock
#' advYr <- 2022 # advice year
#' df <- subset(stfFltStkSum, scenario == "min" & year == advYr)
#'
#' ## effort by fleet and scenario
#' eff <- subset(
#'  stfFltSum, scenario == "min" & year == advYr)[,c("fleet", "effort")]
#' sqEff <- subset(
#'  stfFltSum, scenario == "sq_E" & year == advYr)[,c("fleet", "effort")]
#' names(sqEff)[2] <- "sqEffort"
#' eff <- merge(x = eff, y = sqEff, all.x = TRUE)
#' df <- merge(x = df, y = eff, all.x = TRUE)
#' df$quotaEffort <- df$effort / df$quotaUpt
#'
#' ## Determine most- and least-limiting stock by fleet
#' # restrictive stocks
#' restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
#'  "SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9")
#' fls <- unique(df$fleet)
#' df2 <- vector("list", length(fls))
#' names(df2) <- fls
#' for(i in seq(fls)){
#'  tmp <- subset(df, fleet == fls[i])
#'  tmp$Limitation <- NA # initial NA setting for all stocks
#'
#'  # most-limiting (highest quota uptake in min scenario)
#'  mostLimStk <- subset(tmp, stock %in% restr.stks)
#'  mostLimStk <- mostLimStk$stock[which.max(mostLimStk$quotaUpt)]
#'  tmp$Limitation[which(tmp$stock == mostLimStk)] <- "most"
#'
#'  # least-limiting (lowest quota uptake in max scenario)
#'  leastLimStk <- subset(stfFltStkSum, scenario == "max" & year == advYr &
#'    fleet == fls[i] & stock %in% restr.stks)
#'  leastLimStk <- leastLimStk$stock[which.min(leastLimStk$quotaUpt)]
#'  tmp$Limitation[which(tmp$stock == leastLimStk)] <- "least"
#'
#'  # return result
#'  df2[[i]] <- tmp
#' }
#' df2 <- do.call("rbind", df2)
#'
#' # replace short stock names with ICES stock codes
#' df2$stock <- refTable$stock[match(df2$stock, refTable$stock_short)]
#'
#'
#' # plot
#' p <- plot_effortFltStk(data = df2, refTable = refTable)
#' # png("effortFltStk1.png", width = 8, height = 10, units = "in", res = 400)
#' # print(p); dev.off()
#'
#' # adjust ggplot2 settings
#' p <- p + theme(text = element_text(size = 12))
#' # png("effortFltStk2.png", width = 8, height = 10, units = "in", res = 400)
#' # print(p); dev.off()
#'
plot_effortFltStk <- function(data, refTable,
  xlab = "Stock", ylab = "KW days ('000)",
  fillLegendTitle = "Stock", colLegendTitle = "Limiting stock"){

  stkFill <- data.frame(stock = unique(data$stock))
  stkFill <- merge(x = stkFill, y = refTable, all.x = TRUE)
  stkFill <- stkFill[order(stkFill$order),]
  stkColors <- stkFill$col
  names(stkColors) <- stkFill$stock

  stkColorScale <- scale_colour_manual(
    name = fillLegendTitle, values = stkColors, aesthetics = c("fill"))

  # ensure plotting order
  data$stock <- factor(data$stock, levels = stkFill$stock)

  p <- ggplot(data) +
    aes(x = stock, y = quotaEffort,
      fill = stock, color = Limitation, group = fleet) +
    facet_wrap(fleet~., scales = 'free_y', ncol = 3) +
    geom_bar(stat = 'identity', size = 1, alpha = 0.7) +
    geom_hline(data=data, aes(yintercept = sqEffort), lty=2) +
    scale_color_manual(values = c('green', 'red'), na.value = NA,
      limits = c('least','most'), labels = c("least", "most (*)")) +
    geom_text(data = subset(data, Limitation == "most"), aes(label = "*"),
      vjust = 0.2, show.legend = FALSE) +
    xlab(xlab) +
    ylab(ylab) +
    stkColorScale +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3, size = 7),
      panel.grid = element_blank(),
      text = element_text(size = 9),
      strip.text = element_text(size = 9)) +
    guides(colour = guide_legend(order = 2),
      fill = guide_legend(order = 1)) +
    labs(fill = fillLegendTitle, color = colLegendTitle)


  return(p)
}






