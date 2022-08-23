#' Fleet effort to uptake stock quotas
#'
#' @param data data.frame Contains information on effort required to uptake
#'   quotas by fleet and stock. Variable names should match those of
#'   \code{\link[mixfishtools]{effortFltStk}}
#'   (i.e. stock, Advice_name, Limitation, effortQuota, sqE_effort).
#' @param refTable data.frame Contains stock look-up information for consistent
#'   plotting of stocks. Variable names should match those of
#'   \code{\link[mixfishtools]{refTable}}.
#'
#' @return plot output of class ggplot
#' @export
#'
#' @examples
#' data(refTable) # reference table with stock advice names, colors, order, etc.
#' data(stfFltSum) # summary of fleet-related variables (e.g. effort)
#' data(stfFltStkSum) # summary of fleet/stock-related catch variables
#'
#' # make data containing effort associated with tac.share uptake by
#' # fleet/stock in advice year (`quotaEffort`). Status quo effort also
#' # needed for reference (`sqEffort`). `Limitation` defines whether a stock
#' # is the most- ("choke"), intermediate- ("interm.") or least-limiting for
#' # a given fleet
#' advYr <- 2022 # advice year
#' df <- subset(stfFltStkSum, scenario == "max" & year == advYr)
#' eff <- subset(
#'   stfFltSum, scenario == "max" & year == advYr)[,c("fleet", "effort")]
#' sqEff <- subset(
#'   stfFltSum, scenario == "sq_E" & year == advYr)[,c("fleet", "effort")]
#' names(sqEff)[2] <- "sqEffort"
#' eff <- merge(x = eff, y = sqEff, all.x = TRUE)
#' df <- merge(x = df, y = eff, all.x = TRUE)
#' df$quotaEffort <- df$effort / df$quotaUpt
#'
#' # add "Limitation", showing which stocks are most- ("choke"),
#' # intermediate- ("interm.") or least-limiting in terms of effort for a
#' # given fleet.
#' # For FCube, this is straightforward given the linear relationship
#' # between F and effort. With FLBEIA, we use the quota uptake info from
#' # the min and max scenarios
#' restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
#'   "SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9")
#' fls <- unique(df$fleet)
#' df2 <- vector("list", length(fls))
#' names(df2) <- fls
#' for(i in seq(fls)){
#'   tmp <- subset(df, fleet == fls[i])
#'   tmp$Limitation <- "interm." # originally set all to intermediate limitation
#'
#'   leastLimStk <- subset(tmp, stock %in% restr.stks)
#'   leastLimStk <- leastLimStk$stock[which.min(leastLimStk$quotaUpt)]
#'   tmp$Limitation[which(tmp$stock == leastLimStk)] <- "least"
#'
#'   chokeStk <- subset(stfFltStkSum, scenario == "min" & year == advYr &
#'     fleet == fls[i] & stock %in% restr.stks)
#'   chokeStk <- chokeStk$stock[which.max(chokeStk$quotaUpt)]
#'   tmp$Limitation[which(tmp$stock == chokeStk)] <- "choke"
#'
#'   df2[[i]] <- tmp
#' }
#' df2 <- do.call("rbind", df2)
#'
#' # add Advice_name corresponding to refTable
#' df2 <- merge(x = df2, y = refTable[,c("stock", "Advice_name")], all.x = TRUE)
#'
#'
#'
#'
#'
#' p <- plot_effortFltStk(data = df2, refTable = refTable)
#' print(p)
#' png("test.png", width = 8, height = 10, units = "in", res = 400); print(p); dev.off()
#'
plot_effortFltStk <- function(data, refTable){

  stkFill <- data.frame(stock = unique(data$stock))
  stkFill <- merge(x = stkFill, y = refTable, all.x = TRUE)
  stkFill <- stkFill[order(stkFill$order),]
  stkColors <- stkFill$col
  names(stkColors) <- stkFill$Advice_name

  stkColorScale <- scale_colour_manual(name = "Effort stock", values = stkColors,
    aesthetics = c("fill"))

  data$Advice_name <- factor(data$Advice_name, levels = stkFill$Advice_name)


  p <- ggplot(data) +
    aes(x = Advice_name, y = quotaEffort, fill = Advice_name, color = Limitation, group = fleet) +
    facet_wrap(fleet~., scales = 'free_y', ncol = 3) +
    geom_bar(stat = 'identity', size = 1, alpha = 0.7) +
    geom_hline(data=data, aes(yintercept = sqEffort), lty=2) +
    scale_color_manual(values = c('red', NA, 'green'), na.value = NA) +
    xlab("Stock") +
    ylab('KW days (000)') +
    labs(fill = 'Effort stock', color = 'Limiting Stock') +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3, size = 7),
      panel.grid = element_blank(),
      text = element_text(size = 9),
      strip.text = element_text(size = 9)) +
    stkColorScale

  return(p)
}






