#' Plot fleet landings taken up relative to recent landings / quota
#'
#' @description Plot of a fleets catch difference from the recent catches or the quota.
#'   By fleet. Most- and least-limiting stocks are also
#'   denoted. Testing in response to WKMIXFISH2.
#'
#' @param data data.frame Contains information on catch by fleet and stock
#' @param basis is a character vector with the basis on which to compare the
#'   scenario landings, either 'recent_catch' or 'Quota'. When 'recent_catch' is
#'   used, the average landings from the defined years (argument `dataYrs`) is used as the
#'   reference instead of the advice year quota ('Quota')
#' @param dataYrs is a vector of years on which to base recent catches. Used
#'   when  `basis = 'recent_catch'`.
#' @param advYr is a vector of the year in which the scenario catches are generated.
#' @param sc is a vector with the scenario to plot, e.g. "min"
#' @param fleets_excl is a vector of fleet names not to plot, e.g. "OTH_OTH"
#' @param refTable data.frame Contains stock look-up information for consistent
#'   plotting of stocks. `Advice_name` defines the stock names corresponding to
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
#'
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @return plot output of class ggplot
#'
#' @examples
#'
#' # make example data
#' data(refTable) # reference table with stock advice names, colors, order, etc.
#' data(stfFltStkSum) # summary of fleet/stock-related catch variables
#' advYr <- 2022 # advice year
#'
#' # replace short stock names with ICES stock codes
#' stfFltStkSum$stock <- refTable$stock[match(stfFltStkSum$stock,
#'   refTable$stock_short)]
#'
#'
#' p <- plot_catch_change(data = stfFltStkSum,
#'  basis = "Quota",
#'  dataYrs = 2020:2022,
#'  advYr = advYr,
#'  sc = "min",
#'  fleets_excl = "OTH_OTH",
#'  refTable = refTable,
#'  xlab = "Stock",
#'  ylab = "landings change (tonnes)",
#'  fillLegendTitle = "Stock",
#'  colLegendTitle = "Limiting stock")
#'
#' print(p)
#'
#' # export plot
#' # png("plot_change.png", width = 8, height = 10, units = "in", res = 400)
#' # print(p); dev.off()
#'
plot_catch_change <- function(data = NULL,
  basis = "recent_catch",
  dataYrs = NULL,
  advYr = NULL,
  sc = "min",
  fleets_excl = NULL,
  refTable = NULL,
  xlab = "Stock",
  ylab = "catch change (tonnes)",
  fillLegendTitle = "Stock",
  colLegendTitle = "Limiting stock") {

## Compute the baseline
if(basis=="recent_catch") {
base <- filter(data, scenario==sc, year %in% dataYrs, !fleet %in% fleets_excl) %>%
  group_by(fleet, stock) %>%
  summarise(catch = mean(catch, na.rm = TRUE))
}
if(basis=="Quota") {
  base <- filter(data, scenario==sc, year %in% advYr, !fleet %in% fleets_excl) %>%
    group_by(fleet, stock) %>% select(-catch) %>% rename(catch=quota) %>%
    summarise(catch = mean(catch, na.rm = TRUE))
}

## The scenario projection
proj <- filter(data, scenario == sc, year == advYr, !fleet %in% fleets_excl) %>%
  group_by(fleet, stock, choke)

## Add the projection and the choke stock
base$proj <- proj$catch[match(paste0(base$fleet, base$stock),
  paste0(proj$fleet, proj$stock))]

base$proj[is.nan(base$proj) | is.na(base$proj)] <- 0

base$choke <- proj$choke[match(paste0(base$fleet, base$stock),
			       paste0(proj$fleet, proj$stock))]
## Compute the difference
base$diff <- base$proj - base$catch

## Plot
## Order the facets by most landings to least landings
fac_ord <- base %>% group_by(fleet) %>%
  summarise(diff = sum(diff))
flt_ord <- c(fac_ord[order(-abs(fac_ord$diff)),"fleet"] %>% as.data.frame())$fleet

## Add the colours
stkFill <- data.frame(stock = unique(data$stock))
stkFill <- merge(x = stkFill, y = refTable, all.x = TRUE)
stkFill <- stkFill[order(stkFill$order),]
stkColors <- stkFill$col
names(stkColors) <- stkFill$stock

base$stock <- factor(base$stock, levels = stkFill$stock)

p1 <- ggplot(base, aes(x = stock, y = diff, fill = stock, colour = choke, group = fleet)) +
  facet_wrap(~factor(fleet, levels = c(flt_ord)), scales = "free_y", ncol = 4) +
  geom_bar(stat = "identity", aes(colour= choke)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c('green', 'red'), na.value = NA,
    limits = c('least','choke'), labels = c("least", "most (*)")) +
  xlab(xlab) + ylab(ylab) +
  geom_text(data = filter(base, choke == "choke"), aes(label = "*"),
    nudge_y = filter(base,choke=="choke")$diff * .2, show.legend = FALSE) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3, size = 7),
    panel.grid = element_blank(),
    text = element_text(size = 9),
    strip.text = element_text(size = 9)) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  labs(fill = fillLegendTitle, color = colLegendTitle)

print(p1)

}
