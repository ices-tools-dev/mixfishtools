#' Bar chart of landings by stock and metier
#'
#' @description Bar chart of landings by stock and by metier/gear groupings.
#' Used in WGMIXFISH-ADVICE
#'
#' @param data data.frame Contains information on the landings (or catch) in
#'   tonnes by stock and metier/gear grouping from the fleet data used at
#'   WGMIXFISH-ADVICE. Stock variable names (`stock`) should match those of
#'   \code{\link[mixfishtools]{refTable}}.
#'
#' @param refTable data.frame A look-up reference table for stocks and associated attributes.
#' The \code{refTable} data.frame lists stock names and
#' corresponding colours for consistency across plots. To be used as a look-up
#' table in converting between variable stock names and printed ones.
#'
#' \itemize{
#'   \item 1) stock - ICES stock codes used in advice
#'   \item 2) order - stock order to be used in plots
#'   \item 3) col - stock colors for plots (e.g. pals::brewer.paired())
#'   \item 4) stock_short - short stock name used in mixed fishery model
#' }
#'
#' @param xlab character X-axis label (Default (blank): `xlab = ""`)
#' @param ylab character Y-axis label (Default: `ylab = "Landings [t]"`)
#' @param fillLegendTitle character Fill legend title
#'
#' Other required variables include: `metier` which defines the metier code or gear
#' grouping code; `value` the value of landings (or catch) for each `stock` and `metier`
#'
#' @details Users will need to provide the data object to produce the plot.
#'
#' @return plot output of class ggplot
#' @import ggplot2
#'
#' @export
#'
#' @examples
#'
#' # make example data
#' data(stfMtStkSum)
#' head(stfMtStkSum)
#' data(refTable)
#' head(refTable)
#'
#' data <- stfMtStkSum
#'
#' # add metier_cat
#' tmp <- strsplit(data$metier, ".", fixed = TRUE)
#' data$metier_cat <- unlist(lapply(tmp, FUN = function(x){x[1]}))
#'
#' # select final data year and a single scenario, and aggregated total landings
#' # by stock and metier
#' datYr <- 2020
#' data <- subset(data, year == datYr & scenario == "min")
#' agg <- aggregate(landings ~ metier_cat + stock, data, FUN = sum, na.rm = TRUE)
#'
#' # In the North Sea model, all Nephrops FUs area aggregated together
#' agg$isNEP <- seq(nrow(agg)) %in% grep("NEP", agg$stock)
#' agg1 <- subset(agg, !isNEP)[,c(1:3)]
#' agg2 <- aggregate(landings ~ metier_cat, data = subset(agg, isNEP),
#'   FUN = sum, na.rm = TRUE)
#' agg2$stock <- "Nephrops"
#' agg <- merge(agg1, agg2, all = TRUE)
#' agg <- agg[,c("stock", "metier_cat", "landings")]
#'
#' names(agg) <- c("stock", "metier","value")
#' agg
#'
#' # subset included metiers
#' metIncl <- c("TR1", "TR2", "BT1", "BT2", "GN1", "GT1", "LL1", "beam_oth",
#'   "pots", "OTH", "MIS")
#' agg <- subset(agg, metier %in% metIncl)
#'
#' # replace stock with ICES stock code
#' agg$stock <- refTable$stock[match(agg$stock, refTable$stock_short)]
#'
#' plot_landByMetStock(data = agg, refTable)
#'
plot_landByMetStock <- function(data, refTable, xlab = "",
  ylab = "Landings ['000 t]", fillLegendTitle = "Stock"){

  if(is.null(data)){
    stop("object, data, does not exist")
  }

  if(!all(c("metier","stock","value")%in% colnames(data))){
    stop("Column names not as expected")
  }

  # get colour scale by merging with refTable
  data <- left_join(data,refTable,by="stock")
  tmp <- unique(data[,c("stock", "col", "order")])
  tmp <- tmp[order(tmp$order),]
  stkColors <- tmp$col
  names(stkColors) <- tmp$stock
  stkColorScale <- scale_colour_manual(name = fillLegendTitle, values = stkColors,
    aesthetics = c("colour", "fill"))

  data$stock <- factor(data$stock, levels = tmp$stock) # orders the stocks

  none <- element_blank()

  p <- ggplot(data,aes(x = metier, y = value/1000, fill=stock)) +
    geom_bar(stat="identity", position = "stack") +
    theme(panel.grid.major = none, panel.grid.minor = none) +
    theme(panel.background = none) +
    theme(panel.border = none) +
    theme(axis.line = element_line(colour = "grey50")) +
    theme_bw() +
    labs(x = xlab, y = ylab, fill = fillLegendTitle)  +
    theme(axis.text.x = element_text(angle = 45, size = 8, vjust=1, hjust=1))  +
    theme(legend.text=element_text(size=8),
          legend.title=element_text(size=8)) +
    theme(axis.text = element_text(lineheight=0.8, size=8)) +
    theme(axis.title = element_text(size=10)) +
    geom_bar(stat="identity", position = "stack", colour="black") +
    stkColorScale

  return(p)
}
