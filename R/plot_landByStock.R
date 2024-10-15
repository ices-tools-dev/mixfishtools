#' Pie chart of landings by stock
#'
#' @description Pie chart of landings by stock. Used in WGMIXFISH-ADVICE
#'
#' @param data data.frame Contains information on the stocks to include and
#'  their landings (or catch) to plot. Stock variable names (`stock`) should
#'  match those of \code{\link[mixfishtools]{refTable}}.
#'  Other required variables include: `value` the value of landings (or catch)
#'  for each stock; and `col` which defines the fill colour as a hex colour code,
#'  by stock, to be used.
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
#' @param ylab character Y-axis label (Default: `ylab = "Landings [t]"`)
#' @param fillLegendTitle character Fill legend title
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
#' data(stfFltStkSum)
#' head(stfFltStkSum)
#'
#' data(refTable)
#' head(refTable)
#'
#' # select final data year and a single scenario, and aggregated total landings
#' datYr <- 2020
#' dat <- subset(stfFltStkSum, year == datYr & scenario == "min")
#' agg <- aggregate(landings ~ stock, dat, sum, na.rm = TRUE)
#'
#' # In the North Sea model, all Nephrops FUs area aggregated together
#' agg$isNEP <- seq(nrow(agg)) %in% grep("NEP", agg$stock)
#'
#' agg <- rbind(subset(agg, !isNEP)[,c(1:2)],
#'   data.frame(stock = "Nephrops", landings = sum(subset(agg, isNEP)$landings)))
#'
#' # replace stock with ICES stock code
#' agg$stock <- refTable$stock[match(agg$stock, refTable$stock_short)]
#'
#' names(agg) <- c("stock", "value")
#' agg
#'
#' plot_landByStock(data = agg, refTable)
#'
plot_landByStock <- function(data, refTable, ylab = "Landings [t]",
  fillLegendTitle = "Stock"){

  if(is.null(data)){
    stop("object, data, does not exist")
  }

  if(!all(c("stock", "value")%in% colnames(data))){
    stop("Column names not as expected")
  }

  # get colour scale by merging with refTable
  data <- left_join(data,refTable,by="stock")
  tmp <- unique(data[,c("stock","col", "order")])
  tmp <- tmp[order(tmp$order),]
  stkColors <- tmp$col
  names(stkColors) <- tmp$stock
  stkColorScale <- scale_colour_manual(name = fillLegendTitle, values = stkColors,
    aesthetics = c("colour", "fill"))

  # ensure plotting order
  data$stock <- factor(data$stock, levels = tmp$stock)

  p <- ggplot(data, aes(x = "", y = value, fill = stock, color = "black")) +
    geom_bar(width = 1, stat = "identity", colour = "black") +
    coord_polar("y", start = 0, direction = -1) +
    theme(axis.text.x = element_blank(),
      panel.border= element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank()) +
    labs(x="", y = ylab, fill = fillLegendTitle) +
    theme(legend.text = element_text(size = 10),
      legend.title = element_text(face = "bold", size = 10)
    ) +
    stkColorScale

  return(p)
}
