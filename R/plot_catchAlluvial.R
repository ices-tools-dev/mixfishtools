#' Alluvial plot of catch
#'
#' @param data data.frame Contains information on catch (or landings)
#'   corresponding to different fleet, metier, and stock combinations.
#'   Variable names should include `stock`, `fleet`, `metier`, and `value`.
#'   Stock variable levels (`stock`) should match those found in `refTable`
#'   containing  color and order information.
#' @param refTable data.frame Contains stock look-up information for consistent
#'   plotting of stocks. `stock` defines the stock names corresponding to
#'   `data` object. `col` defines the color used to fill bars in plot.
#'   `order` defines the order of stocks in the plot facets.
#' @param text_repel logical Should stratum labels  (`stock`, `fleet`, `metier`)
#'   be repelled to prevent overlapping (using `ggrepel::geom_text_repel`)
#'   (Default: `text_repel = FALSE`)
#' @param text_size numeric Value for text label size
#'   (Default: `text_size = 2`)
#' @param mult_x vector. Two values defining the multiplier to expand the
#'   x-axis (Default: `mult_x = c(0.1, 0.1)`). Passes values to
#'   `expansion(mult = mult_x)`.
#' @param nudge_x numeric. Value to nudge direction of stratum labels when
#'   `text_repel = TRUE`. (Default: `nudge_x = 1/3`)
#' @param stratum_width numeric. Width of the stratum bar
#'   (Default: `stratum_width = 1/3`)
#' @param stratum_col color definition. Color used to fll the non-stock strata
#'   (Default: `stratum_col = "white"`)
#' @param xlab character X-axis label (Default: `xlab = NULL` removes label)
#' @param ylab character Y-axis label (Default: `ylab = Catch [t]`)
#' @param fillLegendTitle character Fill legend title
#'   (Default: `fillLegendTitle = "Stock"`)
#' @param plotTitle character Plot title
#'   (Default: `plotTitle = NULL` removes title)
#' @param addLegend logical Should legend of stock fill colors be added
#'
#' @return plot output of class ggplot
#' @import ggplot2 ggalluvial ggrepel
#' @export
#'
#' @examples
#'
#' data("stfMtStkSum")
#' data("refTable")
#'
#' ## create catch data by fleet, metier, and stock
#' dataYr <- 2020
#' # add country for filter (if desired)
#' stfMtStkSum$country <- unlist(lapply(strsplit(stfMtStkSum$fleet, "_"),
#'   function(x){x[1]}))
#' # filter data
#' df <- subset(stfMtStkSum, year == 2020 & scenario == "min" &
#'   country == "BE")[,c("fleet", "metier", "stock", "landings")]
#' df$stock <- refTable$stock[match(df$stock, refTable$stock_short)]
#' names(df)[4] <- "value"
#'
#' plot_catchAlluvial(data = df, refTable = refTable)
#'
#' # repel labels
#' plot_catchAlluvial(data = df, refTable = refTable,
#'   text_repel = TRUE, text_size = 2)
#'
#' # repel labels and suppress legend
#' plot_catchAlluvial(data = df, refTable = refTable,
#'   text_repel = TRUE, text_size = 2, addLegend = FALSE)
#'
plot_catchAlluvial <- function(data, refTable,
  text_repel = FALSE,
  text_size = 2,
  mult_x = c(0.1, 0.1),
  nudge_x = 1/3,
  stratum_width = 1/3,
  stratum_col = "white",
  xlab = NULL, ylab = "Catch [t]",
  fillLegendTitle = "Stock",
  addLegend = TRUE,
  plotTitle = NULL){

  stkFill <- data.frame(stock = unique(data$stock))
  stkFill <- merge(x = stkFill, y = refTable, all.x = TRUE)
  stkFill <- stkFill[order(stkFill$order),]
  stkColors <- stkFill$col
  names(stkColors) <- stkFill$stock
  stkColorScale <- scale_colour_manual(
    name = "Stock", values = stkColors, aesthetics = c("fill"),
    na.value = stratum_col, # define other stratum box fill color (NA)
    limits = \(x) x[!is.na(x)]) # don't show NA fill level in legend

  # ensure plotting order
  data$stock <- factor(data$stock, levels = stkFill$stock)

  p <- ggplot(data = data) +
    aes(axis1 = fleet, axis2 = metier, axis3 = stock, y = value) +
    ggalluvial::geom_alluvium(aes(fill = stock), width = stratum_width) +
    ggalluvial::geom_stratum(aes(fill = stock), width = stratum_width) +
    scale_x_discrete(limits = c("Fleet", "Metier", "Stock"),
      expand = expansion(mult = mult_x)) +
    labs(y = ylab, x = xlab, title = plotTitle) +
    theme_bw() +
    theme(text = element_text(size = 9),
      axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.3,
        size = 9)) +
    stkColorScale

  if(!text_repel){
    p <- p + geom_text(stat = "stratum", aes(label = after_stat(stratum)),
      size = text_size)
  }

  if(text_repel){
    p <- p + ggrepel::geom_text_repel(
      stat = "stratum", aes(label = after_stat(stratum)),
      size = text_size,
      direction = "y",
      nudge_x = nudge_x
    )
  }

  if(!addLegend){
    p <- p + guides(fill="none")
  }

  return(p)
}




