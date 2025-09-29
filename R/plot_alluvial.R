#' Alluvial plot
#'
#' @param data data.frame Contains information on for making an alluvial-type
#'   plot (e.g. catch or landings). The data.frame should contain several
#'   variables for grouping (e.g. `fleet`, `metier`, `stock`) as well as a
#'   variable to plot values (`value`).
#' @param group_vars vector. Contains variable names (order from left to right)
#'   that define the alluvial axes
#'   (Default: `group_vars = c("fleet", "metier", "stock")`)
#' @param fill_var character. Data variable name used for coloring alluvial
#'   (Default: `fill_var = "stock"`).
#' @param refTable data.frame Contains stock look-up information for consistent
#'   plotting of colors. The data.frame should contain a variable that matches
#'   the `fill_var` definition (Default: `fill_var = "stock"`), and also found
#'   in the `data` object. Contains a variable `col` that defines the color
#'   used to fill bars in plot. `order` defines the order of levels used in
#'   plot.
#' @param group_labs vector Contains the x-axis labels used for the group_vars.
#'   (Default: group_labs = group_vars).
#' @param text_repel logical Should stratum labels be repelled to prevent
#'   overlapping (using `ggrepel::geom_text_repel`)
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
#' plot_alluvial(data = df, refTable = refTable,
#'   group_vars = c("fleet", "metier", "stock"),
#'   fill_var = c("stock"))
#'
#' # doesn't make sense, but redefine group_var order
#' plot_alluvial(data = df, refTable = refTable,
#'   group_vars = c("fleet", "stock", "metier"),
#'   fill_var = c("stock"))
#'
#' # a 2-level version
#' plot_alluvial(data = df, refTable = refTable,
#'   group_vars = c("fleet", "stock"),
#'   fill_var = c("stock"))
#'
#' # repel labels
#' plot_alluvial(data = df, refTable = refTable,
#'   group_vars = c("fleet", "metier", "stock"),
#'   text_repel = TRUE, text_size = 2)
#'
#' # repel labels and suppress legend
#' plot_alluvial(data = df, refTable = refTable,
#'   text_repel = TRUE, text_size = 2, addLegend = FALSE)
#'
plot_alluvial <- function(data, refTable,
  group_vars = c("fleet", "metier", "stock"),
  fill_var = c("stock"),
  group_labs = NULL,
  text_repel = FALSE,
  text_size = 2,
  mult_x = c(0.1, 0.1),
  nudge_x = 1/3,
  stratum_width = 1/3,
  stratum_col = "white",
  xlab = NULL,
  ylab = "Catch [t]",
  fillLegendTitle = "Stock",
  addLegend = TRUE,
  plotTitle = NULL){

  if(!fill_var %in% names(refTable)){stop("fill_var must be variable in refTable")}
  if(is.null(group_labs)){group_labs <- group_vars}

  palFill <- data.frame(tmp = unique(data[fill_var]))
  names(palFill) <- fill_var
  palFill <- merge(x = palFill, y = refTable, all.x = TRUE)
  palFill <- palFill[order(palFill$order),]
  palColors <- palFill$col
  names(palColors) <- palFill[,fill_var]
  palColorScale <- scale_colour_manual(
    name = fillLegendTitle, values = palColors, aesthetics = c("fill"),
    na.value = stratum_col, # define other stratum box fill color (NA)
    limits = \(x) x[!is.na(x)]) # don't show NA fill level in legend

  # ensure plotting order
  data[,fill_var] <- factor(data[,fill_var], levels = palFill[,fill_var])

  # Build a named character vector for aesthetics
  axis_mapping <- setNames(group_vars, paste0("axis", seq_along(group_vars)))
  aes_args <- c(axis_mapping, list(y = "value"))


  p <- ggplot(data = data) +
    do.call(aes_string, aes_args) + # aes()
    ggalluvial::geom_alluvium(aes(fill = get(fill_var)), width = stratum_width) +
    ggalluvial::geom_stratum(aes(fill = get(fill_var)), width = stratum_width) +
    scale_x_discrete(limits = group_labs,
      expand = expansion(mult = mult_x)) +
    labs(y = ylab, x = xlab, title = plotTitle) +
    theme_bw() +
    theme(text = element_text(size = 9),
      axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.3,
        size = 9)) +
    palColorScale

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




