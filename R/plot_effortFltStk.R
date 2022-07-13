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
#' data(refTable)
#' data(effortFltStk)
#' p <- plot_effortFltStk(data = effortFltStk, refTable = refTable)
#' print(p)
#'
#'
plot_effortFltStk <- function(data, refTable){

  stkFill <- data.frame(stock = unique(data$stock))
  stkFill <- merge(x = stkFill, y = refTable, all.x = TRUE)
  stkFill <- stkFill[order(stkFill$order),]
  stkColors <- stkFill$col
  names(stkColors) <- stkFill$Advice_name
  stkColorScale <- scale_colour_manual(name = "Effort stock", values = stkColors,
    aesthetics = c("fill"))

  p <- ggplot(data) +
    aes(x = Advice_name, y = quotaEffort, fill = Advice_name, color = Limitation, group = fleet) +
    facet_wrap(fleet~., scales = 'free_y', ncol = 3) +
    geom_bar(stat = 'identity', size = 1, alpha = 0.7) +
    geom_hline(data=data, aes(yintercept = sqE_effort), lty=2) +
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






