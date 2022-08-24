#' Headline advice plot
#'
#' @description Plot summarizing over- and under-quota catches by stock and
#'   scenario. Dashed line displays quota by stock. Colored background further
#'   emphasizes over- and under-quota catches (under - green, over - red).
#'
#' @param data data.frame Contains catch (`catch`) and quota (`quota`) levels
#'   by scenario (`catch`) and stock (`stock`).
#' @param xlab character X-axis label
#' @param ylab character Y-axis label
#'
#' @return plot output of class ggplot
#' @export
#'
#' @examples
#'
#' data("stfFltStkSum")
#' head(stfFltStkSum)
#'
#' df <- stfFltStkSum
#' advYr <- 2022 # advice year
#' restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
#'   "SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9")
#'
#' df <- subset(df, year == advYr & stock %in% restr.stks)
#' tmp1 <- aggregate(catch ~ scenario + stock, data = df, FUN = sum)
#' tmp2 <- aggregate(quota ~ stock, data = subset(df, scenario == "min"),
#'   FUN = sum)
#' catchScenStk <- merge(x = tmp1, y = tmp2, all = TRUE)
#'
#' # re-label scenarios
#' catchScenStk$scenario <- factor(catchScenStk$scenario,
#'   levels = c("sq_E", "max", "min", "cod-ns"),
#'   labels = c("Sq-E", "Max", "Min", "COD-NS"))
#' head(catchScenStk)
#'
#' p <- plot_catchScenStk(data = catchScenStk)
#' print(p)
#'
#' # png("catchScenStk1.png", width = 6, height = 5, units = "in", res = 400)
#' # print(p); dev.off()
#'
plot_catchScenStk <- function(data,
  xlab = "Scenario", ylab = "Catch [t]"){

  data$value <- 1 # dummy for drawing background rects

  p <- ggplot(data = data, mapping = aes(x = scenario, y = catch)) +
    geom_rect(stat = "identity",
      mapping = aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = quota),
      fill = 'green', alpha = 0.05) +
    geom_rect( stat = "identity",
      mapping = aes(xmin = -Inf, xmax = Inf, ymin = quota, ymax = Inf),
      fill = 'red', alpha = 0.05) +
    geom_hline(mapping = aes(yintercept = quota), lty = 2) +
    geom_col(width = 0.5) +
    facet_wrap(~stock, scales = 'free_y') +
    xlab(xlab) +
    ylab(ylab) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme(
      text = element_text(size = 10), legend.position="none",
      axis.text.x = element_text(angle = 90, vjust = 0, hjust=1))

  return(p)
}
