#' Plot proportion of stock landings by metier
#'
#' @description landings proportions of each stock associated with different metiers
#'
#' @param data data.frame Contains information on the stock, metiers and
#'   landings proportions. Required variables are: `Stock` (numeric; numbers
#'   the stocks in alphabetical order), `metCat` (which indicates the metier
#'   categories), `prop_stock` (which indicates proportion of total stock
#'   landings associated with each metier category), and `label` (which
#'   indicates the ICES stock code).
#'
#' @details Users will need to provide the data to produce the plot.
#'
#' @return plot output of class ggplot
#' @import ggplot2
#'
#' @export
#'
#' @examples
#'
#'# Landings - prepare example data
#' data(stock_props)
#' # plot
#' plot_stkMetProp(stock_props)
#'
plot_stkMetProp <- function(data, text_size = 9, face = "plain"){
  # order metier categories by importance in terms of landings over all stocks

  temp <- data %>% group_by(metCat) %>% summarise(prop_stock=sum(prop_stock,na.rm=TRUE))
  temp <- temp[order(temp$prop_stock, decreasing = FALSE),]
  temp <- temp %>% filter(prop_stock >=0.01)
  level_order <- temp$metCat

  p <- ggplot(data %>% filter(metCat %in% level_order)) +
    aes(x = Stock , y = factor(metCat, levels = level_order),  fill = prop_stock) +
    geom_tile(color = "white", lwd = 1, linetype = 1) +
    scale_x_continuous(position = "top",
                       breaks = unique(data$Stock),
                       labels = unique(data$label))+
    scale_fill_gradient(name = "Proportion of\n stock landings",
                        limits = c(0,1),
                        low = "#C6DBEF",
                        high = "#A50F15") +

    theme(panel.grid.major = element_line(colour = "grey"),
          panel.grid.major.y = element_blank(),
          panel.background = element_blank(),
          strip.background = element_rect(colour= NA, fill=NA),
           axis.text.x.top = element_text(size= text_size, angle = 90, hjust = 0, vjust = 0.3),
          axis.text.y = element_text(size= text_size, hjust = 1, vjust = 0),
          axis.text.x.bottom = element_text(size= text_size, angle=45,vjust=1,hjust=1),
          text = element_text(size = text_size, face = face)) +
    coord_fixed() +
    xlab("") +
    ylab("Metier")

  print(p)
}

