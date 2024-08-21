#' Plot landings or catch compositions
#'
#' @description Landings or catch compositions by stock for selected years, countries, fleets, metiers etc
#'
#' @param data data.frame Contains information on fleet data to make catch (landings) compositions.
#' Required variables are: `year`, `area`,`country`, `fleet`, `metier`,`stock`,`landings`, `catch`,
#' and `fleet_type` which indicates if the `fleet` is a `main` or `residual` fleet.
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
#' @param filters list of character strings listing the `year`, `area`,`country`, `fleet` and/or `metier`
#' to filter from \code{data}. Default value of \code{NULL} will produce catch compositions using all data in \code{data}.
#'
#' @param selectors character string of one of `year`, `area`,`country`, `fleet` or `metier`. The chosen
#'  selector will be plotted on the x-axis. Multiple variables can be listed as \code{selectors} and these will be
#'  concatenated into a "label" for plotting. The default value is \code{metier} and will produce catch
#'  compositions by `metier`.
#'
#' @param divider character string of one of `year`, `area`,`country`, `fleet` or `metier`. Only one variable can be
#' listed as a `divider`. The chosen divider will be used to divide the catch compositions into subplots - e.g. one per
#' `fleet`. The default value of \code{NULL} will plot just one catch composition (i.e. no subplots).
#'
#' @param yvar character string of variable to be plotted on the y-axis (Default: yvar = "landings")
#'
#'
#' @details Users will need to provide the data and refTable objects to produce the plot.
#'
#' @return plot output of class ggplot
#' @import ggplot2
#'
#' @export
#'
#' @examples
#'
#' # prepare example data
#' data(refTable)
#' data(stfMtStkSum)
#'
#' # subset data to a single scenario (e.g. min)
#' data <- subset(stfMtStkSum, scenario == "min")
#'
#' # add country and area identifiers (if desired)
#' tmp <- strsplit(data$metier, ".", fixed = TRUE)
#' data$area <- unlist(lapply(tmp, FUN = function(x){ifelse(length(x)==2, x[2], NA)}))
#' tmp <- strsplit(data$fleet, "_", fixed = TRUE)
#' data$country <- unlist(lapply(tmp, FUN = function(x){ifelse(length(x)==2, x[1], NA)}))
#'
#'
#' # replace stock with ICES stock code
#' data$stock <- refTable$stock[match(data$stock, refTable$stock_short)]
#'
#'
#' # Plot catch composition for each fleet over time
#' selectors <- c("year")
#' divider <- c("fleet")
#' p <- plot_catchComp(data, refTable, filters = NULL, selectors, divider, yvar = "catch")
#' print(p)
#'
#' # ggplot format adjustments
#' p2 <- p + theme(text = element_text(size = 8),
#'   axis.text.x = element_text(angle = 90, vjust = 0, hjust=1)) +
#'   facet_wrap(divider,  scales = "fixed") # remove free axes
#'
#' # export plot
#' # png("catchComp1.png", width = 7, height = 7, units = "in", res = 400)
#' #  print(p2); dev.off()
#'
#'
#' # Plot landings composition for each area by country-metier combinations
#' selectors <- c("country", "metier")
#' divider <- c("area")
#' p <- plot_catchComp(data,refTable,filters=NULL,selectors, divider)
#' print(p)
#'
#' # Plot landings composition for each metier by country for 2022
#' filters <- list(year = 2022)
#' selectors <- c("metier")
#' divider <- c("country")
#' plot_catchComp(data, refTable, filters, selectors, divider)
#'
#' # Plot landings compositions for each fleet by metier for Scottish fleets.
#' filters <- list(year=2022, country="SC")
#' selectors <- c("metier")
#' divider <- c("fleet")
#' plot_catchComp(data,refTable,filters,selectors, divider)
#'
#'
plot_catchComp <- function(data, refTable, filters=NULL,
  selectors = "metier", divider = NULL, yvar = "landings"){

  # filters filter the data
  # selectors select the level of data aggregation. They get pasted together
  # into a label which is used to aggregate. i.e. the x axis labels
  # divider provides a variable over which to disaggregate the data for
  # comparison. i.e. facets in the plot
  if(!is.null(filters)){
    # filter
    for (var in names(filters)){
      data <- data %>% filter(.data[[var]] %in% filters[[var]]) # this works but might be a deprecated method
    }
  }

  if(length(divider)>1){
    stop("only 1 variable can be provided as a divider")
  }

  # check area codes. NA = notSpecified
  if(any(is.na(data$area))){
    data$area[is.na(data$area)] <- "notSpecified"
  }

  # aggregate by selectors by concatenating selectors into 1 label
  # label and stock are always selectors
  data$label <- apply(select(ungroup(data),all_of(selectors)),1,paste,collapse="_")
  data <- data %>% group_by(across(all_of(c("label","stock",divider)))) %>%
    summarise(VAR=sum(get(yvar),na.rm=T))

  # get colour scale by merging with refTable
  data <- left_join(data,refTable,by="stock")
  tmp <- unique(data[,c("stock","col", "order")])
  tmp <- tmp[order(tmp$order),]
  stkColors <- tmp$col
  names(stkColors) <- tmp$stock
  stkColorScale <- scale_colour_manual(name = "Stock", values = stkColors,
    aesthetics = c("colour", "fill"))

  data$stock <- factor(data$stock, levels = tmp$stock) # orders the stocks

  # plot
  p <- ggplot(data,aes(x=label,y=VAR,colour=stock,fill=stock))+
    geom_col(position="fill")+
    coord_flip()+ labs(x="",y="",fill="",colour="")+
    theme_bw()+stkColorScale +guides(fill=guide_legend(ncol=1))+guides(colour=guide_legend(ncol=1))

  if(!is.null(divider)){
    p <- p + facet_wrap(divider, scales = "free")
  }

  return(p)

}
