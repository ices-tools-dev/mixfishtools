#' Plot landings or catch compositions
#'
#' @description Landings or catch compositions by stock for selected years, countries, fleets, metiers etc
#'
#' @param data data.frame Contains information on fleet data to make catch (landings) compositions.
#' Required variables are: `year`, `fleet`, `stock`, `landings`. Additional variables, if required,
#' include: `area`, `country`, `catch`, `metier` and `value`.
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
#' @param categories character string of one of `year`, `area`,`country`, `fleet`, `metier` or `stock`. The chosen
#'  category will be plotted on the x-axis. Multiple variables can be listed as \code{categories} and these will be
#'  concatenated into a "label" for plotting. The default value is \code{metier} and will produce catch
#'  compositions by `metier`.
#'
#' @param split character string of one of `year`, `area`,`country`, `fleet`, `metier` or `stock`. The chosen
#'  split will be used to divide the bar chart based on relative proportions. The default value is \code{stock} and will produce
#'  compositions by `stock`.
#'
#' @param facet character string of one of `year`, `area`,`country`, `fleet` or `metier`. Only one variable can be
#' listed as a `facet`. The chosen facet will be used to divide the catch compositions into subplots - e.g. one per
#' `fleet`. The default value of \code{NULL} will plot just one catch composition (i.e. no subplots).
#'
#' @param yvar character string of variable (e.g. 'landings' or 'value') to be plotted on the y-axis (Default: yvar = "landings")
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
#' categories <- c("year")
#' split <- c("stock")
#' facet <- c("fleet")
#' p <- plot_catchCompStk(data, refTable, filters = NULL, categories, split, facet, yvar = "catch")
#' print(p)
#'
#' # ggplot format adjustments
#' p2 <- p + theme(text = element_text(size = 8),
#'   axis.text.x = element_text(angle = 90, vjust = 0, hjust=1)) +
#'   facet_wrap(facet,  scales = "fixed") # remove free axes
#' print(p2)
#'
#' # export plot
#' # png("catchComp1.png", width = 7, height = 7, units = "in", res = 400)
#' #  print(p2); dev.off()
#'
#'
#' # Plot landings composition for each area by country-metier combinations
#' categories <- c("country", "metier")
#' split <- c("stock")
#' facet <- c("area")
#' p <- plot_catchCompStk(data,refTable,filters=NULL,categories, split, facet)
#' print(p)
#'
#' # Plot landings composition for each metier by country for 2022
#' filters <- list(year = 2022)
#' categories <- c("metier")
#' split <- c("stock")
#' facet <- c("country")
#' plot_catchCompStk(data, refTable, filters, categories, split, facet)
#'
#' # Plot landings compositions for each fleet by metier for Scottish fleets.
#' filters <- list(year=2022, country="SC")
#' categories <- c("metier")
#' split <- c("stock")
#' facet <- c("fleet")
#' plot_catchCompStk(data,refTable,filters, categories, split, facet)
#'
#'
plot_catchCompStk <- function(data, refTable, filters=NULL,
  categories = "fleet", split="stock", facet = NULL, yvar = "landings"){

  # filters filter the data
  # categories select the level of data aggregation. They get pasted together
  # into a label which is used to aggregate. i.e. the x axis labels
  # The split indicates the variable over which to calculate the proportions within the category(ies)
  # facet provides a variable over which to disaggregate the data for
  # comparison. i.e. facets in the plot
  if(!is.null(filters)){
    # filter
    for (var in names(filters)){
      data <- data %>% filter(.data[[var]] %in% filters[[var]]) # this works but might be a deprecated method
    }
  }

  if(length(facet)>1){
    stop("only 1 variable can be provided as a facet")
  }

  # check area codes. NA = notSpecified
  if(any(is.na(data$area))){
    data$area[is.na(data$area)] <- "notSpecified"
  }

  # aggregate by categories by concatenating into 1 label

  data$label <- apply(select(ungroup(data),all_of(categories)),1,paste,collapse="_")
  data <- data %>% group_by(across(all_of(c("label",split=split, facet)))) %>%
    summarise(VAR=sum(get(yvar),na.rm=T))

  # get colour scale by merging with refTable
  if(split %in% "stock"){
  data$stock <- data$split
  data <- left_join(data,refTable,by="stock")
  tmp <- unique(data[,c("stock","col", "order")])
  tmp <- tmp[order(tmp$order),]
  stkColors <- tmp$col
  names(stkColors) <- tmp$stock
  datColorScale <- scale_colour_manual(name = "stock", values = stkColors,
    aesthetics = c("colour", "fill"))

  # ensure plotting order
  data$split <- factor(data$stock, levels = tmp$stock)
  }

  # get colour scale
  if(!split %in% "stock"){
        # specify color-palette
    sps = sort(unique(data$split))
    nr.sps = length(sps)

    # define color palette by country
    if(any(grepl("\\.",sps))){
      cc <- unique(sub("\\..*","",sps)) # gets gear from metier
      tb.cc = table(sub("\\..*","",sps))
    }else{
    cc = unique(sub("_.*","",sps)) # gets country code form fleet
    tb.cc = table(sub("_.*","",sps))
     }
    print(length(cc))
    unique.cols = c("#35701D", # green
                    "#C7D705", #yellow-greenish
                    "#800077", # violet
                    "#F29718", # orange
                    "#694C24", # brown
                    "#0B879E", # Teal
                    "#818182", # grey
                    "#F21946", # red
                    "#000D80", # dark-blue
                    "#000000", # black
                    "#CC33FF", # purple
                    "salmon",  # salmon
                    "gold",
                    "deeppink",
                    "cornsilk",
                    "aquamarine")
    #RColorBrewer::brewer.pal(length(cc),"Set1")


    cc_cols = list()
    for(i in seq(tb.cc)){
      nn = tb.cc[[i]]
      if(i == 5){
        start.col = "#B08010"
      }else{
        start.col = "white"
      }
      cc_cols[[i]] = colorRampPalette(c(start.col,unique.cols[i]))(nn+1)[-1]
    }
    cols = do.call(c,cc_cols)

    datColorScale <- scale_colour_manual(name = split, values = cols,
                                         aesthetics = c("colour", "fill"))

    # ensure plotting order
    data$split <- factor(data$split, levels = sps)
  }

  # plot
  p <- ggplot(data,aes(x=label,y=VAR,colour=split,fill=split))+
    geom_col(position="fill")+
    coord_flip()+ labs(x="",y="",fill="",colour="")+
    theme_bw()+datColorScale +guides(fill=guide_legend(ncol=2))+guides(colour=guide_legend(ncol=2))

  if(!is.null(facet)){
    p <- p + facet_wrap(facet, scales = "free")
  }

  print(p)

  return(p)

}
