#' Plot total landings or effort by broad gear categories
#'
#' @description total landings or effort by broad gear categories such as 'otter trawls', ' beam trawls'
#'
#' @param data data.frame Contains information on the total landings or effort
#'   by year and gear categories. Required variables are: `year`, `gearType`
#'   (which indicates the broad gear categories), `value` (which indicates
#'   the total to be plotted), and `dataType` which indicates the type of data
#'   to be plotted (either \code{"landings"}, \code{"days at sea"} or
#'   \code{"KW days"}.
#'
#' @details Users will need to provide the data to produce the plot.
#'
#' @return plot output of class ggplot
#' @import ggplot2
#' @import RColorBrewer
#'
#' @export
#'
#' @examples
#'
#'# Landings - prepare example data
#' data(NSlandings)
#'
#' dat <- NSlandings
#' dat <- rename(dat,gearType=gearCat)
#' dat <- rename(dat,year=Year)
#'
#' # aggregate landings data
#' dat <- dat  %>% group_by(year,gearType) %>%
#'   summarise(value=sum(Landings,na.rm=TRUE))
#' dat$dataType <- "landings"
#'
#' # plot
#' # windows()
#' plot_FOgearType(dat)
#'
#'
#'# Effort - prepare example data
#' data(NSeffort)
#'
#' dat <- NSeffort
#' dat <- rename(dat,gearType=gearCat)
#' dat <- rename(dat,year=Year)
#' dat <- dat  %>% group_by(year,gearType) %>%
#'   summarise(value=sum(Days_at_sea,na.rm=TRUE))
#' dat$dataType <- "days at sea"
#'
#' # plot
#' # windows()
#' plot_FOgearType(dat)
#'
#'
plot_FOgearType <- function(data){


  # colour palette. Need RcolorBrewer
  col.pal <- c(brewer.pal(n = 8, name = "Dark2"),brewer.pal(n=12,name="Set3"))


  # check column names are correct

  if(!all(colnames(data) %in% c("year","gearType","value","dataType"))){
    stop("column names are not as expected. They should be 'year', 'gearType', 'value' and ' dataType'")
  }

  # convert landings to thousand tonnes
  if(all(data$dataType %in% "landings")){
    data$value <- data$value/1000
  }

  # plot

  p1 <- ggplot(data,aes(x=year,y=value,colour=gearType))+geom_line()+theme_bw()+
    scale_colour_manual(values=col.pal)+labs(x="",colour="")

  if(all(data$dataType %in% "landings")){
  p1 <- p1 +labs(y="Landings (thousand tonnes)")
  }

  if(all(data$dataType %in% "days at sea")){
    p1 <- p1 +labs(y="Effort (Days at sea)")
  }

  if(all(data$dataType %in% "KW days")){
    p1 <- p1 +labs(y="Effort (KW days)")
  }

print(p1)

}
