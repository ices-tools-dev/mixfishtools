#' Plot landings (catch) composition by country and species
#'
#' @description landings compositions of species landed by country and metier
#'   category
#'
#' @param data data.frame Contains information on the total landings of species
#'   listed in the WGMIXFISH data call by country and metier category.
#'   Required variables are: `country` (2 letter country code),
#'   `metCat` (which indicates the metier categories), `MIXFISHspp` (which
#'   indicates the 3 letter FAO species code as listed in the WGMIXFISH
#'   data call), and `landings`.
#'
#' @param land_threshold numeric A number, in tonnes, to set a minimum landings
#'   threshold. Metier-country combinations with total landings below this
#'   threshold will not be plotted. Default value is 10 tonnes.
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
#' # aggregate some metiers
#' dat <- dat %>% mutate(metCat = if_else(metCat %in%
#'   c("dredges","beam_oth"),"MIS",metCat))
#' dat <- dat %>% mutate(
#'   metCat = if_else(metCat %in%
#'    c("Dseine_TR3","Dseine_TR1","Dseine_TR2","Dseine_oth"),
#'    "Demersal seine", metCat))
#'
#' # re-aggregate
#' datayear <- 2023
#' dat <- dat %>% filter(Year %in% datayear) %>%
#'   group_by(country=Country,metCat,MIXFISHspp) %>%
#'   summarise(landings=sum(Landings))
#'
#' # Set landings threshold
#' land_th <- 10 # 10 tonnes.
#'
#' # plot
#' # windows()
#' plot_catchCompSpp(dat, land_threshold=land_th)
#'
plot_catchCompSpp <- function(data,land_threshold=10){

  # make the colour scale
  col.pal.MFspecies <- c( brewer.pal(n = 9, name = "YlOrBr")[c(2,4)],
                          brewer.pal(n = 9, name = "Oranges")[c(6)],
                          brewer.pal(n = 9, name = "Reds")[c(8)],
                          brewer.pal(n = 11, name = "PRGn")[c(4,3,2,1)],
                          brewer.pal(n = 9, name = "PiYG")[c(4,2,1)],
                          brewer.pal(n = 9, name = "Blues")[c(4,6,8,9)],
                          brewer.pal(n = 11, name = "BrBG")[c(7:10)],
                          brewer.pal(n = 9, name = "Greens")[c(3,6,8)],
                          brewer.pal(n = 11, name = "BrBG")[c(1:4)],
                          brewer.pal(n = 9, name = "Greys")[c(3,5,7)])

  # group some species together to make plotting easier
  data <- data %>% mutate(MIXFISHspp = if_else(MIXFISHspp %in% c("ANK","MON"),"ANF",MIXFISHspp))
  data <- data %>% mutate(MIXFISHspp = if_else(MIXFISHspp %in% c("MEG","LDB"),"LEZ",MIXFISHspp))
  data <- data %>% mutate(MIXFISHspp = if_else(MIXFISHspp %in% c("GUG","GUR"),"Gurnards",MIXFISHspp))

  # remove less important species from data call to make plotting easier
  data <- data %>% filter(!MIXFISHspp %in% c("OTH","CAA","COE","HAL","HER","MAC","WHB","HOM","SPR"))

  # re-aggregate
  data <- data %>% group_by(country,metCat,MIXFISHspp) %>% summarise(landings=sum(landings))


  # remove country-metCat combinations that have small landings record (fall below threshold)
  tmp <- data %>% group_by(country,metCat) %>%summarise(landings=sum(landings))
  tmp <- tmp %>% mutate(rem=if_else(landings < land_threshold,"remove","keep"))
  data <- left_join(data,tmp[,c("country","metCat","rem")])
  data <- data %>% filter(rem %in% "keep")


  # get propertion caught by each metcat
  tots <- sum(data$landings)
  tots_met <- data %>%  group_by(metCat) %>% summarise(landings=sum(landings))
  tots_met$prop <- round(100*(tots_met$landings/tots),2)
  data <- left_join(data,select(tots_met,c("metCat","prop")))
  data <- data %>% rowwise() %>% mutate(metlabel = paste0(metCat," (",prop,"%)"))

  # set order
  data <- data[order(data$prop,decreasing=T),]
  data$metlabel <- factor(data$metlabel,levels=unique(data$metlabel))

  # set colour scale
  stkColorScale <- scale_colour_manual(name = "Species", values = col.pal.MFspecies,
                                       aesthetics = c("colour", "fill"),
                                       labels=c("Anglerfishes","Brill","Cod","Dab","Flounder","Gurnards","Haddock","Hake","Lemon sole",
                                                "Megrims","Ling","Norway lobster","Norway pout","Plaice","Saithe","Pollack","Smoothhounds",
                                                "Skates and rays","Sole","Turbot","Whiting","Witch"))

  p1 <- ggplot(data,aes(x=country,y=landings,colour=MIXFISHspp,fill=MIXFISHspp))+
    geom_col(position="fill")+facet_wrap(~metlabel,scales="free")+
    coord_flip()+ labs(x="",y="",fill="",colour="")+
    theme_bw()+stkColorScale +guides(fill=guide_legend(ncol=1))+guides(colour=guide_legend(ncol=1))

  print(p1)

}

