#' Plot landings (catch) composition by country and species
#'
#' @description landings compositions of species landed by country and metier
#'   category
#'
#' @param data data.frame Contains information on the total landings of species
#'   listed in the WGMIXFISH data call by country and metier category.
#'   Required variables are: `country` (2 letter country code),
#'   `metCat` (which indicates the metier categories), `spp` (which
#'   indicates the 3 letter FAO species code as listed in the WGMIXFISH
#'   data call), and `landings`.
#'
#' @param refTable data.frame Contains stock look-up information for consistent
#'   plotting of stocks. `spp` defines the species names corresponding to
#'   `data` object. `col` defines the color used to fill bars in plot.
#'   `order` defines the order of stocks in the plot facets. `label` = label to
#'   use in the fill color legend
#'
#' @param land_threshold numeric A number, in tonnes, to set a minimum landings
#'   threshold. Metier-country combinations with total landings below this
#'   threshold will not be plotted. Default value is 10 tonnes.
#'
##' @param fillLegendTitle character Fill legend title
#'   (Default: `fillLegendTitle = "Species"`)
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
#'
#' # rename
#' names(dat)[which(names(dat)=="MIXFISHspp")] <- "spp"
#'
#'
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
#'   group_by(country=Country, metCat, spp) %>%
#'   summarise(landings=sum(Landings))
#'
#' # Set landings threshold
#' land_th <- 10 # 10 tonnes.
#'
#' # group some species together to make plotting easier
#' dat <- dat %>% mutate(spp = if_else(spp %in%
#'   c("ANK","MON"),"ANF", spp))
#' dat <- dat %>% mutate(spp = if_else(spp %in%
#'   c("MEG","LDB"),"LEZ", spp))
#' dat <- dat %>% mutate(spp = if_else(spp %in%
#'   c("GUG","GUR"),"Gurnards", spp))
#'
#' # remove less important species from data call to make plotting easier
#' dat <- dat %>% filter(!spp %in%
#'   c("OTH","CAA","COE","HAL","HER","MAC","WHB","HOM","SPR"))
#'
#' # re-aggregate
#' dat <- dat %>% group_by(country, metCat, spp) %>% summarise(landings=sum(landings))
#'
#'
#' # make color lookup data.frame (refTable)
#' COLS <- c(brewer.pal(n = 9, name = "YlOrBr")[c(2,4)],
#'   brewer.pal(n = 9, name = "Oranges")[c(6)],
#'   brewer.pal(n = 9, name = "Reds")[c(8)],
#'   brewer.pal(n = 11, name = "PRGn")[c(4,3,2,1)],
#'   brewer.pal(n = 9, name = "PiYG")[c(4,2,1)],
#'   brewer.pal(n = 9, name = "Blues")[c(4,6,8,9)],
#'   brewer.pal(n = 11, name = "BrBG")[c(7:10)],
#'   brewer.pal(n = 9, name = "Greens")[c(3,6,8)],
#'   brewer.pal(n = 11, name = "BrBG")[c(1:4)],
#'   brewer.pal(n = 9, name = "Greys")[c(3,5,7)])
#'
#'
#' # plot
#' # possible colors
#' COLS <- c(brewer.pal(n = 9, name = "YlOrBr")[c(2,4)],
#'   brewer.pal(n = 9, name = "Oranges")[c(6)],
#'   brewer.pal(n = 9, name = "Reds")[c(8)],
#'   brewer.pal(n = 11, name = "PRGn")[c(4,3,2,1)],
#'   brewer.pal(n = 9, name = "PiYG")[c(4,2,1)],
#'   brewer.pal(n = 9, name = "Blues")[c(4,6,8,9)],
#'   brewer.pal(n = 11, name = "BrBG")[c(7:10)],
#'   brewer.pal(n = 9, name = "Greens")[c(3,6,8)],
#'   brewer.pal(n = 11, name = "BrBG")[c(1:4)],
#'   brewer.pal(n = 9, name = "Greys")[c(3,5,7)])
#'
#'
#' refTable <- as.data.frame(rbind(
#'   c("ANF", "Anglerfishes"),
#'   c("BLL", "Brill"),
#'   c("COD", "Cod"),
#'   c("DAB", "Dab"),
#'   c("FLE", "Flounder"),
#'   c("Gurnards", "Gurnards"),
#'   c("HAD", "Haddock"),
#'   c("LEM", "Lemon sole"),
#'   c("LIN", "Ling"),
#'   c("PLE", "Plaice"),
#'   c("SKA", "Skates and rays"),
#'   c("SOL", "Sole"),
#'   c("TUR", "Turbot"),
#'   c("WHG", "Whiting"),
#'   c("HKE", "Hake"),
#'   c("POK", "Saithe"),
#'   c("POL", "Pollack"),
#'   c("WIT", "Witch"),
#'   c("LEZ", "Megrim"),
#'   c("NEP", "Norway lobster"),
#'   c("SDV", "Smoothhounds"),
#'   c("NOP", "Norway pout")
#' ))
#' names(refTable) <- c("spp", "label")
#' refTable$col <- COLS[seq(nrow(refTable))]
#' refTable$order <- seq(nrow(refTable))
#'
#' plot_catchCompSpp( data = dat, refTable = refTable, land_threshold=land_th)
#'
#'
#'
plot_catchCompSpp <- function(data, refTable, land_threshold=10, fillLegendTitle = "Species"){

  # remove country-metCat combinations that have small landings record (fall below threshold)
  tmp <- data %>% group_by(country,metCat) %>% summarise(landings=sum(landings))
  tmp <- tmp %>% mutate(rem=if_else(landings < land_threshold,"remove","keep"))
  data <- left_join(data,tmp[,c("country","metCat","rem")])
  data <- data %>% filter(rem %in% "keep")


  # get proportion caught by each metCat
  tots <- sum(data$landings)
  tots_met <- data %>%  group_by(metCat) %>% summarise(landings=sum(landings))
  tots_met$prop <- round(100*(tots_met$landings/tots),2)
  data <- left_join(data,select(tots_met,c("metCat","prop")))
  data <- data %>% rowwise() %>% mutate(metlabel = paste0(metCat," (",prop,"%)"))

  # set order
  data <- data[order(data$prop, decreasing=T),]
  data$metlabel <- factor(data$metlabel,levels=unique(data$metlabel))

  # set colour scale
  palColFill <- data.frame(tmp = unique(data$spp))
  names(palColFill) <- "spp"
  palColFill <- merge(x = palColFill, y = refTable, all.x = TRUE)
  palColFill <- palColFill[order(palColFill$order),]
  palColors <- palColFill$col
  names(palColors) <- palColFill$spp
  palScale <- scale_colour_manual(
    name = fillLegendTitle, values = palColors, labels = palColFill$label, aesthetics = c("color", "fill"),
    na.value = "white", # define other stratum box fill color (NA)
    limits = \(x) x[!is.na(x)]) # don't show NA fill level in legend

  # make order for spp
  data$spp <- factor(data$spp, levels = palColFill$spp)

  p1 <- ggplot(data,aes(x = country, y = landings, colour = spp, fill = spp)) +
    geom_col(position="fill") +
    facet_wrap(~ metlabel, scales = "free") +
    coord_flip() +
    labs(x = "",y = "", fill = "", colour = "") +
    theme_bw() +
    guides(fill = guide_legend(ncol = 1)) +
    guides(colour = guide_legend(ncol = 1)) +
    palScale

  print(p1)

}

