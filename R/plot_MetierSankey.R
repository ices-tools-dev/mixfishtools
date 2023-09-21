#'Metier Sankey plot
#'
#'@description function to plot metier to mixedfish metier flow
#'to provide a description and visualization of how metiers are constructed
#'
#'@param Data data.frame containing the original metier from the accession file
#'the output metier, to be used in the model and a Link value (default assumption
#'is Landings)
#'
#'@param Col_2_Link column name (character) for the linking "value" variable. Default assumption is
#'NA and the function defaults to Landings column
#'
#'@details Users will need to provide a data frame with three columns, two for metiers
#'and one for the value used to link them. The data must be surmised to the metier columns
#'using a group_by statement or similar. Where a metier goes to itself for example SDN_DEF
#'to SDN_DEF you will experiences a doughnut
#'
#'@return a sanky plot, see the example for how to save a static sankey plot.
#'
#'@examples
#'library(tidyverse)
#'library(networkD3)
#'library(htmlwidgets)
#'
#'Dat_4_plot <- Dat_4_sanky %>% filter(Country == "UKE") %>% group_by_at(vars(-Year,-Landings,-Stock)) %>% summarise(Landings=sum(Landings)) %>% ungroup()
#'
#' P <- plot_MetierSankey(Data, Col_2_Link=="Effort")
#'
#' Sankey plots are interactive by nature and are saved as an html, to get a static image they
#' are captured using webshot from the htmlwidgets
#'
#' P <- htmlwidgets::prependContent(P, htmltools::tags$h1("Title"))
#` P <- htmlwidgets::appendContent(P, htmltools::tags$p("Caption"))
#``
#'# save plot
#`saveNetwork(P, file =file.path("Metier_Sankey" ,paste("A_Name","_sn.html",sep="")))
# save as png
#`webshot::webshot(url = file.path("Metier_Sankey" ,paste("A_Name","_sn.html",sep="")), file.path("Metier_Sankey" ,paste(i,"_sn.png",sep="")),
#`vwidth = 640,
#`vheight=840)
#'
#'

plot_MetierSankey <- function(Data, Col_2_Link=NA){
  require(tidyverse)
  require(networkD3)
  require(htmlwidgets)

  if(!c("data.frame") %in% class(Data)){
    stop("Please supply a data frame")
  }
  if( !("Original_Metier")  %in% names(Data) || !("Metier") %in% names(Data)){
    stop("Missing columns Original_Metier or Metier")
  }

  if(is.na(Col_2_Link)==TRUE){
    if("Landings" %in% names(Data)){
    Link_column <- Data$Landings
     }else{
      stop("Missing Landings column and no Col_2_Link supplied")
      }
    } else{
    Link_column <- Data[0:dim(Data)[1],Col_2_Link]
    names(Link_column) <- "Link_column"
       }


    #A connection data frame is a list of flows with intensity for each flow
    links <- data.frame(
      source=Data$Original_Metier,
      target=Data$Metier,
      value=Link_column$Link_column
    )

    # From these flows we need to create a node data frame: it lists every entities involved in the flow
    nodes <- data.frame(
      name=c(as.character(links$source),
             as.character(links$target)) %>% unique()
    )

    # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
    links$IDsource <- match(links$source, nodes$name)-1
    links$IDtarget <- match(links$target, nodes$name)-1

    # Make the Network
    P <- sankeyNetwork(Links = links, Nodes = nodes,
                       Source = "IDsource", Target = "IDtarget",
                       Value = "value", NodeID = "name",
                       sinksRight=FALSE)



   return(P)



}

