#' Metier to Metier to Fleet Sankey plot
#'
#' @description function to plot metier to mixedfish metier and fleet flow
#' to provide a description and visualization of how metiers are constructed
#'
#' @param MetMetData  data.frame containing the original metier from the accession file and
#' the output metier and a Link value (default assumption
#' is Landings)
#' @param MetFleetData data.frame containing the the output metier and the fleet to be used in the model and a Link value (default assumption
#' is Landings)
#' @param Col_2_Link column name (character) for the linking "value" variable. Default assumption is
#' NA and the function defaults to Landings column
#'
#' @details Users will need to provide a data frame with three columns, two for metiers
#' and one for the value used to link them.if a second dataframe is provided to link through to fleets you will need a metier column matching the output metier of the first, a fleet column and a value to link them. The data must be surmised to the metier columns using a group_by statement or similar. Where a metier goes to itself for example SDN_DEF to SDN_DEF you will experiences a doughnut
#'
#' @return a sanky plot, see the example for how to save a static sankey plot.
#' @import networkD3
#' @import htmlwidgets
#' @import htmltools
#'
#' @export
#'
#' @examples
#' mtcars$Name <- rownames(mtcars)
#' dat <- mtcars %>% select(Name,gear,hp)
#' dat$gear <- as.character(dat$gear )
#' names(dat) <- c("Original_Metier","Metier","hp")
#'
#' P <- plot_MetMetFleet(dat,Col_2_Link = "hp")
#'
#' # Sankey plots are interactive by nature and are saved as an html, to get a static image they
#' # are captured using webshot from the htmlwidgets
#'
#' P <- htmlwidgets::prependContent(P, htmltools::tags$h1("Title"))
#' P <- htmlwidgets::appendContent(P, htmltools::tags$p("Caption"))
#'
#' # save plot
#' # saveNetwork(P, file =file.path("Plot_path" ,paste("A_Name","_sn.html",sep="")))
#' # save as png
#' # webshot::webshot(
#' # url = file.path("Plot_path",
#' #   paste("A_Name","_sn.html",sep="")),
#' # file.path("Metier_Sankey", paste(i,"_sn.png",sep="")),
#' # vwidth = 640,
#' # vheight=840)
#'
#'
plot_MetMetFleet <- function(MetMetData,MetFleetData=NULL, Col_2_Link=NA){
  # require(tidyverse)
  # require(networkD3)
  # require(htmlwidgets)

  if(!c("data.frame") %in% class(MetMetData)){
    stop("Please supply a data frame")
  }
  if( !("Original_Metier")  %in% names(MetMetData)){
    stop("Missing columns Original_Metier")
  }
  if( !("Metier")  %in% names(MetMetData)){
    stop("Missing columns Metier ")
  }

  if(is.null(MetFleetData)==FALSE){
    if(!c("data.frame") %in% class(MetFleetData)){
      stop("Please supply a data frame")
    }
    if( !("Metier") %in% names(MetFleetData)){
      stop("Missing column Metier")
    }
    if( !("Fleet") %in% names(MetFleetData)){
      stop("Missing columns Fleet")
    }
  }

  if (is.na(Col_2_Link) == TRUE) {
    if ("Landings" %in% names(MetMetData) ||
        "Landings" %in% names(MetFleetData)) {
      if (is.null(MetFleetData) == FALSE) {
        Link_column <- MetMetData$Landings
      } else{
        Link_column <- rbind(MetMetData$Landings, MetFleetData$Landings)
      }
    }else{
      stop("Missing Landings column and no Col_2_Link supplied")
    }
  }

  if (is.na(Col_2_Link) == FALSE) {
    if (is.null(MetFleetData) == FALSE) {
      Link_column <-
        rbind(MetMetData[0:dim(MetMetData)[1], Col_2_Link], MetFleetData[0:dim(MetFleetData)[1], Col_2_Link])
      names(Link_column) <- "Link_column"
    }else{
      Link_column <- as.data.frame( MetMetData[0:dim(MetMetData)[1], Col_2_Link])
      # Link_column$Link_column <- MetMetData[0:dim(MetMetData)[1], Col_2_Link]
      names(Link_column) <- "Link_column"
    }
  }




  if(is.null(MetFleetData)==FALSE){

    names(MetMetData)[names(MetMetData)=="Original_Metier"] <- "source"
    names(MetMetData)[names(MetMetData)=="Metier"] <- "sink"
    names(MetFleetData)[names(MetFleetData)=="Metier"] <- "source"
    names(MetFleetData)[names(MetFleetData)=="Fleet"] <- "sink"

    Data <- rbind(MetMetData,MetFleetData)

  }else{
    names(MetMetData)[names(MetMetData)=="Original_Metier"] <- "source"
    names(MetMetData)[names(MetMetData)=="Metier"] <- "sink"
    Data <- MetMetData

  }

  #A connection data frame is a list of flows with intensity for each flow
  links <- data.frame(
    source=Data$source,
    target=Data$sink,
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
  P <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name",
                     sinksRight=FALSE)



  return(P)



}

