% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_MetMetFleet.R
\name{plot_MetMetFleet}
\alias{plot_MetMetFleet}
\title{Metier to Metier to Fleet Sankey plot}
\usage{
plot_MetMetFleet(MetMetData, MetFleetData = NULL, Col_2_Link = NA)
}
\arguments{
\item{MetMetData}{data.frame containing the original metier from the accession file and
the output metier and a Link value (default assumption
is Landings)}

\item{MetFleetData}{data.frame containing the the output metier and the fleet to be used in the model and a Link value (default assumption
is Landings)}

\item{Col_2_Link}{column name (character) for the linking "value" variable. Default assumption is
NA and the function defaults to Landings column}
}
\value{
a sanky plot, see the example for how to save a static sankey plot.
}
\description{
function to plot metier to mixedfish metier and fleet flow
to provide a description and visualization of how metiers are constructed
}
\details{
Users will need to provide a data frame with three columns, two for metiers
and one for the value used to link them.if a second dataframe is provided to link through to fleets you will need a metier column matching the output metier of the first, a fleet column and a value to link them. The data must be surmised to the metier columns using a group_by statement or similar. Where a metier goes to itself for example SDN_DEF to SDN_DEF you will experiences a doughnut
}
\examples{
mtcars$Name <- rownames(mtcars)
dat <- mtcars \%>\% select(Name,gear,hp)
dat$gear <- as.character(dat$gear )
names(dat) <- c("Original_Metier","Metier","hp")

P <- plot_MetMetFleet(dat,Col_2_Link = "hp")

# Sankey plots are interactive by nature and are saved as an html, to get a static image they
# are captured using webshot from the htmlwidgets

P <- htmlwidgets::prependContent(P, htmltools::tags$h1("Title"))
P <- htmlwidgets::appendContent(P, htmltools::tags$p("Caption"))

# save plot
# saveNetwork(P, file =file.path("Plot_path" ,paste("A_Name","_sn.html",sep="")))
# save as png
# webshot::webshot(
# url = file.path("Plot_path",
#   paste("A_Name","_sn.html",sep="")),
# file.path("Metier_Sankey", paste(i,"_sn.png",sep="")),
# vwidth = 640,
# vheight=840)


}
