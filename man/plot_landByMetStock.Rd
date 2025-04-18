% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_landByMetStock.R
\name{plot_landByMetStock}
\alias{plot_landByMetStock}
\title{Bar chart of landings by stock and metier}
\usage{
plot_landByMetStock(
  data,
  refTable,
  xlab = "",
  ylab = "Landings ['000 t]",
  fillLegendTitle = "Stock"
)
}
\arguments{
\item{data}{data.frame Contains information on the landings (or catch) in
tonnes by stock and metier/gear grouping from the fleet data used at
WGMIXFISH-ADVICE. Stock variable names (`stock`) should match those of
\code{\link[mixfishtools]{refTable}}.}

\item{refTable}{data.frame A look-up reference table for stocks and associated attributes.
The \code{refTable} data.frame lists stock names and
corresponding colours for consistency across plots. To be used as a look-up
table in converting between variable stock names and printed ones.

\itemize{
  \item 1) stock - ICES stock codes used in advice
  \item 2) order - stock order to be used in plots
  \item 3) col - stock colors for plots (e.g. pals::brewer.paired())
  \item 4) stock_short - short stock name used in mixed fishery model
}}

\item{xlab}{character X-axis label (Default (blank): `xlab = ""`)}

\item{ylab}{character Y-axis label (Default: `ylab = "Landings [t]"`)}

\item{fillLegendTitle}{character Fill legend title

Other required variables include: `metier` which defines the metier code or gear
grouping code; `value` the value of landings (or catch) for each `stock` and `metier`}
}
\value{
plot output of class ggplot
}
\description{
Bar chart of landings by stock and by metier/gear groupings.
Used in WGMIXFISH-ADVICE
}
\details{
Users will need to provide the data object to produce the plot.
}
\examples{

# make example data
data(stfMtStkSum)
head(stfMtStkSum)
data(refTable)
head(refTable)

data <- stfMtStkSum

# add metier_cat
tmp <- strsplit(data$metier, ".", fixed = TRUE)
data$metier_cat <- unlist(lapply(tmp, FUN = function(x){x[1]}))

# select final data year and a single scenario, and aggregated total landings
# by stock and metier
datYr <- 2020
data <- subset(data, year == datYr & scenario == "min")
agg <- aggregate(landings ~ metier_cat + stock, data, FUN = sum, na.rm = TRUE)

# In the North Sea model, all Nephrops FUs area aggregated together
agg$isNEP <- seq(nrow(agg)) \%in\% grep("NEP", agg$stock)
agg1 <- subset(agg, !isNEP)[,c(1:3)]
agg2 <- aggregate(landings ~ metier_cat, data = subset(agg, isNEP),
  FUN = sum, na.rm = TRUE)
agg2$stock <- "Nephrops"
agg <- merge(agg1, agg2, all = TRUE)
agg <- agg[,c("stock", "metier_cat", "landings")]

names(agg) <- c("stock", "metier","value")
agg

# subset included metiers
metIncl <- c("TR1", "TR2", "BT1", "BT2", "GN1", "GT1", "LL1", "beam_oth",
  "pots", "OTH", "MIS")
agg <- subset(agg, metier \%in\% metIncl)

# replace stock with ICES stock code
agg$stock <- refTable$stock[match(agg$stock, refTable$stock_short)]

plot_landByMetStock(data = agg, refTable)

}
