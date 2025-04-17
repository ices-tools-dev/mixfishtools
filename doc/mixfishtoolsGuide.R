## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  cache = FALSE, # TRUE for speed. Remember to empty cache after changes to cached R code sections
  cache.path = "cache/", fig.path = "tex/",
  echo = TRUE, 
  collapse = TRUE,
  comment = "#>",
  fig.width = 6, fig.height = 5, dpi=300, 
  out.width="600px", out.height="500px"
)

## ----ggplot2_adapt_ex, eval=FALSE, include=TRUE-------------------------------
# p <- plot_effortFltStk(data, refTable)
# p <- p + theme(text = element_text(size = 12)) # adjust font size
# print(p)

## ----install, eval=FALSE, include=TRUE----------------------------------------
# # install pre-compiled package (preferred)
# install.packages('mixfishtools', repo = 'https://ices-tools-prod.r-universe.dev')
# 
# # install from source
# library(remotes)
# install_github(repo = "ices-tools-dev/mixfishtools")

## ----load, message=FALSE, warning=FALSE---------------------------------------
library(mixfishtools)

## ----load_extra, message=FALSE, warning=FALSE---------------------------------
library(mixfishtools)

## ----plot_catchScenStk_data_prep, include=FALSE-------------------------------
# make example data
data(stfFltStkSum)
data("refTable")

# subset data to advice year and restrictive stocks
advYr <- 2022 # advice year
restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
  "SOL-NS", "TUR", "WHG-NS", "WIT")
stfFltStkSum <- subset(stfFltStkSum, year == advYr & stock %in% restr.stks)

# data for plotting (catch by scenario and stock)
data <- aggregate(catch ~ scenario + stock, data = stfFltStkSum, FUN = sum)

# replace short stock name with ICES stock code
data$stock <- refTable$stock[match(data$stock, refTable$stock_short)]

# re-order scenarios (sq_E, max, min, ... )
data$scenario <- factor(data$scenario,
  levels = c("min", "max", "sq_E", "cod-ns"),
  labels = c("min", "max", "sq_E", "cod-ns"))

## ----plot_catchScenStk_data---------------------------------------------------
data

## ----plot_catchScenStk_adv_prep, include=FALSE--------------------------------
# make data.frame of advice reference values 
adv <- rbind(
  data.frame(stock = "COD-NS", advice = 14276, lower = 9701, upper = 14276),
  data.frame(stock = "HAD", advice = 128708, lower = 111702, upper = 128708),
  data.frame(stock = "PLE-EC", advice = 6365, lower = 4594, upper = 6365),
  data.frame(stock = "PLE-NS", advice = 142507, lower = 101854, upper = 195622),
  data.frame(stock = "POK", advice = 49614, lower = 30204, upper = 49614),
  data.frame(stock = "SOL-EC", advice = 1810, lower = 1068, upper = 2069),
  data.frame(stock = "SOL-NS", advice = 15330, lower = 9523, upper = 21805),
  data.frame(stock = "TUR", advice = 3609, lower = 2634, upper = 4564),
  data.frame(stock = "WHG-NS", advice = 88426, lower = 70169, upper = 91703),
  data.frame(stock = "WIT", advice = 1206, lower = 875, upper = 1206)
)

# replace short stock name with ICES stock code
adv$stock <- refTable$stock[match(adv$stock, refTable$stock_short)]

## ----plot_catchScenStk_adv----------------------------------------------------
adv

## ----plot_catchScenStk_output, out.width="70%", out.height="70%"--------------
p <- plot_catchScenStk(data = data, adv = adv)
# print(p)

fname <- paste0(tempfile(), ".png")
png(fname, width = 6, height = 5, units = "in", res = 400)
print(p)
out <- dev.off()

tmp <- png::readPNG(fname)
knitr::include_graphics(fname, dpi = floor(dim(tmp)[2]/6))

## ----plot_effortFltStk_data_prep, include=FALSE-------------------------------
data(refTable) # reference table with stock advice names, colors, order, etc.
data(stfFltSum) # summary of fleet-related variables (e.g. effort)
data(stfFltStkSum) # summary of fleet/stock-related catch variables

## get data from advice year

# catches by fleet and stock
advYr <- 2022 # advice year
df <- subset(stfFltStkSum, scenario == "min" & year == advYr)

## effort by fleet and scenario
eff <- subset(
 stfFltSum, scenario == "min" & year == advYr)[,c("fleet", "effort")]
sqEff <- subset(
 stfFltSum, scenario == "sq_E" & year == advYr)[,c("fleet", "effort")]
names(sqEff)[2] <- "sqEffort"
eff <- merge(x = eff, y = sqEff, all.x = TRUE)
df <- merge(x = df, y = eff, all.x = TRUE)
df$quotaEffort <- df$effort / df$quotaUpt

## Determine most- and least-limiting stock by fleet
# restrictive stocks
restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
 "SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9")
fls <- unique(df$fleet)
df2 <- vector("list", length(fls))
names(df2) <- fls
for(i in seq(fls)){
 tmp <- subset(df, fleet == fls[i])
 tmp$Limitation <- NA # initial NA setting for all stocks

 # most-limiting (highest quota uptake in min scenario)
 mostLimStk <- subset(tmp, stock %in% restr.stks)
 mostLimStk <- mostLimStk$stock[which.max(mostLimStk$quotaUpt)]
 tmp$Limitation[which(tmp$stock == mostLimStk)] <- "most"

 # least-limiting (lowest quota uptake in max scenario)
 leastLimStk <- subset(stfFltStkSum, scenario == "max" & year == advYr &
   fleet == fls[i] & stock %in% restr.stks)
 leastLimStk <- leastLimStk$stock[which.min(leastLimStk$quotaUpt)]
 tmp$Limitation[which(tmp$stock == leastLimStk)] <- "least"

 # return result
 df2[[i]] <- tmp
}
df2 <- do.call("rbind", df2)

# replace short stock names with ICES stock codes
df2$stock <- refTable$stock[match(df2$stock, refTable$stock_short)]

data <- df2[,c("fleet", "stock", "catch", "Limitation", "quotaEffort", "sqEffort")]

## ----plot_effortFltStk_data---------------------------------------------------
data[1:100,]

## ----refTab-------------------------------------------------------------------
data("refTable")
refTable

## ----plot_effortFltStk_output, out.width="70%", out.height="70%"--------------
p <- plot_effortFltStk(data = data, refTable = refTable)

fname <- paste0(tempfile(), ".png")
png(fname, width = 8, height = 10, units = "in", res = 400)
print(p)
out <- dev.off()

tmp <- png::readPNG(fname)
knitr::include_graphics(fname, dpi = floor(dim(tmp)[2]/6))

## ----plot_landByStock_data_prep, include = FALSE------------------------------
# make example data
data(stfFltStkSum)
head(stfFltStkSum)

data(refTable)
head(refTable)

# select final data year and a single scenario, and aggregated total landings
datYr <- 2020
dat <- subset(stfFltStkSum, year == datYr & scenario == "min")
agg <- aggregate(landings ~ stock, dat, sum, na.rm = TRUE)

# In the North Sea model, all Nephrops FUs area aggregated together
agg$isNEP <- seq(nrow(agg)) %in% grep("NEP", agg$stock)

agg <- rbind(subset(agg, !isNEP)[,c(1:2)],
  data.frame(stock = "Nephrops", landings = sum(subset(agg, isNEP)$landings)))

# replace stock with ICES stock code
agg$stock <- refTable$stock[match(agg$stock, refTable$stock_short)]

names(agg) <- c("stock", "value")
data <- agg

## ----plot_landByStock_data----------------------------------------------------
data

## ----plot_landByStock_output, out.width="70%", out.height="70%"---------------
p <- plot_landByStock(data = data, refTable)

fname <- paste0(tempfile(), ".png")
png(fname, width = 6, height = 6, units = "in", res = 400)
print(p)
out <- dev.off()

tmp <- png::readPNG(fname)
knitr::include_graphics(fname, dpi = floor(dim(tmp)[2]/6))


## ----plot_landByMetStock_prep, include = FALSE--------------------------------
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
agg$isNEP <- seq(nrow(agg)) %in% grep("NEP", agg$stock)
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
agg <- subset(agg, metier %in% metIncl)

# replace stock with ICES stock code
agg$stock <- refTable$stock[match(agg$stock, refTable$stock_short)]

data <- agg

## ----plot_landByMetStock_data-------------------------------------------------
data

## ----plot_landByMetStock_output, out.width="70%", out.height="70%"------------
p <- plot_landByMetStock(data = data, refTable)

fname <- paste0(tempfile(), ".png")
png(fname, width = 6, height = 5, units = "in", res = 400)
print(p)
out <- dev.off()

tmp <- png::readPNG(fname)
knitr::include_graphics(fname, dpi = floor(dim(tmp)[2]/6))


## ----plot_catchComp_data_prep, include=FALSE----------------------------------
# prepare example data
data(refTable)
data(stfMtStkSum)

# subset data to a single scenario (e.g. min)
data <- subset(stfMtStkSum, scenario == "min")

# add country and area identifiers (if desired)
tmp <- strsplit(data$metier, ".", fixed = TRUE)
data$area <- unlist(lapply(tmp, FUN = function(x){ifelse(length(x)==2, x[2], NA)}))
tmp <- strsplit(data$fleet, "_", fixed = TRUE)
data$country <- unlist(lapply(tmp, FUN = function(x){ifelse(length(x)==2, x[1], NA)}))


# replace stock with ICES stock code
data$stock <- refTable$stock[match(data$stock, refTable$stock_short)]

## ----plot_catchComp_data------------------------------------------------------
data[1:100,]

## ----plot_catchComp_output, out.width="70%", out.height="70%"-----------------
selectors <- c("year")
divider <- c("fleet")
p <- plot_catchComp(data, refTable, filters = NULL, selectors, divider, yvar = "catch")

# ggplot format adjustments
p <- p + theme(text = element_text(size = 8),
  axis.text.x = element_text(angle = 90, vjust = 0, hjust=1)) +
  facet_wrap(divider,  scales = "fixed") # remove free axes

fname <- paste0(tempfile(), ".png")
png(fname, width = 6, height = 6, units = "in", res = 400)
suppressWarnings(print(p))
out <- dev.off()

tmp <- png::readPNG(fname)
knitr::include_graphics(fname, dpi = floor(dim(tmp)[2]/6))

## ----plot_catchComp_output2, out.width="70%", out.height="70%"----------------
selectors <- c("country", "metier")
divider <- c("area")
p <- plot_catchComp(data,refTable,filters = NULL,selectors, divider)
p <- p + theme(text = element_text(size = 8),
  axis.text.x = element_text(angle = 90, vjust = 0, hjust=1)) 

fname <- paste0(tempfile(), ".png")
png(fname, width = 7, height = 6, units = "in", res = 400)
suppressWarnings(print(p))
out <- dev.off()

tmp <- png::readPNG(fname)
knitr::include_graphics(fname, dpi = floor(dim(tmp)[2]/6))

## ----plot_catchComp_output3, out.width="70%", out.height="70%"----------------
filters <- list(year = 2020) # e.g. last historical data year
selectors <- c("metier")
divider <- c("country")

p <- plot_catchComp(data, refTable, filters, selectors, divider)
p <- p + theme(text = element_text(size = 8),
  axis.text.x = element_text(angle = 90, vjust = 0, hjust=1)) 

fname <- paste0(tempfile(), ".png")
png(fname, width = 7, height = 6, units = "in", res = 400)
suppressWarnings(print(p))
out <- dev.off()

tmp <- png::readPNG(fname)
knitr::include_graphics(fname, dpi = floor(dim(tmp)[2]/6))

## ----plot_overUnderFltStk_data_prep, include=FALSE----------------------------
data(refTable) # reference table with stock advice names, colors, order, etc.
data(stfFltSum) # summary of fleet-related variables (e.g. effort)
data(stfFltStkSum) # summary of fleet/stock-related catch variables

## get data from advice year

# catches by fleet and stock
advYr <- 2022 # advice year
df <- subset(stfFltStkSum, scenario == "min" & year == advYr)

## effort by fleet and scenario
eff <- subset(
 stfFltSum, scenario == "min" & year == advYr)[,c("fleet", "effort")]
sqEff <- subset(
 stfFltSum, scenario == "sq_E" & year == advYr)[,c("fleet", "effort")]
names(sqEff)[2] <- "sqEffort"
eff <- merge(x = eff, y = sqEff, all.x = TRUE)
df <- merge(x = df, y = eff, all.x = TRUE)
df$quotaEffort <- df$effort / df$quotaUpt


## Determine most- and least-limiting stock by fleet
# restrictive stocks
restr.stks <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-EC",
 "SOL-NS", "TUR", "WHG-NS", "WIT", "NEP6", "NEP7", "NEP8", "NEP9")
fls <- unique(df$fleet)
df2 <- vector("list", length(fls))
names(df2) <- fls
for(i in seq(fls)){
 tmp <- subset(df, fleet == fls[i])
 tmp$Limitation <- NA # initial NA setting for all stocks

 # most-limiting (highest quota uptake in min scenario)
 mostLimStk <- subset(tmp, stock %in% restr.stks)
 mostLimStk <- mostLimStk$stock[which.max(mostLimStk$quotaUpt)]
 tmp$Limitation[which(tmp$stock == mostLimStk)] <- "most"

 # least-limiting (lowest quota uptake in max scenario)
 leastLimStk <- subset(stfFltStkSum, scenario == "max" & year == advYr &
   fleet == fls[i] & stock %in% restr.stks)
 leastLimStk <- leastLimStk$stock[which.min(leastLimStk$quotaUpt)]
 tmp$Limitation[which(tmp$stock == leastLimStk)] <- "least"

 # return result
 df2[[i]] <- tmp
}
df2 <- do.call("rbind", df2)

# replace short stock names with ICES stock codes
df2$stock <- refTable$stock[match(df2$stock, refTable$stock_short)]
data <- df2[,c("fleet", "stock", "catch", "quotaUpt", "Limitation")]

## ----plot_overUnderFltStk_data------------------------------------------------
data[1:100,]

## ----plot_overUnderFltStk_output, out.width="70%", out.height="70%"-----------
p <- plot_overUnderFltStk(data = df2, refTable = refTable)


fname <- paste0(tempfile(), ".png")
png(fname, width = 8, height = 10, units = "in", res = 400)
suppressWarnings(print(p))
out <- dev.off()

tmp <- png::readPNG(fname)
knitr::include_graphics(fname, dpi = floor(dim(tmp)[2]/6))

## ----plot_catchAlluvial_data_prep, include=FALSE------------------------------
data("stfMtStkSum")
data("refTable")

## create catch data by fleet, metier, and stock
dataYr <- 2020
# add country for filter (if desired)
stfMtStkSum$country <- unlist(lapply(strsplit(stfMtStkSum$fleet, "_"), 
  function(x){x[1]}))
# filter data
data <- subset(stfMtStkSum, year == 2020 & scenario == "min" &
  country == "BE")[,c("fleet", "metier", "stock", "landings")]
data$stock <- refTable$stock[match(data$stock, refTable$stock_short)]
names(data)[4] <- "value"

## ----plot_catchAlluvial_data--------------------------------------------------
data[1:100,]

## ----plot_catchAlluvial_output, out.width="70%", out.height="70%"-------------
p <- plot_catchAlluvial(data = data, refTable = refTable, text_size = 2)

fname <- paste0(tempfile(), ".png")
png(fname, width = 6, height = 6, units = "in", res = 400)
suppressWarnings(print(p))
out <- dev.off()

tmp <- png::readPNG(fname)
knitr::include_graphics(fname, dpi = floor(dim(tmp)[2]/6))

## ----plot_catchAlluvial_output2, out.width="70%", out.height="70%"------------
p <- plot_catchAlluvial(data = data, refTable = refTable, text_size = 2, 
  text_repel = TRUE, stratum_width = 0.2, nudge_x = 0.3, mult_x = c(0.1, 0.3))

fname <- paste0(tempfile(), ".png")
png(fname, width = 6, height = 6, units = "in", res = 400)
suppressWarnings(print(p))
out <- dev.off()

tmp <- png::readPNG(fname)
knitr::include_graphics(fname, dpi = floor(dim(tmp)[2]/6))

## ----plot_catchAlluvial_output3, out.width="70%", out.height="70%"------------
p <- plot_catchAlluvial(data = data, refTable = refTable, text_size = 2, 
  text_repel = TRUE, stratum_width = 0.2, nudge_x = 0.3, mult_x = c(0.1, 0.3), 
  addLegend = FALSE, stratum_col = "grey90")

fname <- paste0(tempfile(), ".png")
png(fname, width = 6, height = 6, units = "in", res = 400)
suppressWarnings(print(p))
out <- dev.off()

tmp <- png::readPNG(fname)
knitr::include_graphics(fname, dpi = floor(dim(tmp)[2]/6))

