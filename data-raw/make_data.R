

# refTable ----------------------------------------------------------------
# Look-up file for plotting order and colors by stock

# load(file = "../2021_NrS_MixedFisheriesAdvice/tmp/plot_funs/tableREf.Rdata", verbose = TRUE)
# refTable <- tableREf
# refTable$lab[16] <- "Sole 7d"
# refTable <- refTable[c(1:2, 13:20, 3:12, 21),]
# refTable$order <- seq(nrow(refTable))
# refTable
# save(refTable, file = "data-raw/refTable.Rdata")

load(file = "data-raw/refTable.Rdata")
refTable
refTable <- refTable[,c("Advice_name", "order", "col", "stock")]
names(refTable) <- c("stock", "order", "col", "stock_short")
refTable
tmp <- subset(refTable, stock_short == "NEP6-9")
tmp$stock <- tmp$stock_short <- "Nephrops"
tmp$order <- max(refTable$order) + 1
refTable <- rbind(refTable, tmp)
refTable

save(refTable, file = "data/refTable.rda")


# stfFltStkSum --------------------------------------------------------------
# fleet and stock summary of short-term forecasts (FLBEIA::fltStkSum)

load(file = "data-raw/stfFltStkSum.Rdata", verbose = TRUE)
head(stfFltStkSum)
stfFltStkSum <- as.data.frame(stfFltStkSum)
save(stfFltStkSum, file = "data/stfFltStkSum.rda", compress = "xz")



# stfFltSum --------------------------------------------------------------
# fleet summary of short-term forecasts (FLBEIA::fltStkSum)

load(file = "data-raw/stfFltSum.Rdata", verbose = TRUE)
head(stfFltSum)
stfFltSum <- as.data.frame(stfFltSum)
save(stfFltSum, file = "data/stfFltSum.rda", compress = "xz")


# stfMtStkSum -------------------------------------------------------------
# fleet /metiet / stock summary of short-term forecasts (FLBEIA::mtStkSum)


load(file = "data-raw/stfMtStkSum.Rdata", verbose = TRUE)
stfMtStkSum <- as.data.frame(stfMtStkSum)
head(stfMtStkSum)
save(stfMtStkSum, file = "data/stfMtStkSum.rda", compress = "xz")





# catchcomp ---------------------------------------------------------------

# dat <- read.csv(file = "data-raw/FO_CatchComp_Data.csv")
# head(dat)
# names(dat) <- c("year", "area", "country", "fleet", "metier", "stock",
#   "landings", "fleet_type")
# catchComp <- dat
# save(catchComp, file = "data/catchComp.rda")




# obsLan -------------------------------------------------------------
# observed stock landings

# dat <- read.csv(file = "data-raw/Figure4_StockLandings.csv")
# head(dat)
# dat <- dat[, c(1,3,4)]
# names(dat) <- c("stock", "value", "col")
# obsLan <- dat
# save(obsLan, file = "data/obsLan.rda")


load(file = "data/stfFltStkSum.rda")
head(stfFltStkSum)

datYr <- 2020
dat <- subset(stfFltStkSum, year == datYr & scenario == "min")
agg <- aggregate(landings ~ stock, dat, sum, na.rm = TRUE)

agg$isNEP <- seq(nrow(agg)) %in% grep("NEP", agg$stock)

agg <- rbind(subset(agg, !isNEP)[,c(1:2)],
  data.frame(stock = "Nephrops", landings = sum(subset(agg, isNEP)$landings)))

agg$stock <- refTable$stock[match(agg$stock, refTable$stock_short)]
agg$col <- refTable$col[match(agg$stock, refTable$stock)]

names(agg) <- c("stock", "value", "col")
agg


# obsLanMt -------------------------------------------------------------
# observed stock landings by metier
#
# dat <- read.csv(file = "data-raw/Figure3_MetierLandings.csv")
# head(dat)
# dat <- dat[, c(1,2,4,5)]
# names(dat) <- c("stock", "metier", "value", "col")
# unique(dat$Metier)
# obsLanMt <- dat
# save(obsLanMt, file = "data/obsLanMt.rda")


load(file = "data/stfMtStkSum.rda")
head(stfMtStkSum)

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




# wgnsskStocks - FLStock objects from WGNSSK ----------------------------------------------------------------

library(FLCore)
library(ggplotFL)

stkNames <- list("HAD", "NEP6", "PLE-EC", "PLE-NS", "POK", "POK_input", "WIT")
res <- lapply(stkNames, FUN = function(x){
  tmp <- load(file = file.path("data-raw", paste0(x, ".RData")), verbose = TRUE)
  stock <- get(tmp)
  return(stock)
})
names(res) <- stkNames

wgnsskStocks <- FLStocks(res)
plot(wgnsskStocks)

save(wgnsskStocks, file = "data/wgnsskStocks.rda", compress = "xz")



# haddockWts - haddock forecast weights ------------------------------------------------
cwt <- read.table("data-raw/had.27.46a20 - Forecast weights - catch-at-age.txt")
lwt <- read.table("data-raw/had.27.46a20 - Forecast weights - landings-at-age.txt")
dwt <- read.table("data-raw/had.27.46a20 - Forecast weights - discards-at-age incl BMS and IBC.txt")
swt <- read.table("data-raw/had.27.46a20 - Forecast weights - stock-at-age.txt")

haddockWts <- list(
  catch = cwt,
  landings = lwt,
  discards = dwt,
  stock = swt
)

save(haddockWts, file = "data/haddockWts.rda", compress = "xz")


