

# refTable ----------------------------------------------------------------
# Look-up file for plotting order and colors by stock

load(file = "../2021_NrS_MixedFisheriesAdvice/tmp/plot_funs/tableREf.Rdata", verbose = TRUE)
refTable <- tableREf
refTable$lab[16] <- "Sole 7d"
refTable <- refTable[c(1:2, 13:20, 3:12, 21),]
refTable$order <- seq(nrow(refTable))
refTable

refTable
refTable <- refTable[,c("Advice_name", "order", "col", "stock")]
names(refTable) <- c("stock", "order", "col", "stock_short")
refTable
save(refTable, file = "data/refTable.rda")


# stfFltStkSum --------------------------------------------------------------
# fleet and stock summary of short-term forecasts (FLBEIA::fltStkSum)

load(file = "../2021_NrS_MixedFisheriesAdvice/tmp/plot_funs/stfFltStkSum.Rdata", verbose = TRUE)
head(stfFltStkSum)
stfFltStkSum <- as.data.frame(stfFltStkSum)
save(stfFltStkSum, file = "data/stfFltStkSum.rda")



# stfFltSum --------------------------------------------------------------
# fleet summary of short-term forecasts (FLBEIA::fltStkSum)

load(file = "../2021_NrS_MixedFisheriesAdvice/tmp/plot_funs/stfFltSum.Rdata", verbose = TRUE)
head(stfFltSum)
stfFltSum <- as.data.frame(stfFltSum)
save(stfFltSum, file = "data/stfFltSum.rda")



