## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  cache = FALSE, cache.path = 'cache/',
  fig.path = 'tex/',
  fig.align = 'center', comment = NA, 
  message = FALSE, warning = FALSE, echo = TRUE,
  tidy.opts=list(width.cutoff = 60), tidy = FALSE)
options(knitr.kable.NA = '')


iFig = 0
iTab = 0


# Define a custom print method for functions
print_function <- function(x) {
  cat(x, "<-", paste(capture.output(print.default(get(x))), collapse = "\n"))
}


## ----packages, message=FALSE, warning=FALSE-----------------------------------
# data manipulation and plotting
library(tidyr)
library(dplyr)
library(ggplot2)

# FLR
library(FLCore) # https://github.com/flr/FLCore
library(ggplotFL) # https://github.com/flr/ggplotFL

# SAM
library(stockassessment) # https://github.com/fishfollower/SAM
library(FLfse) # https://github.com/flr/FLfse

# SPiCT
library(spict) # https://github.com/DTUAqua/spict

# to build output
library(knitr)
library(kableExtra)


## ----blankFLStock, echo=TRUE--------------------------------------------------

# make blank FLStock
flq <- FLQuant(0, dimnames=list(age=3:10, year=1967:2022))
stock_estimated <- FLStock(flq)

# name of stock and description of stock object
name(stock_estimated) <- "pok.27.3a46"
desc(stock_estimated) <- "pok.27.3a46 - FLStock containing stock assessment model estimates"

# set Fbar age range
stock_estimated@range[c("minfbar", "maxfbar")] <- c(4,7)

# add units (adjust if necessary)
units(stock_estimated) <- standardUnits(stock_estimated) # add standard units
unlist(units(stock_estimated))

# summary
stock_estimated

## ----slotInput----------------------------------------------------------------
# maturity at age
A <- array(c(0.19, 0.35, 0.58, 0.82, 0.94, 0.98, 0.99, 1),  
  dim = c(8, 56), dimnames=list(age = 3:10, year = 1967:2022))
A <- FLQuant(A)
stock_estimated@mat <- A

## ----flstockSlots-------------------------------------------------------------
slotNames(stock_estimated)

## ----samExtraction, include=FALSE---------------------------------------------
# example SAM fitted object
data(nscodData)
data(nscodConf)
data(nscodParameters)
fit <- sam.fit(nscodData, nscodConf, nscodParameters, silent = TRUE)

# by default, SAM2FLStock provides the input data only (along with estimates 
# for F and stock)
stock_input <- FLfse::SAM2FLStock(fit)

# set other arguments to extract all estimated values
stock_estimated <- FLfse::SAM2FLStock(fit, catch_estimate=TRUE, mat_est=TRUE, 
  stock.wt_est=TRUE, catch.wt_est=TRUE, m_est=TRUE)

# plot comparison using ggplotFL
L <- FLStocks(obs = stock_input, est = stock_estimated)

## ----samFLStockPlot-----------------------------------------------------------
plot(L) + aes(linetype = stock)

## ----qc-code-2, echo=TRUE, eval=TRUE, results="as.is", collapse = TRUE--------

# check that sum of product (SOP) calculations equal (or close to)
# aggregate slots (in the best case, all.equal is TRUE)
all.equal(c(discards(stock_estimated)), c(computeDiscards(stock_estimated)))
all.equal(c(landings(stock_estimated)), c(computeLandings(stock_estimated)))
all.equal(c(catch(stock_estimated)), c(computeCatch(stock_estimated)))
all.equal(c(stock(stock_estimated)), c(computeStock(stock_estimated)))
all.equal(c(landings(stock_estimated)+discards(stock_estimated)),
  c(catch(stock_estimated)))


# check that the weighted mean of landings.wt and discards.wt equals 
# (or is close to) catch.wt
df <- as.data.frame(stock_estimated)
df <- df |> filter(slot %in%
    c("landings.wt", "discards.wt", "landings.n", "discards.n", "catch.wt")) |>
  pivot_wider(names_from = slot, values_from = data)

df <- df |> rowwise() |>
  mutate(catch.wt.calc =
      weighted.mean(c(landings.wt, discards.wt), c(landings.n, discards.n))) %>%
  ungroup()

all.equal(c(df$catch.wt), c(df$catch.wt.calc))
# plot(catch.wt.calc ~ catch.wt, df); abline(0,1)

## ----spict2flbeiaFun, class.source = 'fold-hide'------------------------------
#' Extract FLStock and biomass dynamics parameters from spict object
#'
#' @description
#' The `spict2flbeia` will extract FLStock and biomass dynamics parameters
#'   from a spict object. As there are no individual weights in surplus
#'   production models, the function assigns a value of 1.0 to all individual
#'   weight slots (`@stock.wt`, `@catch.wt`, `@landings.wt`, `@discards.wt`).
#'   Individual numbers slots (`@stock.n`, `@catch.n`, `@landings.n`,
#'   `@discards.n`) are thus identical to the the sum of products slots
#'   (`@stock`, `@catch`, `@landings`, `@discards`)
#'
#'
#' @param spict_fit fitted spict object fit with \code{\link[spict]{fit.spict}}
#' @param wt_units character. Units to apply to individual weight at age.
#'   Default is "kg", which when combined with with argument `n_units`
#'   (Default "10^3"), will describe the sum of products (e.g. "t")
#' @param n_units character. Units to apply to individual numbers at age
#'   (Default "10^3").
#' @param catch_units character. Units to apply to catches (Default "t").
#' @param stock_name character. Name of stock (Optional, Default = "stk")
#' @param disc numeric vector. Optional vector of discard values. Default
#'   assumes zero discards.
#'
#' @return list. Results contain an FLStock object (`$stk`) and biomass dynamics
#'   parameters required for conditioning those types of stocks in FLBEIA
#'   (`$BDinfo`); in particular, Pella-Tomlinson parameters
#'   (`$BDinfo$PellaTomlinson_pars`) and reference points (`$BDinfo$refPts`)
#'
#' @export
#'
spict2flbeia <- function(
  spict_fit, # fitted spict object
  wt_units = "kg", # units for weight at age
  n_units = "10^3", # units for numbers
  catch_units = "t", # units for catch
  stock_name = "stk", # optional stock name
  disc = NULL # discard time series
){

  # extract biomass and make FLStock
  Bs <- as.data.frame(get.par("logB", spict_fit, exp = TRUE))
  Bs$time <- as.numeric(rownames(Bs))
  Bs$year <- floor(Bs$time)
  yrs <- sort(unique(Bs$year))
  tmp <- data.frame(year = yrs)
  tmp$B <- Bs$est[match(tmp$year, Bs$time)]
  flq <- FLQuant(tmp$B, dim=c(1,nrow(tmp)), dimnames=list(age=1, year=tmp$year), units="t")

  stock <- FLStock(stock=flq, name = stock_name)
  stock@stock.wt[1,] <- 1
  stock@stock.n <- stock@stock / stock@stock.wt
  stock@stock.wt@units <- wt_units
  stock@stock.n@units <- n_units

  # F or harvest rate (averaged over year)
  Fs <- as.data.frame(get.par("logF", spict_fit, exp = TRUE))
  Fs$time <- as.numeric(rownames(Fs))
  Fs$year <- floor(Fs$time)
  tmp <- aggregate(Fs$est, list(year=Fs$year), FUN = mean) # take mean over year?
  names(tmp)[which(names(tmp)=="x")] <- "f"
  stock@harvest[,ac(yrs)] <- tmp$f[match(yrs, tmp$year)]
  stock@harvest@units <- "f"

  # catches
  Cs <- as.data.frame(get.par("logB", spict_fit, exp = TRUE) *
      get.par("logF", spict_fit, exp = TRUE) *
      spict_fit$inp$dt)
  Cs$time <- as.numeric(rownames(Cs))
  Cs$year <- floor(Cs$time)
  tmp <- aggregate(Cs$est, list(year=Cs$year), FUN = sum)
  names(tmp)[which(names(tmp)=="x")] <- "catch"

  stock@catch[,ac(yrs)] <- tmp$catch[match(yrs, tmp$year)]
  stock@catch.wt[,] <- 1
  stock@catch.n[] <- c(stock@catch / stock@catch.wt)
  stock@catch@units <- catch_units
  stock@catch.wt@units <- wt_units
  stock@catch.n@units <- n_units

  # discards
  if(!is.null(disc)){
    stock@discards[,ac(yrs)] <- disc
  }else{
    stock@discards[,ac(yrs)] <- 0
  }
  stock@discards.wt[1,] <- 1
  stock@discards.n[] <- c(stock@discards / stock@discards.wt)
  stock@discards@units <- catch_units
  stock@discards.wt@units <- wt_units
  stock@discards.n@units <- n_units

  # landings
  stock@landings <- stock@catch - stock@discards
  stock@landings.wt[1,] <- 1
  stock@landings.n[] <- c(stock@landings / stock@landings.wt)
  stock@landings@units <- catch_units
  stock@landings.wt@units <- wt_units
  stock@landings.n@units <- n_units

  ## Other pars (not relevant?) ===============================
  stock@mat[1,] <- 1
  stock@harvest.spwn[1,] <- 0
  stock@m[1,] <- 0
  stock@m.spwn[1,] <- 0


  # create BD data for FLBEIA ----------------------------------------------

  tab1 <- sumspict.parest(spict_fit)
  tab3 <- sumspict.states(spict_fit) # intermediate year
  tab5 <- sumspict.predictions(spict_fit) # forecast

  r.stk <- (get.par("logm", spict_fit, exp=T)[2]*
      get.par("logn", spict_fit, exp=T)[2]^
      (get.par("logn", spict_fit, exp=T)[2]/
          (get.par("logn", spict_fit, exp=T)[2]-1)))/
    get.par("logK", spict_fit, exp=T)[2]
  K.stk <- get.par("logK", spict_fit, exp=T)[2]
  p.stk <- get.par("logn", spict_fit, exp=T)[2] - 1

  res <- list()
  res$stk <- stock
  res$BDinfo$par.fixed <- spict_fit$par.fixed
  res$BDinfo$cov.fixed <- spict_fit$cov.fixed
  res$BDinfo$PellaTomlinson_pars <- c("r"=r.stk, "K"= K.stk, "p"=p.stk)

  # Add reference points
  res$BDinfo$refPts <- c(
    "Fmsy" = get.par("logFmsy", spict_fit, exp = TRUE)[,"est"],
    "Bmsy" = get.par("logBmsy", spict_fit, exp = TRUE)[,"est"],
    "MSY" = get.par("MSY", spict_fit)[,"est"]
  )

  return(res)
}

## ----spictCompare, fig.dim=c(6,5)---------------------------------------------
# example spict fit
data(pol)
fit <- fit.spict(pol$albacore)

# extract spict estimate
res <- spict2flbeia(spict_fit = fit)
stock_estimated <- res$stk
op <- par(mfcol = c(3,1), mar = c(3,4,2,2))
plotspict.biomass(fit)
plotspict.catch(fit)
plotspict.f(fit)
par(op)

# compare FLStock
plot(stock_estimated, metrics = list(SSB = ssb, Catch = catch, F = fbar))

## ----NepBlankCreation---------------------------------------------------------

# make blank FLStock
flq <- FLQuant(0, dimnames=list(age=1, year=2001:2019))
stock_input <- FLStock(flq)

# name of stock and description of stock object
name(stock_input) <- "nep.FU6"
desc(stock_input) <- "nep.FU6 - FLStock containing stock assessment model input"

# add and adjust units
units(stock_input) <- standardUnits(stock_input) # add standard units

# NEP units are typically non-standard for numbers, ind. weights, and harvest
units(stock_input@catch.n) <- units(stock_input@landings.n) <- 
  units(stock_input@discards.n) <- 
  units(stock_input@stock.n) <- "1e6" # millions
units(stock_input@catch.wt) <- units(stock_input@landings.wt) <- 
  units(stock_input@discards.wt) <-
  units(stock_input@stock.wt) <- "g" # grams
units(harvest(stock_input)) <- "hr" # harvest rate (proportion)

# summary
stock_input

## ----NepDirectCreation--------------------------------------------------------
## Main numbers, ind. weight, and tot. weight slots

stock_input@landings[] <- c(2574, 1953, 2245, 2153, 3094, 4903, 2966, 1220, 
  2713, 1443, 2070, 2460, 2982, 2503, 1371, 1854, 1963, 1807, 4359)
stock_input@landings.wt[] <- c(21, 20, 22, 23, 24, 23, 25, 27, 24, 25, 27, 27, 
  28, 30, 29, 28, 29, 29, 28)
stock_input@landings.n[] <- stock_input@landings / stock_input@landings.wt

# discard ratio (dead discards)
DR <- c(67, 46, 42, 42, 35, 31, 25, 25, 29, 23, 23, 27, 30, 14.9, 29, 29, 
  22, 21, 20)/100
stock_input@catch.n[] <- stock_input@landings.n / (1-DR)
stock_input@discards.n[] <- stock_input@catch.n - stock_input@landings.n
stock_input@discards.wt[] <- c(9.6, 9.5, 9.6, 9.2, 10.3, 10.6, 10.9, 11, 10.5, 
  11.7, 11, 10.2, 9.8, 13.6, 10, 10.2, 10.3, 11.2, 11.6)
stock_input@discards <- computeDiscards(stock_input)

# apply weighted mean of landings.wt and discards.wt for catch.wt
stock_input@catch.wt <- (stock_input@landings.wt * stock_input@landings.n + 
    stock_input@discards.wt * stock_input@discards.n) / 
  (stock_input@landings.n + stock_input@discards.n)
stock_input@catch <- computeCatch(stock_input)

# stock - survey estimate, stock.wt same as catch.wt  
stock_input@stock.n[] <- c(1685, 1048, 1085, 1377, 1657, 1244, 858, 987, 682, 
  785, 878, 758, 706, 755, 565, 697, 902, 950, 1163)
stock_input@stock.wt <- stock_input@catch.wt
stock_input@stock <- computeStock(stock_input)

# harvest rates
stock_input@harvest[] <- c(20, 16.1, 15.3, 10.9, 11.5, 24, 17.8, 5.9, 22, 9.2, 
  10.9, 15.7, 21, 12.7, 11.1, 12.8, 9.3, 8.1, 16.1)/100

# less essential slots (but needed to calculate ssb in summary plot)
stock_input@mat[] <- 1
stock_input@harvest.spwn[1,] <- 0
stock_input@m[1,] <- 0
stock_input@m.spwn[1,] <- 0


## ----NepIndirectCreation1-----------------------------------------------------
# read in data (i.e. read.csv, read.table)
df <- as.data.frame(stock_input)
df <- df |> filter(!slot %in% c("m", "mat", "harvest.spwn", "m.spwn"))
df <- df |> pivot_wider(id_cols = year, names_from = slot, values_from = data)
df

## -----------------------------------------------------------------------------
# convert to "long" format
mdf <- df |> pivot_longer(cols = !year, names_to = "slot", values_to = "data")

# add age = 1
mdf$age <- "1"

# convert to FLStock
stock_input <- as.FLStock(mdf)

# name of stock and description of stock object
name(stock_input) <- "nep.FU"
desc(stock_input) <- "nep.FU - FLStock containing stock assessment model input"


# add and adjust units
units(stock_input) <- standardUnits(stock_input) # add standard units

# NEP units are typically non-standard for numbers, ind. weights, and harvest
units(stock_input@catch.n) <- units(stock_input@landings.n) <- 
  units(stock_input@discards.n) <- 
  units(stock_input@stock.n) <- "1e6" # millions
units(stock_input@catch.wt) <- units(stock_input@landings.wt) <- 
  units(stock_input@discards.wt) <-
  units(stock_input@stock.wt) <- "g" # grams
units(harvest(stock_input)) <- "hr" # harvest rate (proportion)

## less essential slots (but needed to calculate ssb in summary plot)
stock_input@mat[] <- 1
stock_input@harvest.spwn[1,] <- 0
stock_input@m[1,] <- 0
stock_input@m.spwn[1,] <- 0

## -----------------------------------------------------------------------------
plot(stock_input)

## ----qcSOP, collapse = TRUE---------------------------------------------------
# check that sum of product (SOP) calculations equal (or close to)
# aggregate slots (in the best case, all.equal is TRUE)
all.equal(c(discards(stock_input)), c(computeDiscards(stock_input)))
all.equal(c(landings(stock_input)), c(computeLandings(stock_input)))
all.equal(c(catch(stock_input)), c(computeCatch(stock_input)))
all.equal(c(stock(stock_input)), c(computeStock(stock_input)))
all.equal(c(landings(stock_input)+discards(stock_input)),
  c(catch(stock_input)))

# check that the weighted mean of landings.wt and discards.wt equals 
# (or is close to) catch.wt
df <- as.data.frame(stock_input)
df <- df |> 
  pivot_wider(names_from = slot, values_from = data)

df <- df |> rowwise() |>
  mutate(catch.wt.calc =
      weighted.mean(c(landings.wt, discards.wt), c(landings.n, discards.n))) %>%
  ungroup()

all.equal(c(df$catch.wt), c(df$catch.wt.calc))
# plot(catch.wt.calc ~ catch.wt, df); abline(0,1)

# since we only have a single age group, should stock.wt equal to catch.wt?
all.equal(c(catch.wt(stock_input)), c(stock.wt(stock_input)))

# if harvest rates are reported, do they align with the ratio of catch/stock
# are inconsistencies related to survival rates for discards?
all.equal(c(harvest(stock_input)), c(computeHarvest(stock_input)))
tmp <- data.frame(year = df$year, harvest_rate = df$harvest, "catch_stock" = df$catch / df$stock)
tmp

## ----requestedStocks, echo=FALSE----------------------------------------------
tab <- list(
  WGNSSK = c("cod.27.47d20", "had.27.46a20", "ple.27.7d", "ple.27.420", 
    "pok.27.3a46", "sol.27.7d", "sol.27.4", "tur.27.4" ,"whg.27.47d", 
    "wit.27.3a47d", 
    "nep.fu.5", "nep.fu.6", "nep.fu.7", "nep.fu.8", "nep.fu.9", 
    "nep.fu.10", "nep.fu.32", "nep.fu.33", "nep.fu.34", "nep.27.4outFU"),
  WGCSE = c("cod.27.7a", "cod.27.7e-k", "had.27.7a", "had.27.7bce-k", 
    "ple.27.7a", "sol.27.7a", "sol.27.7e", "sol.27.7fg", "whg.27.7a", 
    "whg.27.7bce-k", 
    "nep.fu.14", "nep.fu.15", "nep.fu.16", "nep.fu.17", "nep.fu.19", 
    "nep.fu.20-21", "nep.fu.22", "nep.27.7outFU"),
  WGBIE = c("ank.27.78abd", "ank.27.8c9a", "bss.27.8ab",  
    "hke.27.3a46-8abd", "hke.27.8c9a", "ldb.27.8c9a", "meg.27.7b-k8abd", 
    "meg.27.8c9a", "mon.27.8c9a", "mon.27.78abd", "pol.27.89a",  "sol.27.8ab",  
    "whg.27.89a",
    "nep.fu.23", "nep.fu.24", "nep.fu.2324"),
  other_WG = c("whb.27.1-91214", "hom.27.2a4a5b6a7a-ce-k8", "mac.27.nea", 
    "sdv.27.nea", "rjc.27.8abd", "rjn.27.678abd", "rju.27.8ab")
)

# Create a matrix with each row containing the values of each list element
tmp <- unlist(lapply(tab, function(x) paste(x, collapse = ", ")), use.names = F)
# tmp <- unname(tmp)
# Convert the matrix to a data frame
df <- data.frame("WG" = names(tab), stocks = tmp)

# Print the data frame
kable(df, longtable = TRUE, booktabs = TRUE) |>
  kable_styling(bootstrap_options = "striped", font_size = 11, 
    repeat_header_continued = TRUE, position = "center") |>
  kableExtra::column_spec(c(1), width="2.5cm") |>
  kableExtra::column_spec(c(2), width="10cm") 

## ----catch-tab-ex,echo=F,results="asis",eval=TRUE-----------------------------

tab <- data.frame(stock= c("ANK","ANK","ANK","ANK","ANK","ANK"),
  year = c(2019,2019,2019,2019,2020,2020),
  category = c("Landings","Discards","Landings","Discards","Landings","Discards"),
  country=c("Belgium","Belgium","all","all","Belgium","Belgium"),
  area=c("8","8","oth","oth","8","8"),
  tonnes=c(999,999,999,999,999,999))
                  

kable(tab, longtable=T, booktabs = T,align="l") |> 
  kable_styling(bootstrap_options = "striped", font_size = 11, 
    repeat_header_continued = TRUE, position = "center") |>
  kableExtra::column_spec(c(1,2,5), width="1.5cm") |> 
  kableExtra::column_spec(c(3,4,6), width="2.5cm")

