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

## ----packages, message=FALSE, warning=FALSE-----------------------------------
# data manipulation and plotting
library(tidyr)
library(dplyr)
library(ggplot2)

# FLR
library(FLCore) # https://github.com/flr/FLCore
library(FLasher) # https://github.com/flr/FLasher
library(ggplotFL) # https://github.com/flr/ggplotFL

# for example data sets
library(mixfishtools) # https://github.com/ices-tools-dev/mixfishtools

# to build output
library(knitr)

## ----FLStockCreation----------------------------------------------------------
data("wgnsskStocks")

stkInp <- wgnsskStocks[["POK_input"]]
stkEst <- wgnsskStocks[["POK"]]

L <- FLStocks(list(input = stkInp, estimated = stkEst))
plot(L) + 
  aes(linetype = stock) +
  scale_color_manual(values = c(8,1)) + 
  scale_linetype_manual(values = c(1,2)) +
  theme_bw()


## ----FLStockExtention---------------------------------------------------------
# forecast year definitions
yrAssess <- 2023 # final year of assessment data
yrNow <- 2024 # intermediate year
yrTAC <- 2025 # advice year
yrTACp1 <- 2026 # advice year +1 (needed to get SSB at end of yrTAC)

# extend FLStock object
stkProj <- stf(object = stkEst, nyears = 3, wts.nyears = 3, 
  fbar.nyears = 3, f.rescale = TRUE, disc.nyears = 3)

# stock-recruitment model (manual input within a geometric mean model)
srPar <- FLPar(c(94047, 94047, 94047), 
  dimnames = list(params="a", year = c(yrNow, yrTAC, yrTACp1), iter = 1))
srMod <- FLSR(model = "geomean", params = srPar)

# View the extended FLStock
df <- as.data.frame(stkProj)
df <- subset(df, slot %in% c("landings.wt", "discards.wt", "catch.wt", "m", "mat", "harvest") & year > (yrAssess-20))
df$forecast <- df$year %in% c(yrNow, yrTAC, yrTACp1)

ggplot(df) + aes(x = year, y = data, group = age, color = forecast) +
  facet_wrap(~slot, scales = "free_y") +
  geom_line(show.legend = F) + 
  scale_color_manual(values = c(8,1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  theme_bw()


## ----stfControl---------------------------------------------------------------
# stf control (Fsq, followed by 2 years at Fmsy)
Fmsy <- 0.316
ctrl <- fwdControl( 
  data.frame(
    year = c(yrNow, yrTAC, yrTACp1),
    value = c(1, Fmsy, Fmsy),
    quant = c("f"),
    relYear = c(yrAssess, NA, NA)                               
  )
)

## ----forecast-----------------------------------------------------------------
# projection
stkProj <- fwd(object = stkProj, control = ctrl, sr = srMod)

# plot
L <- FLStocks(list(assessment = stkEst, forecast = stkProj[,ac(yrAssess:yrTACp1)]))
plot(L) + 
  # aes(linetype = stock) +
  scale_color_manual(values = c(8,1)) + 
  # scale_linetype_manual(values = c(2,1)) +
  theme_bw()

## ----forecastCatchTarg--------------------------------------------------------
TACNow <- 73815
Fmsy <- 0.316

ctrlAlt <- fwdControl( 
  data.frame(
    year = c(yrNow, yrTAC, yrTACp1),
    value = c(TACNow, Fmsy, Fmsy),
    quant = c("catch", "f", "f")
  )
)

stkProjAlt <- fwd(object = stkProj, control = ctrlAlt, sr = srMod)

df <- data.frame(year = yrAssess:yrTACp1, 
  catch = c(catch(stkProjAlt)[, ac(yrAssess:yrTACp1)]),
  fbar = c(fbar(stkProjAlt)[, ac(yrAssess:yrTACp1)])
)

kable(df, digits = 3)


## ----compare------------------------------------------------------------------
# Reported output from single stock headline advice
stfRef <- data.frame(
  model = "NSSK",
  year = 2023:2026,
  catch = c(65865, 78251, 79071, NA),
  landings = c(62691, 74968, 75880, NA),
  fbar = c(0.32, 0.32, 0.316, NA),
  ssb = c(161756, 185632, 195899, 197298)
)

stfDet <- data.frame(
  model = "FLR",
  year = ac(yrAssess:yrTACp1),
  catch = c(catch(stkProj[,ac(yrAssess:yrTACp1)])),
  landings = c(landings(stkProj[,ac(yrAssess:yrTACp1)])),
  fbar = c(fbar(stkProj[,ac(yrAssess:yrTACp1)])),
  ssb = c(ssb(stkProj[,ac(yrAssess:yrTACp1)]))
)  

df <- merge(stfRef, stfDet, all = T)
df <- pivot_longer(df, cols = c(catch, landings, fbar, ssb), 
  names_to = "variable", values_to = "value")
df <- df |>
  filter(
    (variable %in% c("catch", "landings", "fbar") & year <= yrTAC) |
    (variable %in% c("ssb") & year <= yrTACp1))
    
df2 <- pivot_wider(df, names_from = model, values_from = value)
df2$percErr <- round((df2$FLR - df2$NSSK)/df2$NSSK * 100, 1)

ggplot(df) + aes(x = year, y = value, group = model, color = model, shape = model) +
  facet_wrap(~variable, scales = "free_y") +
  geom_line() +
  geom_point(size = 3, stroke = 1) +
  scale_shape_discrete(solid = F) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw()

kable(df2, digits = 3)

ggplot(df2) + aes(x = year, y = percErr) + 
  facet_wrap(~variable) +
  geom_col(fill = 4, color = 4) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_hline(yintercept = c(-10,10), linetype = 3) + 
  theme_bw()

## ----haddockInit--------------------------------------------------------------
# load haddock
stkEst <- wgnsskStocks[["HAD"]]

# trim to only historical years
minyear <- range(stkEst)["minyear"]
stkEst <- stkEst[,ac(minyear:yrAssess)]

# simple extension of FLStock
stkProj <- stf(object = stkEst, nyears = 3, wts.nyears = 3, 
  fbar.nyears = 3, f.rescale = TRUE, disc.nyears = 3)

# control
Fmsy <- 0.174
ctrl <- fwdControl( 
  data.frame(
    year = c(yrNow, yrTAC, yrTACp1),
    value = c(1, Fmsy, Fmsy),
    quant = c("f"),
    relYear = c(yrAssess, NA, NA)                               
  )
)

# stock-recruitment model
srPar <- FLPar(c(1788860, 1788860, 1788860), 
  dimnames = list(params="a", year = c(yrNow, yrTAC, yrTACp1), iter = 1))
srMod <- FLSR(model = "geomean", params = srPar)

# projection
stkProj1 <- fwd(object = stkProj, control = ctrl, sr = srMod)

# plot
L <- FLStocks(list(assessment = stkEst, forecast = stkProj1[,ac(yrAssess:yrTACp1)]))
plot(L) + 
  scale_color_manual(values = c(8,1)) + 
  theme_bw()


# Reported output from single stock headline advice
stfRef <- data.frame(
  model = "NSSK",
  year = 2023:2026,
  catch = c(60979, 66309, 112435, NA),
  landings = c(37702, 53277, 93661, NA),
  fbar = c(0.084, 0.084, 0.174, NA),
  ssb = c(580727, 606378, 535682, 420140)
)

stfDet1 <- data.frame(
  model = "FLR",
  year = ac(yrAssess:yrTACp1),
  catch = c(catch(stkProj1[,ac(yrAssess:yrTACp1)])),
  landings = c(landings(stkProj1[,ac(yrAssess:yrTACp1)])),
  fbar = c(fbar(stkProj1[,ac(yrAssess:yrTACp1)])),
  ssb = c(ssb(stkProj1[,ac(yrAssess:yrTACp1)]))
)  

df1 <- merge(stfRef, stfDet1, all = T)
df1 <- pivot_longer(df1, cols = c(catch, landings, fbar, ssb), 
  names_to = "variable", values_to = "value")
df1 <- df1 |>
  filter(
    (variable %in% c("catch", "landings", "fbar") & year <= yrTAC) |
    (variable %in% c("ssb") & year <= yrTACp1))
df1 <- pivot_wider(df1, names_from = model, values_from = value)
df1$percErr <- round((df1$FLR - df1$NSSK)/df1$NSSK * 100, 1)

ggplot(df1) + aes(x = year, y = percErr) + 
  facet_wrap(~variable) +
  geom_col(fill = 4, color = 4) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_hline(yintercept = c(-10,10), linetype = 3) + 
  theme_bw()


## ----haddockFix---------------------------------------------------------------
stkProj2 <- stkProj

# replace individual weights
data("haddockWts")
catch.wt(stkProj2)[, ac(yrNow:yrTACp1)][] <- t(haddockWts$catch) # catch weight-at-age
landings.wt(stkProj2)[, ac(yrNow:yrTACp1)][] <- t(haddockWts$landings) # landings weight-at-age
discards.wt(stkProj2)[, ac(yrNow:yrTACp1)][] <- t(haddockWts$discards) # discards weight-at-age
stock.wt(stkProj2)[, ac(yrNow:yrTACp1)][] <- t(haddockWts$stock) # catcstockh weight-at-age

stkProj2 <- fwd(object = stkProj2, control = ctrl, sr = srMod)

stfDet2 <- data.frame(
  model = "FLR",
  year = ac(yrAssess:yrTACp1),
  catch = c(catch(stkProj2[,ac(yrAssess:yrTACp1)])),
  landings = c(landings(stkProj2[,ac(yrAssess:yrTACp1)])),
  fbar = c(fbar(stkProj2[,ac(yrAssess:yrTACp1)])),
  ssb = c(ssb(stkProj2[,ac(yrAssess:yrTACp1)]))
)

df2 <- merge(stfRef, stfDet2, all = T)
df2 <- pivot_longer(df2, cols = c(catch, landings, fbar, ssb), 
  names_to = "variable", values_to = "value")
df2 <- df2 |>
  filter(
    (variable %in% c("catch", "landings", "fbar") & year <= yrTAC) |
    (variable %in% c("ssb") & year <= yrTACp1))
df2 <- pivot_wider(df2, names_from = model, values_from = value)
df2$percErr <- round((df2$FLR - df2$NSSK)/df2$NSSK * 100, 1)
df2$stf <- "adjusted"

df <- df1
df$stf <- "original"
df <- rbind(df, df2)
df$stf <- factor(df$stf, levels = c("original", "adjusted"))

ggplot(df) + aes(x = year, y = percErr, fill = stf, colour = stf) + 
  facet_wrap(~variable) +
  geom_col(position = position_dodge(), width = 0.75) +
  scale_fill_manual(values = c(4,5)) + 
  scale_color_manual(values = c(4,5)) + 
  geom_hline(yintercept = 0, linetype = 1) +
  geom_hline(yintercept = c(-10,10), linetype = 3) + 
  theme_bw()


## ----nephropsEx---------------------------------------------------------------
stkEst <- wgnsskStocks[["NEP6"]]
stkEst <- window(stkEst, start = 2001) # trim data, as earlier years only contain landings
stkEst@landings <- computeLandings(stkEst)

# replace discards totals with dead discards only
stkEst@discards[,ac(2001:2023)] <- c(2034, 676, 608, 523, 608, 893, 367, 141, 
  392, 171, 209, 293, 383, 169, 162, 231, 170, 166, 385, 264, 356, 289, 281)
stkEst@discards.n <- stkEst@discards / stkEst@discards.wt

# re-compute catch
stkEst@catch.n <- stkEst@landings.n + stkEst@discards.n
stkEst@catch <- stkEst@landings + stkEst@discards
stkEst@catch.wt[] <- stkEst@catch / stkEst@catch.n

# re-compute harvest rate (hr)
stkEst@harvest <- computeHarvest(stkEst)

# double-check dead discard rate
# stkEst@discards.n / stkEst@catch.n

# for clarity, units can be redefined
units(stkEst)[c("catch", "landings", "discards")] <- "tonnes"
units(stkEst)[c("catch.n", "landings.n", "discards.n")] <- "millions"
units(stkEst)[c("catch.wt", "landings.wt", "discards.wt")] <- "g"

# extend FLStock
stkProj <- stf(object = stkEst, nyears = 3, wts.nyears = 3, 
  fbar.nyears = 3, f.rescale = TRUE, disc.nyears = 3)

# view mean weights
# stkProj@landings.wt[,ac(yrNow:yrTACp1)] # correct
# stkProj@discards.wt[,ac(yrNow:yrTACp1)] # correct

# view dead discard rates (forecast years are a placeholder; ratio of catch)
# stkProj@discards.n[,ac(yrNow:yrTACp1)] # discard rate in forecast years
# stkProj@landings.n[,ac(yrNow:yrTACp1)] # landings rate (1 - discard rate)

# add assumed stock size in forecast years
stockN <- 760  # stock size (million inds)
Fmsy <- 0.0812 # Fmsy harvest rate
Btrigger <- 858 # Trigger stock size
hr <- ifelse(stockN < Btrigger, Fmsy * (stockN/Btrigger), Fmsy) # hcr-based hr

# input stock size in advice year(s)
stkProj@stock.n[,ac(yrNow:yrTACp1)] <- stockN

# compute total removals (catch.n), followed by splits into landings.n and 
# discards.n. 
# then, compute total catch, landings, discards and harvest rate
stkProj@catch.n[,ac(yrNow:yrTACp1)] <- stkProj@stock.n[,ac(yrNow:yrTACp1)] * hr
stkProj@landings.n[,ac(yrNow:yrTACp1)] <- stkProj@catch.n[,ac(yrNow:yrTACp1)] * 
  stkProj@landings.n[,ac(yrNow:yrTACp1)]
stkProj@discards.n[,ac(yrNow:yrTACp1)] <- stkProj@catch.n[,ac(yrNow:yrTACp1)] * 
  stkProj@discards.n[,ac(yrNow:yrTACp1)]

stkProj@catch <- computeCatch(stkProj)
stkProj@landings <- computeLandings(stkProj)
stkProj@discards <- computeDiscards(stkProj)
stkProj@harvest <- computeHarvest(stkProj)

# compare output to reference
stfRef <- data.frame(
  model = "NSSK",
  year = ac(yrTAC),
  catch = c(1204),
  landings = c(1056),
  discards = c(148),
  fbar = c(0.072)
)

stfDet <- data.frame(
  model = "FLR",
  year = ac(yrTAC),
  catch = c(catch(stkProj[,ac(yrTAC)])),
  landings = c(landings(stkProj[,ac(yrTAC)])),
  discards = c(discards(stkProj[,ac(yrTAC)])),
  fbar = c(fbar(stkProj[,ac(yrTAC)]))
)

df <- merge(stfRef, stfDet, all = T)
df <- pivot_longer(df, cols = c(catch, landings, discards, fbar), 
  names_to = "variable", values_to = "value")
df <- pivot_wider(df, names_from = model, values_from = value)
df$percErr <- round((df$FLR - df$NSSK)/df$NSSK * 100, 1)

ggplot(df) + aes(x = year, y = percErr) + 
  facet_wrap(~variable) +
  geom_col(fill = 4, color = 4) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_hline(yintercept = c(-10,10), linetype = 3) + 
  theme_bw()


