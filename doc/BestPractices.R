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

library(kableExtra)
library(dplyr)

## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------
df <- as.data.frame(rbind(
  c('North Sea', 
    'Métiers that fail to catch at least 1\\% of the total landings of any stock, in the data year', 
    'Fleets that contain only the "OTH" metier.'),
  c('Bay of Biscay', 
    'Métiers within a fleet that contribute less than 2\\% to a stock’s landings for that fleet, in the last 3 years. ("MIS" metier)',
    'Fleets catching less than 1\\% of the total catch of all stocks considered, in the last 3 years (country specific "MIS" fleet).'),
  c('Iberian Waters', 'Metiers associated with the small, artisanal, multigear are combined under one metier ("MIS").\nMétiers within a fleet that contribute less than 2\\% to a stock’s landings for that fleet, in the last 3 years ("MIS" metier).', 
    'Fleets catching less than 1\\% of the total catch of all stocks considered, in the last 3 years (country specific "MIS" fleet).'),
  c('Irish Sea',	'Metiers that fail to catch at least 1\\% of the total landings for any stock ("OTHER").',	'"OTH\\_OTH" fleet  combines  all  small  fleets  and  métier  with  landings  < 1\\%  for  any  stock  in  the  model.')
))

df %>%
mutate_all(linebreak) %>%
kable("latex", booktabs = T, escape = F, longtable = TRUE, 
  caption = "Fleet and métier definitions by ecoregion.", 
  col.names = c('', '"OTH" metier/"MIS" metier', '"OTH\\_OTH" fleet/"MIS" fleet')
) |>
column_spec(1:3, width = paste0(c(3,6,6), "cm")) |> 
kable_styling(latex_options = "striped") |> 
kable_styling(latex_option = c("hold_position"), position = "left")  |> 
row_spec(seq(nrow(df)-1), hline_after = T)

## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------
df <- as.data.frame(rbind(
c('North Sea',
    
'Defined by country, gear group and vessel length. Gear groups are static (pots, nets, longlines), otter trawls, demersal seines, beam trawlers, pelagic (purse seine, mid water trawls) and miscellaneous (any other gear types). \nFollowing the initial allocation some vessel length categories within a country and gear group are combined together.
Where appropriate, some specific gear types are separated out from the initial allocation (e.g. gillnets separated from static). \nWithin a country, smaller fleets are added to the miscellaneous gear grouping based on expert opinion.', 
    
'Defined using the DCF métiers definitions used in the cod long-term management plan which are based on mesh size (e.g. TR1, TR2). \nThe metiers are also defined by the area the metier is operating in (i.e. 3.a.20, 4, 6.a, 7.d).', 
    
'Only fleets and metiers operating in the latest data year are considered in the full time series. \nMetiers with catch but no effort are removed.'),
  
c('Celtic Sea', 
    
'Defined by country, gear group and vessel length. Gear groups are static (pots, nets, longlines), otter trawls and demersal seines, beam trawlers, pelagic (any gear where the target assemblage is pelagic fish) and miscellaneous (any other gear types).\n
  Out of area catches for each stock are put into a stock-specific “OTH” fleet.',
    
'Defined by gear type and target assemblage (i.e. DCF metier level 4). \n
  Metiers are combined across ICES divisions (7.bc, 7.fg, 7.hjk) since these are often combined for sampling, management and advice purposes.',
  
'Métiers with effort and no catch are aggregated to the “OTH” fleet.'),
  
c('Bay of Biscay',

'Defined by country, gear group and vessel length. Countries other than Spain and France are grouped together. Gear groups are gillnets, trammelnets, otter trawls, longlines, seines, midwater trawls and miscellaneous (any other gear types).\n
  Following the initial allocation some vessel length categories within a country and gear group are combined together. Additionally, some gear groups within a country are also combined together. \n
  Within a country, smaller fleets are added to the miscellaneous gear grouping (country specific MIS fleet). \n
  Out of area catches for each stock are put into a stock-specific “OTH” fleet.',
  
'Initially defined by gear type, target assemblage and mesh size. \n
  Some metiers with similar catch profiles are combined together. \n
  Smaller metiers with low shares of the catches are combined under one metier in the country-specific MIS fleet. Metiers are combined across ICES divisions 8.abd.',
  
'Métiers with catch but no effort are added to the country-specific MIS fleet.\n
  Métiers with effort and no catch are removed.\n
  Specific purse seine and artisanal fleets are removed from the analysis.'),
  
c('Iberian Waters', 
  
'Defined mostly by country and gear group with occasional use of vessel length. Countries other than Spain and Portugal are grouped together. Gear groups are gillnets, trammelnets, otter trawls, longlines, pair trawls and miscellaneous (any other gear types).\n
  Within a country, smaller fleets are added to the miscellaneous gear grouping (country specific MIS fleet).\n
  Out of area catches for each stock are put into a stock-specific “OTH” fleet.', 
  
'Initially defined by target assemblage and mesh size. Some metiers within a fleet (same country and gear) are combined across mesh sizes.\n
  Metiers associated with the small, artisanal, multigear are combined under one metier (“MIS“).\n
  A mixed category of catches without corresponding effort (including French fleets with catches < 1\\%) are combined into one metier (“OTH”).', 
  
''),
  
c('Irish Sea',
  
'Defined mostly by country and gear group. All vessel length categories are grouped together. Gear groups are otter trawls, demersal seines, beam trawlers, pelagic (midwater trawls targeting pelagic fish) and miscellaneous (any other gear types). Otter trawls fleets are further separated by target assemblage.',

'Defined by gear type and target assemblage (i.e. DCF metier level 4).\n 
Some similar gear types with the same target assemblage are combined together (e.g. OTB/OTM\\_DEF and PTM/OTM\\_SPF).',

'')

))
# rownames(df) <- df[,1]
# colnames(df) <- c('l', '“OTH” metier/”MIS” metier', '“OTH_OTH” fleet/”MIS” fleet')

# pander::pander(df, justify = c('left', 'left', 'left'))
df %>%
mutate_all(linebreak) %>%
kable("latex", booktabs = T, longtable = TRUE, escape = F,
  caption = "Fleet and métier definitions by ecoregion.", 
  col.names = c('', 'Fleets', 'Metiers', 'Additional comments')
) |> 
column_spec(1:4, width = paste0(c(2,6,4,3), "cm")) |> 
kable_styling(latex_options = "striped") |> 
kable_styling(latex_option = c("hold_position",  "repeat_header"), 
  position = "left") |>
row_spec(seq(nrow(df)-1), hline_after = T)

## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------
df <- as.data.frame(rbind(
  c('North Sea', 
    'Specific fleet segments (<15 m) not submitted by Norway. Added to OTH\\_OTH fleet.', 
    ''),
  c('Celtic Sea',
    '',
    'Put in a stock-specific “OTH” fleet (pseudo fleet).'),
  c('Bay of Biscay', 
    'Pelagic fleet data not available. Missing pelagic catches are added to a stock-specific “OTH” fleet (pseudo fleet).',
    'Put in a stock-specific “OTH” fleet (pseudo fleet).'),
  c('Iberian Waters', 
    '', 
    'Put in a stock-specific “OTH” fleet (pseudo fleet).'),
  c('Irish Sea',	
    '',	
    '')
))

df %>%
mutate_all(linebreak) %>%
kable("latex", booktabs = T, escape = F, longtable = TRUE, 
  caption = "Accounting for missing catches in the fleet data.", 
  col.names = c('', 'Missing fleet data',	'Out of area catches')
) |>
column_spec(1:3, width = paste0(c(3,6,6), "cm")) |> 
kable_styling(latex_options = "striped") |> 
kable_styling(latex_option = c("hold_position", "repeat_header"), position = "left")  |> 
row_spec(seq(nrow(df)-1), hline_after = T)

## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------
df <- as.data.frame(rbind(
  c('Maximum (“max”)', 
    'For each fleet, fishing in the advice year stops when all stock shares of that fleet have been caught.', 
    'This scenario highlights the least restrictive stocks and results in overshoot of the advised catch for most stocks.'),
  c('Minimum (“min”)',
    'For each fleet, fishing in the advice year stops when the first stock share of that fleet has been caught.',
    'This scenario is the most precautionary option and can highlight some potential “choke species” issues. This option results in the underutilization of the single-stock advice possibilities of most stocks.'),
  c('Status quo effort (“Sq\\_E”)', 
    'The effort of each fleet in the advice year is set equal to the effort in the most recent historical period (average of last 3 years) for which landings and discard data are available.',
	'This scenario indicates the likely level of catch if there is no change to the fishing effort exerted by each fleet.'),
  c('Single stock ("stock")',
    'The effort of each fleet in the advice year corresponds to the effort needed to take their stock share of the specified “stock”, regardless of other catches. If a fleet does not have any fishing opportunities for the specified stock then status quo effort is used.',
    'This scenario indicates the likely level of catch for other stocks if the single stock advice for the stock of interest is fully taken.'),
  c('Pretty good yield (“pgy”)',
    'The scenario is similar to that of the “min”, but uses the Fmsyupper associated advice. Fmsyupper is not defined for all stocks, and is only presented as a catch option when the stock is shown to be in good status; i.e.  when SSB > Btrigger.',
    'The higher catch advice associated with the upper range of Fmsy reference points may reduce choking behaviour in mixed fisheries and increase overall quota uptake.')
))

df %>%
mutate_all(linebreak) %>%
kable("latex", booktabs = T, escape = F, longtable = TRUE, 
  caption = "Scenario descriptions", 
  col.names = c('Scenario', 'Description',	'Aim')
) |>
column_spec(1:3, width = paste0(c(3,6,6), "cm")) |> 
kable_styling(latex_options = "striped") |> 
kable_styling(latex_option = c("hold_position", "repeat_header"), position = "left")  |> 
row_spec(seq(nrow(df)-1), hline_after = T)

## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------
df <- as.data.frame(rbind(
  c('Description', 
    'Only those stocks that have a full category 1 assessment with age-based or size-based population dynamics or an absolute abundance estimate (e.g., Nephrops) are included.',
    'Includes stocks that have biomass-dynamic models for future population dynamics.',
    'Includes TAC stocks with no population model are included on a “constant CPUE” basis.',
    'Includes all stocks caught by the fleets and fisheries are explicitly included on a “constant CPUE basis”.'),
  c('Strengths', 
    'Technical interactions modelled reflect changing stock abundance,
The conditioning of the model is based on well stabilised robust quantitative stock assessments.
Differences in selectivity by fleet/metier can be introduced and evaluations of changes in selectivity can be evaluated.',
    'Same as (a), technical interactions reflect abundance changes,
It could improve the description/modelling of fishing activity or fleet dynamics',
    'Encompasses a greater number of potential choke stocks,
    It could improve the description/modelling of fishing activity or fleet dynamics',
    'Encompasses all the target stocks so may better reflect fishing effort expected, revenue can be modelled better.
  Can be used to forecast bycatch of sensitive species that are not in the TAC and quota system but are relevant to other management frameworks/directives.
    It could improve the description/modelling of fishing activity or fleet dynamics.'),
  c('Limitations',
    'Does not include all stocks caught by fishery, and possibly not all target stocks,
    May not include the choke stock,
    The definition of métiers could be wrong because other relevant species are not considered.',
    'Does not include all stocks,
May not have a way of projecting future stock size,',
    'Choke effects may be unrealistic due to increases or decreases in abundance,
    Assumption of constant biomass may only be reasonable for short term projections.',
    'Non-quota stocks cannot choke fisheries,
    Difficult to communicate,
    May involve too many stocks to accurately evaluate,
    Potential missing data on stocks,
    Assumption of constant biomass may only be reasonable for short term projections.'),
  c('Examples',
    'Current FCube model for the Celtic Sea',
    'Long-term scenarios of the North Sea FLBEIA model in external projects (Probyfish, Pandora). Current FLBEIA model for the Iberian Waters (one cat.2 stock ank.27.8c9a)',
    'Current FLBEIA model for the Bay of Biscay, and Nephrops stocks in other regions.',
    'Long-term scenarios of the North Sea and Bay of Biscay FLBEIA models in external projects (Probyfish, Pandora) included some bycatch stock.')
))

df %>%
mutate_all(linebreak) %>%
kable("latex", booktabs = T, escape = F, longtable = TRUE, 
  caption = "Stocks included in the models:", 
  col.names = c('Options',	'(a) Only those stocks with full age-based assessments and forecast methods or an absolute abundance estimate',	'(b) Also includes stocks with biomass-dynamics methods',	'(c) Include TAC stocks, even where no analytical population model available.', '(d) Include all stocks, even non-quota.')
) |>
column_spec(1:5, width = paste0(c(2.5, rep(18/4, 4)), "cm")) |> 
kable_styling(latex_options = "striped") |> 
kable_styling(latex_option = c("hold_position", "repeat_header"), position = "left")  |> 
row_spec(seq(nrow(df)-1), hline_after = T)

## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------
df <- as.data.frame(rbind(
  c('Description',
    'The operational unit in the model is a “fleet”, that is a physical group of vessels with a predominant activity (e.g., Dutch beam trawlers of 24-40m). A vessel belongs to only one fleet.',
    'The operational unit in the model is a “fishery”, that is a group of vessels doing a particular activity (e.g., Scottish otter trawlers of 24-40m targeting whitefish in the North Sea). A vessel may take part in several fisheries.',
    'The operational unit is a hierarchy of fleet and fishery so to distinguish between the fleet and its activity in one or more fisheries (métier),'),
  c('Strengths', 
    'Linked to economics of vessels, 
May directly align with licencing and management systems.',
    'Provides full flexibility for effort in fisheries to adjust to species quotas.
Catch compositions linked to definitions of fishery,',
    'Explicit link between physical vessels (fleets) and activity (métier).
Allows for modelling effort allocation to different fisheries (currently based on past shares),
Inclusion of fleet level allows for economic considerations.'),
  c('Limitations',
    'No description of the fisheries themselves,
Limited to fleet level catches (no fleet behaviour possible),
Merges activity in different fisheries,
Polyvalent activity is impossible to identity,',
    'No link to the economic unit,
No constraints on effort as link to physical vessels not included (unrealistic effort),
Definitions of fisheries do not capture all variation in fishing activity (compromise between data availability and classifying fishing activity),
Limited data availability (space, time) to define fisheries,
Polyvalent activity is impossible to identity,
Catches may not match current relative stability shares,',
    'If métier dynamics are not modelled the choke effect is at the fleet level (see Table 3),
Definitions of fisheries do not capture all variation in fishing activity (compromise between data availability and classifying fishing activity),
Limited data availability (space, time) to define fishing métier,'),
  c('Examples',
    '',
    'Previous model MTAC (rejected by ICES as advisory tool for reasons outlined in ICES, 2006).',
    'Current approaches FCube and FLBEIA')
))

df %>%
mutate_all(linebreak) %>%
kable("latex", booktabs = T, escape = F, longtable = TRUE, 
  caption = "Fleet, fishery, and métiers as a model basis:", 
  col.names = c('Options',	'Fleet-based',	'Fishery-based',	'Fleet and métier based')
) |>
column_spec(1:4, width = paste0(c(2.5, rep(18/3, 3)), "cm")) |> 
kable_styling(latex_options = "striped") |> 
kable_styling(latex_option = c("hold_position", "repeat_header"), position = "left")  |> 
row_spec(seq(nrow(df)-1), hline_after = T)

## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------
df <- as.data.frame(rbind(
  c('Description',
    'No adaptation of fleets to quotas,',
    'Provide uncertainty estimates based on (recent) past fishing patterns',
    'Use model to predict how fleets will adapt effort to quotas',
    'Optimisation of effort in métier to maximise catch or revenue given quota constraints constrained to past observed shares of effort.',
    'Optimisation of effort in métier to maximise catch or revenue given quota constraints unconstrained.'),
  c('Strengths', 
    'Identifies choke stocks “if all else the same”,',
    'Simple and easier to understand,	Reflect historic adaptability in fishing patterns,',
    'Provides upper and lower bounds of expected catch,	Allows for fleet adaptation to be considered in choke effects,',
    'Adaptation to quotas within historic observations,',	
    'Maximum flexibility,'),
  c('Limitations',
    'May over or underestimate choke effects based on changing fishing behaviour,
    Simple process model of 1:1 relationship between effort and F/catch,',
    'More difficult to communicate outputs,
    May not capture full flexibility of fleets,',
    'Lack of available models may require simple assumptions,
    More complexity in model,
    Do not have data on all drivers of effort,',
    'Definition of métier can define outcome,
    Remains variability in catchability within métier,
    May not capture full flexibility of fleets,',
    'May result in unrealistic effort allocations (e.g., all to single fishery),
    Definition of métier can define outcome,
    Remains variability in catchability within métier (how to account for it),'),
  c('Examples',
    'Current FCube and FLBEIA applications for all ecoregions',
    '',
    'Simple models could be e.g., effort driven by revenue and historic patterns as in Marchal et al 2013',
    'e.g., MaxProfit routine in FLBEIA',
    'e.g., MaxProfit routine in FLBEIA')
))

df %>%
mutate_all(linebreak) %>%
kable("latex", booktabs = T, escape = F, longtable = TRUE, 
  caption = "Modelling fishing patterns (effort allocation per metier, catchability per metier and species) and resultant fleet catch compositions:", 
  col.names = c('Options',	'(a) Based on past observations',	'(b) Based on past observations, include uncertainty', '(c) Based on fleet dynamic model fitted to past observations', 	'(d) Based on optimisation, but limited to range of past observations',	'(e) Based on optimisation model (unconstrained)')
) |>
column_spec(1:6, width = paste0(c(2.5, rep(18/5, 5)), "cm")) |> 
kable_styling(latex_options = "striped") |> 
kable_styling(latex_option = c("hold_position", "repeat_header"), position = "left")  |> 
row_spec(seq(nrow(df)-1), hline_after = T)

## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------
df <- as.data.frame(rbind(
  c('Description',
    'Fixed selectivity for each fleet and métier based on recent past observation,',
    'Fixed selectivity for each fleet and métier based on recent past observation but capturing historic variability,',
    'Gear selectivity for given fleet or métier adjusted based on anticipated changes for a defined gear being introduced,'),
  c('Strengths', 
    'Based on observation as with single stock advice,
    No evidence or data to deviate from current selection,',
    'Based on observation but captures uncertainty,
Allows for greater understanding of impact of variability on predicted catches,',
    'May capture intended benefits of gear change,
    Ability to evaluate overall effect of gear,
    Opportunity for different scenarios to be developed,'),
  c('Limitations',
    'Does not capture potential solutions to choke effects,
    Data not always available in logbooks (i.e. selectivity device), so assumptions made at national level',
    'Greater data requirements,
    Harder to communicate, 
    Computationally intensive requiring multiple runs,
     Data not always available in logbooks (i.e. selectivity device), so assumptions made at national level
    Only informs uncertainty around outputs that is not use in ICES advice',
    'Intended benefit of gear changes not always realised (unrealistic?), Less practical for short-term forecasts,
    Implications for stock reference points relating to MSY in the long-term,
    Gear studies do not account for fisher behaviour, a net can be fished very differently from the design
    Modelled via changes in metier catchability but catchability accounts for gear selectivity and availability of the stocks so the changes might not capture what’s happening in reality.'),
  c('Examples',
    'North Sea FLBEIA model.',
    '',
    'Long-term scenarios of the North Sea FLBEIA model in external projects (Probyfish, ECOMAN)')
))

df %>%
mutate_all(linebreak) %>%
kable("latex", booktabs = T, escape = F, longtable = TRUE, 
  caption = "Modelling gear selectivity: only available in age disaggregated", 
  col.names = c('Options',	'(a) Based on past observations',	'(b) Based on past observations, include uncertainty in recent past',	'(c) Based on proposed gear modifications and potential impact')
) |>
column_spec(1:6, width = paste0(c(2.5, rep(18/3, 3)), "cm")) |> 
kable_styling(latex_options = "striped") |> 
kable_styling(latex_option = c("hold_position", "repeat_header"), position = "left")  |> 
row_spec(seq(nrow(df)-1), hline_after = T)

## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------
df <- as.data.frame(rbind(
  c('Description',
    'The share of each fleets quota is based on its (recent) past observed share.',
    'The share of each fleet’s quota is based first on the country relative stability share, then on historic shares within country.',
    'The share of each fleet’s quota is based first on the country relative stability share adjusted for observed quota swaps, then on historic shares within country.',
    'The share of each fleet’s quota is optimised to maximise quota uptake within country.'),
  c('Strengths', 
    'Reflects at the same time the relative stability shares, the recent quota exchanges and the recent quota consumption rates',
    'Reflect the real fishing opportunities of the fleets (will likely avoid situations where a country can get choked by a stock even though they have historically underused their fishing opportunities).',	
    'Will accurately reflect the recent practices in quota exchange by the various countries to avoid choking effects',
    'Reflect what could be achieved,'),
  c('Limitations',
    'May not be suitable in case of strong changes in TAC, as countries may decide to change their quota exchange (e.g. to keep quota available if a stock becomes choke), and adjust their quota consumption rate,',
    'Will exaggerate choke effects for countries that are generally able to get extra quota for their potential choke stocks.
    Would require a lot of assumptions',
    'Will not be accurate if TAC changes strongly and countries decide to no longer swap quotas for the stocks with concerns,
    Data availability, no access to complete international quota swaps and national distributions.',
    'Probably multiple solutions and trade-offs,
    Does not reflect current management,
    No available data source'),
  c('Examples',
    'Current FLBEIA and FCube implementations for all ecoregions',
    'Trial runs using FIDES at WGMIXFISH-METHODS 2022',
    'Trial runs using FIDES at WGMIXFISH-METHODS 2022',
    '')
))

df %>%
mutate_all(linebreak) %>%
kable("latex", booktabs = T, escape = F, longtable = TRUE, 
  caption = "Modelling quota-allocations:", 
  col.names = c('Options',	'(a) Based on recently observed landings share',	'(b) Based on country specific relative stability shares', '(c) Based on post-swap shares from historical data',	'(d) Based on optimising within country allocations')
) |>
column_spec(1:5, width = paste0(c(2.5, rep(18/4, 4)), "cm")) |> 
kable_styling(latex_options = "striped") |> 
kable_styling(latex_option = c("hold_position", "repeat_header"), position = "left")  |> 
row_spec(seq(nrow(df)-1), hline_after = T)

## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------
df <- as.data.frame(rbind(
  c('All fleets stop fishing when they reach any quota (min scenario), all quotas (max scenario) or a defined stock quota,',
    'Fleets stop fishing when they reach their quota for a selected set of stocks for that fleet (min) scenario, or all quotas of those stocks (max scenario),',
    'Fleets fishing effort weighted towards some target (e.g., weighted to value or share of catch of a stock),'),
  c('Strengths', 
    'Simple to understand scenarios,
    Reflects the landing obligation policy,',	
    'May be more realistic,
    Reduces severe reduction in catches under the ‘min’ scenario for some fleets with small catches,
    Could be defined with stakeholder input,',
    'Impacts on fleets reduced for fleets that have small catches of limiting stocks,'),
  c('Limitations',
    'Can result in significant under-quota catches of target stocks for small reductions in non-target catches,',
    'Does not reflect landing obligation policy,
    Results in catches above the single stock advice,',
    'Quotas would need to be managed by fleet else intended outcome not realised,
    Weighting process would need to be decided by managers,'),
  c('Examples',
    'Current WGMIXFISH approach',
    '',
    'MTAC approach')
))

df %>%
mutate_all(linebreak) %>%
kable("latex", booktabs = T, escape = F, longtable = TRUE, 
  caption = "Scenario assumptions:", 
  col.names = c('Options',	'(a) Based on simple rules for all fleets',	'(b) Based on bespoke rules for fleets',	'(c) Based on some weighting for each fleet')
) |>
column_spec(1:4, width = paste0(c(2.5, rep(18/3, 3)), "cm")) |> 
kable_styling(latex_options = "striped") |> 
kable_styling(latex_option = c("hold_position", "repeat_header"), position = "left")  |> 
row_spec(seq(nrow(df)-1), hline_after = T)

