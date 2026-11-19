# statsecol

Data sets and a few functions used for teaching on the Statistical Ecology
MSc modules at the University of St Andrews (currently MT5751, MT5764 and
MT5767).

## Installation

```r
install.packages("remotes")
remotes::install_github("chrissuthy/statsecol")
```

## Using the package

```r
library(statsecol)
?statsecol          # overview: data sets listed by topic
?wildebeest         # help page for one data set
head(wildebeest)
```

All data sets are available as soon as the package is attached; there is no
need to call `data()`.

| Topic | Data sets |
|---|---|
| Plot and distance sampling | `plotsamp`, `poppies`, `bowhead_PS`, `bowhead_LT` |
| Population dynamics, state-space models | `wildebeest`, `falcons`, `martin` |
| Occupancy and N-mixture models | `willow`, `redsq`, `gtit`, `titgof`, `burnet`, `voles` |
| Spatial capture-recapture | `BolandCH`, `BolandMask`, `fdbears`, `ocelot1`, `edf`, `tdf_control`, `tdf_disturb`, `ocelot_spatial` |
| Regression, smoothing, impact assessment | `nysted`, `horn`, `tobinsq`, `simdat_nl`, `biod` |

Functions: `plotsample_est()` (abundance from plot counts), `norm.ci()` and
`lognorm.ci()` (confidence intervals).

## For maintainers

Help files are generated with roxygen2. Edit the comments in `R/` (data set
documentation is in `R/data.R`), then run:

```r
devtools::document()
devtools::check()
```

To add a data set, save it to `data/`, add a documentation block to
`R/data.R` and list it in `R/statsecol-package.R` and the table above.
