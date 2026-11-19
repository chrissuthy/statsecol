# statsecol 0.2.0

Documentation audit. No data set has been changed.

## Help files

* Help files are now generated with roxygen2 from `R/data.R`,
  `R/plotsample_est.R`, `R/utils.R` and `R/statsecol-package.R`. Edit those
  files and run `devtools::document()`; do not edit `man/*.Rd` by hand.
* Every data set and exported function now has a complete help page with a
  working example. Empty template sections and placeholder examples
  (`## maybe str(x) ; plot(x) ...`) have been removed.
* New help pages: `biod`, `ci_helpers` (`norm.ci()`, `lognorm.ci()`) and a
  package overview, `?statsecol`, which lists the data sets by topic.
* `BolandCH`, `BolandMask`, `edf`, `tdf_control`, `tdf_disturb` and
  `ocelot_spatial` were undocumented; they are now aliases on the `Boland`
  and `ocelot2` pages.
* The water vole page was named `vole`, but the data set is `voles`, so
  `data(vole)` failed. The page is now `voles` (`?vole` still works).
* Corrected help pages that did not match the data:
    * `wildebeest`: listed 10 variables, including `yr.index` and `se.na`,
      which do not exist. The data have 8 variables.
    * `falcons`: `Yr` was missing; chapter reference corrected to Chapter 3.
    * `gtit`: 263 rows, not 1578.
    * `redsq`: 264 rows, not 265.
    * `ocelot1`: 40 detections, not 41.
    * `bowhead_LT`: `Sample.Label` is the transect identifier.
    * `horn`: `TransectID` is an integer; `YearMonth` format described.
    * `titgof`: produced by `AICcmodavg::Nmix.gof.test()`.
    * `tobinsq`: derived variables defined; company and industry counts
      noted.
    * `willow`: five columns are character because missing values are
      stored as `"."`; documented, with a conversion example.
* `plotsample_est()`: removed the duplicated `\description`, rewrote the
  `dbn` and `method` arguments (the Poisson option was described as
  binomial), and added a Details section giving the variance and interval
  formulas.

## Code

* `plotsample_est()` now checks its arguments and gives an informative
  error for invalid values and for combinations that previously failed
  with `object 'ci.Nhat' not found` (e.g. `method = "percentile"` without
  `dbn = "bootstrap"`). Results for valid input are unchanged.
* `NAMESPACE` now exports `plotsample_est()`, `norm.ci()` and
  `lognorm.ci()` by name instead of by pattern, and imports the `stats`
  functions used.
* Added unit tests (`tests/testthat`).

## Package metadata

* `DESCRIPTION`: fixed the title typo, added MT5764, `Authors@R`, `URL`,
  `BugReports`, `Imports: stats`, `Suggests: secr, testthat`; `Depends`
  raised to R >= 3.5.0, which the saved data files require.
* Added `README.md` and `NEWS.md`; `misc/` is excluded from the built
  package.
