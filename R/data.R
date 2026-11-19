# Documentation for the data sets in data/.
# One block per .RData/.rda file. Edit here, then run devtools::document().

# ---- Plot sampling and distance sampling -----------------------------------

#' Simulated plot sampling data
#'
#' The number of individuals falling in each of 25 square sampling plots.
#' 150 individuals were placed uniformly at random in a 100 x 100 survey
#' region, which is divided into a 5 x 5 grid of 20 x 20 plots, so the 25
#' plots cover the whole region and the counts sum to the true abundance.
#' Subsets of the plots can be used to illustrate plot sampling estimators.
#'
#' @format A data frame with 25 rows and 4 variables:
#' \describe{
#'   \item{Plot}{plot identifier, a factor with levels `1` to `25`}
#'   \item{Count}{number of individuals in the plot}
#'   \item{X}{x coordinate of the plot centre}
#'   \item{Y}{y coordinate of the plot centre}
#' }
#' @source Simulated; see `misc/make_plotsamp.R` in the package source.
#' @seealso [plotsample_est()]
#' @examples
#' plot(plotsamp$X, plotsamp$Y, type = "n", xlim = c(0, 100), ylim = c(0, 100),
#'      asp = 1, xlab = "X", ylab = "Y")
#' abline(h = seq(0, 100, 20), v = seq(0, 100, 20))
#' text(plotsamp$X, plotsamp$Y, labels = plotsamp$Count, cex = 2)
#'
#' sum(plotsamp$Count)  # true abundance
"plotsamp"

#' Poppies plot sampling data
#'
#' Simulated plot sampling data from two poppy meadows, with 20 plots
#' surveyed in each meadow.
#'
#' @format A data frame with 40 rows and 4 variables:
#' \describe{
#'   \item{Population}{meadow identifier (`"1"` or `"2"`), a character vector}
#'   \item{plot}{plot identifier within meadow (1 to 20)}
#'   \item{n}{number of poppies counted in the plot}
#'   \item{a}{area of the plot (100 for every plot)}
#' }
#' @source Simulated.
#' @seealso [plotsample_est()]
#' @examples
#' str(poppies)
#' tapply(poppies$n, poppies$Population, sum)
#' boxplot(n ~ Population, data = poppies, xlab = "Meadow", ylab = "Count")
"poppies"

#' Bowhead whale survey data in plot sampling format
#'
#' Data from the aerial survey of bowhead whales in West Greenland reported
#' in Rekdal et al. (2015), formatted as a (stratified) plot sampling survey
#' in which each transect is a strip. There is one row per transect.
#'
#' @format A data frame with 41 rows and 5 variables:
#' \describe{
#'   \item{stratum}{stratum the transect was in}
#'   \item{transect}{transect identifier}
#'   \item{A}{area of the stratum the transect was in (repeated for each
#'     transect in the stratum)}
#'   \item{a}{area of the transect strip}
#'   \item{n}{number of individuals seen in the strip}
#' }
#' @source Rekdal, S.L., Hansen, R.G., Borchers, D., Bachmann, L.,
#'   Laidre, K.L., Wiig, O., Nielsen, N.H., Fossette, S., Tervo, O. and
#'   Heide-Jørgensen, M.P. (2015) Trends in bowhead whales in West Greenland:
#'   aerial surveys vs. genetic capture-recapture analyses. *Marine Mammal
#'   Science*, **31**(1), 133-154.
#' @seealso [bowhead_LT] for the same survey in line transect format;
#'   [plotsample_est()].
#' @examples
#' str(bowhead_PS)
#'
#' # counts, covered area and stratum area by stratum
#' aggregate(cbind(n, a) ~ stratum + A, data = bowhead_PS, FUN = sum)
"bowhead_PS"

#' Bowhead whale line transect survey data
#'
#' Line transect data from the aerial survey of bowhead whales in West
#' Greenland reported in Rekdal et al. (2015). There is one row per
#' detection; transects with no detections have a single row with `NA` for
#' the detection-level variables (26 such rows), so that their effort is
#' retained. Column names follow the conventions of the \pkg{Distance}
#' package.
#'
#' @format A data frame with 84 rows and 9 variables:
#' \describe{
#'   \item{Region.Label}{stratum the transect was in}
#'   \item{Area}{area of the stratum (repeated for each row in the stratum)}
#'   \item{Sample.Label}{transect identifier}
#'   \item{Effort}{length of the transect}
#'   \item{distance}{perpendicular distance from the transect line to the
#'     detection}
#'   \item{y}{a second distance measurement recorded for each detection; not
#'     described in the original help file, see Rekdal et al. (2015)}
#'   \item{size}{group size (number of whales in the detection)}
#'   \item{bf}{a covariate taking the values 0 to 3 (probably Beaufort sea
#'     state); not described in the original help file}
#'   \item{object}{detection identifier}
#' }
#' @inherit bowhead_PS source
#' @seealso [bowhead_PS] for the same survey in plot sampling format.
#' @examples
#' str(bowhead_LT)
#' hist(bowhead_LT$distance, xlab = "Perpendicular distance",
#'      main = "Bowhead whale detections")
"bowhead_LT"

# ---- Population dynamics and state-space models -----------------------------

#' Serengeti wildebeest data
#'
#' Abundance estimates for the Serengeti wildebeest population from 1960 to
#' 1989, with annual dry-season rainfall and harvest, from Hilborn and
#' Mangel (1997). Abundance was estimated in only 12 of the 30 years; the
#' estimate and its measures of precision are `NA` in years without a survey.
#'
#' @format A data frame with 30 rows (one per year) and 8 variables:
#' \describe{
#'   \item{year}{year (1960 to 1989)}
#'   \item{Nhat}{estimated abundance, in millions of animals (`NA` if no
#'     survey that year)}
#'   \item{sehat}{standard error of `Nhat`, in millions}
#'   \item{cvhat}{coefficient of variation of `Nhat` (`sehat / Nhat`)}
#'   \item{lci}{lower 95% confidence limit, `Nhat - 1.96 * sehat`}
#'   \item{uci}{upper 95% confidence limit, `Nhat + 1.96 * sehat`}
#'   \item{rain}{dry-season rainfall, available for every year}
#'   \item{Catch}{harvest (animals removed), in millions; 0 until 1976 and
#'     0.04 (40,000 animals) from 1977}
#' }
#' @source Hilborn, R. and Mangel, M. (1997) *The Ecological Detective:
#'   Confronting Models with Data*. Princeton University Press.
#' @examples
#' str(wildebeest)
#'
#' # abundance estimates with 95% confidence intervals
#' with(wildebeest, {
#'   plot(year, Nhat, pch = 16, ylim = c(0, max(uci, na.rm = TRUE)),
#'        xlab = "Year", ylab = "Abundance (millions)")
#'   segments(year, lci, year, uci)
#' })
#'
#' # rainfall is available in every year
#' plot(rain ~ year, data = wildebeest, type = "b",
#'      xlab = "Year", ylab = "Dry-season rainfall")
"wildebeest"

#' Peregrine falcon count data
#'
#' Annual counts for the peregrine falcon (*Falco peregrinus*) population
#' breeding in the French Jura mountains from 1964 to 2003, used in
#' Chapter 3 of Kéry and Schaub (2012).
#'
#' @format A data frame with 40 rows (one per year) and 5 variables:
#' \describe{
#'   \item{Year}{year of the count (1964 to 2003)}
#'   \item{Yr}{year rescaled for use as a covariate: `(Year - 1964) / 10`,
#'     i.e. decades since 1964}
#'   \item{PairsT}{total number of adult pairs}
#'   \item{PairsR}{number of reproductive pairs}
#'   \item{Young}{total number of fledged young}
#' }
#' @source Kéry, M. and Schaub, M. (2012) *Bayesian Population Analysis
#'   using WinBUGS: A Hierarchical Perspective*. Academic Press. Data
#'   courtesy of R.-J. Monneret.
#' @examples
#' str(falcons)
#' plot(PairsT ~ Year, data = falcons, type = "b",
#'      xlab = "Year", ylab = "Number of pairs")
"falcons"

#' House martin count data
#'
#' Annual nest counts of house martins from 1990 to 2009, used in Chapter 5
#' (state-space models) of Kéry and Schaub (2012).
#'
#' @format A data frame with 20 rows (one per year) and 2 variables:
#' \describe{
#'   \item{counts}{number of nests counted}
#'   \item{year}{year of the count (1990 to 2009)}
#' }
#' @source Kéry, M. and Schaub, M. (2012) *Bayesian Population Analysis
#'   using WinBUGS: A Hierarchical Perspective*. Academic Press.
#' @examples
#' str(martin)
#' plot(counts ~ year, data = martin, type = "b",
#'      xlab = "Year", ylab = "Nest count")
"martin"

# ---- Occupancy and N-mixture models -----------------------------------------

#' Willow tit detection/non-detection data
#'
#' Willow tit detection/non-detection data from the Swiss Breeding Bird
#' Survey (the original help file gives the year as 2014). Each of 237 sites
#' (1 km quadrats) was visited up to three times in the breeding season.
#'
#' @details
#' **Missing values.** In the visit-level covariates `dur2`, `dur3`,
#' `intensity2`, `intensity3` and `day3`, a missing value (visit not made)
#' is stored as the text `"."` rather than `NA`, so R reads these five
#' columns as character. Convert them before use, for example
#' `as.numeric(ifelse(willow$dur2 == ".", NA, willow$dur2))`; see Examples.
#'
#' @format A data frame with 237 rows (one per site) and 16 variables:
#' \describe{
#'   \item{y.1, y.2, y.3}{detection (1) or non-detection (0) on visits 1 to
#'     3; `NA` if the visit was not made}
#'   \item{elev}{elevation of the site, standardised to mean 0 and standard
#'     deviation 1}
#'   \item{elevsq}{`elev` squared}
#'   \item{forest}{proportion of the site covered by forest (0 to 1)}
#'   \item{dur1, dur2, dur3}{duration of visits 1 to 3, in minutes (`dur2`
#'     and `dur3` are character, see Details)}
#'   \item{length}{length of the survey route, in km}
#'   \item{intensity1, intensity2, intensity3}{survey intensity on visits 1
#'     to 3: duration divided by route length, in minutes per km
#'     (`intensity2` and `intensity3` are character, see Details)}
#'   \item{day1, day2, day3}{survey date of visits 1 to 3, as day of the
#'     season (`day3` is character, see Details)}
#' }
#' @source Swiss Breeding Bird Survey (MHB), Swiss Ornithological Institute.
#' @examples
#' str(willow)
#'
#' # naive occupancy: proportion of sites with at least one detection
#' y <- willow[, c("y.1", "y.2", "y.3")]
#' mean(rowSums(y, na.rm = TRUE) > 0)
#'
#' # convert the columns that use "." for missing values
#' fix <- c("dur2", "dur3", "intensity2", "intensity3", "day3")
#' willow[fix] <- lapply(willow[fix],
#'                       function(x) as.numeric(ifelse(x == ".", NA, x)))
#' summary(willow[fix])
"willow"

#' Red squirrel detection/non-detection data
#'
#' Red squirrel detection/non-detection data from the Swiss Breeding Bird
#' Survey, described in Chapter 10 of Kéry and Royle (2016). Each site
#' (1 km quadrat) was visited two or three times.
#'
#' @format A data frame with 264 rows (one per site) and 11 variables:
#' \describe{
#'   \item{y.1, y.2, y.3}{detection (1) or non-detection (0) on visits 1 to
#'     3; `NA` if the visit was not made (48 sites had no third visit)}
#'   \item{elev}{elevation of the site, in km}
#'   \item{forest}{proportion of the site covered by forest (0 to 1)}
#'   \item{day.1, day.2, day.3}{survey date of visits 1 to 3, as day of the
#'     season (1 is the earliest survey date in the data)}
#'   \item{dur.1, dur.2, dur.3}{duration of visits 1 to 3, in hours}
#' }
#' @source Kéry, M. and Royle, J.A. (2016) *Applied Hierarchical Modeling in
#'   Ecology: Analysis of Distribution, Abundance and Species Richness in R
#'   and BUGS. Volume 1: Prelude and Static Models*. Academic Press. Data
#'   from the Swiss Ornithological Institute. Processed from
#'   `SwissSquirrels.txt` with `misc/_makesqrl.R` in the package source.
#' @examples
#' str(redsq)
#'
#' # naive occupancy
#' y <- redsq[, c("y.1", "y.2", "y.3")]
#' mean(rowSums(y, na.rm = TRUE) > 0)
"redsq"

#' Great tit count data
#'
#' Repeated counts of great tits from the Swiss Breeding Bird Survey,
#' described in Chapter 6 (N-mixture models) of Kéry and Royle (2016). Each
#' site (1 km quadrat) was visited two or three times.
#'
#' @format A data frame with 263 rows (one per site) and 11 variables:
#' \describe{
#'   \item{y.1, y.2, y.3}{number of great tits counted on visits 1 to 3;
#'     `NA` if the visit was not made}
#'   \item{elev}{elevation of the site, in km}
#'   \item{forest}{proportion of the site covered by forest (0 to 1)}
#'   \item{day.1, day.2, day.3}{survey date of visits 1 to 3, as day of the
#'     season}
#'   \item{dur.1, dur.2, dur.3}{duration of visits 1 to 3, in hours}
#' }
#' @source Kéry, M. and Royle, J.A. (2016) *Applied Hierarchical Modeling in
#'   Ecology: Analysis of Distribution, Abundance and Species Richness in R
#'   and BUGS. Volume 1: Prelude and Static Models*. Academic Press. Data
#'   from the Swiss Ornithological Institute.
#' @seealso [titgof] for goodness-of-fit output from a model of these data.
#' @examples
#' str(gtit)
#' plot(y.1 ~ elev, data = gtit, xlab = "Elevation (km)",
#'      ylab = "Count on visit 1")
"gtit"

#' Great tit goodness-of-fit output
#'
#' Saved output from a parametric bootstrap goodness-of-fit test
#' (`Nmix.gof.test()` in the \pkg{AICcmodavg} package, 200 simulations) for
#' the top model in the great tit N-mixture practical, which uses the
#' [gtit] data. It is provided so that the result can be examined without
#' waiting for the bootstrap to run.
#'
#' @format A list of class `"Nmix.chisq"` with 6 elements:
#' \describe{
#'   \item{model.type}{class of the fitted model (`"unmarkedFitPCount"`)}
#'   \item{chi.square}{observed Pearson chi-square statistic}
#'   \item{t.star}{chi-square statistics from the 200 bootstrap simulations
#'     (a 200 x 1 matrix)}
#'   \item{p.value}{bootstrap p-value}
#'   \item{c.hat.est}{estimated overdispersion, c-hat (observed statistic
#'     divided by the mean of the simulated statistics)}
#'   \item{nsim}{number of bootstrap simulations}
#' }
#' @seealso [gtit]
#' @examples
#' str(titgof)
#' titgof$c.hat.est
#'
#' hist(titgof$t.star, xlim = range(c(titgof$t.star, titgof$chi.square)),
#'      xlab = "Chi-square statistic", main = "Bootstrap distribution")
#' abline(v = titgof$chi.square, lty = 2)
"titgof"

#' Six-spot burnet data
#'
#' Detection data for the six-spot burnet moth (*Zygaena filipendulae*)
#' from the Swiss butterfly monitoring programme, used for the dynamic
#' occupancy example in Chapter 13 of Kéry and Schaub (2012).
#'
#' @details
#' In Kéry and Schaub (2012) each site has two replicate counts on each of
#' seven survey days, with the days treated as "seasons". The matrix here
#' has one row per site and one column for each of days 2 to 7, and each
#' value is 0, 1 or 2, which appears to be the number of the two replicate
#' counts on which the species was detected. This description was inferred
#' from the data and the book; check it against the practical that uses
#' the data.
#'
#' @format An integer matrix with 87 rows (sites, row names `"1"` to `"87"`)
#'   and 6 columns (column names `"2"` to `"7"`).
#' @source Kéry, M. and Schaub, M. (2012) *Bayesian Population Analysis
#'   using WinBUGS: A Hierarchical Perspective*. Academic Press.
#' @examples
#' str(burnet)
#' table(burnet)
#'
#' # proportion of sites with at least one detection, by column
#' colMeans(burnet > 0)
"burnet"

#' Scottish water vole data
#'
#' Detection/non-detection data for water voles at 114 habitat patches
#' surveyed in each of 4 years, with patch size and connectivity. In each
#' year a patch was visited up to three times.
#'
#' @format A data frame with 114 rows (one per patch) and 11 variables:
#' \describe{
#'   \item{Patch}{patch identifier}
#'   \item{Length}{patch size (length of the patch)}
#'   \item{Connectivity}{patch connectivity score}
#'   \item{y1, y2, y3, y4}{number of visits on which water voles were
#'     detected in years 1 to 4}
#'   \item{j1, j2, j3, j4}{number of visits made to the patch in years 1 to
#'     4 (1 to 3)}
#' }
#' @references For background on the study system and this kind of model
#'   see Sutherland, C.S., Elston, D.A. and Lambin, X. (2014) A
#'   demographic, spatially explicit patch occupancy model of
#'   metapopulation dynamics and persistence. *Ecology*, **95**(11),
#'   3149-3160.
#' @examples
#' str(voles)
#'
#' # naive occupancy in each year
#' colMeans(voles[, c("y1", "y2", "y3", "y4")] > 0)
#'
#' plot(Connectivity ~ Length, data = voles,
#'      xlab = "Patch length", ylab = "Connectivity")
#' @aliases vole
"voles"

# ---- Spatial capture-recapture ---------------------------------------------

#' Boland leopard camera trap data
#'
#' Camera trap data from the 2010 leopard camera trapping study in the
#' Boland Mountains, South Africa, in the formats used by the \pkg{secr}
#' package. Loading the data makes two objects available: `BolandCH` and
#' `BolandMask`.
#'
#' @details
#' Both objects carry \pkg{secr} classes, so attach \pkg{secr} (which is
#' not installed with this package) to print, summarise and plot them
#' properly.
#'
#' The camera stations (the `traps` attribute of `BolandCH`) have a usage
#' matrix and seven station-level covariates: `alt`, `Landuse`, `Natural`,
#' `dist2.Urban`, `dist2.Water`, `dist2.Natural` and `LUfactor`. The same
#' seven covariates are available for every mask point.
#'
#' @format
#' \describe{
#'   \item{`BolandCH`}{a single-session \pkg{secr} `capthist` object: a
#'     20 x 13 x 44 array (individuals x occasions x detectors) of
#'     detections at 44 camera stations (detector type `"proximity"`),
#'     session `"Limietberg1"`.}
#'   \item{`BolandMask`}{a \pkg{secr} habitat `mask` with 2432 points
#'     (columns `x` and `y`) and a `covariates` attribute.}
#' }
#' @examples
#' dim(BolandCH)
#' dim(BolandMask)
#' names(attr(BolandMask, "covariates"))
#'
#' # camera stations on top of the habitat mask (base graphics)
#' cams <- attr(BolandCH, "traps")
#' plot(BolandMask$x, BolandMask$y, pch = ".", col = "grey", asp = 1,
#'      xlab = "x", ylab = "y")
#' points(cams$x, cams$y, pch = 3, col = "red")
#'
#' if (requireNamespace("secr", quietly = TRUE)) {
#'   summary(BolandCH)
#' }
#' @name Boland
#' @aliases BolandCH BolandMask
#' @docType data
#' @keywords datasets
NULL

#' Fort Drum black bear data
#'
#' Capture and trap data from the Fort Drum (New York) black bear hair
#' snare study described in Gardner et al. (2010): 47 bears detected at 38
#' hair snares over 8 sampling occasions.
#'
#' @format A list with 2 elements:
#' \describe{
#'   \item{`bear_cap_data`}{a data frame with 151 rows (one per detection)
#'     and 4 variables: `session` (always 1), `id` (individual identifier),
#'     `occasion` (sampling occasion, 1 to 8) and `trap` (identifier of the
#'     hair snare).}
#'   \item{`bear_trap_data`}{a data frame with 38 rows (one per hair snare)
#'     and 3 variables: `trap` (hair snare identifier) and `x`, `y` (snare
#'     coordinates, in metres).}
#' }
#' @references Gardner, B., Royle, J.A., Wegan, M.T., Rainbolt, R.E. and
#'   Curtis, P.D. (2010) Estimating black bear density using DNA data from
#'   hair snares. *Journal of Wildlife Management*, **74**(2), 318-325.
#' @examples
#' str(fdbears)
#'
#' # hair snares, with circle size showing the number of detections
#' traps <- fdbears$bear_trap_data
#' ndet <- table(factor(fdbears$bear_cap_data$trap, levels = traps$trap))
#' plot(traps$x, traps$y, pch = 3, asp = 1, xlab = "x", ylab = "y")
#' points(traps$x, traps$y, cex = sqrt(as.numeric(ndet)))
"fdbears"

#' Ocelot camera trap data (single session)
#'
#' Spatially explicit ocelot encounter data from a camera trap survey in
#' the Peruvian Amazon (Kolowski and Alonso, 2010): 22 individuals detected
#' at 23 camera stations over 44 occasions.
#'
#' @format A list with 3 elements:
#' \describe{
#'   \item{`ocelot_cap_data`}{a data frame with 40 rows (one per detection)
#'     and 4 variables: `session` (a factor with the single level
#'     `"PeruOcelotB"`), `ID` (individual identifier), `occasion` (sampling
#'     occasion, 1 to 44) and `trap` (camera station identifier).}
#'   \item{`ocelot_trap_data`}{a data frame with 23 rows (one per camera
#'     station) and 3 variables: `trap` (station identifier) and `x`, `y`
#'     (station coordinates, in metres).}
#'   \item{`ocelot_operation`}{a 23 x 44 integer matrix (stations x
#'     occasions) recording whether each station was operating (1) or not
#'     (0) on each occasion.}
#' }
#' @references Kolowski, J.M. and Alonso, A. (2010) Density and activity
#'   patterns of ocelots (*Leopardus pardalis*) in northern Peru and the
#'   impact of oil exploration activities. *Biological Conservation*,
#'   **143**(4), 917-925.
#' @seealso [ocelot2] for the two-session version of these data.
#' @examples
#' str(ocelot1)
#'
#' # number of occasions each camera station was operating
#' rowSums(ocelot1$ocelot_operation)
"ocelot1"

#' Ocelot camera trap data (two sessions)
#'
#' Ocelot camera trapping data from the Peruvian Amazon collected before
#' (`control`) and during (`disturb`) an oil exploration exercise that used
#' seismic surveys (Kolowski and Alonso, 2010). Loading the data makes four
#' objects available: `edf`, `tdf_control`, `tdf_disturb` and
#' `ocelot_spatial`.
#'
#' @format
#' \describe{
#'   \item{`edf`}{the encounter data: a data frame with 120 rows (one per
#'     detection; 40 in the control session and 80 in the disturbance
#'     session) and 5 variables: `Session` (`"control"` or `"disturb"`),
#'     `ID` (individual identifier), `Occasion` (sampling occasion; up to
#'     44 in the control session and 94 in the disturbance session),
#'     `TrapID` (camera station, a factor) and `Sex` (a factor with levels
#'     `F` and `M`; missing for 2 detections).}
#'   \item{`tdf_control`, `tdf_disturb`}{the trap data for each session:
#'     data frames with 23 rows (one per camera station) and 3 variables:
#'     `TrapID` (station identifier, `A` to `W`) and `x`, `y` (station
#'     coordinates, in metres).}
#'   \item{`ocelot_spatial`}{a data frame with 838 rows (one per point on
#'     a regular grid covering the study area) and 3 variables: `x`, `y`
#'     (coordinates, in metres) and `seis_len` (a spatial covariate
#'     describing seismic line length around the point).}
#' }
#' @inherit ocelot1 references
#' @seealso [ocelot1] for a single-session version of these data.
#' @examples
#' str(edf)
#' table(edf$Session, edf$Sex)
#'
#' # camera stations; darker grid points have seismic lines nearby
#' plot(ocelot_spatial$x, ocelot_spatial$y, pch = 15, asp = 1,
#'      col = grey(1 - (ocelot_spatial$seis_len > 0) / 2),
#'      xlab = "x", ylab = "y")
#' points(tdf_control$x, tdf_control$y, pch = 3, col = "red")
#' @name ocelot2
#' @aliases edf tdf_control tdf_disturb ocelot_spatial
#' @docType data
#' @keywords datasets
NULL

# ---- Regression, smoothing and impact assessment ----------------------------

#' Nysted wind farm data
#'
#' An environmental impact assessment (EIA) data set from a site with two
#' offshore wind farms in Denmark (Rødsand I, built in 2003, and
#' Rødsand II, built in 2010). The objective was to study the potential
#' effects of the wind farms on birds.
#'
#' @details
#' The data are counts of birds seen within some distance of
#' pre-determined tracks (transects) flown by aircraft. Details of the
#' study are in the first report commissioned after Rødsand I was
#' completed (Kahlert et al., 2004).
#'
#' @format A data frame with 31502 rows and 12 variables:
#' \describe{
#'   \item{DistCoast}{distance from the coast, in km}
#'   \item{Phase}{construction phase of the wind farms, a character vector:
#'     `A` is before any farms were installed (11 survey days), `B` is
#'     after Rødsand I was installed (13 survey days) and `C` is after
#'     Rødsand II was installed (5 survey days)}
#'   \item{Area}{observation area for each count, in square km}
#'   \item{Count}{estimated bird abundance using distance sampling, a
#'     method that adjusts for imperfect detection (animals farthest from
#'     the plane are more difficult to observe). Typically used as the
#'     response variable}
#'   \item{Depth}{sea depth at the location of the observation, in metres}
#'   \item{Year}{year the observation was made (2000 to 2011)}
#'   \item{Month}{month the observation was made (1 to 4)}
#'   \item{Day}{day of the month the observation was made}
#'   \item{YearMonth}{year and month of the observation as text, e.g.
#'     `"2000/1"` is January 2000}
#'   \item{XPos}{x coordinate in UTM projection, in metres}
#'   \item{YPos}{y coordinate in UTM projection, in metres}
#'   \item{TransectID}{unique transect identifier, a character vector}
#' }
#' @references Kahlert, J., Petersen, I.K., Fox, A.D., Desholm, M. and
#'   Clausager, I. (2004) Investigations of birds during construction and
#'   operation of Nysted offshore wind farm at Rødsand. NERI report.
#'   <https://tethys.pnnl.gov/sites/default/files/publications/Kahlert-et-al-2004.pdf>
#' @seealso [horn] for a similar data set from the Horns Rev wind farm.
#' @examples
#' str(nysted)
#' table(nysted$Phase)
#'
#' # mean estimated density (birds per square km) by phase
#' with(nysted, tapply(Count, Phase, sum) / tapply(Area, Phase, sum))
"nysted"

#' Horns Rev environmental impact assessment data
#'
#' Data collected to assess the environmental impact of the Horns Rev
#' offshore wind farm in Denmark. The focus is the abundance and
#' distribution of a large sea duck, the common scoter (*Melanitta
#' nigra*).
#'
#' @details
#' The data were collected along tracklines (transects) using aerial
#' survey methods: the tracks are flown by plane and the number of animals
#' at each location on or near the track line is recorded. The observed
#' counts are adjusted for the fact that not all animals at the surface
#' are seen. This correction for imperfect detection was carried out using
#' distance sampling.
#'
#' @format A data frame with 27854 rows and 11 variables:
#' \describe{
#'   \item{XPos}{x coordinate in UTM projection, in metres}
#'   \item{YPos}{y coordinate in UTM projection, in metres}
#'   \item{Year}{year the observation was made (2005 to 2007, 2011, 2012)}
#'   \item{Month}{month the observation was made (1 to 12)}
#'   \item{Day}{day of the month the observation was made}
#'   \item{Depth}{sea depth at the location of the observation, in metres}
#'   \item{TransectID}{unique transect identifier (an integer)}
#'   \item{Area}{observation area for each count, in square km}
#'   \item{Nhat}{estimated scoter abundance from distance sampling (the
#'     response)}
#'   \item{YearMonth}{year and month of the observation pasted together as
#'     an integer with no zero padding of the month, e.g. `200511` is
#'     November 2005 and `20062` is February 2006. Because of this the
#'     values do not sort in date order}
#'   \item{Impact}{factor indicating whether the estimate is from before
#'     (`pre`, 2005 to 2007) or after (`post`, 2011 and 2012) the wind
#'     farm was built}
#' }
#' @seealso [nysted] for a similar data set from the Nysted wind farm.
#' @examples
#' str(horn)
#' table(horn$Impact, horn$Year)
#'
#' # mean estimated density (birds per square km) before and after
#' with(horn, tapply(Nhat, Impact, sum) / tapply(Area, Impact, sum))
"horn"

#' Tobin's Q data
#'
#' Annual financial data for US companies between 1990 and 2014, with
#' Tobin's Q (the ratio of a company's market value to its book value) and
#' covariates. There is one row per company per year; the companies are
#' grouped into industry classes.
#'
#' @details
#' The original description of these data refers to 1240 companies and 41
#' industry classes. The data frame contains 1251 distinct values of `ID`
#' and 42 distinct values of `IndClass`.
#'
#' The derived variables satisfy `TobinsQ = MarketValue / BookValue`,
#' `rLTD = LTD / Assets`, `rPPE = PPE / Assets`,
#' `rEBITDA = EBITDA / Assets`, `rCapEx = CapEx / Sales`,
#' `rRD = RD / Sales` and `rAds = Ads / Sales` (up to rounding). The
#' monetary units of the other variables are not recorded in the package.
#'
#' @format A data frame with 13662 rows and 20 variables:
#' \describe{
#'   \item{ID}{company identifier}
#'   \item{Year}{year (1990 to 2014)}
#'   \item{Assets}{total assets}
#'   \item{CapEx}{capital expenditure}
#'   \item{LTD}{long-term debt}
#'   \item{EBITDA}{earnings before interest, tax, depreciation and
#'     amortisation}
#'   \item{PPE}{property, plant and equipment}
#'   \item{Sales}{sales}
#'   \item{Ads}{advertising expenditure}
#'   \item{RD}{research and development expenditure}
#'   \item{BookValue}{book value}
#'   \item{MarketValue}{market value}
#'   \item{IndClass}{industry class, a character vector}
#'   \item{TobinsQ}{Tobin's Q, `MarketValue / BookValue`}
#'   \item{rLTD}{long-term debt as a proportion of assets}
#'   \item{rCapEx}{capital expenditure as a proportion of sales}
#'   \item{rRD}{research and development expenditure as a proportion of
#'     sales}
#'   \item{rAds}{advertising expenditure as a proportion of sales}
#'   \item{rPPE}{property, plant and equipment as a proportion of assets}
#'   \item{rEBITDA}{EBITDA as a proportion of assets}
#' }
#' @examples
#' str(tobinsq)
#' length(unique(tobinsq$ID))
#' hist(log(tobinsq$TobinsQ[tobinsq$TobinsQ > 0]), xlab = "log(Tobin's Q)",
#'      main = "")
"tobinsq"

#' Simulated non-linear response data
#'
#' Simulated data with a non-linear relationship between a response and a
#' single covariate, used to illustrate smoothing methods. The true mean
#' is included so that fitted curves can be compared with it.
#'
#' @format A data frame with 400 rows and 3 variables:
#' \describe{
#'   \item{x}{the covariate, 400 equally spaced values from 0 to 1}
#'   \item{y}{the response: the true mean plus random noise}
#'   \item{mu}{the true mean of `y` at `x`}
#' }
#' @source Simulated.
#' @examples
#' plot(y ~ x, data = simdat_nl, col = "grey")
#' lines(mu ~ x, data = simdat_nl, lwd = 2)
"simdat_nl"

#' Biodiversity data
#'
#' Counts of five species (`A` to `E`) in each of 16 plots in each of 15
#' years, in wide format (one row per plot and species, one column per
#' year).
#'
#' @details
#' The object was read with \pkg{readr} and is stored as a tibble (class
#' `spec_tbl_df`). It behaves as an ordinary data frame if \pkg{tibble} is
#' not loaded; use `as.data.frame(biod)` to drop the tibble classes.
#'
#' @format A data frame (tibble) with 80 rows and 17 variables:
#' \describe{
#'   \item{Plot}{plot identifier (1 to 16)}
#'   \item{Species}{species identifier (`"A"` to `"E"`), a character vector}
#'   \item{Year1, Year2, Year3, Year4, Year5, Year6, Year7, Year8, Year9,
#'     Year10, Year11, Year12, Year13, Year14, Year15}{count of the species
#'     in the plot in years 1 to 15}
#' }
#' @examples
#' biod <- as.data.frame(biod)
#' str(biod)
#'
#' # total count of each species in each year
#' tot <- rowsum(biod[, -(1:2)], biod$Species)
#' matplot(1:15, t(tot), type = "l", lty = 1, xlab = "Year",
#'         ylab = "Total count")
#' legend("topright", legend = rownames(tot), col = 1:5, lty = 1)
"biod"
