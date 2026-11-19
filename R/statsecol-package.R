#' statsecol: data and functions for the Statistical Ecology MSc
#'
#' Data sets and a small number of functions used for teaching on the
#' Statistical Ecology MSc modules at the University of St Andrews
#' (currently MT5751, MT5764 and MT5767).
#'
#' @section Data sets by topic:
#'
#' **Plot sampling and distance sampling**
#'
#' * [plotsamp]: simulated counts in 25 square plots.
#' * [poppies]: simulated plot counts from two poppy meadows.
#' * [bowhead_PS], [bowhead_LT]: bowhead whale aerial survey, formatted as a
#'   plot (strip) survey and as a line transect survey.
#'
#' **Population dynamics and state-space models**
#'
#' * [wildebeest]: Serengeti wildebeest abundance estimates, rainfall and
#'   harvest.
#' * [falcons]: peregrine falcon counts, French Jura.
#' * [martin]: house martin nest counts.
#'
#' **Occupancy and N-mixture models**
#'
#' * [willow]: willow tit detection/non-detection data.
#' * [redsq]: red squirrel detection/non-detection data.
#' * [gtit]: great tit repeated counts; [titgof] is goodness-of-fit output
#'   for a model of these data.
#' * [burnet]: six-spot burnet moth data.
#' * [voles]: water vole patch occupancy data over four years.
#'
#' **Spatial capture-recapture**
#'
#' * [Boland] (`BolandCH`, `BolandMask`): leopard camera trap data.
#' * [fdbears]: Fort Drum black bear hair snare data.
#' * [ocelot1]: single-session ocelot camera trap data.
#' * [ocelot2] (`edf`, `tdf_control`, `tdf_disturb`, `ocelot_spatial`):
#'   two-session ocelot camera trap data.
#'
#' **Regression, smoothing and impact assessment (MT5764)**
#'
#' * [nysted], [horn]: offshore wind farm impact assessment surveys.
#' * [tobinsq]: Tobin's Q for US companies.
#' * [simdat_nl]: simulated non-linear response data.
#' * [biod]: species counts by plot and year.
#'
#' @section Functions:
#'
#' * [plotsample_est()]: abundance estimation from plot counts.
#' * [norm.ci()], [lognorm.ci()]: normal and lognormal confidence intervals.
#'
#' @importFrom stats pbinom ppois qnorm quantile var
#' @keywords internal
"_PACKAGE"
