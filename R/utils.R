#' Normal and lognormal confidence intervals
#'
#' Confidence intervals for an estimate `x` with standard error `se`,
#' assuming the estimator is either normally or lognormally distributed.
#' These are the helper functions used by [plotsample_est()].
#'
#' @details
#' `norm.ci()` returns the usual symmetric interval
#' \deqn{x \pm z_{1-\alpha/2}\,\mathrm{se}.}
#'
#' `lognorm.ci()` returns \eqn{(x/C,\; x\,C)} with
#' \deqn{C = \exp\left(z_{1-\alpha/2}\sqrt{\log\left(1 + (\mathrm{se}/x)^2\right)}\right),}
#' which is asymmetric about `x` and, for a positive estimate, always
#' positive. It is the interval commonly used for abundance estimates.
#'
#' @param x The estimate (for `lognorm.ci()`, a positive number).
#' @param se The standard error of the estimate.
#' @param alpha Significance level; the interval has coverage
#'   `100 * (1 - alpha)`% (default `0.05`, a 95% interval).
#'
#' @return A numeric vector of length two: the lower and upper confidence
#'   limits.
#'
#' @note The \pkg{boot} package also has a function called `norm.ci()`. If
#'   both packages are attached, use `statsecol::norm.ci()` to be sure which
#'   one is called.
#'
#' @seealso [plotsample_est()]
#' @examples
#' norm.ci(100, 20)
#' lognorm.ci(100, 20)
#'
#' # 90% intervals
#' norm.ci(100, 20, alpha = 0.1)
#' lognorm.ci(100, 20, alpha = 0.1)
#' @name ci_helpers
NULL

#' @rdname ci_helpers
#' @export
norm.ci = function(x,se,alpha=0.05) {
  x + qnorm(1-alpha/2)*c(-1,1)*se
}

#' @rdname ci_helpers
#' @export
lognorm.ci = function(x,se,alpha=0.05) {
  x*exp(qnorm(1-alpha/2)*c(-1,1)*sqrt(log(1+(se/x)^2)))
}
