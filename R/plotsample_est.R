#' Plot sampling estimation of abundance
#'
#' Estimates abundance from plot (quadrat or strip) counts as
#' \eqn{\hat{N} = n/p}, where \eqn{n} is the total count over all sampled plots
#' and \eqn{p} is the proportion of the survey region that was covered by
#' plots (the inclusion probability). Also returns the estimated standard
#' error, coefficient of variation and a confidence interval.
#'
#' @details
#' The variance of \eqn{\hat{N}} is obtained in one of three ways, chosen by
#' `dbn`:
#'
#' * `"binomial"`: \eqn{n \sim \mathrm{Binomial}(N, p)}, giving
#'   \eqn{\widehat{\mathrm{var}}(\hat{N}) = \hat{N}(1-p)/p}.
#' * `"poisson"`: \eqn{n \sim \mathrm{Poisson}(Np)}, giving
#'   \eqn{\widehat{\mathrm{var}}(\hat{N}) = \hat{N}/p}.
#' * `"bootstrap"`: plots are resampled with replacement `B` times and the
#'   variance of the `B` bootstrap estimates is used. This makes no
#'   assumption about the distribution of the counts, but needs the
#'   individual plot counts (a single total is not enough).
#'
#' The confidence interval is then obtained according to `method`:
#'
#' * `"normal"`: \eqn{\hat{N} \pm z_{1-\alpha/2}\,\mathrm{se}(\hat{N})}
#'   (see [norm.ci()]).
#' * `"lognormal"`: assumes \eqn{\hat{N}} is lognormally distributed, which
#'   gives an asymmetric interval that cannot include negative values
#'   (see [lognorm.ci()]).
#' * `"exact"`: only with `dbn = "binomial"` or `"poisson"`. The limits are
#'   the values of \eqn{N} for which the probability of a count at least as
#'   extreme as the one observed is \eqn{\alpha/2} in each tail. Candidate
#'   values of \eqn{N} are searched from \eqn{n} up to `Nmult` times
#'   \eqn{\hat{N}}.
#' * `"percentile"`: only with `dbn = "bootstrap"`. The limits are the
#'   \eqn{\alpha/2} and \eqn{1-\alpha/2} quantiles of the bootstrap estimates.
#'
#' Confidence limits are rounded to the nearest whole animal.
#'
#' @param n Vector of counts, one per sampled plot. A single total count can
#'   be given unless `dbn = "bootstrap"`.
#' @param p Proportion of the survey region covered by the sampled plots
#'   (greater than 0 and no more than 1).
#' @param alpha Significance level; the interval has coverage
#'   `100 * (1 - alpha)`% (default `0.05`, a 95% interval).
#' @param method How the confidence interval is calculated: `"lognormal"`
#'   (default), `"normal"`, `"exact"` or `"percentile"`. See Details for
#'   which combinations of `method` and `dbn` are allowed.
#' @param dbn How the variance is calculated: `"binomial"` (default),
#'   `"poisson"` or `"bootstrap"`. See Details.
#' @param B Number of bootstrap replicates (only used when
#'   `dbn = "bootstrap"`).
#' @param Nmult Multiple of the abundance estimate beyond which the
#'   probability of the observed count is treated as zero. Only used when
#'   `method = "exact"`, to bound the search for the confidence limits.
#'
#' @return A list with elements:
#' \describe{
#'   \item{`Nhat`}{Estimated abundance.}
#'   \item{`se.Nhat`}{Estimated standard error of `Nhat`.}
#'   \item{`cv.Nhat`}{Estimated coefficient of variation of `Nhat`.}
#'   \item{`ci.Nhat`}{Lower and upper confidence limits.}
#'   \item{`method`}{The confidence interval method used.}
#' }
#'
#' @author David Borchers
#' @seealso [norm.ci()], [lognorm.ci()], and the example data sets
#'   [plotsamp], [poppies] and [bowhead_PS].
#'
#' @examples
#' # 25 plots of 20 x 20 covering all of a 100 x 100 region would give p = 1;
#' # suppose instead that only the first 10 plots were surveyed (p = 0.4)
#' n <- plotsamp$Count[1:10]
#' plotsample_est(n, p = 0.4)
#' plotsample_est(n, p = 0.4, method = "normal", dbn = "poisson")
#' plotsample_est(n, p = 0.4, method = "exact")
#'
#' # nonparametric bootstrap of plots, percentile interval
#' set.seed(1)
#' plotsample_est(n, p = 0.4, method = "percentile", dbn = "bootstrap")
#'
#' # a single total count is enough for the binomial and Poisson options
#' plotsample_est(n = 10, p = 0.5, method = "lognormal")
#' @export
plotsample_est = function(n,p,alpha=0.05,method="lognormal",dbn="binomial",B=999,Nmult=3) {
  # ---- input checks (added v0.2.0; estimation code below is unchanged) ----
  if(!is.numeric(n) || length(n)<1 || anyNA(n) || any(n<0)) {
    stop("n must be a vector of non-negative counts with no missing values.")
  }
  if(!is.numeric(p) || length(p)!=1 || is.na(p) || p<=0 || p>1) {
    stop("p must be a single number greater than 0 and no more than 1.")
  }
  if(!is.numeric(alpha) || length(alpha)!=1 || is.na(alpha) || alpha<=0 || alpha>=1) {
    stop("alpha must be a single number between 0 and 1.")
  }
  if(!dbn %in% c("binomial","poisson","bootstrap")) {
    stop("Invalid dbn. It must be 'binomial', 'poisson' or 'bootstrap'.")
  }
  if(dbn=="bootstrap") {
    if(!method %in% c("normal","lognormal","percentile")) {
      stop("Invalid method. For bootstrap, method must be 'normal', 'lognormal' or 'percentile'.")
    }
    if(length(n)<2) {
      stop("dbn = 'bootstrap' needs the individual plot counts (n must have length > 1).")
    }
  }else {
    if(!method %in% c("normal","lognormal","exact")) {
      stop("Invalid method. With dbn '",dbn,"', method must be 'normal', 'lognormal' or 'exact'.")
    }
  }
  # -------------------------------------------------------------------------
  ntot = sum(n)
  Nhat = ntot/p
  if(dbn=="bootstrap") {
    b.Nhat = rep(NA,B)
    for(i in 1:B) b.Nhat[i] = sum(sample(n,size=length(n),replace=TRUE))/p
    se.Nhat = sqrt(var(b.Nhat)*(B-1)/B)
    K = length(n)
    se.Nhat = se.Nhat*sqrt(K/(K-1))
    Nhat = sum(n)/p
    cv.Nhat = se.Nhat/mean(b.Nhat)
    if(method=="normal") {
      ci.Nhat = round(norm.ci(Nhat,se.Nhat,alpha))
    }else if(method=="lognormal") {
      ci.Nhat = round(lognorm.ci(Nhat,se.Nhat,alpha))
    }else if(method=="percentile") {
      ci.Nhat = round(quantile(b.Nhat,probs=c(alpha/2,1-alpha/2)))
    }
  }else {
    if(dbn=="binomial") {
      var.Nhat = Nhat*(1-p)/p
    }else if (dbn=="poisson") {
      var.Nhat = Nhat/p
    }
    se.Nhat = sqrt(var.Nhat)
    cv.Nhat = se.Nhat/Nhat
    if(method=="normal") {
      ci.Nhat = round(norm.ci(Nhat,se.Nhat,alpha))
    }else if(method=="lognormal") {
      ci.Nhat = round(lognorm.ci(Nhat,se.Nhat,alpha))
    }else if(method=="exact") {
      Ns = c(ntot:(Nmult*Nhat))
      cdf = rep(NA,length(Ns))
      if(dbn=="binomial") for(i in 1:length(Ns)) cdf[i] = pbinom(ntot,Ns[i],p)
      else if(dbn=="poisson") for(i in 1:length(Ns)) cdf[i] = ppois(ntot,Ns[i]*p)
      lo = Ns[max(which(cdf>=(1-alpha/2)))]
      up = Ns[min(which(cdf<=(alpha/2)))]
      ci.Nhat = round(c(lo,up))
    }
  }
  return(list(Nhat=Nhat,se.Nhat=se.Nhat,cv.Nhat=cv.Nhat,ci.Nhat=ci.Nhat,method=method))
}
