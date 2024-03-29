\name{plotsample_est}
\alias{plotsample_est}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot sampling estimation}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotsample_est(
  n,
  p,
  alpha = 0.05,
  method = "lognormal",
  dbn = "binomial",
  B = 999,
  Nmult = 3
)
}
\arguments{
\item{n}{Vector of counts in plots}

\item{p}{Proportion of survey region covered by plots}

\item{alpha}{The significance level (defaults to \code{alpha}=0.05 for 95\% level)}

\item{method}{If 'normal', the estimator is assumed to be normally distributed with 
variance calculated assuming the distribution given by \code{dbn}, if 
'lognormal', the estimator is assumed to be lognormally distributed with 
variance calculated assuming the distribution given by \code{dbn}, if 'exact', the 
estimator is assumed to have the parameteric distribution specified by \code{dbn}. If
'percentile' and \code{dbn} is 'bootstrap', the percentile method is used.}

\item{dbn}{If 'binomial', the count is assumed to be binomially distributed (for 
variance estimation, and if \code{method} is 'exact', then for confidence interval 
estimation too.  If 'binomial', the count is assumed to be binomially distributed (for 
variance estimation, and if \code{method} is 'exact', then for confidence interval 
estimation too.  If 'bootstrap' the variance is obtained by nonparametric bootstrap 
of the plots, and the confidence interval is obtained using this and assuming that the 
estimator is normally distributed (if \code{method} is 'bionomial'), or  assuming that the 
estimator is Poisson distributed (if \code{method} is 'poisson'),  or making no 
distributional assumption and using the pecentile method (if \code{method} is 'percentile')}

\item{B}{Number of bootstrap replicates.}

\item{Nmult}{multiple of estimate beyond which probability of getting observed data
is assumed to be zero (just for computational covenience when calculating exact CI).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a list with the following elements:
\itemize{
 \item{$Nhat :}{ Estimated abundance.}
 \item{$se.Nhat :}{ Standard error of the estimator.}
 \item{$cv.Nhat :}{ Coefficient of variance of the estimator.}
 \item{$ci.Nhat:}{ Confidence interval.}
}
}
\author{
David Borchers
}
\description{
Estimates abundance as n/p, where n is total count and p inclusion probability.
Also returns a confidence interval, estimated standard error and CV, using one of
four possible methods (see argument \code{method}.)
}

\examples{
plotsample_est(n=10, p=0.5, method='lognormal')
}

