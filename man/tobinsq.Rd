\name{tobinsq}
\alias{tobinsq}
\docType{data}
\title{
Tobin’s Q data
}
\description{
This 25 year data set, collected from 1240 US companies. The data contains Tobin’s Q values along with corresponding covariates from the US-based companies across 41 industry classes.
}
\usage{data("tobinsq")}
\format{
  A data frame with 13662 observations on the following 20 variables.
  \describe{
    \item{\code{ID}}{a numeric vector}
    \item{\code{Year}}{a numeric vector}
    \item{\code{Assets}}{a numeric vector}
    \item{\code{CapEx}}{a numeric vector}
    \item{\code{LTD}}{a numeric vector}
    \item{\code{EBITDA}}{a numeric vector}
    \item{\code{PPE}}{a numeric vector}
    \item{\code{Sales}}{a numeric vector}
    \item{\code{Ads}}{a numeric vector}
    \item{\code{RD}}{a numeric vector}
    \item{\code{BookValue}}{a numeric vector}
    \item{\code{MarketValue}}{a numeric vector}
    \item{\code{IndClass}}{a character vector}
    \item{\code{TobinsQ}}{a numeric vector}
    \item{\code{rLTD}}{a numeric vector}
    \item{\code{rCapEx}}{a numeric vector}
    \item{\code{rRD}}{a numeric vector}
    \item{\code{rAds}}{a numeric vector}
    \item{\code{rPPE}}{a numeric vector}
    \item{\code{rEBITDA}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(tobinsq)
str(tobinsq)
}
\keyword{datasets}
