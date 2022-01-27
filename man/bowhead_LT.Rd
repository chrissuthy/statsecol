\name{bowhead_LT}
\alias{bowhead_LT}
\docType{data}
\title{
Line transect survey data from bowhead whale survey.
}
\description{
Line transect survey data from the bowhead whale survey reported in Rekdal et al. (2015).
}
\usage{data("bowhead_LT")}
\format{
  A data frame with 84 observations on the following 9 variables.
  \describe{
    \item{\code{Region.Label}}{the stratum the transect was in, a numeric vector}
    \item{\code{Area}}{the area of the startum that the transect was in, repeated for each observation within straum, a numeric vector}
    \item{\code{Sample.Label}}{group ID, observations of >1 individuals will have a ow per animal, but a shared Sample.Label, a numeric vector}
    \item{\code{Effort}}{a numeric vector}
    \item{\code{distance}}{perpendicular distance to the observation, a numeric vector}
    \item{\code{y}}{a numeric vector}
    \item{\code{size}}{a numeric vector}
    \item{\code{bf}}{a numeric vector}
    \item{\code{object}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Rekdal, S.L., Hansen, R.G., Borchers, D., Bachmann, L., Laidre, K.L., Wiig, O., Nielsen, N.H., Fossette, S., Tervo, O. and Heide-Jorgensen, M.P., 2015. Trends in bowhead whales in West Greenland: Aerial surveys vs. genetic capture-recapture analyses. Marine Mammal Science, 31(1), pp.133-154.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(bowhead_LT)
str(bowhead_LT)
}
\keyword{datasets}
