\name{bowhead_PS}
\alias{bowhead_PS}
\docType{data}
\title{
Plot sampling survey data from Bowhead whale survey.
}
\description{
Data from the bowhead whale survey reported in Rekdal et al. (2015), but formatted as 
a plot sampling survey.
}
\usage{data("bowhead_PS")}
\format{
  A data frame with 41 observations on the following 5 variables.
  \describe{
    \item{\code{stratum}}{the stratum the transect was in, a numeric vector}
    \item{\code{transect}}{the transect ID, a numeric vector}
    \item{\code{A}}{the area of the startum that the transect was in, repeated for each transect within straum, a numeric vector}
    \item{\code{a}}{the area of the transect strip, a numeric vector}
    \item{\code{n}}{the number of individuals observed in the strip, a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Rekdal, S.L., Hansen, R.G., Borchers, D., Bachmann, L., Laidre, K.L., Wiig, O., Nielsen, N.H., 
Fossette, S., Tervo, O. and Heide-Jorgensen, M.P., 2015. Trends in bowhead whales in West Greenland: 
Aerial surveys vs. genetic capture-recapture analyses. Marine Mammal Science, 31(1), pp.133-154.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(bowhead_PS)
str(bowhead_PS)
}
\keyword{datasets}
