\name{horn}
\alias{horn}
\docType{data}
\title{
Horn Rev environemntal impact assessment data.
}
\description{
Data collected to assess the environmental impact of the offshore wind farm Horns Rev. Specifically, the focus here is on the abundance and distribution of a large sea duck, the common scoter (\emph{Melanitta nigra}).
}
\usage{data("horn")}
\format{
  A data frame with 27854 observations on the following 11 variables.
  \describe{
    \item{\code{XPos}}{X coordinate in UTM projection (metres).}
    \item{\code{YPos}}{Y coordinate in UTM projection (metres).}
    \item{\code{Year}}{Year observation was made (eg, 2005).}
    \item{\code{Month}}{Month observation was made (1-12).}
    \item{\code{Day}}{Day observation was made (1-31).}
    \item{\code{Depth}}{Sea depth at spatial location of observation in metres.}
    \item{\code{TransectID}}{Unique transect ID (character).}
    \item{\code{Area}}{Observation area for each count (km2).}
    \item{\code{Nhat}}{Estimated scoter abundance from distance sampling (response).}
    \item{\code{YearMonth}}{Concatanated year and month of observation (e.g., \eqn{200511} is November 2005).}
    \item{\code{Impact}}{Factor indicating whether estimate is from before (\code{pre}) of after (\code{post}) the wind farm was built.}
  }
}
\details{
The data are collected along tracklines (transects) from the ocean surface using aerial survey methods. These tracks are followed from the air by plane and the number of animals at each location (on or near the track lines) are recorded. The observed counts are adjusted for the fact that not all animals at the surface are seen. This correction for the imperfect detection process was carried out using distance sampling.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(horn)
str(horn)
}
\keyword{datasets}
