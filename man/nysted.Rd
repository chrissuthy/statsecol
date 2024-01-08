\name{nysted}
\alias{nysted}
\docType{data}
\title{
Nysted wind farm data
}
\description{
An environmental impact assessment (EIA) dataset collected from a site with two off-shore wind farms in Denmark (Rødsand I built in 2003 and 
Rødsand II built in 2010). 
}
\usage{data("nysted")}
\format{
  A data frame with 31502 observations on the following 12 variables.
  \describe{
    \item{\code{DistCoast}}{distance in km from the coast.}
    \item{\code{Phase}}{The construction phase of the wind farms. A character where \code{A} is before any farms were installed (11 survey days), \code{B} is after Rødsand I was installed (13 survey days), and \code{C} is after Rødsand II was installed (5 survey days).}
    \item{\code{Area}}{Observation area for each count (km\eqn{^2}).}
    \item{\code{Count}}{Estimated bird abundance using distance sampling, a method that adjusts for the imperfect detection process i.e. animals farthest from the plane are more difficult to observe. This is typically used as the response variable.}
    \item{\code{Depth}}{Sea depth at spatial location of observation in metres.}
    \item{\code{Year}}{Year observation was made (e.g., \eqn{2000, 2005, 2007}).}
    \item{\code{Month}}{Month observation was made (\eqn{1, 2, 3, 4}).}
    \item{\code{Day}}{Day of the month the observation was made (\eqn{1,2,\ldots,31}).}
    \item{\code{YearMonth}}{Concatanated year and month of observation (e.g., \eqn{2000/1} is January 2000)}
    \item{\code{XPos}}{X coordinate in UTM projection (metres).}
    \item{\code{YPos}}{Y coordinate in UTM projection (metres).}
    \item{\code{TransectID}}{Unique transect ID (character).}
  }
}
\details{
The objective was to study the potential effects of the wind farms on birds. Details about the study can be found in the first report commissioned after Rødsand I was completed (see ditation below). The data are counts of birds seen within some distance of pre-determined tracks (transects) flown by aircraft.}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
Kahlert, J., Petersen, I.K., Fox, A.D., Desholm, M. and Clausager, I., 2004. Investigations of birds during construction and operation of Nysted offshore wind. NERI report. \url{https://tethys.pnnl.gov/sites/default/files/publications/Kahlert-et-al-2004.pdf}
}
\examples{
data(nysted)
str(nysted)
}
\keyword{datasets}
