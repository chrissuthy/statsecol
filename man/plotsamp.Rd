\name{plotsamp}
\alias{plotsamp}
\docType{data}
\title{
Simulated plot sampling data.
}
\description{
A dataset containing the number of individuals that fall withing each of 25 square sampling plots.}
\usage{data("plotsamp")}
\format{
  A data frame with 25 observations on the following 4 variables.
  \describe{
    \item{\code{Plot}}{plot identifier, a factor with 25 levels (e.g, \code{1} \code{2} ... \code{25})}
    \item{\code{Count}}{the number of animals observed in each plot, a numeric vector}
    \item{\code{X}}{the X coordinate of the plot center, a numeric vector}
    \item{\code{Y}}{the Y coordinate of the plot center, a numeric vector}
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
data(plotsamp)
plot(plotsamp$X,plotsamp$Y, pch="", xlim=c(0,100),ylim=c(0,100), asp=1)
abline(h=seq(0,100,20),v=seq(0,100,20))
text(plotsamp$X,plotsamp$Y,labels=plotsamp$Count, cex=2)
}
\keyword{datasets}
