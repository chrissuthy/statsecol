\name{poppies}
\alias{poppies}
\docType{data}
\title{
Poppies plot sampling data 
}
\description{
Simulated plot sampling data collected on two popie meadows.
}
\usage{data("poppies")}
\format{
  A data frame with 40 observations on the following 4 variables.
  \describe{
    \item{\code{Population}}{the meadow identifier, a character vector}
    \item{\code{plot}}{the plot identifier, a numeric vector}
    \item{\code{n}}{the number of popies observed in the plot, a numeric vector}
    \item{\code{a}}{the area of the plot (same for all plots), a numeric vector}
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
data(poppies)
str(poppies)
}
\keyword{datasets}
