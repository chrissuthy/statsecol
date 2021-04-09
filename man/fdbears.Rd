\name{fdbears}
\alias{fdbears}
\docType{data}
\title{
Fort Drum bear data
}
\description{
Trap and capture data for the Fort Drum hair snare stduy described in Gardener et al. (2010).
}
\usage{data("fdbears")}
\format{
  The format is:
List of 2
 $ : num [1:151, 1:4] 1 1 1 1 1 1 1 1 1 1 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : NULL
  .. ..$ : chr [1:4] "session" "id" "occasion" "trap"
 $ :'data.frame':	38 obs. of  3 variables:
  ..$ trap: int [1:38] 1 2 3 4 5 6 7 8 9 10 ...
  ..$ x   : num [1:38] 4.48e+11 4.39e+11 4.39e+11 4.42e+11 4.42e+11 ...
  ..$ y   : num [1:38] 4885900 4881388 4878611 4883961 4881130 ...
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
data(fdbears)
## maybe str(fdbears) ; plot(fdbears) ...
}
\keyword{datasets}
