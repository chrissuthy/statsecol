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
 $ bear_cap_data :'data.frame':	151 obs. of  4 variables:
  ..$ session : num [1:151] 1 1 1 1 1 1 1 1 1 1 ...
  ..$ id      : num [1:151] 1 1 1 1 1 1 1 1 1 1 ...
  ..$ occasion: num [1:151] 1 3 5 5 5 6 6 7 7 8 ...
  ..$ trap    : num [1:151] 2 5 5 31 34 5 31 30 34 5 ...
 $ bear_trap_data:'data.frame':	38 obs. of  3 variables:
  ..$ trap: int [1:38] 1 2 3 4 5 6 7 8 9 10 ...
  ..$ x   : num [1:38] 447694 438789 439232 442142 441987 ...
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
