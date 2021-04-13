\name{ocelot1}
\alias{ocelot1}
\docType{data}
\title{
Ocelot camera trap data
}
\description{
Spatially explicit ocelot encounter histories from the Peruvian Amazon. Data from Kolowski & Alonso (2010). 
}
\usage{data("ocelot1")}
\format{
  The format is:
List of 3
 $ ocelot_cap_data :'data.frame':	41 obs. of  4 variables:
  ..$ session : Factor w/ 1 level "PeruOcelotB": 1 1 1 1 1 1 1 1 1 1 ...
  ..$ ID      : int [1:41] 12 10 4 2 2 2 2 2 9 5 ...
  ..$ occasion: int [1:41] 36 38 6 34 16 32 40 9 43 20 ...
  ..$ trap    : num [1:41] 1 3 6 6 7 7 7 8 8 9 ...
 $ ocelot_trap_data:'data.frame':	23 obs. of  3 variables:
  ..$ trap: num [1:23] 1 2 3 4 5 6 7 8 9 10 ...
  ..$ x   : num [1:23] 426679 427827 428874 430053 426655 ...
  ..$ y   : num [1:23] 9805656 9805718 9805775 9805711 9804631 ...
 $ ocelot_operation: int [1:23, 1:44] 0 0 0 1 1 1 1 1 1 1 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : NULL
  .. ..$ : chr [1:44] "1" "2" "3" "4" ...
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
data(ocelot1)
## maybe str(ocelot1) ; plot(ocelot1) ...
}
\keyword{datasets}
