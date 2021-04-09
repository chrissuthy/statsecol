\name{titgof}
\alias{titgof}
\docType{data}
\title{
Great tit goodness-of-fit output}
\description{
The goodness-of-fit output from \code{N.mix.gof()} for the top model from the great tit analysis in the N-mixture practiccal.
}
\usage{data("titgof")}
\format{
  The format is:
List of 6
 $ model.type: chr "unmarkedFitPCount"
 $ chi.square: num 2910
 $ t.star    : num [1:200, 1] 755 726 730 695 661 ...
 $ p.value   : num 0
 $ c.hat.est : num 4.01
 $ nsim      : num 200
 - attr(*, "class")= chr "Nmix.chisq"
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
data(titgof)
## maybe str(titgof) ; plot(titgof) ...
}
\keyword{datasets}
