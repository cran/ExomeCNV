\name{get.AUC}
\alias{get.AUC}
\alias{get.alpha}
\alias{get.alpha.inv}
\alias{get.power}
\alias{get.power.inv}
\alias{get.sensitivity}
\alias{get.specificity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate theoretical specificity, sensitivity, area under curve (AUC).
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculate theoretical specificity, sensitivity, area under curve (AUC) given copy number ratio, window size, and sequence read length.
These are used internally.
}
\usage{
get.AUC(chi, r, W, l, rho, overdisperse = "no", phi = 1, od.alpha = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chi}{
%%     ~~Describe \code{chi} here~~
x-coverage, number of sequenced bases at one position
}
  \item{r}{
%%     ~~Describe \code{r} here~~
cutoff ratio used to call amplification/deletion
}
  \item{W}{
%%     ~~Describe \code{W} here~~
window size in bp
}
  \item{l}{
%%     ~~Describe \code{l} here~~
read length in bp
}
  \item{rho}{
%%     ~~Describe \code{rho} here~~
true copy number ratio e.g. 1.5 for 1 copy gain, 0.5 for 1 copy loss
}
  \item{overdisperse}{
%%     ~~Describe \code{overdisperse} here~~
overdispersion model: 
	"no" -- no overdispersion, var = mu
	"quasi-likelihood" or "ql" -- quasi-likelihood where var = phi*mu
	"negative binomial" or "nb" -- negative binomial where var = mu + alpha*mu^2
}
  \item{phi}{
%%     ~~Describe \code{phi} here~~
overdispersion constant as modeled by quasi-likelihood approach where var = phi*mu
}
  \item{od.alpha}{
%%     ~~Describe \code{od.alpha} here~~
overdispersion constant as modeled by negative binomial approach where var = mu + alpha*mu^2
}
}
\references{
%% ~put references to the literature/web site here ~
https://secure.genome.ucla.edu/index.php/ExomeCNV_User_Guide
}
\author{
%%  ~~who you are~~
Fah Sathirapongsasuti <fsathira@fas.harvard.edu>
}
\examples{
get.AUC(chi=35, r=1.4, W=500, l=70, rho=1.5, overdisperse="no")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ CNV }

