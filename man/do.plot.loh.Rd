\name{do.plot.loh}
\alias{do.plot.loh}
\alias{do.plot.one.loh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot output of LOH calls.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Plot results of \code{LOH.classify()} or \code{multi.LOH.analyze()} over the background of BAF or deviation of BAF.
}
\usage{
do.plot.loh(the.loh, normal, tumor, method, lim.quantile = 0.99, 
		color = "red", plot.style = c("dev", "baf"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{the.loh}{
%%     ~~Describe \code{the.loh} here~~
LOH calls as a result of \code{multi.LOH.analyze()}
}
  \item{normal}{
%%     ~~Describe \code{normal} here~~
BAF information for all heterozygous positions in the normal exome. Same as the input for \code{LOH.classify}.
}
  \item{tumor}{
%%     ~~Describe \code{tumor} here~~
BAF information for all heterozygous positions in the tumor exome. Same as the input for \code{LOH.classify}.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
Method used to call the LOH in \code{LOH.classify} and \code{multi.LOH.analyze()}.
}
  \item{lim.quantile}{
%%     ~~Describe \code{lim.quantile} here~~
Percentage of all data to display. If set to 1, display every data point (which can be highly skewed by the outliers).
}
  \item{color}{
%%     ~~Describe \code{color} here~~
Color string to signify LOH region.
}
  \item{plot.style}{
%%     ~~Describe \code{plot.style} here~~
The style of background BAF information. If plot.style is "dev", uses |baf.tumor - baf.normal|; if "baf", just plot baf.normal (gray) and baf.tumor (blue).
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{do.plot.eCNV}}
}
\examples{
normal = read.delim("http://genome.ucla.edu/~fah/ExomeCNV/data/normal.small.baf.txt", 
		header=TRUE)
tumor = read.delim("http://genome.ucla.edu/~fah/ExomeCNV/data/tumor.small.baf.txt", 
		header=TRUE)
eLOH = LOH.analyze(normal, tumor, alpha=0.05, method="two.sample.fisher")
con = url("http://genome.ucla.edu/~fah/ExomeCNV/demo/demo.the.loh.RData")
load(con)
close(con)
do.plot.loh(the.loh, normal, tumor, "two.sample.fisher", plot.style="baf")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ LOH }
