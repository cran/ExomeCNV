\name{expand.loh}
\alias{expand.loh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Propagate LOH calls to all heterozygous positions within each LOH segment.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
LOH calls are done on (large) genomic intervals; this function help reassign those LOH calls to individual positions within in the interval, in case user wants LOH calls on each position (for example, when counting the number of heterozygous positions with LOH).
}
\usage{
expand.loh(the.loh, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{the.loh}{
%%     ~~Describe \code{the.loh} here~~
Result of \code{multi.LOH.analyze}
}
  \item{data}{
%%     ~~Describe \code{data} here~~
A data.frame of BAF data at all heterozygous positions. This can be the same as the input for \code{LOH.analyze}
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
normal = read.delim("http://genome.ucla.edu/~fah/ExomeCNV/data/normal.baf.txt", 
			header=TRUE)
con = url("http://genome.ucla.edu/~fah/ExomeCNV/demo/demo.the.loh.RData")
load(con)
close(con)
expanded.loh = expand.loh(the.loh, normal)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
