\name{guesstimate.contamination}
\alias{guesstimate.contamination}
\alias{guesstimate.contarmination}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate admixture/contamination rate in a sample.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Admixture/contarmination here refers to the DNA content with normal copy number that is present in sample with copy number variation.
An example is the non-cancer (normal) tissue found in a tumor biopsy sample.
The approximation is made through deviation of log coverage ratio from zero in a region with evidence for LOH or deletion.
}
\usage{
guesstimate.contarmination(logR, region.idx = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{logR}{
%%     ~~Describe \code{logR} here~~
Log coverage ratio as calculated by \code{calculate.logR}
}
  \item{region.idx}{
%%     ~~Describe \code{region.idx} here~~
A vector of indices or logical values indicating exons with evidence for LOH/deletion.
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
normal = read.table("http://genome.ucla.edu/~fah/ExomeCNV/data/normal.chr21.coverage", 
			header=TRUE, sep='\t')
con = url("http://genome.ucla.edu/~fah/ExomeCNV/demo/logR/demo.logR.chr21.RData")
load(con)
close(con)
admix.rate = guesstimate.contamination( logR=logR, region.idx=(normal$chr == "chr21") )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ CNV }
\keyword{ Sample Admixture }
