\name{save.logR}
\alias{save.logR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Save logR in files by chromosome.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Save logR in files by chromosome. This is to prepare input for \code{classify.eCNV}
It requires exome to have chr and each line correspond to logR.
}
\usage{
save.logR(all.logR, exome, name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{all.logR}{
%%     ~~Describe \code{all.logR} here~~
A vector of all log coverage ration as a result of \code{calculate.logR}
}
  \item{exome}{
%%     ~~Describe \code{exome} here~~
A data.frame representing exome. It needs to have \code{chr} field. Can be the same as \code{normal} used in \code{calculate.logR}.
}
  \item{name}{
%%     ~~Describe \code{name} here~~
File name to be saved.
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
chr.list=paste("chr",c("19","20","21"),sep="")

suffix = ".small.coverage"

prefix = "http://genome.ucla.edu/~fah/ExomeCNV/data/normal."
normal = read.all.coverage(prefix, suffix, chr.list, header=TRUE)

prefix = "http://genome.ucla.edu/~fah/ExomeCNV/data/tumor."
tumor = read.all.coverage(prefix, suffix, chr.list, header=TRUE)

demo.logR = calculate.logR(normal, tumor)
save.logR(demo.logR, normal, "demo")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ CNV }

