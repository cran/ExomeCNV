\name{write.output}
\alias{write.output}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Generate output files from ExomeCNV outputs.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Generate output files from ExomeCNV outputs. The files geneated are:
1. .cnv.txt file with all CNV calls
2. .exon.lrr.txt file containing log coverage ratio for each exon
3. .segment.lrr.txt file containing log coverage ratio for each segment (as defined by CBS)
4. .segment.copynumber.txt file containing copy number calls for each segment
5. .cnv.png file, a plot of the results
}
\usage{
write.output(eCNV, cnv, name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eCNV}{
%%     ~~Describe \code{eCNV} here~~
Result of \code{multi.CNV.analyze}
}
  \item{cnv}{
%%     ~~Describe \code{cnv} here~~
Result of \code{classify.eCNV}
}
  \item{name}{
%%     ~~Describe \code{name} here~~
File name prefix for all the output files
}
}
\references{
%% ~put references to the literature/web site here ~
https://secure.genome.ucla.edu/index.php/ExomeCNV_User_Guide
}
\author{
%%  ~~who you are~~
Fah Sathirapongsasuti
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
chr.list=c("chr19","chr20","chr21")

suffix = ".RData"
prefix = paste("http://genome.ucla.edu/~fah/ExomeCNV/demo/",
	"demo.small.eCNV.9999.9999.spec.c.5/demo.small.eCNV.9999.9999.spec.c.5.",
	sep="")
demo.eCNV = read.eCNV(prefix, suffix, chr.list, url=TRUE)

con = url("http://genome.ucla.edu/~fah/ExomeCNV/demo/demo.cnv.RData")
load(con)
close(con)

write.output(demo.eCNV, demo.cnv, "demo")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ CNV }

