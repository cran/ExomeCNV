\name{do.plot.eCNV}
\alias{do.plot.eCNV}
\alias{do.plot.one.eCNV}
\alias{cnv.points}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot CNV calls over log ratio.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Plot results of \code{classify.eCNV()} or \code{multi.CNV.analyze()}.
}
\usage{
do.plot.eCNV(all.ecnv, pch = "*", lim.quantile = 0.99, style = "idx", 
             bg.cnv = NULL, line.plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{all.ecnv}{
%%     ~~Describe \code{all.ecnv} here~~
Result of \code{classify.eCNV()} or \code{multi.CNV.analyze()}
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
Character to be used for plotting one data point.
}
  \item{lim.quantile}{
%%     ~~Describe \code{lim.quantile} here~~
Percentage of all data to display. If set to 1, display every data point (which can be highly skewed by the outliers).
}
  \item{style}{
%%     ~~Describe \code{style} here~~
What to use on the x-axis. The two options are: \code{idx} for exon idices and \code{bp} for actual base-pair coordinate of the exons.
}
  \item{bg.cnv}{
%%     ~~Describe \code{bg.cnv} here~~
Raw data to plot as background for interval plot (\code{line.plot=TRUE}). This should be in data.frame that has "chr", "probe_end", and "logR".
}
  \item{line.plot}{
%%     ~~Describe \code{line.plot} here~~
Boolean option to plot CNV as line.
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
chr.list = paste("chr",c("19","20","21"), sep="")
suffix = ".RData"
prefix = paste("http://genome.ucla.edu/~fah/ExomeCNV/demo/",
	"demo.eCNV.9999.9999.spec.c.5/demo.eCNV.9999.9999.spec.c.5.",
	sep="")
demo.eCNV = read.eCNV(prefix, suffix, chr.list, url=TRUE)
do.plot.eCNV(demo.eCNV, lim.quantile=0.99, style="idx", line.plot=FALSE)

chr.list=paste("chr",c("19","20","21"), sep="")
suffix = ".small.coverage"
prefix = "http://genome.ucla.edu/~fah/ExomeCNV/data/normal."
normal = read.all.coverage(prefix, suffix, chr.list, header=TRUE)
prefix = "http://genome.ucla.edu/~fah/ExomeCNV/data/tumor."
tumor = read.all.coverage(prefix, suffix, chr.list, header=TRUE)
demo.logR = calculate.logR(normal, tumor)

## The following will take a while to run (~3-5 mins)
demo.cnv = multi.CNV.analyze(normal, tumor, logR=demo.logR, all.cnv.ls=NULL,
			coverage.cutoff=5, min.spec=0.99, min.sens=0.99, option="auc", 
			c=0.5, sdundo=c(2), alpha=c(0.01))
do.plot.eCNV(demo.cnv, lim.quantile=0.99, style="bp", bg.cnv=demo.eCNV, line.plot=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ CNV }

