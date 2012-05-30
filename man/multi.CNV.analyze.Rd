\name{multi.CNV.analyze}
\alias{multi.CNV.analyze}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Use Circular Binary Segmentation to create segments and call CNV on each segment.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Run \code{CNV.analyze} under configurations specified in \code{sdundo} and \code{alpha}. \code{sdundo} and \code{alpha} have to be of the same length. 
A list of other finer CNV interval may be supplied in \code{all.cnv.ls}.  Finally it merges all results to form a final CNV interval list.
}
\usage{
multi.CNV.analyze(normal, tumor, logR = NULL, all.cnv.ls = NULL, 
		  coverage.cutoff = 10, admix = 0.3, c = admix, read.len = 70, 
		  l = read.len, sdundo = c(1, 2), alpha = c(0.05, 0.01), 
		  min.spec = 0.99, min.sens = 0.9, option = "auc")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{normal}{
%%     ~~Describe \code{normal} here~~
a data.frame of depth of coverage of normal (control) sample. See \code{read.all.coverage()} for more information.
}
  \item{tumor}{ 
%%     ~~Describe \code{tumor} here~~
a data.frame of depth of coverage of tumor (case) sample. See \code{read.all.coverage()} for more information.
}
  \item{logR}{
%%     ~~Describe \code{logR} here~~
a vector of log ratio as calculated by \code{calculate.logR}.
}
  \item{all.cnv.ls}{
%%     ~~Describe \code{all.cnv.ls} here~~
a list of other finer CNV interval (usually the outputs of \code{classify.eCNV})
}
  \item{coverage.cutoff}{
%%     ~~Describe \code{coverage.cutoff} here~~
a number to use as a cutoff for minimum average coverage that should be considered. If this minimum coverage is not met, CNV will not be called for that segment.
}
  \item{admix}{
contamination rate (admixture rate), the proportion of the normal cells in the tumor samples.
}
  \item{c}{
(depreciated) same as \code{admix}
}
  \item{read.len}{
sequence read length.
}
  \item{l}{
(depreciated) same as \code{read.len}
}
  \item{sdundo}{
%%     ~~Describe \code{sdundo} here~~
the number of SDs between means to keep a split.
This is a parameter for CBS as used in \code{DNAcopy} package.
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
significance levels for the test to accept change-points.
This is a parameter for CBS as used in \code{DNAcopy} package.
}
  \item{min.spec}{
%%     ~~Describe \code{min.spec} here~~
desired minimum specificity.
}
  \item{min.sens}{
%%     ~~Describe \code{min.sens} here~~
desired minimum sensitivity (power).
}
  \item{option}{
%%     ~~Describe \code{option} here~~
objective quantity to optimize over when minimum sensitivity and specificity are achieved. Possible opetions are \code{sens} for sensitivity, \code{spec} for specificity, \code{auc} for area under curve = (specificity + sensitivity)/2.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This is a wrapper function for \code{classify.eCNV}. It first segments the genome (exome) using CBS with variable fineness levels (as specified by \code{sdundo} and \code{alpha}). Then go through each segment and call CNV. Finally, merge all intervals from finest level (exon) to coarest level, prioritizing finer level to improve sensitivity.
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
chr.list = c("chr19","chr20","chr21")

suffix = ".small.coverage"
prefix = "http://genome.ucla.edu/~fah/ExomeCNV/data/normal."
normal = read.all.coverage(prefix, suffix, chr.list, header=TRUE)
prefix = "http://genome.ucla.edu/~fah/ExomeCNV/data/tumor."
tumor = read.all.coverage(prefix, suffix, chr.list, header=TRUE)

demo.logR = calculate.logR(normal, tumor)

suffix = ".RData"
prefix = paste("http://genome.ucla.edu/~fah/ExomeCNV/demo/",
	"demo.small.eCNV.9999.9999.spec.c.5/demo.small.eCNV.9999.9999.spec.c.5.",
	sep="")
demo.eCNV = read.eCNV(prefix, suffix, chr.list, url=TRUE)

## The following will take a while to run (~3-5 mins)
# demo.cnv = multi.CNV.analyze(normal, tumor, logR=demo.logR, all.cnv.ls=list(demo.eCNV), 
# 			coverage.cutoff=5, min.spec=0.99, min.sens=0.99, option="auc", 
#			c=0.5, sdundo=c(2), alpha=c(0.05))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ CNV }

