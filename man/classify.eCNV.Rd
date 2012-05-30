\name{classify.eCNV}
\alias{classify.eCNV}
\alias{classify.logR}
\alias{get.r.cutoff}
\title{
Call CNV on each exon based on log ratio of read depth.
}
\description{
Calculate specificity and sensitivity (power) of detecting CNV based on depth of coverage and log ratio of all exons. Make a call when sufficient specificity and sensitivity are achieved.
}
\usage{
classify.eCNV(normal, tumor, logR = NULL, min.spec = 0.9, min.sens = 0.9, 
	      option = "auc", admix = 0.3, c = admix, read.len = 70,
	      l = read.len, normal.chrs = c("chr1",
	     "chr2","chr3","chr4","chr5","chr6","chr7","chr8",
	     "chr9","chr10","chr11","chr12","chr13","chr14",
	     "chr15","chr16","chr17","chr18","chr19","chr20",
	     "chr21","chr22","chrX","chrY"),
	     test.num.copy = c(1,3))
}
\arguments{
  \item{normal}{
a data.frame of depth of coverage of normal (control) sample. See \code{read.all.coverage()} for more information.
}
  \item{tumor}{
a data.frame of depth of coverage of tumor (case) sample. See \code{read.all.coverage()} for more information.
}
  \item{logR}{
a vector of log ratio as calculated by \code{calculate.logR}.
}
  \item{min.spec}{
desired minimum specificity.
}
  \item{min.sens}{
desired minimum sensitivity (power).
}
  \item{option}{
objective quantity to optimize over when minimum sensitivity and specificity are achieved. Possible opetions are \code{sens} for sensitivity, \code{spec} for specificity, \code{auc} for area under curve = (specificity + sensitivity)/2.
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
  \item{normal.chrs}{
a vector of strings indicating chromosomes that are believed to have normal copy numbers. Default to whole genome.
}
  \item{test.num.copy}{
copy numbers to be tested. 1 for deletion, 3 for duplication, 4 and beyond for amplification. Default to (1,3,4,5).
}
}
\details{
This is the main function to call CNV at exon level. It first computes power based on depth of coverage of the exon. With sufficient power and specificity, a CNV call is made based on the log ratio of depth of coverage while optimizing for specificity, sensitivity or AUC (depending on the option set by user). Power calculation and CNV calling are based on the ratio of normal random variables.
}
\value{
a data.frame with collowing fields:
\item{tumor.average.coverage}{average coverage of tumor exon}
\item{logR}{log ratio of depth of coverage}
\item{ratio}{ratio of of depth of coverage}
\item{copy.number}{CNV call. 1 = deletion, 2 = normal, 3 and more = amplification}
\item{lower.cutoff}{cutoff value used to call the CNV}
\item{upper.cutoff}{same as \code{lower.cutoff}}
\item{spec}{predicted specificity associated with the CNV call.}
\item{sens}{predicted sensitivity (power) associated with the CNV call.}
}
\references{
https://secure.genome.ucla.edu/index.php/ExomeCNV_User_Guide
}
\author{
Fah Sathirapongsasuti <fsathira@fas.harvard.edu>
}
\seealso{
\code{\link{calculate.logR}}
}
\examples{
normal = read.table("http://genome.ucla.edu/~fah/ExomeCNV/data/normal.chr21.small.coverage", 
		header=TRUE, sep='\t')
tumor = read.table("http://genome.ucla.edu/~fah/ExomeCNV/data/tumor.chr21.small.coverage", 
		header=TRUE, sep='\t')
con = url("http://genome.ucla.edu/~fah/ExomeCNV/demo/logR/demo.small.logR.chr21.RData")
load(con)
close(con)

ecnv = classify.eCNV(normal, tumor, logR, min.spec=0.9999, min.sens=0.9999, 
		option="spec", c=0.5, l=70)
}
\keyword{ CNV }

