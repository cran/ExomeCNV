\name{ExomeCNV-package}
\alias{ExomeCNV-package}
\alias{ExomeCNV}
\docType{package}
\title{
Exome Sequencing-Based CNV and LOH Detection.
}
\description{
ExomeCNV is an R package tailored to detection of CNV (Copy-Number Variants) and LOH (Loss of Heterozygosity) from exome sequencing data. It exploits the unique discrete feature of exon definitions and incredible cross-sample consistency of depth-of-coverage. ExomeCNV is most suitable when paired samples (e.g. tumor-normal pair) are available. Both of the paired samples should be processed and sequenced in a similar manner (e.g. same library prep, sequencer, average depth-of-coverage, etc.).
}
\details{
\tabular{ll}{
Package: \tab ExomeCNV\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-01-27\cr
License: \tab LGPL-2.1\cr
LazyLoad: \tab yes\cr
}
See user guide at https://secure.genome.ucla.edu/index.php/ExomeCNV_User_Guide
}
\author{
Fah Sathirapongsasuti <fsathira@fas.harvard.edu>
}
\references{
%% ~put references to the literature/web site here ~
https://secure.genome.ucla.edu/index.php/ExomeCNV_User_Guide
}
\keyword{ CNV }
\keyword{ LOH }
\examples{
chr.list=paste("chr",c("19","20","21"), sep="")

suffix = ".small.coverage"

prefix = "http://genome.ucla.edu/~fah/ExomeCNV/data/normal."
normal = read.all.coverage(prefix, suffix, chr.list, header=TRUE)

prefix = "http://genome.ucla.edu/~fah/ExomeCNV/data/tumor."
tumor = read.all.coverage(prefix, suffix, chr.list, header=TRUE)

demo.logR = calculate.logR(normal, tumor)

demo.eCNV = c()
for (i in 1:length(chr.list)) {
	idx = (normal$chr == chr.list[i])
	ecnv = classify.eCNV(normal=normal[idx,], tumor=tumor[idx,], 
			logR=demo.logR[idx], min.spec=0.9999, min.sens=0.9999, 
			option="spec", c=0.5, l=70)
	demo.eCNV = rbind(demo.eCNV, ecnv)
}

}
