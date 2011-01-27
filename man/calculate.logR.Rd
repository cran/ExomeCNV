\name{calculate.logR}
\alias{calculate.logR}
\alias{normalize.logR}
\alias{chr.median}
\title{
Calculate log ratio of depth of coverage between paired samples.
}
\description{
Calculate log ratio of depth of coverage between paired samples (e.g. tumor/normal). This is to be used as input for classify.logR().
The ratios are normalize by the total number of reads and adjusted so that median log ratio of exons on "normal" chromosomes is zero.
Normal chromosome is defined by input \code{normal.chrs}.
}
\usage{
calculate.logR(normal, tumor, normal.chrs = c("chr1", "chr2", "chr3",
                 "chr4", "chr5", "chr6", "chr7", "chr8", "chr9",
                 "chr10", "chr11", "chr12", "chr13", "chr14", "chr15",
                 "chr16", "chr17", "chr18", "chr19", "chr20", "chr21",
                 "chr22", "chrX", "chrY"))
}
\arguments{
  \item{normal}{
a data.frame of depth of coverage of normal (control) sample. See \code{read.all.coverage()} for more information.
}
  \item{tumor}{
a data.frame of depth of coverage of tumor (case) sample. See \code{read.all.coverage()} for more information.
}
  \item{normal.chrs}{
a vector of strings indicating chromosomes that are believed to have normal copy numbers. Default to whole genome.
}
}
\value{
a vector of log ratios.
}
\references{
https://secure.genome.ucla.edu/index.php/ExomeCNV_User_Guide
}
\author{
Fah Sathirapongsasuti <fsathira@fas.harvard.edu>
}
\seealso{
\code{\link{read.all.coverage}}
}
\examples{
chr.list=c("chr19","chr20","chr21")
suffix = ".small.coverage"
prefix = "http://genome.ucla.edu/~fah/ExomeCNV/data/normal."
normal = read.all.coverage(prefix, suffix, chr.list, header=TRUE)
prefix = "http://genome.ucla.edu/~fah/ExomeCNV/data/tumor."
tumor = read.all.coverage(prefix, suffix, chr.list, header=TRUE)

demo.logR = calculate.logR(normal, tumor)

}
\keyword{ CNV }

