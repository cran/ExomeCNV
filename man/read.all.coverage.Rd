\name{read.all.coverage}
\alias{read.all.coverage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read all coverage files by chromosome.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Go through each chromosome and read coverage files (as prepared by the script available at 
http://genome.ucla.edu/~fah/ExomeCNV/ExomeCNV/script).
}
\usage{
read.all.coverage(prefix, suffix, chr.list = c("chr1", "chr2", "chr3",
                 "chr4", "chr5", "chr6", "chr7", "chr8", "chr9",
                 "chr10", "chr11", "chr12", "chr13", "chr14", "chr15",
                 "chr16", "chr17", "chr18", "chr19", "chr20", "chr21",
                 "chr22", "chrX", "chrY"), header = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prefix}{
%%     ~~Describe \code{prefix} here~~
Prefix of a coverage file name, particularly the part right before "chr#".
For exampe, if the file names are: normal.chr1.coverage, normal.chr2.coverage, ..., the prefix is "normal." and the suffix is ".coverage".
This assumes that the coverage files are saved separately by chromosome,
and the file names differ at just the chromosome number. See example at: http://genome.ucla.edu/~fah/ExomeCNV/data.
}
  \item{suffix}{
%%     ~~Describe \code{suffix} here~~
Suffix of the file name, particularly the part following "chr#". See \code{prefix} for example. 
}
  \item{chr.list}{
%%     ~~Describe \code{chr.list} here~~
A vector of chromosome names in the exome. Should be of format "chr#" where # is the chromosome number. This is assuming that the coverage files are broken up by chromosomes, one file per chromosome. If all chromosomes are contained in one file, set \code{chr.list=c("")} and modify prefix/suffix so that they concatenate into the right filename.
}
  \item{header}{
%%     ~~Describe \code{header} here~~
Logical, inidcating if the coverage file has header or not.
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ CNV }

