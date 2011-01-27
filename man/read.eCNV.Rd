\name{read.eCNV}
\alias{read.eCNV}
\title{
Read all .RData files produced by classify.eCNV().
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This is a very simple function that facilitate reading .RData files produced by \code{classify.eCNV}.
It makes an assumption that the .RData file contains a variable named \code{ecnv}, which is a very strong assumption. 
So if you wish to use this function, you need to name the output from \code{classify.eCNV} as \code{ecnv}.
The best way to go about this is to follow the example in http://genome.ucla.edu/~fah/ExomeCNV/demo/demo.R and 
http://genome.ucla.edu/~fah/ExomeCNV/demo/do.eCNV.R.
}
\usage{
read.eCNV(eCNV.prefix, eCNV.suffix, chr.list=c("chr1", "chr2", "chr3",
                 "chr4", "chr5", "chr6", "chr7", "chr8", "chr9",
                 "chr10", "chr11", "chr12", "chr13", "chr14", "chr15",
                 "chr16", "chr17", "chr18", "chr19", "chr20", "chr21",
                 "chr22", "chrX", "chrY"), url=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eCNV.prefix}{
  The part of the .RData file name that precedes chromosome name.  For example, if your file is named "demo.chr19.RData", the prefix is "demo." and suffix is ".RData".
}
  \item{eCNV.suffix}{
  The part of the .RData file name that follows chromosome name.  For example, if your file is named "demo.chr19.RData", the prefix is "demo." and suffix is ".RData".
}
  \item{chr.list}{
  A vector of chromosome names as appeared in the .RData file name.  For example, if your files are "demo.chr19.RData", "demo.chr20.RData", and "demo.chr21.RData", \code{chr.list} should be \code{c("chr19","chr20","chr21")}.
}
  \item{url}{
  A boolean indicating if the file source is a URL.
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ CNV }

