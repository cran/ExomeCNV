\name{combine.CNV}
\alias{combine.CNV}
\alias{my.cols}
\alias{do.merge.cnv.intervals}
\alias{make.chunk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combine results of DNAcopy and classify.eCNV into one table.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
combine results of DNAcopy and classify.eCNV into one table
}
\usage{
combine.CNV(cnv.ls)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cnv.ls}{
%%     ~~Describe \code{cnv.ls} here~~
list of results from classify.eCNV, ranked by coarseness (finest to coarsest)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
 algorithm:
	start from the finest classification (usually exon-level eCNV)
	for each of the coarser classification
		for each cnv interval
			match with exon in its range
			if the copy number is not classified (NA) or match
				assign/change copy# and log ratio value
	merge adjacent exons with same log ratio and copy number classification
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
## this function is only for internal use
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ CNV }

