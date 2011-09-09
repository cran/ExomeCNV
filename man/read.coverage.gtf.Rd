\name{read.coverage.gtf}
\alias{read.coverage.gtf}
\alias{read.gtf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read coverage file in GTF format as created by Howie Goodell.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Read coverage file in GTF format (as prepared by Howie Goodell) and reformat it to be usable by ExomeCNV.
}
\usage{
read.coverage.gtf(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Exon coverage file in GTF format. See example at http://genome.ucla.edu/~fah/ExomeCNV/data/sampleCoverage.gtf.
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
coverage = read.coverage.gtf("http://genome.ucla.edu/~fah/ExomeCNV/data/sampleCoverage.gtf")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ CNV }

