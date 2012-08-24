\name{read.coverage.gatk}
\alias{read.coverage.gatk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read coverage file produced by The Genome Analysis Toolkit (GATK).
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Read coverage file produced by The Genome Analysis Toolkit (GATK; http://www.broadinstitute.org/gsa/wiki/index.php/The_Genome_Analysis_Toolkit) and reformat it to be usable by ExomeCNV.
For exact command to produce the coverage file, see https://secure.genome.ucla.edu/index.php?title=ExomeCNV_User_Guide#GATK_DepthOfCoverage.
The only three important columns are in the GATK-generated file are: Target, total_coverage, and average_coverage.
Note that it is okay that the last column of the resulting data.frame is "NA".
}
\usage{
read.coverage.gatk(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Exon coverage file as produced by GATK. See example at http://genome.ucla.edu/~fah/ExomeCNV/data/sampleCoverage.gatk.txt.
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
coverage = read.coverage.gatk("http://genome.ucla.edu/~fah/ExomeCNV/data/sampleCoverage.gatk.txt")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ CNV }

