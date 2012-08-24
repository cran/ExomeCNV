\name{CNV.analyze}
\alias{CNV.analyze}
\alias{get.proper.cnv.positions}
\alias{strip.chr.name}
\title{
A wrapper function of DNAcopy that prepare input and run CBS.
}
\description{
Preprocess the data and run DNAcopy (to do Circular Binary Segmentation). It also offers an option to plot log ratios without performing DNAcopy.
}
\usage{
CNV.analyze(normal, tumor, logR = NULL, coverage.cutoff = 15,
                 normal.chrs = c("chr1", "chr2", "chr3", "chr4", "chr5",
                 "chr6", "chr7", "chr8", "chr9", "chr10", "chr11",
                 "chr12", "chr13", "chr14", "chr15", "chr16", "chr17",
                 "chr18", "chr19", "chr20", "chr21", "chr22", "chrX",
                 "chrY"), normal.chr = normal.chrs, c = 0.5, write.file = FALSE, 
                 file = NULL, doDNAcopy = TRUE, sdundo = 1, smooth = TRUE, 
                 alpha = 0.01, plot.cnv = TRUE)
}
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
  \item{coverage.cutoff}{
%%     ~~Describe \code{coverage.cutoff} here~~
a hard cutoff to exclude exons with low depth of coverage from consideration.
}
  \item{normal.chrs}{
%%     ~~Describe \code{normal.chrs} here~~
a vector of strings indicating chromosomes that are believed to have normal copy numbers. Default to whole genome.
}
  \item{normal.chr}{
%%     ~~Describe \code{normal.chr} here~~
degenerous. same as \code{normal.chrs} above.
}
  \item{c}{
%%     ~~Describe \code{c} here~~
contamination rate (admixture rate), the proportion of the normal cells in the tumor samples.
}
  \item{write.file}{
%%     ~~Describe \code{write.file} here~~
Boolean option to write the result of DNAcopy to file.
}
  \item{file}{
%%     ~~Describe \code{file} here~~
File name of the result of DNAcopy to be written.
}
  \item{doDNAcopy}{
%%     ~~Describe \code{doDNAcopy} here~~
Boolean option to do DNAcopy. If FALSE, it will plot log ratios without doing Circular Binary Segmentation.
}
  \item{sdundo}{
%%     ~~Describe \code{sdundo} here~~
Option to pass on to DNAcopy. See \code{segment} in Package DNAcopy for more details.
}
  \item{smooth}{
%%     ~~Describe \code{smooth} here~~
Option to pass on to DNAcopy. See \code{segment} in Package DNAcopy for more details.}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
Option to pass on to DNAcopy. See \code{segment} in Package DNAcopy for more details.}
  \item{plot.cnv}{
%%     ~~Describe \code{plot.cnv} here~~
Boolean option to plot the results.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The function first calculates log ratios using \code{calculate.logR} then call \code{segment} (from Package DNAcopy) using exon midpoints as the probe positions.

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{cnv}{ A data.frame with the following fields:
	\code{chr}	chromosome on which the exon is located;
	\code{probe}	probe name;
	\code{probe_start}	starting position of the exon;
	\code{probe_end}	ending position of the exon;
	\code{size}	size of the exon;
	\code{targeted.base}	the number of bases targeted by exome sequencing;
	\code{sequenced.base}	the number of bases sequenced (mapped);
	\code{coverage}	total number of bases sequenced;
	\code{average.coverage}	average depth of coverage of the exon;
	\code{base.with..10.coverage}	the number of bases in the exon with >= 10 fold coverage
}
\item{cna}{output from \code{segment}, can be used to plot. See \code{segment} in Package DNAcopy for more details.}
\item{logR}{a vector of log ratios returned from \code{calculate.logR}}
}
\references{
%% ~put references to the literature/web site here ~
https://secure.genome.ucla.edu/index.php/ExomeCNV_User_Guide
}
\author{
%%  ~~who you are~~
Fah Sathirapongsasuti <fsathira@fas.harvard.edu>
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{calculate.logR}}
}
\examples{
data(chr.hash)
normal = read.table("http://genome.ucla.edu/~fah/ExomeCNV/data/normal.chr19.small.coverage", 
		header=TRUE, sep='\t')
tumor = read.table("http://genome.ucla.edu/~fah/ExomeCNV/data/tumor.chr19.small.coverage", 
		header=TRUE, sep='\t')
con = url("http://genome.ucla.edu/~fah/ExomeCNV/demo/logR/demo.small.logR.chr19.RData")
load(con)
close(con)
cnv = CNV.analyze(normal, tumor, logR = logR, coverage.cutoff = 15, 
		normal.chrs = row.names(chr.hash), c = 0.5, write.file = FALSE, 
		file = NULL, doDNAcopy = TRUE, sdundo = 1, smooth = TRUE, 
		alpha = 0.01, plot.cnv = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ CNV }

