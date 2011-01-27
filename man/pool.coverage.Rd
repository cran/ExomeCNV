\name{pool.coverage}
\alias{pool.coverage}
\alias{find.max.of.2lists}
\alias{pool.coverage.from.files}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pooling a compendium of "normal" exome samples.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Pooling a compendium of "normal" exome samples to form a reference sample to use as a normal control in case no matched normal is available.
In germline CNV discovery one may not have a matched normal sample to compare against. Pooling is proposed an an alternative whereby many "normal" samples can be aveaged and used to serve as a reference.
These functions facilitate the averaging of those samples. All samples are assumed to be in the same prescribed format.
}
\usage{
pool.coverage(all.data)
pool.coverage.from.files(infile.prefix.list, infile.suffix =
                 "exon_parsed.coverage", exome, chr.list = c("chr1",
                 "chr2", "chr3", "chr4", "chr5", "chr6", "chr7",
                 "chr8", "chr9", "chr10", "chr11", "chr12", "chr13",
                 "chr14", "chr15", "chr16", "chr17", "chr18", "chr19",
                 "chr20", "chr21", "chr22", "chrX", "chrY"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{all.data}{
%%     ~~Describe \code{all.data} here~~
A list of all data.frame's for coverages.
}
  \item{infile.prefix.list}{
%%     ~~Describe \code{infile.prefix.list} here~~
A vector of file prefixes. Similar to that used in \code{read.all.coverage}.
}
  \item{infile.suffix}{
%%     ~~Describe \code{infile.suffix} here~~
A suffix string for coverage files. Similar to that used in \code{read.all.coverage}.
}
  \item{exome}{
%%     ~~Describe \code{exome} here~~
A data.frame defining exome (with chr, probe_start, probe_end, and name as columns).
}
  \item{chr.list}{
%%     ~~Describe \code{chr.list} here~~
A vector of chromosomes in the exome. Similar to that used in \code{read.all.coverage}.
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
# note that this makes no biological sense, just an example
normal = read.table("http://genome.ucla.edu/~fah/ExomeCNV/data/normal.chr21.coverage", 
			header=TRUE, sep='\t')
tumor = read.table("http://genome.ucla.edu/~fah/ExomeCNV/data/tumor.chr21.coverage", 
			header=TRUE, sep='\t')
pool = pool.coverage(list(normal, tumor))

exome = read.delim("http://genome.ucla.edu/~fah/ExomeCNV/data/exome.sample.bed", 
			header=FALSE)
names(exome) = c("chr", "probe_start", "probe_end", "name")
suffix = ".coverage"
prefix.list = c("http://genome.ucla.edu/~fah/ExomeCNV/data/tumor.",
		"http://genome.ucla.edu/~fah/ExomeCNV/data/normal.")
pool = pool.coverage.from.files(prefix.list, suffix, exome, 
				chr.list=c("chr19","chr20","chr21"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ CNV }
\keyword{ Pooling }
