\name{multi.LOH.analyze}
\alias{multi.LOH.analyze}
\alias{make.loh.chunk}
\alias{do.merge.loh.intervals}
\alias{combine.LOH}
\alias{do.DNAcopy}
\alias{make.loh.strata}
\alias{combine.baf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Use Circular Binary Segmentation to create segments and call LOH on each segment based on BAF using specified method.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This is analogous to \code{multi.CNV.analysis} for LOH. User can control fineness of segmentation by adjusting \code{sdundo} and \code{alpha}.
}
\usage{
multi.LOH.analyze(normal = NULL, tumor = NULL, all.loh.ls = NULL, 
		  min.spec = 0.95, test.alpha = NULL, 
		  method = c("deviation.half.norm", "variance.f", 
		  "deviation.wilcox", "deviation.t", "CHM", "mantelhaen", 
		  "two.sample.fisher", "two.sample.prop", "only.tumor", 
		  "only.normal"), sdundo = c(1, 2), alpha = c(0.05, 0.01))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{normal}{
%%     ~~Describe \code{normal} here~~
BAF information for all heterozygous positions in the normal exome. It should be a data.frame with four fields: chr, position, coverage, and baf.
"baf" here doesn't refer to frequency but the raw count.
Example can be found at 
https://secure.genome.ucla.edu/index.php/ExomeCNV_User_Guide#LOH_Calling_Procedure
}
  \item{tumor}{
%%     ~~Describe \code{tumor} here~~
Same as \code{normal}. 
If method is "only.tumor" or "only.normal", only one of tumor or normal is required, respectively.
}
  \item{all.loh.ls}{
%%     ~~Describe \code{all.loh.ls} here~~
A list of LOH calls to be merged. Usually a result of \code{LOH.analyze}.
The order the LOH calls determines priority of the calls.
}
  \item{min.spec}{
%%     ~~Describe \code{min.spec} here~~
Minimum specificity acceptable. Always equal to 1-alpha, where alpha is Type I error rate.
}
  \item{test.alpha}{
%%     ~~Describe \code{test.alpha} here~~
Type I error rate for the statistical test (not to be confused with \code{alpha} below). Only one of \code{alpha} and \code{min.spec} needs to be specified.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
Type of test to use to call LOH. See \code{details}.
}
  \item{sdundo}{
%%     ~~Describe \code{sdundo} here~~
the number of SDs between means to keep a split.
This is a parameter for CBS as used in \code{DNAcopy} package.
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
significance levels for the test to accept change-points.
This is a parameter for CBS as used in \code{DNAcopy} package.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Statistical tests that can be used in calling LOH are based on three test statistics: 
# BAF as count statistic
# Variance of BAF, reflecting the amount of deviation of BAF away from its central value (~0.5)
# Absolute deviation of BAF from the null value of 0.5
# Difference between BAF's in case and control samples

Each test statistic allows for different tests and is based on different assumptions. 

Options "only.tumor" and "only.normal" use only one sample (case or control) to perform binomial test against null p=0.5. We can model LOH as a binomial event, asking among N reads mapped to the position, how likely is it to observe a certain number of B-allele (BAF).

Options "two.sample.fisher" and "two.sample.prop" are similar to the binomial test for one sample above but instead of testing the observed proportion against the null value of 0.5, they compare the observed proportion between case and control. This can be modeled by binomial distribution (two.sample.prop) or hypergeometric distribution (Fisher's exact test; two.sample.fisher), hence the two possible tests.

Option "variance.f" performs F-test to compare variances of case and control BAF's

Options "deviation.wilcox" and "deviation.t" perform t-test and Wilcoxon Rank Sum (Mann-Whitney) Test, respectively.  This is to compare the mean value of the absolute deviation of BAF from 0.5 (i.e. |BAF - 0.5|).

Option "deviation.half.norm" is based on the observation that the distribution of BAF difference between case and control are normally distributed around 0. Thus the absolute value follows folded-normal distribution. Under LOH, the absolute difference will have a higher mean value, and we can measure and test the increase in the difference using half-normal distribution.

Option "CMH" or "mantelhaen" uses Cochran-Mantel-Haenszel Chi-sq test for common odds ratio equal to 1.
It requires that the number of stata N >= 2. In case N = 1, it is equivalent to Pearson's Chi-sq (prop.test).
This is useful when trying to call LOH for segments, which contain multiple heterozygous positions, each with its own contigency table.
The only problem with this test is that it requires phasing information, which does not always exist. Thus it is not recommended for use.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
An object of class \code{eCNV}. See \code{link{classify.eCNV}} for more information.
}
\references{
%% ~put references to the literature/web site here ~
https://secure.genome.ucla.edu/index.php/ExomeCNV_User_Guide
}
\author{
%%  ~~who you are~~
Fah Sathirapongsasuti <fsathira@fas.harvard.edu>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{multi.CNV.analyze}}
}
\examples{
normal = read.delim("http://genome.ucla.edu/~fah/ExomeCNV/data/normal.small.baf.txt", 
			header=TRUE)
tumor = read.delim("http://genome.ucla.edu/~fah/ExomeCNV/data/tumor.small.baf.txt", 
			header=TRUE)
eLOH = LOH.analyze(normal, tumor, alpha=0.05, method="two.sample.fisher")

### WARNING! These examples take a long time to run (~15-20 mins).
# the.loh = multi.LOH.analyze(normal, tumor, all.loh.ls=list(eLOH), test.alpha=0.001,
#			 	method="variance.f", sdundo=c(0,0), alpha=c(0.5,0.1))
# the.loh = multi.LOH.analyze(normal, tumor, all.loh.ls=list(eLOH), min.spec=0.99, 
#				method="deviation.wilcox", sdundo=c(0), alpha=c(0.1))
# the.loh = multi.LOH.analyze(normal, tumor, all.loh.ls=list(eLOH), min.spec=0.999999, 
#				method="CMH", sdundo=c(2), alpha=c(0.1))
# the.loh = multi.LOH.analyze(normal, tumor, all.loh.ls=list(eLOH), min.spec=0.9999, 
#				method="two.sample.fisher", sdundo=c(0,0), alpha=c(0.1,0.05))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ LOH }

