\name{LOH.analyze}
\alias{LOH.analyze}
\alias{get.end.position}
\alias{one.sample.loh.test}
\alias{find.min.of.2lists}
\alias{two.sample.loh.test}
\alias{two.sample.loh.deviation.test}
\alias{phalfnorm}
\alias{two.sample.loh.CMH.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Call LOH on each heterozygous position using specified test statistic.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Call LoH from BAF using specified test. It returns a vector of T/F indicating LOH status. If it runs CMH test, strata is required as well as normal or tumor with each row corresponding to each stratum.
}
\usage{
LOH.analyze(normal = NULL, tumor = NULL, strata = NULL, alpha = 0.05, 
	method = c("deviation.half.norm", "variance.f", "deviation.wilcox", 
	"deviation.t", "CMH", "mantelhaen", "two.sample.fisher", 
	"two.sample.prop", "only.tumor", "only.normal") )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{normal}{
%%     ~~Describe \code{normal} here~~
BAF information for all heterozygous positions in the normal exome. It should be a data.frame with four fields: chr, position, coverage, and baf
"baf" here doesn't refer to frequency but the raw count.
Example can be found at https://secure.genome.ucla.edu/index.php/ExomeCNV_User_Guide#LOH_Calling_Procedure
}
  \item{tumor}{
%%     ~~Describe \code{tumor} here~~
Same as \code{normal}. 
If method is "only.tumor" or "only.normal", only one of tumor or normal is required, respectively.
}
  \item{strata}{
%%     ~~Describe \code{strata} here~~
A list of 2x2xN matrices, each corresponds to a segment. N is the number of heterozygous positions in a segment.
Use \code{make.loh.strata}.
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
Type I error rate used in the exonic LOH test.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
Type of test to use to call LOH. See \code{details}.
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

\references{
%% ~put references to the literature/web site here ~
https://secure.genome.ucla.edu/index.php/ExomeCNV_User_Guide
}
\author{
%%  ~~who you are~~
Fah Sathirapongsasuti <fsathira@fas.harvard.edu>
}

\examples{
normal = read.delim("http://genome.ucla.edu/~fah/ExomeCNV/data/normal.small.baf.txt", 
		header=TRUE)
tumor = read.delim("http://genome.ucla.edu/~fah/ExomeCNV/data/tumor.small.baf.txt", 
		header=TRUE)
eLOH = LOH.analyze(normal, tumor, alpha=0.05, method="two.sample.fisher")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ LOH }

