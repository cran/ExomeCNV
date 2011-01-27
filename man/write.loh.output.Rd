\name{write.loh.output}
\alias{write.loh.output}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate an output file for LOH calls.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Generate .loh.txt file for the LOH call. This is a very simple function; it's essentially a wrapper for \code{write.table}.
}
\usage{
write.loh.output(loh, name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{loh}{
%%     ~~Describe \code{loh} here~~
A data.frame
}
  \item{name}{
%%     ~~Describe \code{name} here~~
Prefix for the file name (to follow by .loh.txt)
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
con = url("http://genome.ucla.edu/~fah/ExomeCNV/demo/demo.the.loh.RData")
load(con)
close(con)
write.loh.output(the.loh, "demo.eloh")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ LOH }

