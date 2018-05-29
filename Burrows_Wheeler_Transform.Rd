\name{Burrows-Wheeler Transformation}
\alias{Burrows-Wheeler Transformation}

\title{
%%  ~~function to do ... ~~
}
\description{
The Burrows-Wheeler transform (BWT) rearranges a character string into runs of similar characters. In particular, the transformation is reversible, without needing to store any additional data. In fact, it is an algorithm used to prepare data for use with data compression techniques such as bzip2. It was invented by Michael Burrows and David Wheeler in 1994 while Burrows was working at DEC Systems Research Center in Palo Alto, California.
}
\usage{
bwt(x): The forward-transform

ibwt(y): The inverse transform
}
\arguments{
  \item{x}{
An input string, e.g. 'Hello World'.
}
  \item{y}{
An input string, e.g. 'odlH!rellWo', which returns 'Hello World' when ibwt('odlH!rellWo') is applied.
  }
}
\details{
}
\value{
Returns a string of the same length when applied (see examples below).
}
\references{
M. Burrows and D. J. Wheeler. A block-sorting lossless data compression algorithm. Systems Research Center, 1994.
}
}
\author{
Wei Hao Khoong
}
\note{

}
\examples{
bwt('Hello World')
> bwt('Hello World')
[1] "odlH!rellWo "

> ibwt('odlH!rellWo')
[1] "oleloWrlo!"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~bwt }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~ibwt }% __ONLY ONE__ keyword per line
