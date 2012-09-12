\name{.fregexpr}
\alias{.fregexpr}
\title{Pattern matching.}
\usage{
  .fregexpr(pattern, filename)
}
\arguments{
  \item{pattern}{\code{character}, string containing a
  regular expression}

  \item{filename}{\code{character}, name of file}
}
\value{
  \code{double}, position of match
}
\description{
  This function looks for matches to argument
  \code{pattern} in a file \code{filename}.
}
\author{
  Jarek Tuszynski (SAIC)
  \email{jaroslaw.w.tuszynski@saic.com}
}
\seealso{
  \code{\link[base]{regexpr}}
}
\keyword{internal}

