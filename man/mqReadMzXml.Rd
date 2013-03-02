\name{mqReadMzXml}
\alias{mqReadMzXml}
\alias{mqReadMzXml-deprecated}
\title{Reads mass spectrometry data into MALDIquant.}
\usage{
  mqReadMzXml(path, ...)
}
\arguments{
  \item{path}{\code{character}, path to \emph{directory} or
  a single \emph{mzXML} file.}

  \item{\dots}{arguments to be passed to
  \code{\link[readMzXmlData]{readMzXmlDir}} or
  \code{\link[readMzXmlData]{readMzXmlFile}}.}
}
\value{
  Returns a list of
  \code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\description{
  This function is deprecated and will be removed in the
  next release. Use
  \code{\link[MALDIquantForeign]{importMzXml}} instead. \cr
  Reads all mass spectrometry data in mzXML format in a
  specified path into
  \code{\link[MALDIquant]{MALDIquant-package}}
  \code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\details{
  See \code{\link[readMzXmlData]{readMzXmlDir}} or
  \code{\link[readMzXmlData]{readMzXmlFile}} for details.
}
\author{
  Sebastian Gibb \email{mail@sebastiangibb.de}
}
\references{
  See website:
  \url{http://strimmerlab.org/software/maldiquant/}
}
\seealso{
  \code{\link[readMzXmlData]{readMzXmlDir}},
  \code{\link[readMzXmlData]{readMzXmlFile}},
  \code{\link[MALDIquant]{MALDIquant-package}},
  \code{\link[MALDIquant]{MassSpectrum-class}}
}
\keyword{IO}

