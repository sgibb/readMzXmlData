\name{mqReadMzXml}
\alias{mqReadMzXml}
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
  Reads all mass spectrometry data in mzXML format in a
  specified path into
  \code{\link[MALDIquant]{MALDIquant-package}}
  \code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\details{
  See \code{\link[readMzXmlData]{readMzXmlDir}} or
  \code{\link[readMzXmlData]{readMzXmlFile}} for details.
}
\examples{
## load library
library("readMzXmlData")

## get examples directory
exampleDirectory <- system.file("Examples", package="readMzXmlData")

## read example spectra
spec <- mqReadMzXml(exampleDirectory)

## plot spectra
par(mfrow=c(2, 1))
lapply(spec, plot)
par(mfrow=c(1, 1))
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

