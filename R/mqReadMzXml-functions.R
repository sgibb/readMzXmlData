## Copyright 2010-2012 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This file is part of readMzXmlData for R and related languages.
##
## readMzXmlData is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## readMzXmlData is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with readMzXmlData. If not, see <http://www.gnu.org/licenses/>

#' Reads mass spectrometry data into MALDIquant.
#' 
#' This function is deprecated and will be removed in the next release.
#' Use \code{\link[MALDIquantForeign]{importMzXml}} instead. \cr
#' Reads all mass spectrometry data in mzXML format in a specified path into
#' \code{\link[MALDIquant]{MALDIquant-package}}
#' \code{\link[MALDIquant]{MassSpectrum-class}} objects.
#' 
#' See \code{\link[readMzXmlData]{readMzXmlDir}} or
#' \code{\link[readMzXmlData]{readMzXmlFile}} for details.
#' 
#' @param path \code{character}, path to \emph{directory} or a single
#'  \emph{mzXML} file.
#' @param \dots arguments to be passed to
#' \code{\link[readMzXmlData]{readMzXmlDir}} or
#' \code{\link[readMzXmlData]{readMzXmlFile}}.
#' @return Returns a list of \code{\link[MALDIquant]{MassSpectrum-class}}
#'  objects.
#' @author Sebastian Gibb \email{mail@@sebastiangibb.de}
#' @seealso \code{\link[readMzXmlData]{readMzXmlDir}},
#' \code{\link[readMzXmlData]{readMzXmlFile}},
#' \code{\link[MALDIquant]{MALDIquant-package}},
#' \code{\link[MALDIquant]{MassSpectrum-class}}
#' @references See website: \url{http://strimmerlab.org/software/maldiquant/}
#' @keywords IO
#' @aliases mqReadMzXml-deprecated
#' @rdname mqReadMzXml
#' @export
#' 
mqReadMzXml <- function(path, ...) {
  .Deprecated("MALDIquantForeign::importMzXml")

  if (!file.exists(path)) {
    stop("Path ", sQuote(path), " doesn't exists!")
  }

  if (!require("MALDIquant")) {
    stop("Could not load package ", sQuote("MALDIquant"), ".")
  }

  if (!file.info(path)$isdir) {
    s <- readMzXmlFile(mzXmlFile=path, ...)

    ## make list structure equal for single spectrum mzXML files
    if (!is.null(s$spectrum$mass)) {
      l <- list()
      l[[1]] <- s
      s <- l
    }
  } else {
    s <- readMzXmlDir(mzXmlDir=path, ...)
  }
  s <- lapply(s, function(x) {
    return(createMassSpectrum(mass=x$spectrum$mass,
                              intensity=x$spectrum$intensity,
                              metaData=x$metaData))
  })
  return(s)
}

