\name{.base64decode}
\alias{.base64decode}
\title{Convert R vectors from the Base64 format.}
\usage{
  .base64decode(z, what, size = NA, signed = TRUE,
    endian = .Platform$endian,
    compressionType = c("none", "gzip"))
}
\arguments{
  \item{z}{String with Base64 code, using
  [A-Z,a-z,0-9,+,/,=] subset of characters}

  \item{what}{Either an object whose mode will give the
  mode of the vector to be created, or a character vector
  of length one describing the mode: one of \code{numeric},
  \code{double}, \code{integer}, \code{int},
  \code{logical}, \code{complex}, \code{character},
  \code{raw}. Same as variable \code{what} in
  \code{\link[base]{readBin}} functions.}

  \item{size}{integer. The number of bytes per element in
  the byte stream stored in \code{r}. The default,
  \sQuote{\code{NA}}, uses the natural size.  Same as
  variable \code{size} in \code{\link[base]{readBin}}
  functions.}

  \item{signed}{logical. Only used for integers of sizes 1
  and 2, when it determines if the quantity stored as raw
  should be regarded as a signed or unsigned integer. Same
  as variable \code{signed} in \code{\link[base]{readBin}}
  functions.}

  \item{endian}{If provided, can be used to swap
  endian-ness. Using \dQuote{swap} will force swapping of
  byte order. Use \dQuote{big} (big-endian, aka IEEE, aka
  \dQuote{network}) or \dQuote{little} (little-endian,
  format used on PC/Intel machines) to indicate type of
  data encoded in \code{raw} format. Same as variable
  \code{endian} in \code{\link[base]{readBin}} functions.}

  \item{compressionType}{character. Type of compression to
  use for decompression of \code{z}. Same as variable
  \code{type} in \code{\link{memDecompress}}.}
}
\value{
  Function \code{\link{.base64decode}} returns a vector of
  type \code{what}.
}
\description{
  Convert R vectors of any type from the Base64 format for
  encrypting any binary data as string using alphanumeric
  subset of ASCII character set.\cr This is an internal
  function and should normally not used by the user.
}
\details{
  This function was taken from \pkg{caTools} 1.11
  \emph{R/base64.R}.
}
\author{
  Jarek Tuszynski (SAIC)
  \email{jaroslaw.w.tuszynski@saic.com}
}
\references{
  \itemize{ \item Base64 description in \emph{Connected: An
  Internet Encyclopedia}
  \url{http://www.freesoft.org/CIE/RFC/1521/7.htm} \item
  MIME RFC 1341 \url{http://www.faqs.org/rfcs/rfc1341.html}
  \item MIME RFC 1421
  \url{http://www.faqs.org/rfcs/rfc1421.html} \item MIME
  RFC 2045 \url{http://www.faqs.org/rfcs/rfc2045.html}
  \item Portions of the code are based on Matlab code by
  Peter Acklam
  \url{http://home.online.no/~pjacklam/matlab/software/util/datautil/}
  }
}
\seealso{
  \code{\link[caTools]{base64decode}} from \pkg{caTools}
  package for original documentation and examples.

  \code{\link[base]{readBin}}, \code{\link[base]{writeBin}}
}

