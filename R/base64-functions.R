## Copyright 2011-2012 Sebastian Gibb
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

## completely taken from caTools 1.11 R/base64.R
## modification by Sebastian Gibb <mail@sebastiangibb.de>:
## - roxygenize documentation
## - remove base64encode function
## - prepend a dot to function name
## - add compression support

#===========================================================================#
# caTools - R library                                                       #
# Copyright (C) 2005 Jarek Tuszynski                                        #
# Distributed under GNU General Public License version 3                    #
#===========================================================================#

#===============================================================================
# The Base64 encoding is designed to encode arbitrary binary information for 
# transmission by electronic mail. It is defined by MIME (Multipurpose Internet 
# Mail Extensions) specification RFC 1341, RFC 1421, RFC 2045 and others. 
# Triplets of 8-bit octets are encoded as groups of four characters, each 
# representing 6 bits of the source 24 bits. Only a 65-character subset 
# ([A-Z,a-z,0-9,+,/,=]) present in all variants of ASCII and EBCDIC is used, 
# enabling 6 bits to be represented per printable character
#===============================================================================

#' Convert R vectors from the Base64 format.
#' 
#' Convert R vectors of any type from the Base64 format for encrypting any
#' binary data as string using alphanumeric subset of ASCII character set.\cr
#' This is an internal function and should normally not used by the user.
#' 
#' This function was taken from \pkg{caTools} 1.11 \emph{R/base64.R}.
#' 
#' @param z String with Base64 code, using [A-Z,a-z,0-9,+,/,=] subset of
#'  characters
#' @param what Either an object whose mode will give the mode of the vector to
#'  be created, or a character vector of length one describing the mode: one of
#'  \code{numeric}, \code{double}, \code{integer}, \code{int}, \code{logical},
#'  \code{complex}, \code{character}, \code{raw}. Same as variable \code{what} 
#'  in \code{\link[base]{readBin}} functions.
#' @param size integer. The number of bytes per element in the byte stream
#'  stored in \code{r}. The default, \sQuote{\code{NA}}, uses the natural size.
#'  Same as variable \code{size} in \code{\link[base]{readBin}} functions.
#' @param signed logical. Only used for integers of sizes 1 and 2, when it
#'  determines if the quantity stored as raw should be regarded as a signed or
#'  unsigned integer. Same as variable \code{signed} in 
#'  \code{\link[base]{readBin}} functions.
#' @param endian If provided, can be used to swap endian-ness. Using
#'  \dQuote{swap} will force swapping of byte order. Use \dQuote{big} 
#'  (big-endian, aka IEEE, aka \dQuote{network}) or \dQuote{little}
#'  (little-endian, format used on PC/Intel machines) to
#'  indicate type of data encoded in \code{raw} format. Same as variable
#'  \code{endian} in \code{\link[base]{readBin}} functions.
#' @param compressionType character. Type of compression to use for 
#'  decompression of \code{z}. Same as variable \code{type} in 
#'  \code{\link{memDecompress}}.
#' @return Function \code{\link{.base64decode}} returns a vector of type
#'  \code{what}.
#' @rdname base64-decode
#' @author Jarek Tuszynski (SAIC) \email{jaroslaw.w.tuszynski@@saic.com}
#' @seealso \code{\link[caTools]{base64decode}} from \pkg{caTools} package for
#'  original documentation and examples.
#' 
#' \code{\link[base]{readBin}}, \code{\link[base]{writeBin}}
#' @references 
#'  \itemize{
#'    \item Base64 description in \emph{Connected: An
#'          Internet Encyclopedia}
#'          \url{http://www.freesoft.org/CIE/RFC/1521/7.htm}
#'    \item MIME RFC 1341 \url{http://www.faqs.org/rfcs/rfc1341.html}
#'    \item MIME RFC 1421 \url{http://www.faqs.org/rfcs/rfc1421.html}
#'    \item MIME RFC 2045 \url{http://www.faqs.org/rfcs/rfc2045.html}
#'    \item Portions of the code are based on Matlab code by Peter Acklam
#'          \url{http://home.online.no/~pjacklam/matlab/software/util/datautil/}
#'  }
#'
.base64decode = function(z, what, size=NA, signed=TRUE, endian=.Platform$endian,
                         compressionType=c("none", "gzip"))
{  
  library(bitops)                 # needed for bitOr and bitAnd
  if (!is.character(z)) 
    stop("base64decode: Input argument 'z' is suppose to be a string")
  if (length(z)==1) z = strsplit(z, NULL)[[1]] # convert string to array of characters
  if (length(z)%%4!=0) 
   warning("In base64decode: Length of base64 data (z) not a multiple of 4.")
  #-----------------------------------
  # Now perform the following mapping
  #   A-Z  ->  0  - 25
  #   a-z  ->  26 - 51
  #   0-9  ->  52 - 61
  #   +    ->  62
  #   /    ->  63
  #   =    ->  64  - special padding character
  #  otherwise -1
  #-----------------------------------
  alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
  alpha = strsplit(alpha, NULL)[[1]]    # convert string to array of characters
  y     = match(z, alpha, nomatch=-1)-1 # lookup number of each character
  if (any(y == -1)) 
    stop("base64decode: Input string is not in Base64 format")
  if (any(y == 64)) y = y[y != 64]      # remove padding
  neByte = length(y);                   # number of encoded bytes
  nBlock = ceiling(neByte/4);           # number of blocks/groups
  ndByte = 3 * nBlock                   # number of decoded bytes
  
  # add padding if necessary
  if (neByte < 4*nBlock) y[(neByte+1) : (4*nBlock)] = 0;
  dim(y) = c(4, nBlock);                # shape into a matrix
  x = matrix(as.integer(0), 3, nBlock); # for the decoded data
 
  #---------------------------------------------
  # Rearrange every 4 bytes into 3 bytes
  #    y = 00aaaaaa 00bbbbbb 00cccccc 00dddddd
  # to form
  #    x = aaaaaabb bbbbcccc ccdddddd
  # This section is based on Matlab code by Peter Acklam
  # http://home.online.no/~pjacklam/matlab/software/util/datautil/
  #---------------------------------------------
  x[1,] = bitops::bitOr(bitops::bitShiftL(y[1,], 2), bitops::bitShiftR(y[2,], 4))
  x[2,] = bitops::bitOr(bitops::bitShiftL(y[2,], 4), bitops::bitShiftR(y[3,], 2))
  x[3,] = bitops::bitOr(bitops::bitShiftL(y[3,], 6), y[4,])
  x = bitops::bitAnd(x, 255) # trim numbers to lower 8-bits
  
  # remove padding
  if (neByte %% 4 == 2) x = x[1:(ndByte-2)]
  if (neByte %% 4 == 3) x = x[1:(ndByte-1)]
  
  # perform final conversion from 'raw' to type given by 'what'
  r = as.raw(x)

  ## add compression support
  compressionType <- match.arg(compressionType, several.ok=FALSE)
  r <- memDecompress(from=r, type=compressionType)

  TypeList = c("logical", "integer", "double", "complex", "character", "raw", 
               "numeric", "int")
  if (!is.character(what) || length(what) != 1 || !(what %in% TypeList)) 
    what <- typeof(what)
  if (what=="raw") return(r)
  if (is.na(size)) size = switch(match(what, TypeList), 4, 4, 8, 16, 2, 1, 8, 4) 
  n = length(r)
  if (n%%size) stop("raw2bin: number of elements in 'r' is not multiple of 'size'")
  x = readBin(r, what, n = n%/%size, size=size, signed=signed, endian=endian)
  if (what=="character")  x = paste(x, collapse = "") # convert arrays of characters to strings
  return (x)
}

