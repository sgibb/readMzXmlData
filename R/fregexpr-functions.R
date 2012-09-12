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

## completely taken from caMassClass 1.9 R/mzXML.R
## modification by Sebastian Gibb <mail@sebastiangibb.de>:
## - prepend a dot to function name

#===========================================================================#
# Written by Jarek Tuszynski. Copyright 2001-2003 SAIC.                     #
# Software developed in conjunction with the National Cancer Institute      #
# Distributed under GNU General Public License version 3                    #
#===========================================================================#

#' Pattern matching.
#'
#' This function looks for matches to argument \code{pattern} in a file
#' \code{filename}.
#'
#' @param pattern \code{character}, string containing a regular expression 
#' @param filename \code{character}, name of file
#'
#' @return \code{double}, position of match
#'
#' @author Jarek Tuszynski (SAIC) \email{jaroslaw.w.tuszynski@@saic.com}
#' @seealso \code{\link[base]{regexpr}}
#
#' @rdname fregexpr 
#' @keywords internal
#'
.fregexpr = function(pattern, filename) {
    ## similar to gregexpr but operating on files not strings
    buf.size <- 1024;
    n <- file.info(filename)$size;
    pos <- NULL;
    fp <- file(filename, "rb");

    for (d in seq(1, n, by=buf.size)) {
        m <- ifelse( n-d > buf.size, buf.size, n-d);
        p <- gregexpr(pattern, readChar(fp, m))[[1]];
        if (p[1]>0) {
            pos <- c(pos, p+d-1);
        }
    }
    close(fp)
    if (is.null(pos)) {
        pos <- -1;
    }
    return (pos)
}
