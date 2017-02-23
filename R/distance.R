# Neighbr
#
# Copyright (c) 2017 Zementis, Inc. 
#
# This file is part of the Neighbr package for R.
#
# The Neighbr package is free software: you can redistribute it and/or 
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 2 of 
# the License, or (at your option) any later version.
#
# The Neighbr package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. Please see the
# GNU General Public License for details (http://www.gnu.org/licenses/).
######################################################################################


#' Calculate the distance between two vectors.
#'
#' @param x,y Numeric vectors.
#' @param measure Distance measure ("euclidean" or "squared_euclidean").
#' @return The distance between \code{x} and \code{y}.
#' @details Distance measures in this package are based on those defined
#' in the \href{http://dmg.org/pmml/v4-3/ClusteringModel.html#xsdElement_ComparisonMeasure}{PMML specification}.
#' Distances are calculated using the following equations:\cr
#'
#' Euclidean: \eqn{(\sum((x_i - y_i)^2))^0.5}\cr\cr
#' Squared euclidean: \eqn{\sum((x_i - y_i)^2)}\cr\cr
#'
#' The input vectors must be of the same length.
#' 
#'
#' @examples
#' distance(c(-0.5,1),c(0.4,1.6),"euclidean")
#' distance(c(-0.5,1),c(0.4,1.6),"squared_euclidean")
#'
#' @seealso \code{\link{similarity}},
#' \href{http://dmg.org/pmml/v4-3/ClusteringModel.html#xsdElement_ComparisonMeasure}{PMML comparison measures}
#'
#' @export
distance <- function(x,y,measure) {
  .check_distance_inputs(x,y)
  if (measure=="euclidean") {
    return(.euclidean(x,y))
  } else if (measure=="squared_euclidean") {
    return(.squared_euclidean(x,y))
  } else {
    stop("not an allowed distance measure")
  }
}


.euclidean <- function(x,y) {
  return((sum((x - y) ^ 2))^(0.5))
}

.squared_euclidean <- function(x,y) {
  return(sum((x - y) ^ 2))
}







