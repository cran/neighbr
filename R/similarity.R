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


#' Calculate the similarity between two vectors of logicals.
#'
#' @param x,y Logical or numeric vectors.
#' @param measure Similarity measure ("simple_matching", "jaccard", or "tanimoto")
#' @return The similarity between \code{x} and \code{y}.
#' @details Input vectors must consist of logical or numeric elements TRUE,FALSE or 0,1 
#' (not factors). Similarity measures in this package are based on those defined in 
#' the \href{http://dmg.org/pmml/v4-2-1/ClusteringModel.html#xsdElement_ComparisonMeasure}{PMML specification}.
#' Similarity ranges from 0 (no similarity) to 1 (identical).
#'
#' For logical vectors \code{x} and \code{y}, we define the following:\cr
#'
#' \code{a11} = number of times where x_i=1 and y_i=1\cr
#' \code{a10} = number of times where x_i=1 and y_i=0\cr
#' \code{a01} = number of times where x_i=0 and y_i=1\cr
#' \code{a00} = number of times where x_i=0 and y_i=0\cr
#'
#' Similarities are calculated using the following formulas:\cr
#'
#' Simple matching: \eqn{(a11 + a00) / (a11 + a10 + a01 + a00)}\cr\cr
#' Jaccard: \eqn{(a11) / (a11 + a10 + a01)}\cr\cr
#' Tanimoto: \eqn{ (a11 + a00) / (a11 + 2 * (a10 + a01) + a00)}\cr\cr
#'
#' @examples
#' similarity(c(0,1,1),c(0,0,1),"simple_matching")
#' similarity(c(0,1,1),c(0,0,1),"jaccard")
#' similarity(as.logical(c(0,1,1)),as.logical(c(0,0,1)),"tanimoto")
#'
#' @seealso \code{\link{distance}},
#' \href{http://dmg.org/pmml/v4-3/ClusteringModel.html#xsdElement_ComparisonMeasure}{PMML comparison measures}
#'
#' @export
similarity <- function(x,y,measure) {
  .check_similarity_inputs(x,y)
  if (measure=="simple_matching") {
    return(.simple_matching(x,y))
  } else if (measure=="jaccard") {
    return(.jaccard(x,y))
  } else if (measure=="tanimoto") {
    return(.tanimoto(x,y))
  } else {
    stop("not an allowed similarity measure")
  }
}

.simple_matching <- function(x,y) {
  a11 <- sum(x==1 & y==1) # number of times where x=1 and y=1
  a10 <- sum(x==1 & y==0) # number of times where x=1 and y=0
  a01 <- sum(x==0 & y==1) # number of times where x=0 and y=1
  a00 <- sum(x==0 & y==0) # number of times where x=0 and y=0
  return((a11 + a00) / (a11 + a10 + a01 + a00))
}

.jaccard <- function(x,y) {
  a11 <- sum(x==1 & y==1) # number of times where x=1 and y=1
  a10 <- sum(x==1 & y==0) # number of times where x=1 and y=0
  a01 <- sum(x==0 & y==1) # number of times where x=0 and y=1
  a00 <- sum(x==0 & y==0) # number of times where x=0 and y=0
  return(a11 / (a11 + a10 + a01))
  # return (sum(x & y) / sum(x | y))
}

.tanimoto <- function(x,y) {
  a11 <- sum(x==1 & y==1) # number of times where x=1 and y=1
  a10 <- sum(x==1 & y==0) # number of times where x=1 and y=0
  a01 <- sum(x==0 & y==1) # number of times where x=0 and y=1
  a00 <- sum(x==0 & y==0) # number of times where x=0 and y=0
  return((a11 + a00) / (a11 + 2*(a10 + a01) + a00))
}