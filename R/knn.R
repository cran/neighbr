# Neighbr
#
# Copyright (c) 2017-2020, Software AG, Darmstadt, Germany and/or Software AG
# USA Inc., Reston, VA, USA, and/or its subsidiaries and/or its affiliates
# and/or their licensors.
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


#' Classification, regression, and clustering with k nearest neighbors.
#'
#' @param train_set Data frame containing the training instances, with features
#'   and any targets and IDs.
#' @param test_set Data frame containing the test instances, with feature
#'   columns only.
#' @param k Number of nearest neighbors.
#' @param categorical_target Categorical target variable.
#' @param continuous_target Continuous target variable.
#' @param comparison_measure Distance or similarity measure.
#' @param categorical_scoring_method Categorical scoring method.
#' @param continuous_scoring_method Continuous scoring method.
#' @param return_ranked_neighbors Number of ranked neighbors to return. A 0
#'   indicates no ranked neighbors. Must not exceed k.
#' @param id Column containing unique identifiers for each row in the training
#'   set. Only used when \code{return_ranked_neighbors} > 0.
#'
#' @return An object of class \code{neighbr}, which is a list of the following:
#'   \item{call}{The original call to \code{knn}.}
#'   \item{k}{Number of nearest neighbors.}
#'   \item{categorical_target}{Categorical target variable.}
#'   \item{continuous_target}{Continuous target variable.}
#'   \item{comparison_measure}{Distance or similarity measure.}
#'   \item{categorical_scoring_method}{Categorical scoring method.}
#'   \item{continuous_scoring_method}{Continuous scoring method.}
#'   \item{return_ranked_neighbors}{Number of ranked neighbors to return.}
#'   \item{id}{ID variable.}
#'   \item{features}{List of feature names.}
#'   \item{function_name}{Function name, used when generating PMML. One of
#'   "classification", "regression", "clustering", or "mixed".}
#'   \item{categorical_levels}{Levels of the categorical target.}
#'   \item{num_train_rows}{Number of training instances.}
#'   \item{num_test_rows}{Number of test instances.}
#'   \item{train_set}{Data frame with training instances.}
#'   \item{test_set_scores}{Data frame with scores for the test set.}
#'
#'
#' @details The algorithm can score data with continuous or logical features.
#'
#' The algorithm can predict either a continuous or categorical target, or both
#' (but no more than one of each), as well as return the closest neighbors
#' ranked by distance or similarity. If no continuous or categorical target is
#' provided, return_ranked_neighbors must be non-zero, and ranked neighbors will
#' be returned.
#'
#' There is no \code{predict} method for \code{knn}. The scored test set is returned
#' as part of the \code{neighbr} object. The data to be scored must be passed in
#' with the training data to \code{knn()}.
#'
#' Supported distance measures (used with continuous features): euclidean, squared_euclidean.
#'
#' Supported similarity measures (used with logical features): simple_matching, jaccard, tanimoto.
#'
#' Currently, only one type of categorical_scoring_method and
#' continuous_scoring_method are supported (majority vote and average, respectively).
#'
#' Logical features must consist of 0,1 or TRUE,FALSE values.
#'
#' Categorical non-logical features must be transformed before being used.
#'
#' The categorical target does not have to be of factor class, but is assumed to be not continuous.
#'
#' The distance and similarity measures in this package are based on those
#' defined in the
#' \href{http://dmg.org/pmml/v4-3/ClusteringModel.html#xsdElement_ComparisonMeasure}{PMML
#' specification}.
#'
#' Several of the elements in the returned list are only used when converting
#' the knn model to PMML (for example, \code{function_name}).
#'
#' For more details and examples, see the vignette by running the following:
#'
#' \code{vignette("neighbr-help")}
#'
#' @examples
#' # continuous features with continuous target, categorical target,
#' # and neighbor ranking
#'
#' data(iris)
#'
#' # add an ID column to the data for neighbor ranking
#' iris$ID <- c(1:150)
#'
#' # train set contains all predicted variables, features, and ID column
#' train_set <- iris[1:145,]
#'
#' # omit predicted variables or ID column from test set
#' test_set <- iris[146:150,-c(4,5,6)]
#'
#' fit <- knn(train_set=train_set,test_set=test_set,
#'            k=5,
#'            categorical_target="Species",
#'            continuous_target= "Petal.Width",
#'            comparison_measure="euclidean",
#'            return_ranked_neighbors=3,
#'            id="ID")
#'
#' @seealso \code{\link{similarity}}, \code{\link{distance}},
#' \href{http://dmg.org/pmml/v4-3/KNN.html}{PMML KNN specification}
#'
#' @export
knn <- function(train_set,test_set,
                 k=3,
                 categorical_target=NULL,
                 continuous_target=NULL,
                 comparison_measure,
                 categorical_scoring_method="majority_vote",
                 continuous_scoring_method="average",
                 return_ranked_neighbors=0,
                 id=NULL) {

  # initial checks

  # check for data frames
  if ((!is.data.frame(train_set)) | (!is.data.frame(test_set))) {
    stop("train_set and test_set must be data frames")
  }

  # check for missing data
  if (anyNA(train_set) | anyNA(test_set)) {
    stop("missing values not allowed in train_test or test_set")
  }

  # make a list of all targets that are not NULL:
  targets_list <- c()
  if(!is.null(categorical_target)) {targets_list <- c(targets_list,categorical_target)}
  if(!is.null(continuous_target)) {targets_list <- c(targets_list,continuous_target)}
  if(!is.null(id)) {targets_list <- c(targets_list,id)}

  #check that elements of target_list are included in train_set
  if (!all(targets_list %in% names(train_set))) {stop("all specified targets (categorical, continuous, and/or id) must be present in train_set")}


  #all train_set features that are not targets must be included in test_set; features must be in same order in both train and test
  features <- setdiff(names(train_set),targets_list) #remove targets from names(train_set)

  # if(!all(features==names(test_set))) {stop("features and column order in test_set must match those in train_set")}

  if(identical(features,names(test_set)) == FALSE) {stop("features and column order in test_set must match those in train_set")}

  # if return_ranked_neighbors is non-zero, check that id is specified
  if (return_ranked_neighbors<0) {stop("return_ranked_neighbors must be 0 or greater")}
  if (return_ranked_neighbors>0 & is.null(id)) {stop("if return_ranked_neighbors > 0, id column must be specified")}

  # check that at least one target is specified
  if (is.null(categorical_target) & is.null(continuous_target) & return_ranked_neighbors==0) {
    stop("specify a target or set return_ranked_neighbors to greater than 0")
  }

  # check that ID in train_set is unique
  if (!is.null(id)) {if(anyDuplicated(train_set[,id])!=0) {stop("id column in train_set must be unique")}}

  # check that return_ranked_neighbors <= k
  if (return_ranked_neighbors > k) {stop("return_ranked_neighbors must not be more than k")}

  # check that scoring methods and comparison measure are allowed
  if (!is.element(categorical_scoring_method, allowed_categorical_scoring_methods)) {
    stop(paste(categorical_scoring_method, "is not an allowed categorical scoring method"))
  }

  if (!is.element(continuous_scoring_method, allowed_continuous_scoring_methods)) {
    stop(paste(continuous_scoring_method, "is not an allowed continuous scoring method"))
  }

  if (!is.element(comparison_measure, c(allowed_distance_measures,allowed_similarity_measures))) {
    stop(paste(comparison_measure, "is not an allowed distance or similarity measure"))
  }

  # check that all train_set and test_set features do not contain factors
  if ((any(sapply(train_set[,features],class)=="factor")) | (any(sapply(test_set,class)=="factor"))) {
    stop("train_set and test_set must not contain factors")
  }

  # if comparison_measure is a logical similarity measure, all features must be 1 or 0, and must not be factors
  if (is.element(comparison_measure, allowed_similarity_measures)) {

    #check that all features in train_set and test_set consist of {0,1} or {TRUE,FALSE}
    # train_set_unique_elements <- sort(unique(unlist(train_set[,features]))) #sort is unnecessary?
    train_set_unique_elements <- unique(unlist(train_set[,features]))
    if (!all(train_set_unique_elements %in% c(0,1))) {
      stop("if using a similarity measure, all train_set features must consist of {0,1} or {TRUE,FALSE}")
    }

    # test_set_unique_elements <- sort(unique(unlist(test_set))) #sort is unnecessary?
    test_set_unique_elements <- unique(unlist(test_set))
    if (!all(test_set_unique_elements %in% c(0,1))) {
      stop("if using a similarity measure, all test_set features must consist of {0,1} or {TRUE,FALSE}")
    }
  }

  #get levels for categorical target if it exists
  categorical_levels <- NULL

  # from categorical_target, continuous_target, and return_ranked_neighbors, determine type of task (classification, regression, clustering)
  if (!is.null(categorical_target) & !is.null(continuous_target)) {
    function_name <- "mixed"
    categorical_levels <- levels(train_set[,categorical_target])
  } else if (!is.null(categorical_target)) {
    function_name <- "classification"
    categorical_levels <- levels(train_set[,categorical_target])
  } else if (!is.null(continuous_target)) {
    function_name <- "regression"
  } else if (is.null(categorical_target) & is.null(continuous_target)) {
    function_name <- "clustering"
  } else {
    stop("cannot determine function_name from categorical_target and continous_target")
  }

  num_train_rows <- nrow(train_set)
  num_test_rows <- nrow(test_set)

  # set up the test_set_scores data frame and any predicted target columns
  test_set_scores <- test_set[,FALSE] #test_set_scores has the correct number of rows, but no columns

  if(!is.null(categorical_target)) { test_set_scores$categorical_target <- rep(-99, num_test_rows) } # add a categorical_target column with dummy vals
  if(!is.null(continuous_target)) { test_set_scores$continuous_target <- rep(-99, num_test_rows) } # add a continuous_target column with dummy vals


  if(return_ranked_neighbors>0) {
    ranked_neighbors_names <- paste("neighbor", c(1:return_ranked_neighbors), sep="")
    ranks_df <- data.frame(matrix(vector(), num_test_rows, return_ranked_neighbors, dimnames=list(c(), ranked_neighbors_names)), stringsAsFactors=F)
    test_set_scores <- cbind(test_set_scores,ranks_df) # add neighbor rank columns with dummy vars
  }

  # make predictions for every row in test_set
  for (i in 1:num_test_rows) {

    # .print_progress(i,num_test_rows,5) #print progress

    test_set_row <- test_set[i,]

    distances <- .compute_distance_to_train(test_set_row, train_set, num_train_rows, k=k,
                                           categorical_target=categorical_target, continuous_target=continuous_target,
                                           id=id,targets_list=targets_list,comparison_measure=comparison_measure)

    #calculate categorical winner
    if (!is.null(categorical_target)){
      categorical_winner <- .calculate_categorical_winner(scoring_method = categorical_scoring_method, train_set = train_set,
                                                         target_array = distances[,categorical_target], target=categorical_target)
      test_set_scores$categorical_target[i] <- categorical_winner
    }

    #calculate continuous winner
    if (!is.null(continuous_target)){
      continuous_winner <- .calculate_continuous_winner(scoring_method = continuous_scoring_method,train_set = train_set,
                                                       target_array = distances[,continuous_target],target=continuous_target)
      test_set_scores$continuous_target[i] <- continuous_winner
    }

    #calculate ranked neighbors
    if (return_ranked_neighbors > 0){
      ranked_neighbors <- distances[,id][1:return_ranked_neighbors]
      ranked_columns_end <- ncol(test_set_scores)
      ranked_columns_start <- ranked_columns_end - return_ranked_neighbors + 1
      test_set_scores[i,ranked_columns_start:ranked_columns_end] <- ranked_neighbors
    }
  }

  call <- match.call()

  # create the return object
  return_obj <- list(call=call,
                     k=k,
                     categorical_target=categorical_target,
                     continuous_target=continuous_target,
                     comparison_measure=comparison_measure,
                     categorical_scoring_method=categorical_scoring_method,
                     continuous_scoring_method=continuous_scoring_method,
                     return_ranked_neighbors=return_ranked_neighbors,
                     id=id,
                     features=features,
                     function_name=function_name,
                     categorical_levels=categorical_levels,
                     num_train_rows=num_train_rows,
                     num_test_rows=num_test_rows,
                     train_set=train_set,
                     test_set_scores=test_set_scores)

  class(return_obj) <- "neighbr"
  return(return_obj)

}

#' @export
print.neighbr <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
}
