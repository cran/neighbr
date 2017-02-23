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


# global variables
allowed_categorical_scoring_methods <- c("majority_vote") #TODO: c("majority_vote", "weighted_majority_vote")
allowed_continuous_scoring_methods <-  c("average") #TODO: c("average","median", "weighted_average")
allowed_distance_measures <- c("euclidean", "squared_euclidean") #TODO: c("euclidean", "squared_euclidean", "chebychev", "city_block", "minkowski")
allowed_similarity_measures <- c("simple_matching", "jaccard","tanimoto") #TODO: c("simple_matching", "jaccard", "tanimoto", "binary_similarity")

# slower for-loop version of function
.compute_distance_to_train_for <- function(test_set_row,train_set,num_train_rows,k,categorical_target,
                                           continuous_target,id,targets_list,comparison_measure) {

  return_table <- train_set[,names(train_set) %in% targets_list,drop=FALSE]

  return_table$measure <- rep(-1,num_train_rows)

  comparison_measure_function <- get(paste0(".",comparison_measure))

  train_set <- train_set[,!names(train_set) %in% targets_list,drop=FALSE] #replaces line 20 in loop below
  
  for(i in 1:nrow(train_set)) {
    row <- train_set[i,]
    return_table$measure[i] <- comparison_measure_function(row,test_set_row)
  }

  # if comparison_measure is used for distance (and not similarity), set decr_flag to FALSE for sorting
  if(comparison_measure %in% allowed_distance_measures) {decr_flag=FALSE} else {decr_flag=TRUE}

  return_table_ordered <- return_table[order(return_table$measure,decreasing=decr_flag),]
  return_table_ordered <- return_table_ordered[1:k,] #only return k neighbors

  return(return_table_ordered)
}


# faster apply version of function
.compute_distance_to_train <- function(test_set_row,train_set,num_train_rows,k,categorical_target,
                                       continuous_target,id,targets_list,comparison_measure) {
  
  return_table <- train_set[,names(train_set) %in% targets_list,drop=FALSE]
  
  return_table$measure <- rep(-1,num_train_rows)
  
  comparison_measure_function <- get(paste0(".",comparison_measure))
  
  train_set <- train_set[,!names(train_set) %in% targets_list,drop=FALSE] #replaces line 20 in loop below
  
  res <- apply(train_set,1,function(x) {comparison_measure_function(x,test_set_row)})
  return_table$measure <- res
  
  
  # if comparison_measure is used for distance (and not similarity), set decr_flag to FALSE for sorting
  if(comparison_measure %in% allowed_distance_measures) {decr_flag=FALSE} else {decr_flag=TRUE}
  
  return_table_ordered <- return_table[order(return_table$measure,decreasing=decr_flag),]
  return_table_ordered <- return_table_ordered[1:k,] #only return k neighbors
  
  return(return_table_ordered)
}









.calculate_categorical_winner <- function(scoring_method,train_set,target_array,target) {
  #depending on scoring_method, will call either .majority_vote() or weighted_majority_vote()

  winner <- .majority_vote(target_array=target_array,train_set=train_set,target=target)
  return(winner)
}

.calculate_continuous_winner <- function(scoring_method,train_set,target_array,target) {
  #depending on scoring_method, will call median, average, or weighted_average()

  winner <- mean(target_array)
  return(winner)
}


.majority_vote <- function(target_array,train_set,target) {

  tt <- table(target_array)

  max_tt <- max(tt)
  ties <- names(tt[tt==max_tt]) #if more than one category, then will have tie
  if (length(ties)==1) { #one category is most frequent among the neighbors
    return(ties)
  } else { #in a tie, if one category has larger number of cases in the training data
    # tt2 <- table(train_set$Adjusted)
    tt2 <- table(train_set[,target])
    max_tt2 <- max(tt2)
    ties2 <- names(tt2[tt2==max_tt2])
    if (length(ties2)==1) {
      return(ties2)
    } else { #still in a tie, one category is lexically smaller
      return(sort(ties2)[1]) #return the lexically smaller value
    }
  }
}

.check_distance_inputs <- function(x,y){
  if(length(x) != length(y)) {stop("input vectors must be of the same length")}
  if(is.factor(x) | is.factor(y)) {stop("input vectors must not be factors")}
}

.check_similarity_inputs <- function(x,y){
  if(length(x) != length(y)) {stop("input vectors must be of the same length")}
  if(is.factor(x) | is.factor(y)) {stop("input vectors must not be factors")}

  # x,y must consist of {0,1}. Can be all 0, or all 1.
  # if(!setequal(c(x,y),c(0,1))) {
  #   if(!setequal(c(x,y),c(0))) {
  #     if(!setequal(c(x,y),c(1))) {
  #       stop("all elements must consist of {0,1}")
  #     }
  #   }
  # }
  xy_unique_elements <- union(x,y)
  if (!all(xy_unique_elements %in% c(0,1))) {
    stop("all vectors must consist of {0,1}")
  }
}

#print progress in knn()
.print_progress <- function(i,total_rows,print_interval) {
  if ((i %% print_interval) == 0) {
    cat(paste0("working on test instance ", i, " of ", total_rows),fill=TRUE)
  }
}





