library(neighbr)
context("utility functions")

test_that(".check_distance_inputs produces correct error messages", {
  expect_silent(.check_distance_inputs(c(0,1),c(1,2)))
})

test_that(".check_distance_inputs produces no messages for correct input", {
  expect_silent(.check_distance_inputs(c(0,1),c(1,2)))
})


# .check_distance_inputs <- function(x,y){
#   if(length(x) != length(y)) {stop("input vectors must be of the same length")}
#   if(is.factor(x) | is.factor(y)) {stop("input vectors must not be factors")}
# }
