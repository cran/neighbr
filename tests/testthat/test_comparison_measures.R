context("comparison measure functions")

test_that("distance matches expected output", {
  expect_equal(distance(c(1),c(1),"euclidean"),0)
  expect_equal(distance(c(-1,2.5,3),c(1,5,8.9),"euclidean"),6.71267457873)
  
  expect_equal(distance(c(1),c(1),"squared_euclidean"),0)
  expect_equal(distance(c(-1,2.5,3),c(1,5,8.9),"squared_euclidean"),45.0600)
})

test_that("similarity matches expected output", {
  expect_equal(similarity(c(1),c(1),"simple_matching"),1)
  expect_equal(similarity(c(0,0,1,1),c(0,1,0,0),"simple_matching"),0.25)
  
  expect_equal(similarity(c(1),c(1),"jaccard"),1)
  expect_equal(similarity(c(0,0,1,1),c(0,1,0,0),"jaccard"),0)
  
  expect_equal(similarity(c(1),c(1),"tanimoto"),1)
  expect_equal(similarity(c(0,0,1,1),c(0,1,0,0),"tanimoto"),0.142857142857)
})


test_that("comparison measure inputs are of same length", {
  expect_error(distance(c(1),c(1,2),"euclidean"),"input vectors must be of the same length")
  expect_error(distance(c(1,2),c(1,2,3),"squared_euclidean"),"input vectors must be of the same length")
  expect_error(similarity(c(1,0,0),c(1,0),"simple_matching"),"input vectors must be of the same length")
  expect_error(similarity(c(1),c(1,0),"jaccard"),"input vectors must be of the same length")
  expect_error(similarity(c(1,0,0),c(1,0),"euclidean"),"input vectors must be of the same length")
})

test_that("comparison measure inputs are not factors", {
  expect_error(distance(as.factor(c(0,2,0.4,1)),c(0,1,5,1),"euclidean"),"input vectors must not be factors")
  expect_error(distance(as.factor(c(0,2,0.4,1)),c(0,1,5,1),"squared_euclidean"),"input vectors must not be factors")
  expect_error(similarity(as.factor(c(0,1,0,1)),as.factor(c(0,1,1,1)),"simple_matching"),"input vectors must not be factors")
  expect_error(similarity(c(0,1,0),as.factor(c(0,1,1)),"jaccard"),"input vectors must not be factors")
  expect_error(similarity(as.factor(c(1,0,1)),c(1,1,1),"tanimoto"),"input vectors must not be factors")
})

test_that("similarity measure input elements consist of {0,1}", {
  expect_error(similarity(c(0,1,0),c(0,1,2),"simple_matching"),"all vectors must consist of {0,1}")
  expect_error(similarity(c(4.3,1),c(0,1),"jaccard"),"all vectors must consist of {0,1}")
  expect_error(similarity(c(0,1,0,-9.3),c(0,1,2,0.8),"tanimoto"),"all vectors must consist of {0,1}")
})

###deprecated

# test_that("euclidean matches", {
#   expect_equal(euclidean(c(1),c(1)),0)
#   expect_equal(euclidean(c(-1,2.5,3),c(1,5,8.9)),6.71267457873)
# })

# test_that("squared_euclidean matches", {
#   expect_equal(squared_euclidean(c(1),c(1)),0)
#   expect_equal(squared_euclidean(c(-1,2.5,3),c(1,5,8.9)),45.0600)
# })
# 
# test_that("simple_matching matches", {
#   expect_equal(simple_matching(c(1),c(1)),1)
#   expect_equal(simple_matching(c(0,0,1,1),c(0,1,0,0)),0.25)
# })
# 
# test_that("jaccard matches", {
#   expect_equal(jaccard(c(1),c(1)),1)
#   expect_equal(jaccard(c(0,0,1,1),c(0,1,0,0)),0)
# })
# 
# test_that("tanimoto matches", {
#   expect_equal(tanimoto(c(1),c(1)),1)
#   expect_equal(tanimoto(c(0,0,1,1),c(0,1,0,0)),0.142857142857)
# })
# 
# 
# test_that("comparison measure inputs are of same length", {
#   expect_error(euclidean(c(1),c(1,2)),"input vectors must be of the same length")
#   expect_error(squared_euclidean(c(1,2),c(1,2,3)),"input vectors must be of the same length")
#   expect_error(simple_matching(c(1,0,0),c(1,0)),"input vectors must be of the same length")
#   expect_error(jaccard(c(1),c(1,0)),"input vectors must be of the same length")
#   expect_error(tanimoto(c(1,0,0),c(1,0)),"input vectors must be of the same length")
# })
# 
# test_that("comparison measure inputs are not factors", {
#   expect_error(euclidean(as.factor(c(0,2,0.4,1)),c(0,1,5,1)),"input vectors must not be factors")
#   expect_error(squared_euclidean(as.factor(c(0,2,0.4,1)),c(0,1,5,1)),"input vectors must not be factors")
#   expect_error(simple_matching(as.factor(c(0,1,0,1)),as.factor(c(0,1,1,1))),"input vectors must not be factors")
#   expect_error(jaccard(c(0,1,0),as.factor(c(0,1,1))),"input vectors must not be factors")
#   expect_error(tanimoto(as.factor(c(1,0,1)),c(1,1,1)),"input vectors must not be factors")
# })
# 
# test_that("similarity measure input elements consist of {0,1}", {
#   expect_error(simple_matching(c(0,1,0),c(0,1,2)),"all vectors must consist of {0,1}")
#   expect_error(jaccard(c(4.3,1),c(0,1)),"all vectors must consist of {0,1}")
#   expect_error(tanimoto(c(0,1,0,-9.3),c(0,1,2,0.8)),"all vectors must consist of {0,1}")
# })
